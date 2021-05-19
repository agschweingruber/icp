import torch
import numpy as np
import os
from pytorch_lightning.loggers import MLFlowLogger
from pytorch_lightning.metrics.functional import accuracy, auroc, roc
from tqdm import tqdm


class Metrics(object):

    def get_opt_threshold(self, categorical_feature_idcs):
        max_seq_len = len(max(self.all_targets, key=lambda x: len(x)))
        _thresholds = torch.empty(max_seq_len)
        if self.args.variable_threshold:
            targets = [target[:, categorical_feature_idcs] for target in self.all_targets]
            preds = [torch.sigmoid(pred[:, categorical_feature_idcs]) for pred in self.all_preds]
            for time_idx in range(max_seq_len):
                targets_timestep = torch.tensor([target[time_idx] for target in targets if len(target) > time_idx])
                preds_timestep = torch.tensor([pred[time_idx] for pred in preds if len(pred) > time_idx])
                fpr, tpr, thresholds = roc(preds_timestep, targets_timestep)
                optimal_idx = np.argmax(tpr - fpr)
                _thresholds[time_idx] = thresholds[optimal_idx]
        else:
             flat_targets = torch.cat([target[:, categorical_feature_idcs] for target in self.all_targets]).cpu()
             flat_preds = torch.cat([torch.sigmoid(pred[:, categorical_feature_idcs]) for pred in self.all_preds]).cpu()
             fpr, tpr, thresholds = roc(flat_preds, flat_targets)
             optimal_idx = np.argmax(tpr - fpr)
             _thresholds[:] = thresholds[optimal_idx]
        return _thresholds

    def find_optimal_categorical_threshold(self):
        categorical_feature_idcs = self.categorical_feature_idcs
        if isinstance(categorical_feature_idcs, list):
            thr = [self.get_opt_threshold(cat_idx) for cat_idx in self.categorical_feature_idcs]
            self.categorical_thresholds = thr
            return torch.stack(thr)
        else:
            thr = self.get_opt_threshold(categorical_feature_idcs).unsqueeze(0)
            self.categorical_thresholds = thr
            return thr

    def calc_categorical_metrics(self, categorical_feature_idcs, thr_idx):
        # Define helper functions:
        select_func = lambda x: x[:, categorical_feature_idcs].flatten()
        # Get relevant timeframe for targets and preds:
        thresholds_flat = torch.cat([self.categorical_thresholds[thr_idx][:len(target)] for target in self.all_targets]).cpu()
        targets_flat = torch.cat([select_func(target) for target in self.all_targets]).cpu()
        preds_flat = torch.cat([torch.sigmoid(select_func(pred)) for pred in self.all_preds]).cpu()
        pred_tar_thr = [[_p, _t, _thr] for _p, _t, _thr in zip(preds_flat, targets_flat, thresholds_flat) if not torch.isnan(_t)]
        preds_flat = torch.stack([_d[0] for _d in pred_tar_thr])
        targets_flat = torch.stack([_d[1] for _d in pred_tar_thr]).int()
        thresholds_flat = torch.stack([_d[2] for _d in pred_tar_thr])
        preds_bool = (preds_flat > thresholds_flat).float()
        auc = auroc(preds_flat, targets_flat)
        acc = accuracy(preds_bool.int(), targets_flat)
        return auc, acc

    def categorical_metrics(self):
        metrics_by_idx = torch.tensor([list(self.calc_categorical_metrics(cat_idx, thr_idx))
                                           for thr_idx, cat_idx in enumerate(self.categorical_feature_idcs)])
        metric_by_categorical_idcs = {}
        for i, idx in enumerate(self.categorical_feature_idcs):
            metric_by_categorical_idcs[self.target_names[idx] + '_auc'] = metrics_by_idx[i, 0].item()
            metric_by_categorical_idcs[self.target_names[idx] + '_acc'] = metrics_by_idx[i, 1].item()

        return metric_by_categorical_idcs

    def save_preds_and_targets(self, to_disk=False):
        """Save data, targets and predictions"""

        if to_disk and isinstance(self.logger, MLFlowLogger) and hasattr(self.logger, '_experiment_id'):
            self.path = os.path.join('mlruns', self.logger._experiment_id, self.logger._run_id, 'artifacts')
            end_device = torch.device("cpu")
        else:
            to_disk = False
            end_device = self.device

        all_data = []
        all_targets = []
        all_preds = []
        val_loader = self.val_dataloader()

        valid_epochs = 1
        if self.args.block_size:
            valid_epochs = 200 if to_disk else 20

        for _ in tqdm(range(valid_epochs), disable=False if to_disk else True, total=valid_epochs):
            for idx, (data, target, idxs, lens) in enumerate(val_loader):
                for pat_data, pat_target, pat_len in zip(data, target, lens):
                   all_data.append(pat_data[:pat_len].to(end_device))
                   all_targets.append(pat_target[:pat_len].to(end_device))

                with torch.no_grad():
                    data = data.to(self.device)
                    preds = self(data).type_as(data)
                if to_disk and self.mode == 'phase':
                    preds = torch.sigmoid(preds)
                for _pred, _len in zip(preds, lens):
                    all_preds.append(_pred[:_len].to(end_device))

        if to_disk:
            feature_names = np.array(val_loader.dataset.dataset.dataset.feature_names)
            target_names = np.array(val_loader.dataset.dataset.dataset.target_names)
            torch.save(feature_names, os.path.join(self.path, 'feature_names.pt'))
            torch.save(target_names, os.path.join(self.path, 'target_names.pt'))
            parent_folder = '/'.join(self.path.split('/')[:2])
            data_symlink = os.path.join(parent_folder, f'data{self.all_data[0].sum().abs().int().item() + sum([ord(x) for x in self.args.data])}.pt')
            if not os.path.isfile(data_symlink):
                torch.save(all_data, os.path.abspath(data_symlink))
            os.symlink(os.path.abspath(data_symlink), os.path.join(self.path, 'data.pt'))
            torch.save(all_targets, os.path.join(self.path, 'target.pt'))
            torch.save(all_preds, os.path.join(self.path, 'pred.pt'))
            torch.save(self.args.__dict__, os.path.join(self.path, 'args.pt'))
            self.to("cpu")
            torch.save(self, os.path.join(self.path, 'model.pt'))
            self.save_preds_and_targets_testset(self.path)
        else:
            self.all_data = all_data
            self.all_targets = all_targets
            self.all_preds = all_preds

    def save_preds_and_targets_testset(self, path):
        test_loader = self.test_dataloader()
        end_device = torch.device("cpu")
        test_preds = []
        test_targets = []

        for idx, (data, target, idxs, lens) in enumerate(test_loader):
            for pat_data, pat_target, pat_len in zip(data, target, lens):
                test_targets.append(pat_target[:pat_len].to(end_device))

            with torch.no_grad():
                data = data.to(self.device)
                preds = self(data).type_as(data)
            if self.mode == 'phase':
                preds = torch.sigmoid(preds)
            for _pred, _len in zip(preds, lens):
                test_preds.append(_pred[:_len].to(end_device))

        torch.save(test_targets, os.path.join(self.path, 'target_test.pt'))
        torch.save(test_preds, os.path.join(self.path, 'pred_test.pt'))
