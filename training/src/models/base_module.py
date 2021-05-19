import os
import torch
from .loss import SequentialLoss
from pytorch_lightning.core.lightning import LightningModule
from torch.utils.data import DataLoader
from src.utils import Collate
from src.dataset import IcuDataset
from .metrics import Metrics


class BaseModule(LightningModule, Metrics):

    _metrics = {}

    def ___init__(self, args):
        super().__init__()
        self.args = args
        self._get_data(args, data_mode='init')
        self.loss_func = SequentialLoss(self.categorical_feature_idcs, self.class_weights, self.target_names,
                                        self.args.norm_seq_len)
        self.learning_rate = args.lr
        self.batch_size = args.batch_size
        self.hidden_size = args.hidden_size
        self.dropout_val = args.dropout
        self.mode = args.mode
        self.categorical_outcome = args.categorical_outcome
        self.batch_size = args.batch_size

    def _get_data(self, args, data_mode=''):
        self.data_path = args.data.replace('data/', '')
        self.val_mode = args.val_mode
        self.split_id = args.split_id
        baseset = IcuDataset(self.data_path, args, data_mode)
        self.input_tensor_shape, self.out_size = baseset.get_model_input_output()
        # Define dataset
        dev_idcs = torch.load(os.path.join("data", self.data_path, "dev_idcs.pt"))
        dev_dataset = torch.utils.data.Subset(baseset, dev_idcs)

        if self.val_mode == 'full':
            val_idcs = torch.load(os.path.join("data", self.data_path, "val_idcs.pt"))
        elif self.val_mode == 'split3':
            val_idcs = torch.load(os.path.join("data", self.data_path, "3", f"{self.split_id}.pt"))
        elif self.val_mode == 'split5':
            val_idcs = torch.load(os.path.join("data", self.data_path, "5", f"{self.split_id}.pt"))
        else:
            raise ValueError("Not a valid val_mode")

        train_idcs = [idx for idx in range(len(dev_dataset)) if idx not in val_idcs]

        test_idcs = torch.load(os.path.join("data", self.data_path, "test_idcs.pt"))
        testset = torch.utils.data.Subset(baseset, test_idcs)
        trainset = torch.utils.data.Subset(dev_dataset, train_idcs)
        validset = torch.utils.data.Subset(dev_dataset, val_idcs)

        if data_mode == 'init':
            self.feature_names = validset.dataset.dataset.feature_names
            self.target_names = validset.dataset.dataset.target_names
            self.class_weights = baseset.class_weights
            self.categorical_feature_idcs = validset.dataset.dataset.categorical_feature_idcs

            print("Len baseset: ", len(baseset))
            print('Input shape', self.input_tensor_shape)
            print("Devset size: ", len(dev_dataset))
            print("Trainset size: ", len(trainset))
            print("Validationset size: ", len(validset))
            print("Testset size: ", len(testset))

        return trainset, validset, testset


    def train_dataloader(self):
        return DataLoader(self._get_data(self.args)[0],
                          batch_size=self.batch_size,
                          num_workers=self.args.num_workers,
                          pin_memory=self.args.pin_mem,
                          shuffle=True,
                          collate_fn=Collate())

    def val_dataloader(self):
        return DataLoader(self._get_data(self.args)[1],
                          batch_size=self.batch_size,
                          num_workers=self.args.num_workers,
                          shuffle=False,
                          collate_fn=Collate())

    def test_dataloader(self):
        return DataLoader(self._get_data(self.args)[2],
                          batch_size=self.batch_size,
                          num_workers=self.args.num_workers,
                          shuffle=False,
                          collate_fn=Collate())

    @property
    def metrics(self):
        """ Dict with metrics """
        return self._metrics

    def get_progress_bar_dict(self):
        tqdm_dict = super().get_progress_bar_dict()
        if 'v_num' in tqdm_dict:
            del tqdm_dict['v_num']
        return tqdm_dict

    def on_sanity_check_start(self):
        pass

    def on_sanity_check_end(self):
        pass

    def configure_optimizers(self):
        return torch.optim.Adam(self.parameters(), lr=self.args.lr)

    def get_loss(self, data, target):
        mask = torch.isnan(target).type_as(data).bool()
        pred = self(data)
        loss = self.loss_func(pred, target, mask)
        return loss

    def training_step(self, batch, batch_idx):
        data, target, idcs, lens = batch
        # Augment data: add Gaussian noise
        if self.args.gauss_std:
            data = torch.normal(data, self.args.gauss_std)
        # Get loss
        loss = self.get_loss(data, target)
        # Log loss
        self.log('train_loss', loss, on_step=True, on_epoch=True, prog_bar=True)
        return loss

    def on_validation_start(self):
        self.save_preds_and_targets()

    def validation_step(self, batch, batch_idx):
        data, target, idcs, lens = batch
        loss = self.get_loss(data, target)
        self.log('val_loss', loss, on_step=True, on_epoch=False, prog_bar=True, logger=False)
        return loss

    def validation_epoch_end(self, outputs):
        self.save_preds_and_targets()
        self._metrics['val_loss'] = torch.tensor(outputs[0]).mean()

        if self.categorical_feature_idcs:
            self.find_optimal_categorical_threshold()
            categorical_metrics = self.categorical_metrics()
            self._metrics.update(categorical_metrics)

        self.log_dict(self._metrics, prog_bar=True)

    def test_step(self, batch, batch_idx):
        data, target, idcs, lens = batch
        loss = self.get_loss(data, target)
        self.log('test_loss', loss, on_epoch=True, on_step=False, prog_bar=True, logger=False)
        return loss

    def __getstate__(self):
        excluded_subnames = [
            '_LRFinder', '_get_new_optimizer', '_in_memory', 'all_data', 'all_targets', 'all_preds',
            'train_dataloader', 'val_dataloader', 'test_dataloader', 'hidden', 'logger', 'trainer', 'baseset',
            'dev_dataset', 'trainset', 'validset', 'testset'
        ]
        state = {k: v for k, v in self.__dict__.items() if all([not subname in k for subname in excluded_subnames])}

        return state
