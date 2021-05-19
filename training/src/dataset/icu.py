import os
import random
import torch
import pandas as pd
from tqdm import tqdm
from torch.nn.utils.rnn import pad_sequence
import math


class IcuDataset(torch.utils.data.Dataset):
    def __init__(self, data_path, args, data_mode=''):
        self.hidden_size = args.hidden_size
        self.block_size = args.block_size
        db_feat = args.db_feat
        outcome_tar = args.outcome_tar
        data_path = data_path.replace('data/', '', 1)
        mode = args.mode
        input_mask = args.input_mask

        labor_inputs = ',Labor_aPTT,Labor_Erythrocyten,Labor_EVB,Labor_Harnstoff_N,Labor_Hk,Labor_INR,Labor_Kreatinin,Labor_Leukocyten,Labor_Lymphocyten,Labor_Magnesium,Labor_MCH,Labor_MCHC,Labor_MCV,Labor_Monocyten,Labor_Neutrophile,Labor_Thrombocyten,Labor_Phosphat,Labor_Triglyceride,Labor_Basophile,Labor_Cholesterin,Labor_CK,Labor_CK_MB,Labor_Eosinophile,Labor_Troponin,Labor_TSH,Labor_Albumin,Labor_Alk,Labor_ALT,Labor_AST,Labor_LDH,Labor_Lipase,Labor_pankreasspez,Labor_CRP,Labor_fT3,Labor_fT4'


        # hard-coded training modes
        if mode == 'phase':
            categorical_icp = True
            target_block_names = ['ICP_pred']
        elif mode == 'icp':
            categorical_icp = False
            target_block_names = ['ICP_critical_long', 'ICP_critical_short']
            if 'Vital_ICP,Vital_CPP' not in input_mask:
                input_mask += ',Vital_ICP,Vital_CPP'
            args.input_mask = input_mask
        else:
            pass

        if not db_feat:
            input_mask += ',DB_eICU,DB_MIMIC,DB_UKE'
        if hasattr(args, "block_labor") and  args.block_labor:
            input_mask += labor_inputs

        if not outcome_tar:
            target_block_names += ['Outcome']

        target_block_names += ['Period', 'Period_pred']

        # Load data:
        self.input = pd.read_pickle(os.path.join("data", data_path, "inputs.pkl"))
        self.target = pd.read_pickle(os.path.join("data", data_path, "targets.pkl"))

        if args.verbose and data_mode == 'init':
            print("INPUT", self.input.describe())
            print("TARGET", self.target.describe())

        cache_path = os.path.join('data/cache', data_path)
        inputs_pt = os.path.join(cache_path, 'inputs.pt')
        targets_pt = os.path.join(cache_path, 'targets.pt')

        if os.path.isdir(cache_path) and os.path.isfile(inputs_pt)\
                and os.path.isfile(targets_pt):
            self.inputs = torch.load(inputs_pt)
            self.targets = torch.load(targets_pt)
        else:
            self.inputs = []
            self.targets = []
            pbar = tqdm(desc='Dataset Preparation' ,total=len(self.input))
            for idx, pat_id in enumerate(self.input["Pat_ID"].unique()):
                pat_idx = self.input["Pat_ID"] == pat_id
                pat_input_df = self.input.loc[pat_idx].drop(columns=["Pat_ID"])
                pat_target_df = self.target.loc[pat_idx.tolist()]
                self.inputs.append(torch.tensor(pat_input_df.astype(float).to_numpy()))
                self.targets.append(torch.tensor(pat_target_df.astype(float).to_numpy()))
                pbar.update(pat_idx.sum())
            pbar.close()
            if not os.path.isdir(cache_path):
                os.makedirs(cache_path)
            torch.save(self.inputs, inputs_pt)
            torch.save(self.targets, targets_pt)

        self.lens = torch.tensor([len(_input) for _input in self.inputs])
        self.feature_names = list(self.input.drop(columns=["Pat_ID"]).columns)
        self.target_names = list(self.target.columns)

        # Mask not used categories
        self.input_mask = torch.ones(len(self.feature_names)).bool()
        if input_mask:
            masking_names = [str(item) for item in input_mask.split(',') if str(item)]
            mask = [self.feature_names.index(feature) for feature in masking_names if feature in self.feature_names]
            self.input_mask[mask] = False

        if args.feature_pass:
            # Features that are used:
            feature_pass_names = [str(item) for item in args.feature_pass.split(',')]
            feature_block_names = [feature for feature in self.feature_names if feature not in feature_pass_names and feature]
        else:
            feature_block_names = []

        self.target_mask = torch.ones(len(self.target_names)).bool()

        # Now we have the names of features/targets that are not used and should be masked:
        for feature in feature_block_names:
            input_idx = self.feature_names.index(feature)
            self.input_mask[input_idx] = False
        for target in target_block_names:
            if target in self.target_names:
                target_idx = self.target_names.index(target)
                self.target_mask[target_idx] = False

        if data_mode == 'init':
            # Apply masks to names:
            if torch.any(~self.input_mask):
                print("Filtering in input: ", end="")
                for idx, name in enumerate(self.feature_names):
                    if not self.input_mask[idx]:
                        print(name, end="")
                print()

        self.feature_names = [name for idx, name in enumerate(self.feature_names) if self.input_mask[idx]]
        self.target_names = [name for idx, name in enumerate(self.target_names) if self.target_mask[idx]]

        self.categorical_feature_idcs = []
        if categorical_icp:
            self.categorical_feature_idcs += [self.target_names.index(name) for name in self.target_names  if name.startswith("ICP_critical_")]
        if outcome_tar:
            self.categorical_feature_idcs += [self.target_names.index("Outcome")]

        if data_mode == 'init' and args.verbose:
            print("Feature names: ", self.feature_names)
            print("Target names: ", self.target_names)
            print("Num Features: ", len(self.feature_names))
            print("Num targets: ", len(self.target_names))

        # Apply masks:
        self.inputs = [tens[:, self.input_mask] for tens in self.inputs]
        self.targets = [tens[:, self.target_mask] for tens in self.targets]

        self.class_weights = {}
        if self.categorical_feature_idcs:
            for _cat_idx in self.categorical_feature_idcs:
                _cat_neg = torch.tensor([(tar[:, _cat_idx] == 0.0).sum() for tar in self.targets]).sum()
                _cat_pos = torch.tensor([(tar[:, _cat_idx] == 1.0).sum() for tar in self.targets]).sum()
                self.class_weights[_cat_idx] = _cat_neg / _cat_pos.float()
                if data_mode == 'init' and args.verbose:
                    print("Target Ratio", self.target_names[_cat_idx], _cat_neg.float() / (_cat_neg + _cat_pos), " Weight: ", self.class_weights[_cat_idx])

        # Calculate size of hidden state storage:
        max_seq_len = self.block_size if self.block_size else max(self.inputs, key=lambda seq: seq.shape[0]).shape[0]

        if not self.block_size:
            self.inputs = pad_sequence(self.inputs, batch_first=True, padding_value=0)
            self.targets = pad_sequence(self.targets, batch_first=True, padding_value=math.nan)

        self.input_tensor_shape = (len(self.inputs), max_seq_len, len(self.feature_names))

    def __len__(self):
        return len(self.targets)

    def __getitem__(self, index):
        x = self.inputs[index]
        y = self.targets[index]
        seq_len = self.lens[index]

        if self.block_size:
            total_len =  self.block_size
            start_idx = random.randint(0, seq_len - total_len) if seq_len > total_len else 0
            end_idx = start_idx + total_len
            x = x[start_idx:end_idx]
            y = y[start_idx:end_idx]

            if len(x) < self.block_size:
                padding = torch.zeros(size=(self.block_size - seq_len, x.shape[1]), dtype=x.dtype)
                x = torch.cat((padding, x))

            if self.block_size:
                y = y[-1]
                y = y.unsqueeze(0)

            seq_len = end_idx - start_idx
        else:
            start_idx = 0

        return x.float(), y.float(), (index, start_idx), seq_len

    def get_model_input_output(self):
        """ Returns full shape for input and number of features for target for first timestep of first patient"""
        return self.input_tensor_shape, len(self.targets[0][0])
