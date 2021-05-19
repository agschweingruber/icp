import torch
from src.utils import pop_feature


class SequentialLoss:
    def __init__(self, categorical_feature_idcs, class_weights, target_names,
                 norm_seq_len):
        self.categorical_feature_idcs = categorical_feature_idcs
        self.target_names = target_names
        self.norm_seq_len = norm_seq_len
        self.class_weights = class_weights
        self.cat_loss_fn = torch.nn.BCEWithLogitsLoss(reduction='none')
        self.regression_loss_fcn = torch.nn.MSELoss(reduction='none')

    def __call__(self, pred, target, mask):
        """Calculates the loss and considers missing values in the loss given by mask"""
        # Apply mask:
        target[mask] = 0.0
        step_mask = mask.sum(-1).bool()

        # Pop out the categoricals:
        if self.categorical_feature_idcs:
            categorical_loss = 0
            pred_categorical, pred = pop_feature(pred, self.categorical_feature_idcs)
            target_categorical, target = pop_feature(target, self.categorical_feature_idcs)
            for idx, cat_idx in enumerate(self.categorical_feature_idcs):
                cat_preds = pred_categorical[:, :, idx]
                cat_targets = target_categorical[:, :, idx]
                cat_loss = self.cat_loss_fn(cat_preds, cat_targets)
                if self.class_weights:
                    cat_loss[cat_targets == 1] *= self.class_weights[cat_idx]
                categorical_loss += cat_loss
            categorical_loss[step_mask] = 0

        # Calculate the loss of the regression on all other features:
        rest_loss = self.regression_loss_fcn(pred, target)
        rest_loss[step_mask] = 0

        sum_rest_loss = rest_loss.sum(dim=-1).sum(dim=-1)
        if self.categorical_feature_idcs:
            sum_categorical_loss = categorical_loss.sum(-1)#.sum(-1)
        else:
            sum_categorical_loss = 0.0

        # Norm loss per seq len and per non NAN targets.
        # Basically, this reduces the weight of longer sequences and of sequences with more NaNs.
        # Add 1 to avoid zero division:
        if self.norm_seq_len:
            count_per_pat = (~step_mask).sum(dim=-1) + 1
        else:
            count_per_pat = 1

        # Aggregate losses:
        loss = ((sum_rest_loss + sum_categorical_loss) /
                count_per_pat).mean()
        return loss
