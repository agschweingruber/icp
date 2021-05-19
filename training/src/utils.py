import torch


def pop_feature(input_, feat_idx):
    """Pops out the feature at idx feat_idx in the last dimension of the list of tensors or tensor."""
    if not isinstance(feat_idx, list):
        feat_idx = [feat_idx]
    # Get indices of features to keep:
    idcs = list(filter(lambda x: x not in feat_idx,
                       range(input_[0].shape[-1])))
    if isinstance(input_, list):
        out = [seq[:, feat_idx] for seq in input_]
        popped = [seq[:, idcs] for seq in input_]
    elif isinstance(input_, torch.Tensor):
        if len(input_.size()) == 3:
            out = input_[:, :, feat_idx]
            popped = input_[:, :, idcs]
        elif len(input_.size()) == 2:
            out = input_[:, feat_idx]
            popped = input_[:, idcs]
    else:
        raise TypeError("Unknown input type: ", type(input_))
    return out, popped


class Collate:
    def __call__(self, batch):
        inputs = [tensor[0] for tensor in batch]
        targets = [tensor[1] for tensor in batch]
        idxs = [tensor[2] for tensor in batch]
        lens = torch.tensor([tensor[3] for tensor in batch])
        inputs = torch.stack(inputs)
        targets = torch.stack(targets)

        return inputs, targets, idxs, lens
