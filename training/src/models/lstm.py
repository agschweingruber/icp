import torch
import torch.nn as nn
from .base_module import BaseModule
from torch.nn.utils.rnn import pack_padded_sequence, pad_packed_sequence


class LitLSTM(BaseModule):
    def __init__(self, args):
        BaseModule.___init__(self, args)

        self.input_mlp = nn.Sequential(
            nn.Linear(self.input_tensor_shape[-1], self.hidden_size),
            nn.ReLU(),
        )
        self.rnn = nn.LSTM(self.hidden_size,
                           self.hidden_size,
                           dropout=self.dropout_val,
                           num_layers=self.args.n_layers,
                           batch_first=True)
        self.layer_norm = nn.LayerNorm(normalized_shape=[self.hidden_size]) if self.args.lay_norm else nn.Identity()
        self.out_mlp = nn.Sequential(
            nn.Dropout(self.dropout_val) if self.dropout_val else nn.Identity(),
            nn.Linear(self.hidden_size, self.hidden_size), nn.ReLU(),
            nn.Linear(self.hidden_size, self.out_size))

    def forward(self, x, lens=None):
        batch_size, seq_len, num_features = x.size()
        x = self.input_mlp(x)
        if lens is not None:
            x = pack_padded_sequence(x, lens.cpu(), batch_first=True, enforce_sorted=False)
        x, self.hidden = self.rnn(x)
        if lens is not None:
            x, lens = pad_packed_sequence(x, batch_first=True)
        x = self.layer_norm(x)
        x = self.out_mlp(x)
        return x

    def training_step(self, batch, batch_idx):
        data, target, idcs, lens = batch

        if self.args.gauss_std:
            data = torch.normal(data, self.args.gauss_std)

        output = self(data, lens=lens)
        target = target[:, :output.shape[1]]
        loss_mask = torch.isnan(target)
        loss = self.loss_func(output, target, loss_mask)
        return {'loss': loss, 'log': {'train_loss': loss.item()}}

    def configure_optimizers(self):
        optimizer = torch.optim.Adam(self.parameters(), lr=self.args.lr)
        scheduler = torch.optim.lr_scheduler.StepLR(optimizer, 1, gamma=0.98)
        return [optimizer], [scheduler]
