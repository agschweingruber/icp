import numpy as np
from pytorch_lightning.loggers import MLFlowLogger
from pytorch_lightning.callbacks import EarlyStopping
from optuna.integration import PyTorchLightningPruningCallback
from src.arguments import get_args
from pytorch_lightning import Trainer, seed_everything
import optuna
import torch
import warnings
from src.models import LitLSTM
import torch.multiprocessing
torch.multiprocessing.set_sharing_strategy('file_system')
warnings.filterwarnings("ignore")


def get_trainer_params(args):
    use_half = True if args.use_half and torch.cuda.is_available() else False
    return {
        "gpus": args.n_gpus,
        "benchmark": True,
        "amp_level": 'O2',
        "precision": 16 if use_half else 32,
        "max_epochs": args.max_epochs,
        "auto_lr_find": args.auto_lr,
        "auto_scale_batch_size": 'binsearch' if args.auto_batch else None,
        "fast_dev_run": bool(args.smoke_test),
        "gradient_clip_val": args.grad_clip,
    }


def train(args):
    seed_everything(args.seed)
    model = LitLSTM(args)

    logger = MLFlowLogger(experiment_name='Default')

    early_stop_callback = EarlyStopping(
        monitor='val_loss', min_delta=0.005, patience=3, verbose=args.verbose, mode='min') if args.early else None

    trainer = Trainer(
        log_gpu_memory='all' if args.verbose else None,
        track_grad_norm=2 if args.verbose else -1,
        logger=logger,
        weights_summary='full',
        callbacks=[early_stop_callback],
        accumulate_grad_batches=args.acc_grads,
        profiler=args.verbose,
        **get_trainer_params(args),
    )

    logger.log_hyperparams(model.args)
    trainer.fit(model)
    trainer.test(model, test_dataloaders=model.test_dataloader())
    model.save_preds_and_targets(to_disk=True)
    logger.finalize()

    return logger.run_id


def get_trial_params(trial):
    return {
        'lr':
        trial.suggest_float("learning_rate", 1e-5, 1e-3),
        'hidden_size':
        trial.suggest_int("hidden_size", 4, 9),
        'dropout':
        trial.suggest_float("dropout", 0.0, 0.25),
        'gauss_std':
        trial.suggest_float("gauss_std", 0.0, 0.15),
        'n_layers':
        trial.suggest_int("n_layers", 1, 4),
        'acc_grads':
        trial.suggest_int("acc_grads", 1, 4),
    }


def objective(trial, args):
    params = get_trial_params(trial)
    params['hidden_size'] = 2**params['hidden_size']
    params['acc_grads'] = 2**params['acc_grads']

    early_stopper = EarlyStopping(
        monitor='val_loss', min_delta=0.005, patience=3, mode='min')
    callbacks = [early_stopper, PyTorchLightningPruningCallback(
        trial, monitor="val_loss")]

    if args.model_type == 'attnlstm':
        params['attn_width'] = trial.suggest_int("attn_width", 3, 64)

    if 'split' in args.val_mode:
        dataset_hour = args.data.split('_')[-1]
        logger = MLFlowLogger(experiment_name=f'Optuna_{dataset_hour}h_{args.val_mode[-1]}_split')
        print(f'Optuna_{dataset_hour}_{args.val_mode[-1]}_split')
        val_losses = []
        for _split_id in range(int(args.val_mode[-1])):
            print(f"Split {_split_id} Trial {trial.number}")
            args.__dict__["split_id"] = 0
            for key in params:
                args.__dict__[str(key)] = params.get(key)
            model = LitLSTM(args)
            trainer = Trainer(
                logger=logger,
                callbacks=callbacks,
                **get_trainer_params(args),
            )
            logger.log_hyperparams(model.args)
            args.__dict__["val_mode"] = args.val_mode
            args.__dict__["split_id"] = _split_id
            model._get_data(args, data_mode='init')
            trainer.fit(model)
            trainer.test(model, test_dataloaders=model.test_dataloader())
            # logger.finalize()
            val_losses.append(model.metrics['val_loss'])

        # log mean val loss for later retrieval of best model
        mean_val_loss = torch.stack(val_losses).mean()
        logger.log_metrics({"mean_val_loss": mean_val_loss}, step=0)
        logger.finalize()
        return mean_val_loss

    elif args.val_mode == 'full':
        logger = MLFlowLogger(experiment_name='Optuna_full')
        for key in params:
            args.__dict__[str(key)] = params.get(key)
        model = LitLSTM(args)
        trainer = Trainer(
            logger=logger,
            callbacks=callbacks,
            **get_trainer_params(args),
        )
        logger.log_hyperparams(model.args)
        trainer.fit(model)
        trainer.test(model, test_dataloaders=model.test_dataloader())
        model.save_preds_and_targets(to_disk=True)
        logger.finalize()
        return model.metrics['val_loss']


def tune(args):
    pruner = optuna.pruners.MedianPruner()
    study = optuna.create_study(direction="minimize", pruner=pruner)
    study.optimize(lambda trial: objective(trial, args),
                   n_trials=args.trials,
                   gc_after_trial=False,
                   n_jobs=args.num_jobs)
    print("Number of finished trials: {}".format(len(study.trials)))
    print("Best trial:")
    trial = study.best_trial
    print("  Value: {}".format(trial.value))
    print("  Params: ")
    for key, value in trial.params.items():
        print("    {}: {}".format(key, value))


def main(args):
    if args.tune:
        tune(args)
    else:
        train(args)


if __name__ == '__main__':
    args = get_args()
    main(args)
