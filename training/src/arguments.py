import argparse


def get_args(args=None):

    parser = argparse.ArgumentParser()

    # General:
    parser.add_argument('--mode', type=str, default='icp', choices=['', 'icp', 'phase'])
    parser.add_argument('--block_labor', type=int, default=0, help='Mask Labor features')
    parser.add_argument('--db_feat', type=int, default=0)
    parser.add_argument('--outcome_tar', type=int, default=0)

    parser.add_argument('--val_mode', type=str, default='full', choices=['full', 'split3', 'split5'])
    parser.add_argument('--split_id', type=int, default=None, choices=[0, 1, 2, 3, 4])
    parser.add_argument('--verbose', action='store_true', help="Print more info")
    parser.add_argument('--smoke_test', type=int, default=0)
    parser.add_argument('--variable_threshold', type=int, default=0)
    parser.add_argument('--seed', type=int, default=2021)
    # Input and output related options
    parser.add_argument('--data', type=str, default='MIMIC/paper_dataset_yeojo_60_median_2', help='Folder containing data')
    parser.add_argument('--block_size', type=int, default=0, help='Block size as int')
    parser.add_argument('--categorical_outcome', type=int, default=0, help='Instead of predicting an outcome and a'
                                                                           'related death + release time, predict '
                                                                           'if the patient dies in N amount of hours.')
    parser.add_argument('--n_layers', type=int, default=1)
    parser.add_argument('--lay_norm', type=int, default=1)

    parser.add_argument('--input_mask', type=str,
                        help="""list of features to exclude, all names (might be outdated):
                        Bili, Ca, Cl, FCOHb, FO2, Glu, HCO3, Hb_BGA, K, Lac,Na, PCO2, PO2,
                        SBE, pH, sO2, BIS, CPP, HF, ICP, SpO2, Temp, diast, mittl, syst,
                        ALT, Bilirubin, EVB, Erythrocyten, Fibrinogen, GFR, Harnstoff,
                        Harnstoff-N, Hb, Hk, INR, Kreatinin, MCH, MCHC, MCV, Quick, TZ,
                        Thrombocyten, aPTT, Alter, Geschlecht, Outcome""", default="")
    parser.add_argument('--target_mask', type=str, help='See --input_mask', default="")
    parser.add_argument('--feature_pass', type=str, help='Let only these features be included in the model', default="")
    parser.add_argument('--target_pass', type=str,
                        help='Let only these targets be predicted by the model. '
                             'To change whether outcome and outcome death time is predicted use other arguments '
                             '(use_outcome, predict_outcome_time)',
                        default="")

    # Hyperparameters
    parser.add_argument('--weigh_loss', type=int, default=1,
                        help="Weigh the loss of the classes depending on how often they appear in the dataset.")
    parser.add_argument('--lr', type=float, default=0.0005)
    parser.add_argument('--norm_seq_len', type=int, default=1, help="Normalize the loss by length of the sequence "
                                                                    "length per patient. If turned off patients with "
                                                                    "longer seq lens are given more weight in training")
    parser.add_argument('--max_epochs', type=int, default=100)
    parser.add_argument('--hidden_size', type=int, default=32)
    parser.add_argument('--batch_size', type=int, default=16)
    parser.add_argument('--dropout', type=float, default=0.2, help='Dropout value')
    parser.add_argument('--gauss_std', type=float, default=0.1,
                        help='Percentage of Gaussian Noise added to input values')
    # Performance
    parser.add_argument('--n_gpus', type=int, default=1, help="Number of gpus to use")
    parser.add_argument('--early', type=int, default=1, help="Early stopping callback")
    parser.add_argument('--auto_batch', type=int, default=0, help="Scale batch size automatically. Use 'power' or 'binsearch'")
    parser.add_argument('--auto_lr', default=0, type=int)
    parser.add_argument('--acc_grads', default=1, type=int)
    parser.add_argument('--grad_clip', default=0.5, type=float)
    parser.add_argument('--use_half', type=int, default=0)
    parser.add_argument('--num_workers', type=int, default=2, help='determine the number of dataloader workers')
    parser.add_argument('--pin_mem', type=int, default=1, help="pin memory before transfer to GPU")

    # Optuna
    parser.add_argument('--tune', type=int, default=0, help="Use optuna for hyper-parameter search")
    parser.add_argument('--trials', type=int, default=16, help="Number of trials per experiment")
    parser.add_argument('--num_jobs', type=int, default=1, help="Number of parallel trials to start")


    return parser.parse_args(args)
