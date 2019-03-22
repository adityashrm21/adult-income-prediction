# importing required libraries/modules
import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from incomePrediction.utils import Util

utils = Util()

sample_train = pd.DataFrame({'predictor' : ['abc'], 'income' : [1]})
sample_test = pd.DataFrame({'predictor' : ['abc'], 'income' : [1]})

class TestUtil():

    def test_get_train_test(self):

        # testing correct output types
        X_train, y_train, X_test, y_test = utils.get_train_test(sample_train,
                                                                sample_test)
        assert type(X_train) == pd.core.frame.DataFrame
        assert type(X_test) == pd.core.frame.DataFrame

        assert type(y_train) == pd.core.series.Series
        assert type(y_test) == pd.core.series.Series

        # checking correct shapes and sizes
        assert sample_train.shape[0] == X_train.shape[0]
        assert (sample_train.shape[1] - 1)  == X_train.shape[1]

        assert sample_test.shape[0] == X_test.shape[0]
        assert (sample_test.shape[1] - 1)  == X_test.shape[1]

        assert y_train.shape[0] == X_train.shape[0]
        assert y_test.shape[0]  == X_test.shape[0]

    def test_process_columns(self):

        processed_df = utils.process_columns(sample_train, ['income'], 'str')

        assert type(processed_df) == pd.core.frame.DataFrame
        assert processed_df.income.dtype == np.object
