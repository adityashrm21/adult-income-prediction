# importing required libraries/modules
import pandas as pd
import numpy as np
import pickle
import json
from sklearn.externals import joblib
from sklearn.model_selection import RandomizedSearchCV

class Util(object):
    """
    This class uses the preprocessed dataset from the EDA
    to build and export the model for deployement
    """

    def get_train_test(self, train, test):
        '''
        This function seperates the response and the predictor variables
        for the train and test dataset

        parameters:
        ----------
        train : a pandas data frame
                The training dataset

        test : a pandas data frame
                The test dataset

        returns:
        -------
        X_train : A pandas data frame containing predictors for train data

        y_train : A pandas Series with the response variable for train data

        X_test : A pandas data frame containing predictors for test data

        y_test : A pandas Series with the response variable for test data

        examples:
        --------
        >>> utils.get_train_test(self, train, test)
        >>> X_train, y_train, X_test, y_test (returned)
        '''
        if type(train) != pd.core.frame.DataFrame:
            raise TypeError("Training datatype is invalid, make sure\
                        you are passing a pandas dataframe!")
        if type(test) != pd.core.frame.DataFrame:
            raise TypeError("Test datatype is invalid, make sure\
                        you are passing a pandas dataframe!")

        y_train = train.income
        X_train = train.drop(['income'], axis=1)

        y_test = test.income
        X_test = test.drop(['income'], axis=1)

        return X_train, y_train, X_test, y_test

    def process_columns(self, df, cols, desired_type):
        '''
        This function converts the data type of provided columns in a dataframe
        to a specific data type

        parameters:
        ----------
        df : a pandas data frame
                A data frame whose column types are to be changed

        cols : list
                A of columns whose type has to be changed

        desired_type : a valid python data type
                The data type to which the columns need to be converted to

        returns:
        -------
        df : A pandas data frame with desired data types for the columns
                provided in the input

        examples:
        --------
        >>> utils.process_columns(self, df, ['workclass', 'education'], 'str')
        >>> df (returned)
        '''

        if type(df) != pd.core.frame.DataFrame:
            raise TypeError("Input df datatype is invalid, make sure\
                        you are passing a pandas dataframe!")
        if type(cols) != list:
            raise TypeError("cols datatype is invalid, make sure\
                        you are passing a list!")
        if type(desired_type) != str:
            raise TypeError("desired datatype is invalid, make sure\
                        you are passing it as a string!")

        for col in cols:
            df[col] = df[col].astype(desired_type)
        return df

    def get_best_params(self, model, params, X_train, y_train):
        '''
        This function returns the best parameters doing cross-validation
        using Randomized search on the parameters provided

        parameters:
        ----------
        model : a scikit-learn model
                A model which you want to use on the dataset for prediction

        params : dict
                A dictionary of parameters for the corresponding model

        X_train : pandas dataframe
                    The training dataset with predictors

        y_train : pandas Series
                    The training labels

        returns:
        -------
        dict : A dictionary containing the best parameters selected based
                on the randomized search

        examples:
        --------
        >>> utils.get_best_params(RandomForestClassifier(n_estimators=200),
                                {'max_depth': np.arange(2, 10),
                     'min_samples_split': np.arange(2, 6)})
        >>> {'min_samples_split': 4, 'max_depth': 8}
        '''
        if type(X_train) != pd.core.frame.DataFrame:
            raise TypeError("Training datatype is invalid, make sure\
                        you are passing a pandas dataframe!")
        if type(y_train) != pd.core.series.Series:
            raise TypeError("Training labels datatype is invalid, make sure\
                        you are passing a pandas series!")
        if type(params) != dict:
            raise TypeError("params datatype is invalid, make sure\
                        you are passing a dictionary!")

        clf_rf = RandomizedSearchCV(model, params, cv=10, n_iter=10)
        clf_rf.fit(X_train, y_train)

        return clf_rf.best_params_

    def export_data(self, X, model):
        '''
        This function exports column datatypes, trained model and
        columns data from training dataset to external files

        parameters:
        ----------
        X : a pandas dataframe
            The training dataset to be used for exporting data

        model : a scikit-learn model
                The model which you want to use on the dataset for prediction

        returns:
        -------

        examples:
        --------
        >>> utils.export_data(X_train, model)
        '''
        if type(X) != pd.core.frame.DataFrame:
            raise TypeError("Input X datatype is invalid, make sure\
                        you are passing a pandas dataframe!")

        with open('columns.json', 'w') as fh:
            json.dump(X.columns.tolist(), fh)

        with open('dtypes.pickle', 'wb') as fh:
            pickle.dump(X.dtypes, fh)

        joblib.dump(model, 'model.pickle')
