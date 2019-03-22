# importing required libraries/modules
import pandas as pd
import numpy as np
import os
import pickle
from utils import Util
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression
from warnings import simplefilter
simplefilter(action='ignore', category=FutureWarning)

def main():
    util = Util()

    # reading the data
    train = pd.read_csv("data/train.csv")
    test = pd.read_csv("data/test.csv")

    # processing the categorical columns
    print("Processing the categorical columns...")
    cols = ['workclass', 'education', 'marital_status', 'occupation',
            'race', 'sex', 'native_country', 'income']
    train = util.process_columns(train, cols, 'str')
    test = util.process_columns(test, cols, 'str')

    # getting the training and test predictors and labels
    print("Getting the training and test predictors and labels...")
    X_train, y_train, X_test, y_test = util.get_train_test(train, test)

    params = {'C' : 10.0**np.arange(-7, 7)}
    model = LogisticRegression()

    best_params = None
    # set this parameter to 0 if you already have done CV before
    cross_val = 1
    if not cross_val:
        print("Model and data info already exported, \
                loading exported parameters!")
        with open('best_params.pickle', 'rb') as f:
            best_params = pickle.load(f)
    else:
        print("Getting the best parameters using cross-validation...")
        best_params = util.get_best_params(model, params, X_train, y_train)
        with open('best_params.pickle', 'wb') as f:
            pickle.dump(best_params, f)

    print("Cross-validation completed, fitting the model...")
    model = LogisticRegression(C = best_params['C'])
    model.fit(X_train, y_train)

    print("The accuracy on the training data is:", model.score(X_train, y_train))
    print("The accuracy on the test data is:", model.score(X_test, y_test))

    # exporting model and data info required to deploy the model
    print("Exporting model and data info required to deploy the model...")
    util.export_data(X_train, model)
    print("Process completed!")

if __name__ == "__main__":
    main()
