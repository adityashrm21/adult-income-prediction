# Adult Income Prediction using Flask app on Heroku

Follow the steps provided below to reproduce the whole project.

### Setting up a virtual environment

We'll use a virtual environment for this one.
All of the necessary dependencies exist in `requirements.txt`.

Install pipenv using the instructons given in [this repository](https://github.com/pypa/pipenv).

After installing `pipenv`, from the project folder (i.e., where this readme lives in your computer), create a local virtual environment and install the project dependencies from inside the root directory using `requirements.txt` with the command:

```bash
$ pipenv install -r requirements.txt
```
We would need to activate the virtual environment to start working inside it.
To activate this project's virtualenv, run the following:
```
$ pipenv shell
```
You can deactivate the virtualenv by either typing `exit` or pressing `CTRL+d`

Now you should have everything installed that we need.

### Data format before cleaning

This information is directly copied from the [UCI datasets repository for adult dataset](https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)

- income: >50K, <=50K.
- age: continuous.
- workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
- fnlwgt: continuous.
- education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
- education-num: continuous.
- marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
- occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
- relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
- race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
- sex: Female, Male.
- capital-gain: continuous.
- capital-loss: continuous.
- hours-per-week: continuous.
- native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

### Run the EDA script to export cleaned data

```bash
R -e rmarkdown::render"('eda_adult.Rmd', clean=TRUE, output_format='pdf_document')"
```
The script `eda_adult.pdf` will contain the exploratory analysis report on the adult income dataset.

### Data format after cleaning

The data format has been converted to numbers for each variable and binned together (some categorical variables) into different categories (passed as strings) which are as defined below:

- income: 0 ('>50K'), 1 ('<=50K')
- age: continuous.
- workclass: 0 ('State-gov', 'Federal-gov', 'Local-gov'), 1 ('Self-emp-not-inc', 'Self-emp-inc', 'Without-pay', 'Never-worked'), 2 ('Private'), -1 ('unknown')
- education: 0 ("HS-grad", "11th", "9th", "7th-8th", "5th-6th", "10th", "Preschool", "12th", "1st-4th"), 1 ("Bachelors", "Some-college", "Assoc-acdm", "Assoc-voc"), 2 ("Masters", "Prof-school", "Doctorate", "Assoc-voc")
- marital_status: 0 ('Married-civ-spouse', 'Married-spouse-absent', 'Married-AF-spouse'), 1 ('Never-married','Divorced', 'Separated','Widowed')
- occupation: 0 ("Priv-house-serv", "Handlers-cleaners", "Other-service", "Armed-Forces", "Machine-op-inspct", "Farming-fishing", "Adm-clerical"), 1 ("Tech-support", "Craft-repair", "Protective-serv", "Transport-moving", "Sales"), 2 ("Exec-managerial", "Prof-specialty"), -1 ("unknown")
- race: 0 ("White"), 1 ("Black"), 2 ("Asian-Pac-Islander", "Amer-Indian-Eskimo", "Other")
- sex: 0 ("Female"), 1 ("Male")
- capital-gain: continuous.
- capital-loss: continuous.
- hours-per-week: continuous.
- native_country: 1 ("United-States"), 0 (all the rest of countries)

Note: `unknown` refers to the cells that were missing in the respective columns (`?`)

### Model choice

Execute the `main.py` script which will train a model on the cleaned data and export a it along with the required data info for deployment on Heroku.

```bash
python3 incomePrediction/main.py
```
I chose the `LogisticRegression` classifier from scikit-learn to get predictions (The test accuracy obtained is quite well ~ 85%). Cross-validation is done to choose the important hyperparameter (`C`) to control the degree of regularization. The script can be modified to use and tune any classifier available in `scikit-learn`. Both the training and test accuracies are comparable and hence, there seems to be no overfitting. I chose to go with Logistic Regression because it is a simple linear classifier whose results are interpretable and this is what I would expect from a model on such a dataset where the predictor-response relationship seems to be important in the analysis.

### Deploy the model on Heroku

The exported model is deployed as a microservice on Heroku using the steps given in [this repository](https://github.com/LDSSA/heroku-model-deploy#sign-up-and-set-up-at-heroku).

### Steps to get predictions

The model has been deployed on heroku and to get the predictions, you can use the following curl request:

```bash
curl -X POST https://adult-income-prediction.herokuapp.com/predict -d '{"id": 10, "observation": {"age": 39, "workclass": "2", "education": "2", "marital_status": "0", "occupation": "2", "race" : "0", "sex": "1", "capital_gain": 1230, "capital_loss": 0, "hours_per_week": 55, "native_country": "1"}}' -H "Content-Type:application/json"
```

Note: Make sure to use a separate observation id for each request (if you want to store the results properly) as we are storing the results in an sqlite database and this would be the primary identifier for the records.

### Testing framework

I have used the `pytest` library to test the `Util` class. To run the tests use the following command from the root directory of the project:

```bash
pytest incomePrediction/tests/
```

Due to shortage on time, I could not cover all kinds of tests but I did set up a basic test infrastructure which could be extended to test the remaining code (unit, integration and e2e tests).
