import os
import pandas as pd

import matplotlib.pyplot as plt
import numpy as np
from sklearn.model_selection import train_test_split

os.chdir('C:\\Users\\veena\\Downloads')

df_99_percent = pd.read_csv('df99_percent.csv')

all_scopus = pd.read_csv('all_scopus_with_authorNum&paperLength.csv')

all_scopus.drop(all_scopus.columns.difference(['author number', 'paper length in pages', 'cited by']), 1, inplace=True)
print(all_scopus)

# https://datatofish.com/dropna/
all_scopus = all_scopus.dropna()

x_authorNum = all_scopus['author number']
x_paperLen = all_scopus['paper length in pages']
y = all_scopus['cited by']

print(all_scopus)

# https://stackoverflow.com/questions/31521170/scikit-learn-train-test-split-with-indices

import math
from sklearn.linear_model import LinearRegression

def scikit_learn_method(x, y, min_x, max_x, max_y, ln_bool, df=all_scopus, test_size=0.2, random_state=0):

    # https://stackoverflow.com/questions/42988348/typeerror-cannot-convert-the-series-to-class-float
    if ln_bool:
        y = np.log(y)

    # set random_state = 0 for consistent seed
    x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.2, random_state=0)

    # reshape (-1, 1) - gives us 1 sample; no need to reshape y
    # https://datatofish.com/dropna/
    # https://stackoverflow.com/questions/18691084/what-does-1-mean-in-numpy-reshape
    # https://stackoverflow.com/questions/53723928/attributeerror-series-object-has-no-attribute-reshape
    # https://stackoverflow.com/questions/35082140/preprocessing-in-scikit-learn-single-sample-depreciation-warning

    x_train = x_train.values.reshape(-1, 1)
    x_test = x_test.values.reshape(-1, 1)

    model_withOutliers = LinearRegression()
    model_withOutliers = model_withOutliers.fit(x_train, y_train)

    print('y-hat = %sx + %s' % (model_withOutliers.coef_[0], model_withOutliers.intercept_))

    # https://stackoverflow.com/questions/41635448/how-can-i-draw-scatter-trend-line-on-matplot-python-pandas/41635626
    from sklearn.metrics import r2_score

    plt.scatter(x, y) # with outliers
    plt.title('With outliers')
    m, b = model_withOutliers.coef_[0], model_withOutliers.intercept_
    plt.plot(x, m*x+b)
    plt.show()

    text = f"$y={m:0.3f}\;x{b:+0.3f}$\n$R^2 = {r2_score(y, m * x + b):0.3f}$"
    plt.gca().text(0.05, 0.95, text, transform=plt.gca().transAxes,
                   fontsize=14, verticalalignment='bottom')

    # https://www.scikit-yb.org/en/latest/api/regressor/peplot.html
    from sklearn.linear_model import Lasso
    from yellowbrick.regressor import PredictionError

    lasso_model = Lasso()
    visualizer = PredictionError(lasso_model)

    visualizer.fit(x_train, y_train)  # Fit the training data to the visualizer
    visualizer.score(x_test, y_test)  # Evaluate the model on the test data
    visualizer.show()

    # https://stackoverflow.com/questions/28876243/how-to-delete-the-current-row-in-pandas-dataframe-during-df-iterrows

    plt.xlim(min_x, max_x) # without outliers
    plt.ylim(0, max_y)
    plt.title('Without outliers')

    plt.scatter(x, y)
    plt.show()

    text = f"$y={m:0.3f}\;x{b:+0.3f}$\n$R^2 = {r2_score(y, m*x+b):0.3f}$"
    plt.gca().text(0.05, 0.95, text, transform=plt.gca().transAxes,
                   fontsize=14, verticalalignment='bottom')

    y_pred_with_outliers = model_withOutliers.predict(x_test)

    sum_outliers = 0

    for i in range(len(df)):
        squared_with_outliers = (y_test - y_pred_with_outliers) ** 2
        sum_outliers += squared_with_outliers

    mean = sum_outliers / len(df)

    rms = mean ** 0.5

    rms_value = 0

    for element in rms:
        rms_value += element
    rms_value = rms_value / len(rms)

    print('Root mean squared, with outliers:', rms_value)

# http://www.datasciencemadesimple.com/log-natural-logarithmic-value-column-pandas-python-2/
# https://stackoverflow.com/questions/42988348/typeerror-cannot-convert-the-series-to-class-float

# split test set into dev and post-dev sets
#X_dev = X_test[0:len(X_test)//2]
#X_test_afterDev = X_test[len(X_test)//2]

#Y_dev = Y_test[0:len(Y_test)//2]
#Y_test_afterDev = Y_test[len(Y_test)//2]

scikit_learn_method(x_authorNum, y, 0, 40, 500, ln_bool=False)
scikit_learn_method(x_authorNum, y, 0, 40, 6, ln_bool=True)

scikit_learn_method(x_paperLen, y, 0, 100, 100, ln_bool=False)
