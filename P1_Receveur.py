#Download dataset p1_iris.csv from Blackboard. The data set consists of 150
#samples from each of three species of Iris (Iris setosa, Iris virginica and Iris ver- sicolor).
#Four features were measured from each sample: the length and the width of the sepals
#and petals, in centimeters. 

import pandas as pd

#Load the dataset into a DataFrame structure using pandas.read_csv. Explore the
#data and briefly summarize your findings. Use the T5-1. Classification tutorial as an
#example.

data.columns = ['sepal length', 'sepal width', 'petal length', 'petal width', 'class']

print(data.head())

from pandas.api.types import is_numeric_dtype

for col in data.columns:
    if is_numeric_dtype(data[col]):
        print('%s:' % (col))
        print('\t Mean = %.2f' % data[col].mean())
        print('\t Standard deviation = %.2f' % data[col].std())
        print('\t Minimum = %.2f' % data[col].min())
        print('\t Maximum = %.2f' % data[col].max())
        
print(data['class'].value_counts())

print(data.describe(include='all'))

print('Covariance:')
print(data.cov())

print('Correlation:')
print(data.corr())

data.boxplot()

#Build the Decision Tree Classifier and plot the resulting tree (again using T5-1.
#Classification tutorial as a starting point). Explore different options and criteria for
#building of the decision tree and summarize your findings.



#Split the data into training and testing sets and explore the behavior of train and test
#errors depending on the choice of the model and the relative size of train and test sets.
#Summarize your findings.

from sklearn.model_selection import train_test_split

data1 = data.sample(frac=1, replace=False)
test, train = train_test_split(data1, train_size=100, test_size=50)

Y = train['class']
X = train.drop(['class'],axis=1)

clf = tree.DecisionTreeClassifier(criterion='gini',max_depth=8)
clf = clf.fit(X, Y)

testY = test['class']
testX = test.drop(['class'],axis=1)
predY = clf.predict(testX)
predictions = pd.concat([test[['class']],pd.Series(predY,name='Predicted Class')], axis=1)
predictions

from sklearn.metrics import accuracy_score

print('Accuracy on test data is %.2f' % (accuracy_score(testY, predY)))


#Exploration
#Download dataset p1_credit_data.csv from Blackboard. The data set contains
#financial ratios, industry sector, and credit ratings for a list of corporate customers. This
#is simulated, not real data. The first column is a customer ID. Then we have five columns
#of financial ratios: (i) Working capital / Total Assets (WC_TA); (ii) Retained Earnings /
#Total Assets (RE_TA); (iii) Earnings Before Interests and Taxes / Total Assets (EBIT_TA);
#(iv) Market Value of Equity / Book Value of Total Debt (MVE_BVTD); (v) Sales / Total
#Assets (S_TA). Next, we have an industry sector label, an integer value ranging from 1 to
#12. The last column has the credit rating assigned to the corporate customer.



#Suppose that you are hired as a data mining consultant by a FinTech company that is
#looking to build a predictive model based on this dataset. Following the groundwork laid
#during the warm-up exercise build a decision tree model that calculates the corporate
#credit rating from the financial ratios. Discuss the approach, the findings and the
#challenges.