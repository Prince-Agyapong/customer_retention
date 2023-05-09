# customer_retention
Customer Retention Analysis based on Credit Card Data

This project aims to predict customer retention based on credit card data, 
using two machine learning models: MARS and ANN. The target variable, 'Bad', 
indicates whether an individual is likely to default on their loan or credit 
card payment and is used as a proxy for customer retention.

Dataset

The dataset used in this analysis contains information on credit card customers and their behavior,
including demographic information, credit limits, transaction amounts, and payment history. 
The dataset can be found in the data directory.

Models

Two machine learning models were trained on the dataset: MARS and ANN.
MARS stands for Multivariate Adaptive Regression Splines, 
a flexible and powerful modeling technique that can capture non-linear relationships between variables. 
ANN stands for Artificial Neural Networks, which are modeled after the structure of the human brain and 
are commonly used for complex pattern recognition tasks.

Evaluation Metrics

The performance of the models was evaluated using ROC curves and KS plots, 
as well as the AUC value, which measures the overall performance of the model in predicting customer retention. 
The AUC value ranges from 0.5 to 1, with a value of 0.5 indicating random classification and a value of 1 indicating perfect classification.
