# forescating-nasdaq100-stock

The code in this repository is meant to demonstrate a simple instance of using machine learning to predict stock prices. The data was derived through the quantmod package, and uses the top 100 companies from the NASDAQ to predict a desired number of days in advance. To decrease run time, an instance of XGBOOST is initiated, yet the code is not ompitimized in anyway.

The model uses the AUC methods of validation.
