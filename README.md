# forescating-nasdaq100-stock

The code in this repository is meant to demostrate a simple instance of using machine learning to predict 
stock market trends. The data was dervived from Yahoo Fiances through the quantmod package, and uses the top 100 companies in the NASDAQ
to predct 1, 2, 3, 4 ,5, 10, and 20 in advance. To decrease run time, an instance of XGBOOST is initiated, yet the code is not 
ompitimized in anyway.

The model uses the AUC methods of validation with a current value of .7843 as of 2018-01-26.
