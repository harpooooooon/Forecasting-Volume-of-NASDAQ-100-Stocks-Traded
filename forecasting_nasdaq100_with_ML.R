# libraries ####
  library(ggplot2)
  library(reshape2)
  library(PerformanceAnalytics)      
  library(quantmod)
  library(xgboost)
  library(xts)
  library(pROC)

# NASDAQ 100 Indx ####
  
  
  nasdaq100_symbols <- c( 'AAPL', 'ADBE', 'ADI', 'ADSK', 'AKAM', 'ALTR', 'ALXN', 
                          'AMAT', 'AMGN', 'AMZN', 'BBBY', 'BIDU', 'BIIB',
                          'BRCM', 'CA', 'CELG', 'CERN', 'CHKP', 'CHRW', ' CHTR', 'CMCSA',
                          'COST', 'CSCO', 'CTRX', 'CTXS', 'DISCA', 'DISCK', 'DISH',
                          'DLTR', 'DTV', 'EBAY', 'EQIX', 'ESRX', 'EXPD', 'EXPE', 'FAST',
                          'FB', 'FFIV', 'FISV', 'FOXA', 'GILD', 'GMCR', 'GOOG', 'GOOGL',
                          'GRMN', 'HSIC', 'ILMN', 'INTC', 'INTU', 'ISRG', 'KLAC', 'KRFT',
                          'LBTYA', 'LLTC', 'LMCA', 'LMCK', 'LVNTA', 'MAR', 'MAT', 'MDLZ',
                          'MNST', 'MSFT', 'MU', 'MXIM', 'MYL', 'NFLX', 'NTAP', 'NVDA',
                          'NXPI', 'ORLY', 'PAYX', 'PCAR', 'PCLN', 'QCOM', 'QVCA', 'REGN',
                          'ROST', 'SBAC', 'SBUX', 'SIAL', 'SIRI', 'SNDK', 'SPLS', 'SRCL',
                          'STX', 'SYMC', 'TRIP', 'TSCO', 'TSLA', 'TXN', 'VIAB', 'VIP',
                          'VOD', 'VRSK', 'VRTX', 'WDC', 'WFM', 'WYNN', 'XLNX', 'YHOO')
  
  getSymbols(nasdaq100_symbols)
  
  nasdaq100 <- data.frame(as.xts(merge(AAPL, ADBE, ADI, ADSK, AKAM, ALTR, ALXN, 
                                       AMAT, AMGN, AMZN, BBBY, BIDU, BIIB,
                                       BRCM, CA, CELG, CERN, CHKP, CHRW, CHTR, CMCSA,
                                       COST, SCO, CTRX, CTXS, DISCA, DISCK, DISH,
                                       DLTR, DTV, EBAY, EQIX, ESRX, EXPD, EXPE, FAST,
                                       FB, FFIV, FISV, FOXA, GILD, GMCR, GOOG, GOOGL,
                                       GRMN, HSIC, ILMN, INTC, INTU, ISRG, KLAC, KRFT,
                                       LBTY, LLTC, LMCA, LMCK, LVNTA, MAR, MAT, MDLZ,
                                       MNST, MSFT, MU, MXIM, MYL, NFLX, NTAP, NVDA,
                                       NXPI, ORLY, PAYX, PCAR, PCLN, QCOM, QVCA, REGN,
                                       ROST, SBAC, SBUX, SIAL, SIRI, SNDK, SPLS, SRCL,
                                       STX. SYMC, TRIP, TSCO, TSLA, TXN, VIAB, VIP,
                                       VOD, VRSK, VRTX, WDC, WFM, WYNN, XLNX, YHOO)))
  
  head(nasdaq100[, 1:12], 2)
  
  
  
  
  # set out come variable by ####
    outcomeSymbol <- 'FISV.Volume'
    
    nasdaq100 <- xts(nasdaq100, 
                     order.by = as.Date(rownames(nasdaq100)))
    
    # shift date back one day 
    nasdaq100 <- as.data.frame(merge(nasdaq100, 
                                     lml = lag(nasdaq100[, outcomeSymbol], -1)))
    
    # if tomorrow's volume is smaller then 0, elif tomorrows value is bigger 1
    nasdaq100$outcome <- ifelse(nasdaq100[, paste0(outcomeSymbol, '1')] > nasdaq100[, outsomeSymbol], 1, 0)
    
    # remove shifted down volume field, it will be unnecessay
    nasdaq100 <- nasdaq100[, !names(nasdaq100) %in% c(paste(outcomeSymbol, '1'))]
    
  # cast date to true date and order in decreasing order ####
    GetDiffDay <- function(objDF, days = c(10), offLimitsSymbols = c('outcome'), roundByScale = 3){
      # sort tdates in decreasing order
      ind <- sapply(objDF, is.numeric)
      for(sym in names(objDF)[ind]){
        if(!sym %in% offLimitsSymbols){
          print(paste('****************', sym))
          objDF[, sym] <- round(scale(objDF[, sym]), roundByScale)
          
          
          print(paste('theColName', sym))
            for (day in days){
              objDF[paste0(sym, '_', day)] <- c(diff(objDF[, sym], lag = day), rep(x = 0, day)) * -1
            }
          
          }
        
        }
      
      return(objDF)
      
    }
    
  
  # call the above functions ####
    nasdaq100 <- GetDiffDay(nasdaq100,
                            day = c(1,2,3,4,5,10,20),
                            offLimitsSymbols =  c('outcome'),
                            roundByScale = 3)
  
  # drop most recent entry since it doesn't have an outcome
    nasdaq100 <- nasdaq100[2:nrow(nasdaq100),]  
    
  # using POSIXlt to add day of the week, month, month and year for future data points derived from model
    nasdaq100$week <- as.POSIXlt(nasdaq100$date)$wday
    nasdaq100$yday <- as.POSIXlt(nasdaq100$date)$mday
    nasdaq100$mon <- as.POSIXlt(nasdaq100$date)$mon
    
      
  # remove date field and shuffle data frame
    nasdaq100 <- subset(nasdaq100, select = -c(date))
    nasdaq100 <- nasdaq100[sample(nrow(nasdaq100)), ]
    
    
  # modeling ####
    
  predictorNames <- names(nasdaq100)[names(nasdaq100) != 'outcome']
  set.seed(1234)    
  
  # determining train and testing data sets
  split <- sample(nrow(nasdaq100), floor(0.7*nrow(nasdaq100)))
  train <- nasdaq100[split, ]
  test <- nasdaq100[-split, ]
  
  bst <- sgboost(data = as.matrix(train[, predictorNames]),
                 label = train$outcome,
                 verbose = 0,
                 eta = 0.1,
                 gamma = 50,
                 noround = 50,
                 colsample_bytree = 0.1,
                 subsample = 8.6m,
                 objective = "binary:logistic")
  
  predictions <- predict(bst, as.matrix(test[, predictorNames]), 
                         outputmargin = TRUE)
    
  # analysis of model ####  
    
    auc <- roc(test$outcome, predictions)
    print(paste('AUC score -->  ', auc$auc))
  
    
    
    
    
    
    