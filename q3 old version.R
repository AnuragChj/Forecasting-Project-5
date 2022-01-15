library(readr)
library(forecast)
library(zoo)

data <- read_csv("/Users/hewanci/Documents/Forecasting/Project\ and\ data-20211207/Projectdata.csv")

tsdata <- xts(data[,2:19], as.Date(data$date))

temp <- read_csv("/Users/yingding/Desktop/Forcasting M2/Project and data-20220107/California_temp_max_min.csv")

tstemp <- xts(data[,2:3], as.Date(temp$Date))

split1 <- sample(c(rep(0, 0.7 * nrow(tsdata)), rep(1, 0.3 * nrow(tsdata))))
table(split1) 
train1 <- head(tsdata, 1378)
test1 <- tail(tsdata, 591)

split2<- sample(c(rep(0, 0.8 * nrow(data)), rep(1, 0.2 * nrow(data))))
table(split2) 
train2 <- head(tsdata, 1575)
test2 <- tail(tsdata, 394)

split3<- sample(c(rep(0, 0.9 * nrow(data)), rep(1, 0.1 * nrow(data))))
table(split3) 
train3 <- head(tsdata, 1772)
test3 <- tail(tsdata, 197)

train_names <- c('train1', 'train2', 'train3')
test_names <- c('test1', 'test2', 'test3')

variables <- colnames(train1)

h = seq(1:28)  # the horizons

#rmsse function
rmsseerror  <- function(m,i,k,n){
  result <- data.frame(m)
  actual <- data.frame(get(paste0("test",i))[1:k,n])
  rmsse_1=rmse(actual[,1],result$Point.Forecast)
  rmsse_2=1/((nrow(actual)-1))*sum(diff((actual[,1]))^2) %>% sqrt()#
  rmsse <- rmsse_1/rmsse_2 #final value  
  return(rmsse)
}



# Naive

for (i in 1:length(train_names)){
  for (n in 1:length(variables)){
    for (k in h){
      naivef <- naive(get(paste0("train",i))[,n],h=k)
      print(paste0("train",i))
      print(colnames(get(paste0("train",i))[,n]))
      print(paste0('horizon is',k))
      print(summary(naivef))
      rmsse <- rmsseerror(hw,i,k,n) 
      print(paste0("RMSSE is ", rmsse))
   
    }
  }
}


# sNaive

for (i in 1:length(train_names)){
  for (n in 1:length(variables)){
    for (k in h){
      snaivef <- snaive(get(paste0("train",i))[,n],h=k)
      print(paste0("train",i))
      print(colnames(get(paste0("train",i))[,n]))
      print(paste0('horizon is',k))
      print(summary(naivef))
      rmsse <- rmsseerror(hw,i,k,n) 
      print(paste0("RMSSE is ", rmsse))
   
    }
  }
}


# ES

for (i in 1:length(train_names)){
  for (n in 1:length(variables)){
    for (k in h){
      esf <- ses(get(paste0("train",i))[,n],h=k)
      print(paste0("train",i))
      print(colnames(get(paste0("train",i))[,n]))
      print(paste0('horizon is',k))
      print(summary(esf))
      rmsse <- rmsseerror(hw,i,k,n) 
      print(paste0("RMSSE is ", rmsse))
   
    }
  }
}


# MA

for (i in 1:length(train_names)){
  for (n in 1:length(variables)){
    for (k in h){
      maf = forecast(ma(get(paste0("train",i))[,n], order = 15), h=k )
      print(paste0("train",i))
      print(colnames(get(paste0("train",i))[,n]))
      print(paste0('horizon is',k))
      print(summary(maf))
      rmsse <- rmsseerror(hw,i,k,n) 
      print(paste0("RMSSE is ", rmsse))
   
    }
  }
}


#SARIMA


for (i in 1:length(train_names)){
  for (n in 1:length(variables)){
    for (k in h){
      sarima<-forecast(auto.arima(get(paste0("train",i))[,n],D=1,seasonal=TRUE), h=k)
      print(paste0("train",i))
      print(colnames(get(paste0("train",i))[,n]))
      print(paste0('horizon is',k))
      print(summary(sarima))
      rmsse <- rmsseerror(hw,i,k,n) 
      print(paste0("RMSSE is ", rmsse))
    }
  }
}

#SARIMAX

for (i in 1:length(train_names)){
  for (n in 1:length(variables)){
    for (k in h){
      sarimax<-forecast(auto.arima(tsdata$Hobbies_CA_1, xreg =tstemp$Air.min,seasonal = TRUE),xreg =tstemp$Air.min,h=1)
      print(paste0("train",i))
      print(colnames(get(paste0("train",i))[,n]))
      print(paste0('horizon is',k))
      print(summary(sarima))
      rmsse <- rmsseerror(hw,i,k,n) 
      print(paste0("RMSSE is ", rmsse))
    }
  }
 }
  
    
#Holt winters
  
  
  for (i in 1:length(train_names)){
    for (n in 1:length(variables)){
      for (k in h){
        hw<-forecast(HoltWinters(get(paste0("train",i))[,n],gamma = FALSE), h=k)
        rmsse <- rmsseerror(hw,i,k,n)  
        print(paste0("train",i))
        print(colnames(get(paste0("train",i))[,n]))
        print(paste0('horizon is',k))
        print(summary(hw))
        print(paste0("RMSSE is ", rmsse))
         
      }
    }
  }
  
#ESX
  
for (i in 1:length(train_names))
{
    for (n in 1:length(variables)){
      for (k in h){
        esx<-ses(get(paste0("train",i))[,n],h=k, xreg =tstemp$Air.min)
        print(paste0("train",i))
        print(colnames(get(paste0("train",i))[,n]))
        print(paste0('horizon is',k))
        print(summary(esx))
        rmsse <- rmsseerror(hw,i,k,n) 
        print(paste0("RMSSE is ", rmsse))
    }
  }
}
    
# State Space

for (i in 1:length(train_names)){
  for (n in 1:length(variables)){
    for (k in h){
    tbats <- tbats(ts(get(paste0("train",i)))[,n])
    print(paste0("train",i))
    print(colnames(get(paste0("train",i))[,n]))
    pred <- forecast(tbats, h=k)
    print(pred <- pred$mean)
    }
  }
}
  
  
