library(readr)
library(forecast)
library(zoo)

data <- read_csv("/Users/hewanci/Documents/Forecasting/Project\ and\ data-20211207/Projectdata.csv")

tsdata <- xts(data[,2:19], as.Date(data$date))

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

# Naive

for (i in 1:length(train_names)){
  for (n in 1:length(variables)){
    for (k in h){
      naivef <- naive(get(paste0("train",i))[,n],h=k)
      print(paste0("train",i))
      print(colnames(get(paste0("train",i))[,n]))
      print(paste0('horizon is',k))
      print(summary(naivef))
    }
  }
}


# sNaive

for (i in 1:length(train_names)){
  for (n in 1:length(variables)){
    for (k in h){
      snaivef <- snaive(get(paste0("train",i))[,n],h=2*30)
      print(paste0("train",i))
      print(colnames(get(paste0("train",i))[,n]))
      print(paste0('horizon is',k))
      print(summary(naivef))
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
    }
  }
}
