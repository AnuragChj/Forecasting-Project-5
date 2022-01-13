```{r setup}
library(readr)
library(forecast)
library(xts)
getwd()
library(dplyr)
library(fpp)
library(fpp3)
library(gridExtra)
library(ggplot2)
library(ranger)
library(ftsa)
library(Metrics)
library(smooth)
library(Mcomp)
library(rmarkdown)


#Step 1 read the csv datasets files
```{r}
data = read.csv("/Users/Bryan/Google Drive/Cours/DSBA/T1/1. Forecasting & Predictive Analysis/6. Project/Projectdata.csv")
```


```{r}
tsdata <- xts(data[,2:19], as.Date(data$date))
tsdata
```
```
#Step 2 perform training and testing data split
split: 70-30
```{r} 
split1<- sample(c(rep(0, 0.7 * nrow(tsdata)), rep(1, 0.3 * nrow(tsdata))))
train1 <- head(tsdata, 1378)
test1 <- tail(tsdata, 591)
test1
```
head(data_ts, round(nrow(data_ts) * 0.6))

split: 80-20
```{r}
split2<- sample(c(rep(0, 0.8 * nrow(data)), rep(1, 0.2 * nrow(data))))
table(split2) 
train2 <- head(tsdata, 1575)
test2 <- tail(tsdata, 394)

```

split: 90-10
```{r}
split3<- sample(c(rep(0, 0.9 * nrow(data)), rep(1, 0.1 * nrow(data))))
table(split3) 
train3 <- head(tsdata, 1772)
test3 <- tail(tsdata, 197)
```

# Aggregation by type of item

hobbies = c('Hobbies_CA_1',  'Hobbies_CA_2', 'Hobbies_CA_3')
household_1 = c('Household_1_CA_1',  'Household_1_CA_2', 'Household_1_CA_3')
household_2 = c('Household_2_CA_1',  'Household_2_CA_2', 'Household_2_CA_3')
foods_1 = c('Foods_1_CA_1',  'Foods_1_CA_2', 'Foods_1_CA_3')
foods_2 = c('Foods_2_CA_1',  'Foods_2_CA_2', 'Foods_2_CA_3')
foods_3 = c('Foods_3_CA_1',  'Foods_3_CA_2', 'Foods_3_CA_3')






# household_1
household_1_train1 = train1$Household_1_CA_1 + train1$Household_1_CA_2 + train1$Household_1_CA_3
household_1_test1 = test1$Household_1_CA_1 + test1$Household_1_CA_2 + test1$Household_1_CA_3
colnames(household_1_train1) <- c("household_1_agg")
colnames(household_1_test1) <- c("household_1_agg")

# household_2
household_2_train1 = train1$Household_2_CA_1 + train1$Household_2_CA_2 + train1$Household_2_CA_3
household_2_test1 = test1$Household_2_CA_1 + test1$Household_2_CA_2 + test1$Household_2_CA_3
colnames(household_2_train1) <- c("household_2_agg")
colnames(household_2_test1) <- c("household_2_agg")

# Foods1
Foods_1_train1 = train1$Foods_1_CA_1 + train1$Foods_1_CA_2 + train1$Foods_1_CA_3
Foods_1_test1 = test1$Foods_1_CA_1 + test1$Foods_1_CA_2 + test1$Foods_1_CA_3
colnames(Foods_1_train1) <- c("Foods_1_agg")
colnames(Foods_1_test1) <- c("Foods_1_agg")

# Foods2
Foods_2_train1 = train1$Foods_2_CA_1 + train1$Foods_2_CA_2 + train1$Foods_2_CA_3
Foods_2_test1 = test1$Foods_2_CA_1 + test1$Foods_2_CA_2 + test1$Foods_2_CA_3
colnames(Foods_2_train1) <- c("Foods_2_agg")
colnames(Foods_2_test1) <- c("Foods_2_agg")

# Foods3
Foods_3_train1 = train1$Foods_3_CA_1 + train1$Foods_3_CA_2 + train1$Foods_3_CA_3
Foods_3_test1 = test1$Foods_3_CA_1 + test1$Foods_3_CA_2 + test1$Foods_3_CA_3
colnames(Foods_3_train1) <- c("Foods_3_agg")
colnames(Foods_3_test1) <- c("Foods_3_agg")


# Produce forecasts for each aggregates using previous models
train_names <- c('hobbies_train1', 'household_1_train1', 'household_2_train1', 'Foods1_train1', 'Foods2_train1', 'Foods3_train1')
test_names <- c('hobbies_test1', 'household_1_test1', 'household_2_test1', 'Foods1_test1', 'Foods2_test1', 'Foods3_test1')


h = seq(1:28)  # the horizons

# Naive

for (i in 1:length(train_names)){
    for (k in h){
      naivef <- naive(get(paste0("train",i))[,1],h=k)
      print(paste0("train",i))
      print(colnames(get(paste0("train",i))[,1]))
      print(paste0('horizon is',k))
      print(summary(naivef))
    }
}


# sNaive

for (i in 1:length(train_names)){
    for (k in h){
      snaivef <- snaive(get(paste0("train",i))[,1],h=2*30)
      print(paste0("train",i))
      print(colnames(get(paste0("train",i))[,1]))
      print(paste0('horizon is',k))
      print(summary(naivef))
    }
}


## MA

for (i in 1:length(train_names)){
    for (k in h){
      maf = forecast(ma(get(paste0("train",i))[,1], order = 15), h=k )
      print(paste0("train",i))
      print(colnames(get(paste0("train",i))[,1]))
      print(paste0('horizon is',k))
      print(summary(maf))
    }
}


## ES


for (i in 1:length(train_names)){
  for (k in h){
    esf <- ses(get(paste0("train",i))[,1], h=1)
    print(paste0("train",i))
    print(colnames(get(paste0("train",i))[,1]))
    print(summary(esf))
  }
}


#SARIMA


for (i in 1:length(train_names)){
  for (k in h){
      sarima<-forecast(auto.arima(get(paste0("train",i))[,1],D=1,seasonal=TRUE), h=k)
      print(paste0("train",i))
      print(colnames(get(paste0("train",i))[,1]))
      print(paste0('horizon is',k))
      print(summary(sarima))
  }
}

#SARIMAX

for (i in 1:length(train_names)){
    for (k in h){
      sarimax<-forecast(auto.arima(tsdata$Hobbies_CA_1, xreg =tstemp$Air.min,seasonal = TRUE),xreg =tstemp$Air.min,h=1)
      print(paste0("train",i))
      print(colnames(get(paste0("train",i))[,1]))
      print(paste0('horizon is',k))
      print(summary(sarima))
    }
}


  
  #Holt winters
  
  
  for (i in 1:length(train_names)){
    for (n in 1:length(variables)){
      for (k in h){
        hw<-forecast(HoltWinters(get(paste0("train",i))[,n],gamma = FALSE), h=k)
        print(paste0("train",i))
        print(colnames(get(paste0("train",i))[,n]))
        print(paste0('horizon is',k))
        print(summary(sarima))
        
      }
    }
  }



# Aggregation at Store level

store_1 = c('Hobbies_CA_1', 'Household_1_CA_1', 'Household_2_CA_1', 'Foods_1_CA_1', 'Foods_2_CA_1', 'Foods_3_CA_1')
store_2 = c('Hobbies_CA_2', 'Household_1_CA_2', 'Household_2_CA_2', 'Foods_1_CA_2', 'Foods_2_CA_2', 'Foods_3_CA_2')
store_3 = c('Hobbies_CA_3', 'Household_1_CA_3', 'Household_2_CA_3', 'Foods_1_CA_3', 'Foods_2_CA_3', 'Foods_3_CA_3')


# Store 1
CA_1_train1 = train1$Hobbies_CA_1 + train1$Household_1_CA_1 + train1$Household_2_CA_1 + train1$Foods_1_CA_1 + train1$Foods_2_CA_1 + train1$Foods_3_CA_1
CA_1_test1 = test1$Hobbies_CA_1 + test1$Household_1_CA_1 + test1$Household_2_CA_1 + test1$Foods_1_CA_1 + test1$Foods_2_CA_1 + test1$Foods_3_CA_1
colnames(CA_1_train1) <- c("Store_1")
colnames(CA_1_test1) <- c("Store_1")


# Store 2
CA_2_train1 = train1$Hobbies_CA_2 + train1$Household_1_CA_2 + train1$Household_2_CA_2 + train1$Foods_1_CA_2 + train1$Foods_2_CA_2 + train1$Foods_3_CA_2
CA_2_test1 = test1$Hobbies_CA_2 + test1$Household_1_CA_2 + test1$Household_2_CA_2 + test1$Foods_1_CA_2 + test1$Foods_2_CA_2 + test1$Foods_3_CA_2
colnames(CA_2_train1) <- c("Store_2")
colnames(CA_2_test1) <- c("Store_2")

# Store 3
CA_3_train1 = train1$Hobbies_CA_3 + train1$Household_1_CA_3 + train1$Household_2_CA_3 + train1$Foods_1_CA_3 + train1$Foods_2_CA_3 + train1$Foods_3_CA_3
CA_3_test1 = test1$Hobbies_CA_3 + test1$Household_1_CA_3 + test1$Household_2_CA_3 + test1$Foods_1_CA_3 + test1$Foods_2_CA_3 + test1$Foods_3_CA_3
colnames(CA_3_train1) <- c("Store_3")
colnames(CA_3_test1) <- c("Store_3")

# Produce forecasts for each aggregates using previous models
train_names <- c('CA_1_train1', 'CA_2_train1', 'CA_3_train1')
test_names <- c('CA_1_test1', 'CA_2_test1', 'CA_3_test1')

h = seq(1:28)  # the horizons

# Naive

for (i in 1:length(train_names)){
  for (k in h){
    naivef <- naive(get(paste0("train",i))[,1],h=k)
    print(paste0("train",i))
    print(colnames(get(paste0("train",i))[,1]))
    print(paste0('horizon is',k))
    print(summary(naivef))
  }
}


# sNaive

for (i in 1:length(train_names)){
  for (k in h){
    snaivef <- snaive(get(paste0("train",i))[,1],h=2*30)
    print(paste0("train",i))
    print(colnames(get(paste0("train",i))[,1]))
    print(paste0('horizon is',k))
    print(summary(naivef))
  }
}


## MA

for (i in 1:length(train_names)){
  for (k in h){
    maf = forecast(ma(get(paste0("train",i))[,1], order = 15), h=k )
    print(paste0("train",i))
    print(colnames(get(paste0("train",i))[,1]))
    print(paste0('horizon is',k))
    print(summary(maf))
  }
}


## ES


for (i in 1:length(train_names)){
  for (k in h){
    esf <- ses(get(paste0("train",i))[,1], h=1)
    print(paste0("train",i))
    print(colnames(get(paste0("train",i))[,1]))
    print(summary(esf))
  }
}


#SARIMA


for (i in 1:length(train_names)){
  for (k in h){
    sarima<-forecast(auto.arima(get(paste0("train",i))[,1],D=1,seasonal=TRUE), h=k)
    print(paste0("train",i))
    print(colnames(get(paste0("train",i))[,1]))
    print(paste0('horizon is',k))
    print(summary(sarima))
  }
}

#SARIMAX

for (i in 1:length(train_names)){
  for (k in h){
    sarimax<-forecast(auto.arima(tsdata$Hobbies_CA_1, xreg =tstemp$Air.min,seasonal = TRUE),xreg =tstemp$Air.min,h=1)
    print(paste0("train",i))
    print(colnames(get(paste0("train",i))[,1]))
    print(paste0('horizon is',k))
    print(summary(sarima))
  }
}


  
  #Holt winters
  
  
  for (i in 1:length(train_names)){
    for (n in 1:length(variables)){
      for (k in h){
        hw<-forecast(HoltWinters(get(paste0("train",i))[,n],gamma = FALSE), h=k)
        print(paste0("train",i))
        print(colnames(get(paste0("train",i))[,n]))
        print(paste0('horizon is',k))
        print(summary(sarima))
        
      }
    }
  }

