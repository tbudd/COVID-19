library(data.table)
library(tidyverse)

# read raw data
raw.data <- read_csv("time_series_covid19_confirmed_US.csv")
glob.data<-read_csv("time_series_covid19_recovered_global.csv")

va.data<-raw.data[raw.data$Province_State=="Virginia",]
aaf.data<-va.data[va.data$Admin2=="Alexandria" | va.data$Admin2=="Arlington" | va.data$Admin2=="Fairfax",]
aaf.data[1:57]<-list(NULL)


# create data frame with Date and Cumulative Colums from raw data
cum<-colSums(aaf.data)
date<-as.Date(colnames(aaf.data),"%m/%d/%Y")
covid<-data.frame(date,cum)
colnames(covid) <- c("Date", "cumulative")

# add sequence column to use for curvefitting
covid['seq'] <- c(1:23)

# curve fit
covid.fit<-curvefit(covid$seq,covid$cumulative, extrapol = 20, plot.curves = FALSE)

# create plotting dataframe from curve fitting results only for actual Cumulative and Logistics fit
forplot<-data.frame(covid.fit$models_y[,1],covid.fit$models_y[,2],covid.fit$models_y[,6])
colnames(forplot)<-c("Day","Cumulative","Logistic")

ggplot(forplot,aes(Day)) + 
  geom_line(aes(y = Cumulative, color = "Actual"),lwd=1.1) + 
  geom_line(aes(y = Logistic, color = "Logistics Curve Fit"),lwd=1.1) +
  labs(title = "NOVA Actual Cumulative and Logistics Function Fit for Covid-19 Cases", 
       x = "Days since March 8, 2020", y = "Cumulative Confirmed Cases")