library(data.table)
library(tidyverse)
library(REAT)

# read raw data
raw.us.time_series <- read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

va.data <- raw.us.time_series[raw.us.time_series$Province_State=="Virginia",]
aaf.data <- va.data[va.data$Admin2=="Alexandria" | va.data$Admin2=="Arlington" | va.data$Admin2=="Fairfax",]
aaf.data[1:57] <- list(NULL)

# create data frame with Date and Cumulative Colums from raw data
cum <- colSums(aaf.data)
date <- as.Date(colnames(aaf.data),"%m/%d/%Y")
covid <- data.frame(date,cum)
colnames(covid) <- c("Date", "cumulative")

# add sequence column to use for curvefitting
covid['seq'] <- c(1:length(covid$cumulative))

# create velocity column
velSubtrahend <- c(0,covid$cumulative)
velSubtrahend <- velSubtrahend[1:length(velSubtrahend)-1]
covid['velocity'] <- (covid$cumulative - velSubtrahend)

# create acceleration column
accSubtrahend <- c(0,covid$velocity)
accSubtrahend <- accSubtrahend[1:length(accSubtrahend)-1]
covid['acceleration'] <- (covid$velocity - accSubtrahend)

# curve fit
covid.fit<-curvefit(covid$seq,covid$cumulative, extrapol = 20, plot.curves = FALSE)

# create plotting dataframe from curve fitting results only for actual Cumulative and Logistics fit
forplot <- data.frame(covid.fit$models_y[,1],covid.fit$models_y[,2],covid.fit$models_y[,6])
colnames(forplot) <- c("Day","Cumulative","Logistic")

ggplot(covid,aes(x = Date, y = velocity)) + geom_line(lwd = 1.1) +
  labs(title = "NOVA Covid-19 Cases Velocity")

ggplot(covid,aes(x = Date, y = acceleration)) + geom_line(lwd = 1.1) +
  labs(title = "NOVA Covid-19 Cases Acceleration")

ggplot(forplot,aes(Day)) + 
  geom_line(aes(y = Cumulative, color = "Actual"),lwd=1.1) + 
  geom_line(aes(y = Logistic, color = "Logistics Curve Fit"),lwd=1.1) +
  labs(title = "NOVA Actual Cumulative and Logistics Function Fit for Covid-19 Cases", 
       x = "Days since March 8, 2020", y = "Cumulative Confirmed Cases")
