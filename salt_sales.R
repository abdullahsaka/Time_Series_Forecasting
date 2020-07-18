# Load packages
library("forecast") 

# Import Data
demandData <- read.csv("/Users/sakahome/Rprojects/IDS 552/salt_sales.csv",sep=',')

# Extract time series and plot
dataFreq= 4 
startEntry= c(2012,2) 
demandTS <- ts(demandData$Demand, frequency=dataFreq,start=startEntry) 

# Plot time series
plot(demandTS,main = "Salt demand faced by the Tahoe Salt company",xlab="Year.Quarter",ylab="Salt demand") 

# Decompose time series and plot#
tsDecomp <- decompose(demandTS, type="multiplicative") 
plot(tsDecomp) 

# Prepare time series for forecasting
trainSetStart = c(2012,2)
trainSetEnd =  c(2015,3)
testSetStart = c(2015,4)
testSetEnd = c(2017,1)
demandTrain <- window(demandTS,start=trainSetStart,end=trainSetEnd) 
demandTest <- window(demandTS,start=testSetStart,end=testSetEnd) 

# Execute forecasting model
numForcPeriods = 6
HWForcModel <- HoltWinters(demandTrain,seasonal="multiplicative") 
HWForecast <- forecast:::forecast.HoltWinters(HWForcModel, h=numForcPeriods) 

# Plott actual vs Holt Winters values
plot(HWForecast, main="Plot of training demand, testing demand, and forecast with 80% and 95% prediction intervals",
     xlab="Year.Quarter", 
     ylab="Demand") 
lines(demandTest,col=2) 
legend("topleft", lty=1, col=c(1,4,2), 
legend=c("Training Demand","Forecast","Testing Demand")) 

# Analyze forecasting error
error = HWForecast$mean - demandTest 
AD=abs(error) 

# #Create empty vectors to store errors
MSE <- matrix(nrow = numForcPeriods, ncol = 1)
colnames(MSE) <- "MSE"

MAD <- matrix(nrow = numForcPeriods, ncol = 1)
colnames(MAD) <- "MAD"
MAPE <- matrix(nrow = numForcPeriods, ncol = 1)
colnames(MAPE) <- "MAPE"

bias <- matrix(nrow = numForcPeriods, ncol = 1)
colnames(bias) <- "bias"

TS <- matrix(nrow = numForcPeriods, ncol = 1)
colnames(TS) <- "TS"

# Compute errors 
for(t in 1:numForcPeriods){
      MSE[t] <- mean(error[1:t]*error[1:t])
      MAD[t] <- mean(AD[1:t])
      MAPE[t] <- mean(100*abs(error[1:t]/demandTest[1:t]))
      bias[t] <- sum(error[1:t])
      TS[t]= bias[t]/MAD[t]
}

# Combined vectors into a dataframe, also appending year and quarter information in the first two columns
error_Meas <- data.frame(floor(time(error)),cycle(error),demandTest,HWForecast$mean,error,AD,MSE,MAD,MAPE,bias,TS)
colnames(error_Meas)[1] <- "Year"
colnames(error_Meas)[2] <- "Qtr"
colnames(error_Meas)[3] <- "Actual demand"
colnames(error_Meas)[4] <- "Forecast"
error_Meas
