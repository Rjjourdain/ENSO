## clear variables & window
rm(list=ls(all=TRUE)) 

library(biwavelet)
library(forecast)
library(Hmisc)

##Load Climate Division data
setwd("/Users/Ryan/Documents")
caliPCP = read.table("AllCaliPCP.csv", sep = ",", header = FALSE)

##Fix the values for months in which no data has been collected 
caliPCP[caliPCP==-9.99] <- NA

## Add headers to each column
names(caliPCP)[1:13] <- c("Date", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

## change the format of the data to have only one column
caliPCP_1 <- data.frame(caliPCP[,1],stack(caliPCP[,-1]))

#partitition strings split up into characters
ID <- caliPCP[,1]
State <- substring(ID,1,1)
Div <- substring(ID,2,3)
Date <- substring(ID,6)
Type <- substring(ID,4,5)

##Manipulate data to be organized by State, Division, Type, Date & Month
modelcaliPCP <- data.frame(State = State,Div = Div ,Type=Type,Date  = Date , Month = stack(caliPCP[,-1]))

#Organize new dataframe by date
modelD1 <- modelcaliPCP[order(modelcaliPCP$Div,modelcaliPCP$Date),]

##Change format of dataframe to be organized by State, Division, Type, Date & Preciptiation
#Change format of date to year-month-day
modelcaliPCP2 <- data.frame(State = modelcaliPCP[,1],
			Div = modelcaliPCP[,2],
			Type = modelcaliPCP[,3],
			Date = as.Date(paste(modelcaliPCP[,4],modelcaliPCP[,6],'01',sep='/'),format='%Y/%b/%d'), 			Precipitation = modelcaliPCP[,5])

##Save table to an excel file
write.table(modelcaliPCP2, file = "AllCaliPCPNew.csv", sep = ",", row.names = FALSE)

###Accquire the data for the specific climate division

#Climate Division 1
ClimDiv1<-subset(modelcaliPCP2,Div == "01",select=State:Precipitation)
ClimDiv1 <- ClimDiv1[order(ClimDiv1$Date),]

#Climate Division 2
ClimDiv2<-subset(modelcaliPCP2,Div == "02",select=State:Precipitation)
ClimDiv2 <- ClimDiv2[order(ClimDiv2$Date),]

#Climate Division 3
ClimDiv3<-subset(modelcaliPCP2,Div == "03",select=State:Precipitation)
ClimDiv3 <- ClimDiv3[order(ClimDiv3$Date),]

#Climate Division 4
ClimDiv4<-subset(modelcaliPCP2,Div == "04",select=State:Precipitation)
ClimDiv4 <- ClimDiv4[order(ClimDiv4$Date),]

#Climate Division 5
ClimDiv5<-subset(modelcaliPCP2,Div == "05",select=State:Precipitation)
ClimDiv5 <- ClimDiv5[order(ClimDiv5$Date),]

#Climate Division 6
ClimDiv6<-subset(modelcaliPCP2,Div == "06",select=State:Precipitation)
ClimDiv6 <- ClimDiv6[order(ClimDiv6$Date),]

#Climate Division 7
ClimDiv7<-subset(modelcaliPCP2,Div == "07",select=State:Precipitation)
ClimDiv7 <- ClimDiv7[order(ClimDiv7$Date),]


### zClimate Divison 1 Plots

# read guage data
monthlyD1 <- ClimDiv1
monthlyD1<-monthlyD1[which(is.na(monthlyD1$Precipitation) == FALSE),]


# Change the date format to represent the first of every month
monthlyD1$Date <- as.Date(as.character(monthlyD1$Date), format='%Y-%m-%d')

# time series
monthlyD1_TS <- ts(monthlyD1$Precipitation, start = c(1900+as.POSIXlt(monthlyD1[1,4])$year, 1+as.POSIXlt(monthlyD1[1,4])$mon), 
		end = c(1900+as.POSIXlt(monthlyD1[dim(monthlyD1)[1],4])$year, 1+as.POSIXlt(monthlyD1[dim(monthlyD1)[1],4])$mon),
		frequency = 12)

# remove seasonal and trend components
monthlyD1_STL <-stl(monthlyD1_TS, "periodic",na.action=na.omit)

## Plot STL
par(cex.lab=2, cex.axis=2, cex.main=2) 
plot(monthlyD1_STL, main = "Precipitation in California, Climate Div. 1")

# obtain only the remainder data
monthlyD1_remainder1 <- monthlyD1_STL$time.series[,3]

# Calculate the arima model
monthlyD1_arima <- auto.arima(monthlyD1_remainder1)

## Plot Remainder Data with confidence intervals
par(cex.lab=1.5, cex.axis=1.5, cex.main=1.5) 
plot(monthlyD1_remainder1, main = "Precipitation Remainder in California, Climate Div. 1", xaxt='n', yaxt='n', ylab = "Frequency")
abline(h=0, col = "red")
abline(h=1.96*sqrt(monthlyD1_arima$sigma2), col = "green")
abline(h=1.96*-sqrt(monthlyD1_arima$sigma2), col = "green")
axis(1, las=1, at=2*0:monthlyD1[dim(monthlyD1)[1],4], cex=1.5)
axis(2, las=1)

# Acquire dates outside the lines of confidence
aboveD1=monthlyD1$Date[which(monthlyD1_remainder1 > 1.96*sqrt(monthlyD1_arima$sigma2))]
belowD1=monthlyD1$Date[which(monthlyD1_remainder1 < 1.96*-sqrt(monthlyD1_arima$sigma2))]

# X-Axis for plotting
remainder1 <- data.frame(Date = monthlyD1[,4], Residual = monthlyD1_remainder1)
wavelet1 <- wt(subset(ClimDiv1, select = c("Date", "Precipitation"))[which(is.na(ClimDiv1$Precipitation) == FALSE),])
remainder1$Date <- as.Date(remainder1$Date, format = "%Y-%m-%d")
TIME <- strptime( paste(remainder1$Date, remainder1$Date), format = "%Y-%m-%d ")
LABELS <- seq(from = TIME[1], to = TIME[length(TIME)], by = "5 year")
xAxis <- seq(from = TIME[1], to = TIME[length(TIME)], by = "1 month")
Location <- NA
for (i in 1:length(LABELS)) { Location[i] <- which(LABELS[i] == xAxis) }
LABELS <- format(LABELS, "%Y")
wav1 <- wt(subset(ClimDiv1, select = c("Date", "Precipitation"))[which(is.na(ClimDiv1$Precipitation) == FALSE),])

remainder1 <- data.frame(Date = monthlyD1[,4], Residual = monthlyD1_remainder1)
remainder1$Date <- as.Date(remainder1$Date, format = "%Y-%m-%d")
 WAV1 <- wt(remainder1)
 
 
## Plot Wavelet with intensity spectrum
par(oma=c(1, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
plot(WAV1, type="power.corr.norm", main=paste("Precipitation in California, Climate Div. 1", sep=""), ylab = "Period", xlab = "", lwd.sig=1, xaxt='n', plot.cb=TRUE)
title(xlab="Time", line=4) # move labeling of x axis
axis(side = 1, at = Location, labels = LABELS, tick = TRUE, las = 2)

### Climate Divison 2 Plots

# read guage data
monthlyD2 <- ClimDiv2
monthlyD2<-monthlyD2[which(is.na(monthlyD2$Precipitation) == FALSE),]


# Change the date format to represent the first of every month
monthlyD2$Date <- as.Date(as.character(monthlyD2$Date), format='%Y-%m-%d')

# time series
monthlyD2_TS <- ts(monthlyD2$Precipitation, start = c(1900+as.POSIXlt(monthlyD2[1,4])$year, 1+as.POSIXlt(monthlyD2[1,4])$mon), 
		end = c(1900+as.POSIXlt(monthlyD2[dim(monthlyD2)[1],4])$year, 1+as.POSIXlt(monthlyD2[dim(monthlyD2)[1],4])$mon),
		frequency = 12)

# remove seasonal and trend components
monthlyD2_STL <-stl(monthlyD2_TS, "periodic",na.action=na.omit)



# plot STL
par(cex.lab=2, cex.axis=2, cex.main=2) 
plot(monthlyD2_STL, main = "Precipitation in California, Climate Div. 2")


# obtain only the remainder data
monthlyD2_remainder2 <- monthlyD2_STL$time.series[,3]

# Calculate the arima model
monthlyD2_arima <- auto.arima(monthlyD2_remainder2)


## Plot Remainder Data with confidence intervals
par(cex.lab=1.5, cex.axis=1.5, cex.main=1.5) 
plot(monthlyD2_remainder2, main = "Precipitation Remainder in California, Climate Div. 2", xaxt='n', yaxt='n', ylab = "Frequency")
abline(h=0, col = "red")
abline(h=1.96*sqrt(monthlyD2_arima$sigma2), col = "green")
abline(h=1.96*-sqrt(monthlyD2_arima$sigma2), col = "green")
axis(1, las=1, at=2*0:monthlyD2[dim(monthlyD2)[1],4], cex=1.5)
axis(2, las=1)

# Acquire dates outside the lines of confidence
aboveD2=monthlyD2$Date[which(monthlyD2_remainder2 > 1.96*sqrt(monthlyD2_arima$sigma2))]
belowD2=monthlyD2$Date[which(monthlyD2_remainder2 < 1.96*-sqrt(monthlyD2_arima$sigma2))]


# X-Axis for plotting
remainder2 <- data.frame(Date = monthlyD2[,4], Residual = monthlyD2_remainder2)
wavelet2 <- wt(subset(ClimDiv2, select = c("Date", "Precipitation"))[which(is.na(ClimDiv2$Precipitation) == FALSE),])
remainder2$Date <- as.Date(remainder2$Date, format = "%Y-%m-%d")
TIME <- strptime( paste(remainder2$Date, remainder2$Date), format = "%Y-%m-%d ")
LABELS <- seq(from = TIME[1], to = TIME[length(TIME)], by = "5 year")
xAxis <- seq(from = TIME[1], to = TIME[length(TIME)], by = "1 month")
Location <- NA
for (i in 1:length(LABELS)) { Location[i] <- which(LABELS[i] == xAxis) }
LABELS <- format(LABELS, "%b '%y")
wav2 <- wt(subset(ClimDiv2, select = c("Date", "Precipitation"))[which(is.na(ClimDiv2$Precipitation) == FALSE),])

remainder2 <- data.frame(Date = monthlyD2[,4], Residual = monthlyD2_remainder2)
remainder2$Date <- as.Date(remainder2$Date, format = "%Y-%m-%d")
 WAV2 <- wt(remainder2)
 
## Plot Wavelet with intensity spectrum
par(oma=c(1, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
plot(WAV2, type="power.corr.norm", main=paste("Precipitation in California, Climate Div. 2", sep=""), ylab = "Period", xlab = "", lwd.sig=1, xaxt='n', plot.cb=TRUE)
title(xlab="Time", line=4) # move labeling of x axis
axis(side = 1, at = Location, labels = LABELS, tick = TRUE, las = 2)


### Climate Divison 3 Plots

# read guage data
monthlyD3 <- ClimDiv3
monthlyD3<-monthlyD3[which(is.na(monthlyD3$Precipitation) == FALSE),]


# Change the date format to represent the first of every month
monthlyD3$Date <- as.Date(as.character(monthlyD3$Date), format='%Y-%m-%d')

# time series
monthlyD3_TS <- ts(monthlyD3$Precipitation, start = c(1900+as.POSIXlt(monthlyD3[1,4])$year, 1+as.POSIXlt(monthlyD3[1,4])$mon), 
		end = c(1900+as.POSIXlt(monthlyD3[dim(monthlyD3)[1],4])$year, 1+as.POSIXlt(monthlyD3[dim(monthlyD3)[1],4])$mon),
		frequency = 12)

# remove seasonal and trend components
monthlyD3_STL <-stl(monthlyD3_TS, "periodic",na.action=na.omit)



##Plot STL
par(cex.lab=2, cex.axis=2, cex.main=2) 
plot(monthlyD3_STL, main = "Precipitation in California, Climate Div. 3")


# obtain only the remainder data
monthlyD3_remainder3 <- monthlyD3_STL$time.series[,3]

# Calculate the arima model
monthlyD3_arima <- auto.arima(monthlyD3_remainder3)

## Plot Remainder Data with confidence intervals
par(cex.lab=1.5, cex.axis=1.5, cex.main=1.5) 
plot(monthlyD3_remainder3, main = "Precipitation Remainder in California, Climate Div. 3", xaxt='n', yaxt='n', ylab = "Frequency")
abline(h=0, col = "red")
abline(h=1.96*sqrt(monthlyD3_arima$sigma2), col = "green")
abline(h=1.96*-sqrt(monthlyD3_arima$sigma2), col = "green")
axis(1, las=1, at=2*0:monthlyD3[dim(monthlyD3)[1],4], cex=1.5)
axis(2, las=1)

# Acquire dates outside the lines of confidence
aboveD3=monthlyD3$Date[which(monthlyD3_remainder3 > 1.96*sqrt(monthlyD3_arima$sigma2))]
belowD3=monthlyD3$Date[which(monthlyD3_remainder3 < 1.96*-sqrt(monthlyD3_arima$sigma2))]


# X-Axis for plotting
remainder3 <- data.frame(Date = monthlyD3[,4], Residual = monthlyD3_remainder3)
wavelet3 <- wt(subset(ClimDiv3, select = c("Date", "Precipitation"))[which(is.na(ClimDiv3$Precipitation) == FALSE),])
remainder3$Date <- as.Date(remainder3$Date, format = "%Y-%m-%d")
TIME <- strptime( paste(remainder3$Date, remainder3$Date), format = "%Y-%m-%d ")
LABELS <- seq(from = TIME[1], to = TIME[length(TIME)], by = "5 year")
xAxis <- seq(from = TIME[1], to = TIME[length(TIME)], by = "1 month")
Location <- NA
for (i in 1:length(LABELS)) { Location[i] <- which(LABELS[i] == xAxis) }
LABELS <- format(LABELS, "%b '%y")
wav3 <- wt(subset(ClimDiv3, select = c("Date", "Precipitation"))[which(is.na(ClimDiv3$Precipitation) == FALSE),])

remainder3 <- data.frame(Date = monthlyD3[,4], Residual = monthlyD3_remainder3)
remainder3$Date <- as.Date(remainder3$Date, format = "%Y-%m-%d")
 WAV3 <- wt(remainder3)

## Plot Wavelet with intensity spectrum
par(oma=c(1, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
plot(WAV3, type="power.corr.norm", main=paste("Precipitation in California, Climate Div. 3", sep=""), ylab = "Period", xlab = "", lwd.sig=1, xaxt='n', plot.cb=TRUE)
title(xlab="Time", line=4) # move labeling of x axis
axis(side = 1, at = Location, labels = LABELS, tick = TRUE, las = 2)

### Climate Divison 4 Plots

# read guage data
monthlyD4 <- ClimDiv4
monthlyD4<-monthlyD4[which(is.na(monthlyD4$Precipitation) == FALSE),]


# Change the date format to represent the first of every month
monthlyD4$Date <- as.Date(as.character(monthlyD4$Date), format='%Y-%m-%d')

# time series
monthlyD4_TS <- ts(monthlyD4$Precipitation, start = c(1900+as.POSIXlt(monthlyD4[1,4])$year, 1+as.POSIXlt(monthlyD4[1,4])$mon), 
		end = c(1900+as.POSIXlt(monthlyD4[dim(monthlyD4)[1],4])$year, 1+as.POSIXlt(monthlyD4[dim(monthlyD4)[1],4])$mon),
		frequency = 12)

# remove seasonal and trend components
monthlyD4_STL <-stl(monthlyD4_TS, "periodic",na.action=na.omit)



## Plot STL
par(cex.lab=2, cex.axis=2, cex.main=2) 
plot(monthlyD4_STL, main = "Precipitation in California, Climate Div. 4")


# obtain only the remainder data
monthlyD4_remainder4 <- monthlyD4_STL$time.series[,3]

# Calculate the arima model
monthlyD4_arima <- auto.arima(monthlyD4_remainder4)

## Plot Remainder Data with confidence intervals
par(cex.lab=1.5, cex.axis=1.5, cex.main=1.5) 
plot(monthlyD4_remainder4, main = "Precipitation Remainder in California, Climate Div. 4", xaxt='n', yaxt='n', ylab = "Frequency")
abline(h=0, col = "red")
abline(h=1.96*sqrt(monthlyD4_arima$sigma2), col = "green")
abline(h=1.96*-sqrt(monthlyD4_arima$sigma2), col = "green")
axis(1, las=1, at=2*0:monthlyD4[dim(monthlyD4)[1],4], cex=1.5)
axis(2, las=1)

# Acquire dates outside the lines of confidence
aboveD4=monthlyD4$Date[which(monthlyD4_remainder4 > 1.96*sqrt(monthlyD4_arima$sigma2))]
belowD4=monthlyD4$Date[which(monthlyD4_remainder4 < 1.96*-sqrt(monthlyD4_arima$sigma2))]


# X-Axis for plotting
remainder4 <- data.frame(Date = monthlyD4[,4], Residual = monthlyD4_remainder4)
wavelet4 <- wt(subset(ClimDiv4, select = c("Date", "Precipitation"))[which(is.na(ClimDiv4$Precipitation) == FALSE),])
remainder4$Date <- as.Date(remainder4$Date, format = "%Y-%m-%d")
TIME <- strptime( paste(remainder4$Date, remainder4$Date), format = "%Y-%m-%d ")
LABELS <- seq(from = TIME[1], to = TIME[length(TIME)], by = "5 year")
xAxis <- seq(from = TIME[1], to = TIME[length(TIME)], by = "1 month")
Location <- NA
for (i in 1:length(LABELS)) { Location[i] <- which(LABELS[i] == xAxis) }
LABELS <- format(LABELS, "%b '%y")
wav4 <- wt(subset(ClimDiv4, select = c("Date", "Precipitation"))[which(is.na(ClimDiv4$Precipitation) == FALSE),])

remainder4 <- data.frame(Date = monthlyD4[,4], Residual = monthlyD4_remainder4)
remainder4$Date <- as.Date(remainder4$Date, format = "%Y-%m-%d")
 WAV4 <- wt(remainder4)
 
## Plot Wavelet with intensity spectrum
par(oma=c(1, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
plot(WAV4, type="power.corr.norm", main=paste("Precipitation in California, Climate Div. 4", sep=""), ylab = "Period", xlab = "", lwd.sig=1, xaxt='n', plot.cb=TRUE)
title(xlab="Time", line=4) # move labeling of x axis
axis(side = 1, at = Location, labels = LABELS, tick = TRUE, las = 2)

### Climate Divison 5 Plots

# read guage data
monthlyD5 <- ClimDiv5
monthlyD5<-monthlyD5[which(is.na(monthlyD5$Precipitation) == FALSE),]


# Change the date format to represent the first of every month
monthlyD5$Date <- as.Date(as.character(monthlyD5$Date), format='%Y-%m-%d')

# time series
monthlyD5_TS <- ts(monthlyD5$Precipitation, start = c(1900+as.POSIXlt(monthlyD5[1,4])$year, 1+as.POSIXlt(monthlyD5[1,4])$mon), 
		end = c(1900+as.POSIXlt(monthlyD5[dim(monthlyD5)[1],4])$year, 1+as.POSIXlt(monthlyD5[dim(monthlyD5)[1],4])$mon),
		frequency = 12)

# remove seasonal and trend components
monthlyD5_STL <-stl(monthlyD5_TS, "periodic",na.action=na.omit)



## Plot STL
par(cex.lab=2, cex.axis=2, cex.main=2) 
plot(monthlyD5_STL, main = "Precipitation in California, Climate Div. 5")


# obtain only the remainder data
monthlyD5_remainder5 <- monthlyD5_STL$time.series[,3]

# Calculate the arima model
monthlyD5_arima <- auto.arima(monthlyD5_remainder5)

## Plot Remainder Data with confidence intervals
par(cex.lab=1.5, cex.axis=1.5, cex.main=1.5) 
plot(monthlyD5_remainder5, main = "Precipitation Remainder in California, Climate Div. 5", xaxt='n', yaxt='n', ylab = "Frequency")
abline(h=0, col = "red")
abline(h=1.96*sqrt(monthlyD5_arima$sigma2), col = "green")
abline(h=1.96*-sqrt(monthlyD5_arima$sigma2), col = "green")
axis(1, las=1, at=2*0:monthlyD5[dim(monthlyD5)[1],4], cex=1.5)
axis(2, las=1)

# Acquire dates outside the lines of confidence
aboveD5=monthlyD5$Date[which(monthlyD5_remainder5 > 1.96*sqrt(monthlyD5_arima$sigma2))]
belowD5=monthlyD5$Date[which(monthlyD5_remainder5 < 1.96*-sqrt(monthlyD5_arima$sigma2))]

# X-Axis for plotting
remainder5 <- data.frame(Date = monthlyD5[,4], Residual = monthlyD5_remainder5)
wavelet5 <- wt(subset(ClimDiv5, select = c("Date", "Precipitation"))[which(is.na(ClimDiv5$Precipitation) == FALSE),])
remainder5$Date <- as.Date(remainder5$Date, format = "%Y-%m-%d")
TIME <- strptime( paste(remainder5$Date, remainder5$Date), format = "%Y-%m-%d ")
LABELS <- seq(from = TIME[1], to = TIME[length(TIME)], by = "5 year")
xAxis <- seq(from = TIME[1], to = TIME[length(TIME)], by = "1 month")
Location <- NA
for (i in 1:length(LABELS)) { Location[i] <- which(LABELS[i] == xAxis) }
LABELS <- format(LABELS, "%b '%y")
wav5 <- wt(subset(ClimDiv5, select = c("Date", "Precipitation"))[which(is.na(ClimDiv5$Precipitation) == FALSE),])

remainder5 <- data.frame(Date = monthlyD5[,4], Residual = monthlyD5_remainder5)
remainder5$Date <- as.Date(remainder5$Date, format = "%Y-%m-%d")
 WAV5 <- wt(remainder5)
 
## Plot Wavelet with intensity spectrum
par(oma=c(1, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
plot(WAV5, type="power.corr.norm", main=paste("Precipitation in California, Climate Div. 5", sep=""), ylab = "Period", xlab = "", lwd.sig=1, xaxt='n', plot.cb=TRUE)
title(xlab="Time", line=4) # move labeling of x axis
axis(side = 1, at = Location, labels = LABELS, tick = TRUE, las = 2)

### Climate Divison 6 Plots

# read guage data
monthlyD6 <- ClimDiv6
monthlyD6<-monthlyD6[which(is.na(monthlyD6$Precipitation) == FALSE),]


# Change the date format to represent the first of every month
monthlyD6$Date <- as.Date(as.character(monthlyD6$Date), format='%Y-%m-%d')

# time series
monthlyD6_TS <- ts(monthlyD6$Precipitation, start = c(1900+as.POSIXlt(monthlyD6[1,4])$year, 1+as.POSIXlt(monthlyD6[1,4])$mon), 
		end = c(1900+as.POSIXlt(monthlyD6[dim(monthlyD6)[1],4])$year, 1+as.POSIXlt(monthlyD6[dim(monthlyD6)[1],4])$mon),
		frequency = 12)

# remove seasonal and trend components
monthlyD6_STL <-stl(monthlyD6_TS, "periodic",na.action=na.omit)



## Plot STL
par(cex.lab=2, cex.axis=2, cex.main=2) 
plot(monthlyD6_STL, main = "Precipitation in California, Climate Div. 6[LA/SD]")


# obtain only the remainder data
monthlyD6_remainder6 <- monthlyD6_STL$time.series[,3]

# Calculate the arima model
monthlyD6_arima <- auto.arima(monthlyD6_remainder6)

## Plot Remainder Data with confidence intervals
par(cex.lab=1.5, cex.axis=1.5, cex.main=1.5) 
plot(monthlyD6_remainder6, main = "Precipitation Remainder in California, Climate Div. 6 [LA/SD]", xaxt='n', yaxt='n', ylab = "Frequency")
abline(h=0, col = "red")
abline(h=1.96*sqrt(monthlyD6_arima$sigma2), col = "green")
abline(h=1.96*-sqrt(monthlyD6_arima$sigma2), col = "green")
axis(1, las=1, at=2*0:monthlyD6[dim(monthlyD6)[1],4], cex=1.5)
axis(2, las=1)

# Acquire dates outside the lines of confidence
aboveD6=monthlyD6$Date[which(monthlyD6_remainder6 > 1.96*sqrt(monthlyD6_arima$sigma2))]
belowD6=monthlyD6$Date[which(monthlyD6_remainder6 < 1.96*-sqrt(monthlyD6_arima$sigma2))]

# X-Axis for plotting
remainder6 <- data.frame(Date = monthlyD6[,4], Residual = monthlyD6_remainder6)
wavelet6 <- wt(subset(ClimDiv6, select = c("Date", "Precipitation"))[which(is.na(ClimDiv6$Precipitation) == FALSE),])
remainder6$Date <- as.Date(remainder6$Date, format = "%Y-%m-%d")
TIME <- strptime( paste(remainder6$Date, remainder6$Date), format = "%Y-%m-%d ")
LABELS <- seq(from = TIME[1], to = TIME[length(TIME)], by = "5 year")
xAxis <- seq(from = TIME[1], to = TIME[length(TIME)], by = "1 month")
Location <- NA
for (i in 1:length(LABELS)) { Location[i] <- which(LABELS[i] == xAxis) }
LABELS <- format(LABELS, "%Y")
wav6 <- wt(subset(ClimDiv6, select = c("Date", "Precipitation"))[which(is.na(ClimDiv6$Precipitation) == FALSE),])

remainder6 <- data.frame(Date = monthlyD6[,4], Residual = monthlyD6_remainder6)
remainder6$Date <- as.Date(remainder6$Date, format = "%Y-%m-%d")
 WAV6 <- wt( remainder6)


##Plot Wavelet with intensity spectrum
par(oma=c(1, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
plot(WAV6, type="power.corr.norm", main=paste("Precipitation in California, Climate Div. 6 [LA/SD]", sep=""), ylab = "Period", xlab = "", lwd.sig=1, xaxt='n', plot.cb=TRUE)
title(xlab="Time", line=4) # move labeling of x axis
axis(side = 1, at = Location, labels = LABELS, tick = TRUE, las = 2)

### Climate Divison 7 Plots

# read guage data
monthlyD7 <- ClimDiv7
monthlyD7<-monthlyD7[which(is.na(monthlyD7$Precipitation) == FALSE),]


# Change the date format to represent the first of every month
monthlyD7$Date <- as.Date(as.character(monthlyD7$Date), format='%Y-%m-%d')

# time series
monthlyD7_TS <- ts(monthlyD7$Precipitation, start = c(1900+as.POSIXlt(monthlyD7[1,4])$year, 1+as.POSIXlt(monthlyD7[1,4])$mon), 
		end = c(1900+as.POSIXlt(monthlyD7[dim(monthlyD7)[1],4])$year, 1+as.POSIXlt(monthlyD7[dim(monthlyD7)[1],4])$mon),
		frequency = 12)

# remove seasonal and trend components
monthlyD7_STL <-stl(monthlyD7_TS, "periodic",na.action=na.omit)



## Plot STL
par(cex.lab=2, cex.axis=2, cex.main=2) 
plot(monthlyD7_STL, main = "Precipitation in California, Climate Div. 7")


# obtain only the remainder data
monthlyD7_remainder7 <- monthlyD7_STL$time.series[,3]

# Calculate the arima model
monthlyD7_arima <- auto.arima(monthlyD7_remainder7)

par(cex.lab=1.5, cex.axis=1.5, cex.main=1.5) 
plot(monthlyD7_remainder7, main = "Precipitation Remainder in California, Climate Div. 7", xaxt='n', yaxt='n', ylab = "Frequency")
abline(h=0, col = "red")
abline(h=1.96*sqrt(monthlyD7_arima$sigma2), col = "green")
abline(h=1.96*-sqrt(monthlyD7_arima$sigma2), col = "green")
axis(1, las=1, at=2*0:monthlyD7[dim(monthlyD7)[1],4], cex=1.5)
axis(2, las=1)

# Acquire dates outside the lines of confidence
aboveD7=monthlyD7$Date[which(monthlyD7_remainder7 > 1.96*sqrt(monthlyD7_arima$sigma2))]
belowD7=monthlyD7$Date[which(monthlyD7_remainder7 < 1.96*-sqrt(monthlyD7_arima$sigma2))]

# X-Axis for plotting
remainder7 <- data.frame(Date = monthlyD7[,4], Residual = monthlyD7_remainder7)
wavelet7 <- wt(subset(ClimDiv7, select = c("Date", "Precipitation"))[which(is.na(ClimDiv7$Precipitation) == FALSE),])
remainder7$Date <- as.Date(remainder7$Date, format = "%Y-%m-%d")
TIME <- strptime( paste(remainder7$Date, remainder7$Date), format = "%Y-%m-%d ")
LABELS <- seq(from = TIME[1], to = TIME[length(TIME)], by = "5 year")
xAxis <- seq(from = TIME[1], to = TIME[length(TIME)], by = "1 month")
Location <- NA
for (i in 1:length(LABELS)) { Location[i] <- which(LABELS[i] == xAxis) }
LABELS <- format(LABELS, "%b '%y")
wav7 <- wt(subset(ClimDiv7, select = c("Date", "Precipitation"))[which(is.na(ClimDiv7$Precipitation) == FALSE),])

remainder7 <- data.frame(Date = monthlyD7[,4], Residual = monthlyD7_remainder7)
remainder7$Date <- as.Date(remainder7$Date, format = "%Y-%m-%d")
 WAV7 <- wt(remainder7)
 
## Plot Wavelet with intensity spectrum
par(oma=c(1, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
plot(WAV7, type="power.corr.norm", main=paste("Precipitation in California, Climate Div. 7", sep=""), ylab = "Period", xlab = "", lwd.sig=1, xaxt='n', plot.cb=TRUE)
title(xlab="Time", line=4) # move labeling of x axis
axis(side = 1, at = Location, labels = LABELS, tick = TRUE, las = 2)
