# Function to get data #
getData <- function(sql, n = -1){
  #setup connection
  res <- dbSendQuery(wrds, sql)
  dbHasCompleted(res)
  
  #perform fetch
  returnData <- fetch(res, n)
  
  #clear memory
  dbClearResult(res)
  return(returnData)
}

buildOutputMatrix <- function(daily,monthly,annual,fiveYear){
  returnsMatrix <- matrix(ncol= 2,nrow = 4)
  colnames(returnsMatrix) <- c("Arithmetic Mean","Geometric Mean")
  row.names(returnsMatrix) <- c("Daily","Monthly","Annually","5 Yearly")
  
  #daily
  returnsMatrix["Daily",1] <- mean(daily$vwretd)
  returnsMatrix["Daily",2] <- prod(1+daily$vwretd)^(1/length(daily$vwretd))-1
  
  
  #monthly
  returnsMatrix["Monthly",1] <- mean(monthly$vwretd)
  returnsMatrix["Monthly",2] <- prod(1+monthly$vwretd)^(1/length(monthly$vwretd))-1
  
  #yearly
  returnsMatrix["Annually",1] <- mean(annual$vwretd)
  returnsMatrix["Annually",2] <- prod(1+annual$vwretd)^(1/length(annual$vwretd))-1
  
  #5year
  returnsMatrix["5 Yearly",1] <- mean(fiveYear$vwretd)
  returnsMatrix["5 Yearly",2] <- prod(1+fiveYear$vwretd)^(1/length(fiveYear$vwretd))-1
  
  returnsMatrix
  
}

CleanupRatesDaily <- function(sp500.dailyreturns,rates.daily){
  mergedData <- merge.xts(sp500.dailyreturns,rates.daily,join = "left")
  na.approx(mergedData)
}


#2) #####STOCK DATA########
#DAILY
sql.dailyreturns <- "SELECT caldt, vwretd FROM CRSPQ.DSP500 WHERE year(caldt) >= 1973 AND year(caldt) < 2015"
sp500.dailyreturns <- getData(sql.dailyreturns)

#Get Time Series
library(xts)
sp500.dailyreturns$caldt <- as.Date(sp500.dailyreturns$caldt)
sp500.dailyreturns <- xts(sp500.dailyreturns[,-1],order.by = sp500.dailyreturns$caldt)
colnames(sp500.dailyreturns) <- "vwretd"

#MONTHLY
sp500.monthlyreturns <- apply.monthly(1+sp500.dailyreturns,FUN=prod) - 1

#ANNUALLY
sp500.annualreturns <- apply.yearly(1+sp500.dailyreturns,FUN=prod) - 1

#5YEARLY
#split data into 5 year sets
fiveyearsplit <- split(1+sp500.annualreturns[,1],f = "years",k=5)
#get endpoints of these sets
fiveyearEndpoints <- lapply(lapply(fiveyearsplit, index),max)
#calculate the 5 year returns for each set
sp500.5yearreturns <- do.call(rbind,lapply(fiveyearsplit, prod))
#combine 4 year returns and endpoints. Remove first one, as it had only 2 years in the set
sp500.5yearreturns <- xts(sp500.5yearreturns[-1,]-1,as.Date(unlist(fiveyearEndpoints[-1])))
colnames(sp500.5yearreturns) <- "vwretd"

#3 ######CALCULATE STOCK MEANS######
buildOutputMatrix(sp500.dailyreturns,sp500.monthlyreturns,sp500.annualreturns,sp500.5yearreturns)


#4 ######TREASURY RATES#######

##DAILY
sql.dailyRates <- "SELECT caldt,tdyld,tdduratn FROM CRSP.TFZ_DLY_RF2 WHERE year(caldt)>=1973 AND year(caldt) < 2015 AND kytreasnox = 2000061"
rates.daily <- getData(sql.dailyRates)

rates.daily$CALDT <- as.Date(rates.daily$CALDT)
rates.daily$TDYLD <- (1+rates.daily$TDYLD)^(1/rates.daily$TDDURATN) - 1
rates.daily <- xts(rates.daily$TDYLD,order.by = rates.daily$CALDT)
colnames(rates.daily) <- "rate"
##the number of daily Rates from CRSP doesn't match the daily SP500 returns.
## Few of the reasons are i) Some Public holidays have data in SP500, but not in CRSP daily rate 
# The number of days from the CRSP treasury file doesn't match the number of days in SP500.
# instead of using the cleanedup data, we can also use the FF risk free rate
#sql.dailyRates1 <- "SELECT date,rf,mktrf FROM ff.FACTORS_DAILY WHERE year(date) >= 1973 AND year(date) < 2015"
#rates.daily1 <- getData(sql.dailyRates1)

returns.rates.combineddaily <- CleanupRatesDaily(sp500.dailyreturns , rates.daily)

##MONTHLY
sql.monthlyRates <- "SELECT mcaldt,tmyld FROM CRSP.TFZ_MTH_RF2 WHERE year(mcaldt)>=1973 AND year(mcaldt) < 2015 AND kytreasnox = 2000061"
rates.monthly <- getData(sql.monthlyRates)

#ANNUAL AND 5 year RATES
sqlRates <- "SELECT caldt,b1ret,b5ret FROM CRSP.ACTI WHERE year(caldt) >= 1973 AND year(caldt) < 2015"
rates <- getData(sqlRates)
rates$caldt <- as.Date(rates$caldt)
rates <- xts(rates[,-1],order.by = rates$caldt)

rates.1year <- rates$b1ret

fiveyrateSplit <- split(rates,f = "years",k=5)
fiveyearlyPoints <- lapply(lapply(fiveyrateSplit, index),max)
fiveyearlyDates <- as.Date(unlist(fiveyearlyPoints))
rates.5year <- rates[fiveyearlyDates]$b5ret[-1,]


#####EXCESS RETURNS######
excess.dailyreturns <- returns.rates.combineddaily$vwretd - returns.rates.combineddaily$rate
excess.monthlyreturns <- sp500.monthlyreturns$vwretd - rates.monthly$TMYLD
excess.annualreturns <- sp500.annualreturns$vwretd - rates.1year
excess.5yearreturns <- sp500.5yearreturns$vwretd - rates.5year

buildOutputMatrix(excess.dailyreturns,excess.monthlyreturns,excess.annualreturns,excess.5yearreturns)
buildOutputMatrix(sp500.dailyreturns,sp500.monthlyreturns,sp500.annualreturns,sp500.5yearreturns)
