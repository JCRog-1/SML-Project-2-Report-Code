data <- load("trainsData.RData")

#Convert into nice data frame format----
textToSeconds <- function(textTime){
  seconds <- as.numeric(strsplit(textTime,split = ":")[[1]]) %*% c(60*60,60,1)
  return(as.numeric(seconds))
}
data.convert <- function(trainingData){
  peak.days <- c('Monday','Tuesday','Wednesday','Thursday','Friday')
  
  matrixData <- data.frame(
    delayNotts=rep(0, length(trainingData)),
    EL = rep(0,length(trainingData)),
    day=rep(" ", length(trainingData)),
    delayShef=rep(0, length(trainingData)),
    no.stations.LeSh = rep(0,length(trainingData)),
    delay.dep.Leeds = rep(0, length(trainingData)),
    #delay.dep.NH = rep(0, length(trainingData)),
    delay.dep.WK = rep(0, length(trainingData)),
    delay.dep.BN = rep(0, length(trainingData)),
    delay.dep.MH = rep(0, length(trainingData)),
    hour = rep(0, length(trainingData)),
    peak.time = rep(0,length(trainingData)),
    flag.time = rep(0,length(trainingData)),
    Leeds.trains = rep(0, length(trainingData)),
    Shef.trains = rep(0, length(trainingData)),
    Notts.trains = rep(0, length(trainingData)),
    Leeds.av.delay = rep(0, length(trainingData)),
    Shef.av.delay = rep(0, length(trainingData)),
    Notts.av.delay = rep(0, length(trainingData)),
    Leeds.trains.dif = rep(0, length(trainingData)),
    Shef.trains.dif = rep(0, length(trainingData)),
    Notts.trains.dif = rep(0, length(trainingData)),
    Leeds.av.delay.dif = rep(0, length(trainingData)),
    Shef.av.delay.dif = rep(0, length(trainingData)),
    Notts.av.delay.dif = rep(0, length(trainingData)),
    max_delay = rep(0,length(trainingData)),
    mean_delay = rep(0,length(trainingData)),
    # sum_delay = rep(0,length(trainingData)),
    # sd_delay = rep(0,length(trainingData)),
    stringsAsFactors=FALSE
  )
  
  for (i in 1:length(trainingData)) {
    dummy <- trainingData[[i]]
    if(dummy$arrival$delay.secs[1] > 0){
      matrixData$EL[i] <- 1
    }
    matrixData$day[i] <- dummy$timings$day.week[1]
    arrSheffield <- textToSeconds(tail(dummy$timings$arrival.time,1))
    schSheffield <- textToSeconds(tail(dummy$timings$arrival.schedule,1))
    matrixData$delayShef[i] <- arrSheffield - schSheffield
    matrixData$delayNotts[i] <- dummy$arrival$delay.secs[1]
    matrixData$no.stations.LeSh[i] <- (length(dummy$timings$departure.from) - 1)
    depLeeds <- textToSeconds(head(dummy$timings$departure.time,1))
    schLeeds <- textToSeconds(head(dummy$timings$departure.schedule,1))
    matrixData$delay.dep.Leeds[i] <- depLeeds - schLeeds
    # if ('NORMNTN' %in% dummy$timings$departure.from){
    #   depNH <- textToSeconds(dummy$timings$departure.time[which(dummy$timings$departure.from == 'NORMNTN')])
    #   schNH <- textToSeconds(dummy$timings$departure.schedule[which(dummy$timings$departure.from == 'NORMNTN')])
    #   matrixData$delay.dep.NH[i] <- depNH - schNH
    # }
    if ('WKFLDKG' %in% dummy$timings$departure.from){
      depWK <- textToSeconds(dummy$timings$departure.time[which(dummy$timings$departure.from == 'WKFLDKG')])
      schWK <- textToSeconds(dummy$timings$departure.schedule[which(dummy$timings$departure.from == 'WKFLDKG')])
      matrixData$delay.dep.WK[i] <- depWK - schWK
    }
    if ('BNSLY' %in% dummy$timings$departure.from){
      depBN <- textToSeconds(dummy$timings$departure.time[which(dummy$timings$departure.from == 'BNSLY')])
      schBN <- textToSeconds(dummy$timings$departure.schedule[which(dummy$timings$departure.from == 'BNSLY')])
      matrixData$delay.dep.BN[i] <- depBN - schBN
    }
    if ('MEADWHL' %in% dummy$timings$departure.from){
      depMH <- textToSeconds(dummy$timings$departure.time[which(dummy$timings$departure.from == "MEADWHL")])
      schMH <- textToSeconds(dummy$timings$departure.schedule[which(dummy$timings$departure.from == 'MEADWHL')])
      matrixData$delay.dep.MH[i] <- depMH - schMH
    }
    matrixData$hour[i] <- dummy$congestion$hour
    if((dummy$timings$day.week[1] %in% peak.days) & (dummy$congestion$hour %in% c(8,12,17))){
      matrixData$peak.time[i] <- 1
    }
    if(dummy$congestion$hour == 7){
      matrixData$flag.time[i] <- -1
    }
    if(dummy$timings$day.week[1] == 'Saturday' & dummy$congestion$hour == 9){
      matrixData$flag.time[i] <- -1
    }
    if(dummy$timings$day.week[1] == 'Sunday' & dummy$congestion$hour == 11){
      matrixData$flag.time[i] <- -1
    }
    matrixData$Leeds.trains[i] <- dummy$congestion$Leeds.trains
    matrixData$Shef.trains[i] <- dummy$congestion$Sheffield.trains
    matrixData$Notts.trains[i] <- dummy$congestion$Nottingham.trains
    matrixData$Leeds.av.delay[i] <- 60*dummy$congestion$Leeds.av.delay
    matrixData$Shef.av.delay[i] <- 60*dummy$congestion$Sheffield.av.delay
    matrixData$Notts.av.delay[i] <- 60*dummy$congestion$Nottingham.av.delay
    matrixData$Leeds.trains.dif[i] <- dummy$congestion$Leeds.trains - 
      historicalCongestion[which(historicalCongestion[,1] == dummy$timings$day.week[1] 
                                 & historicalCongestion[,2] == dummy$congestion$hour),3]
    matrixData$Shef.trains.dif[i] <- dummy$congestion$Sheffield.trains - 
      historicalCongestion[which(historicalCongestion[,1] == dummy$timings$day.week[1] 
                                 & historicalCongestion[,2] == dummy$congestion$hour),5]
    matrixData$Notts.trains.dif[i] <- dummy$congestion$Nottingham.trains - 
      historicalCongestion[which(historicalCongestion[,1] == dummy$timings$day.week[1] 
                                 & historicalCongestion[,2] == dummy$congestion$hour),7]
    matrixData$Leeds.av.delay.dif[i] <- 60*(dummy$congestion$Leeds.av.delay - 
                                              historicalCongestion[which(historicalCongestion[,1] == dummy$timings$day.week[1] 
                                                                         & historicalCongestion[,2] == dummy$congestion$hour),4])
    matrixData$Shef.av.delay.dif[i] <- 60*(dummy$congestion$Sheffield.av.delay - 
                                             historicalCongestion[which(historicalCongestion[,1] == dummy$timings$day.week[1] 
                                                                        & historicalCongestion[,2] == dummy$congestion$hour),6])
    matrixData$Notts.av.delay.dif[i] <- 60*(dummy$congestion$Nottingham.av.delay - 
                                              historicalCongestion[which(historicalCongestion[,1] == dummy$timings$day.week[1] 
                                                                         & historicalCongestion[,2] == dummy$congestion$hour),8])
    
    matrixData$max_delay[i] <- max(c(matrixData$delay.dep.Leeds[i],matrixData$delay.dep.NH[i],matrixData$delay.dep.WK[i],matrixData$delay.dep.BN[i],matrixData$delay.dep.MH[i]))
    matrixData$mean_delay[i] <- sum(c(matrixData$delay.dep.Leeds[i],matrixData$delay.dep.NH[i],matrixData$delay.dep.WK[i],matrixData$delay.dep.BN[i],matrixData$delay.dep.MH[i]))/(matrixData$no.stations.LeSh[i] + 1)
    # matrixData$sum_delay[i] <- sum(c(matrixData$delay.dep.Leeds[i],matrixData$delay.dep.NH[i],matrixData$delay.dep.WK[i],matrixData$delay.dep.BN[i],matrixData$delay.dep.MH[i]))
    # matrixData$sd_delay[i] <- sqrt(sum(c((matrixData$delay.dep.Leeds[i] - matrixData$mean_delay[i])**2,
    #                                 (matrixData$delay.dep.NH[i] - matrixData$mean_delay[i])**2,
    #                                 (matrixData$delay.dep.WK[i] - matrixData$mean_delay[i])**2,
    #                                 (matrixData$delay.dep.BN[i] - matrixData$mean_delay[i])**2,
    #                                 (matrixData$delay.dep.MH[i] - matrixData$mean_delay[i])**2))/matrixData$no.stations.LeSh[i])
    
  }
  
  matrixData[,2] <- factor(matrixData[,2])
  matrixData[,3] <- factor(matrixData[,3])
  matrixData[,5] <- factor(matrixData[,5])
  matrixData[,10] <- factor(matrixData[,10])
  
  return(matrixData)
}
matrixData <- data.convert(trainingData = trainingData)
df <- matrixData

#Code to run first Linear model 
model <- lm(delayNotts~delayShef + max_delay + peak.time + hour, data = df)

#Code to produce assumption checking plots 
res <- residuals(model)
hist(res,freq = FALSE,breaks = 50)
lines(seq(-500,700,length.out = 1000),dnorm(seq(-500,700,length.out = 1000),mean = 0,sd = sqrt(var(res))))
plot(late[,1],fitted(model))
lines(seq(-500,4000,length.out = 1000),seq(-500,4000,length.out = 1000))
plot(fitted(model),res)
abline(0,0)
qqnorm(res)
qqline(res)

#GAM code
library(mgcv)
train_data <-  df

gam(delayNotts ~ 
      hour +
      s(delayShef, k = 12) +
      day +
      s(mean_delay, k = 12) +
      s(Leeds.av.delay.dif, k = 20) +
      s(Shef.trains.dif, k = 20) +
      s(Shef.av.delay.dif, k = 20) +
      peak.time,
    data = train_data,
    method = "ML")

#Code for final model 
posdf <- df[which(df$delayNotts > 0),]
negdf <- df[which(df$delayNotts < 0),]

#code for the classifier 
library(rpart)
late_classifier <- rpart(EL ~ delayShef + mean_delay + max_delay, data = train_data, method = 'class')

#Linear model for early arrivals 
early_model <- lm(delayNotts~delayShef + flag.time + hour, data = negdf)

#GAM for late arrivals 
late_model <- gam(delayNotts ~ 
                    hour +
                    s(delayShef, k = 12) +
                    day +
                    s(mean_delay, k = 12) +
                    s(Leeds.av.delay.dif, k = 20) +
                    s(Shef.trains.dif, k = 20) +
                    s(Shef.av.delay.dif, k = 20) +
                    peak.time,
                  data = posdf,
                  method = "ML")
                              
















