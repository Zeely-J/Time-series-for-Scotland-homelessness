#library the packages
library(ggplot2)
library(fpp2)
library(fUnitRoots)
library(fGarch)
library(fBasics)
library(TSA)

#read the data
nation_df <- read.csv('natl.yr.mth.data.csv', header = TRUE)
council_df <- read.csv('council.mth.data.csv', header = TRUE)
council_df$Assessments[2456] <- mean(council_df[2455, 4], council_df[2457, 4])
temp <- seq.Date(from = as.Date('2002/05/01',format = '%Y/%m/%d'), 
                 to = as.Date('2019/03/01', format = '%Y/%m/%d'),
                 by = "month")
temp <- format(temp, format = '%Y/%m')

#population average growth rate
year <- levels(as.factor(nation_df$Year))
grorate <- numeric(16)
for(i in 2:17){
  grorate[i-1] <- (nation_df$pop[which(nation_df$Year==year[i])][1] / nation_df$pop[which(nation_df$Year==year[i-1])][1]) - 1
}
ave_growth <- mean(grorate)
nation_df$pop[which(nation_df$Year==year[18])] <- nation_df$pop[which(nation_df$Year==year[17])][1] * (1+ave_growth)

#plot of the application rate, assessment rate and repeat assessment rate
apprate <- ts((nation_df$Homeless.app / nation_df$pop), start = c(2002, 4), frequency = 12)
assessrate <- ts((nation_df$Homeless.assess / nation_df$pop), start = c(2002, 4), frequency = 12)
repassessrate <- ts((nation_df$Assess.repeats / nation_df$pop), start = c(2002, 4), frequency = 12)

cbind('application rate' = apprate, 
      'assessment rate' = assessrate, 
      'repeat assessment rate' = repassessrate) %>%
  autoplot(facet=TRUE)

#seasonal plot of application rate and repeat assessment rate
ggseasonplot(apprate, polar = TRUE)
ggseasonplot(repassessrate, polar = TRUE)

#stl model for application rate and repeat assessment rate
apprate_stl_model <- stlm(apprate, robust = TRUE, method = 'ets')
autoplot(apprate_stl_model$stl)
apprate_trend <- 1-(var(apprate_stl_model$stl[, 'Remainder'])/
                    (var(apprate_stl_model$stl[, 'Trend']+apprate_stl_model$stl[, 'Remainder'])))
apprate_seasonality <- 1-(var(apprate_stl_model$stl[, 'Remainder'])/
                          (var(apprate_stl_model$stl[, 'Seasonal12']+apprate_stl_model$stl[, 'Remainder'])))
F_apprate_trend <- max(c(0, apprate_trend))
F_apprate_season <- max(c(0, apprate_seasonality))

repassessrate_stl_model <- stlm(repassessrate, robust = TRUE, method = 'ets')
autoplot(repassessrate_stl_model$stl)
repassess_trend <- 1-(var(repassessrate_stl_model$stl[, 'Remainder'])/
                        (var(repassessrate_stl_model$stl[, 'Trend']+repassessrate_stl_model$stl[, 'Remainder'])))
repassess_seasonality <- 1-(var(repassessrate_stl_model$stl[, 'Remainder'])/
                              (var(repassessrate_stl_model$stl[, 'Seasonal12']+repassessrate_stl_model$stl[, 'Remainder'])))
F_reprate_trend <- max(c(0, repassess_trend))
F_reprate_season <- max(c(0, repassess_seasonality))

#arima model building
#unit root test of application rate and its differencing
adfTest(apprate, lags = 12, type = 'ct')
diff_apprate <- diff(apprate)
adfTest(diff_apprate, lags=12, type='ct')

#the acf and pacf of the application rate
ggtsdisplay(diff_apprate)

#arima model of application rate
apprate_arima <- Arima(apprate, 
                       order = c(4, 1, 1), 
                       seasonal = list(order = c(4, 0, 1)))
summary(apprate_arima)
checkresiduals(apprate_arima)

#unit root test of the repeat assessment rate and its difference
adfTest(repassessrate, lags = 12, type = 'ct')
diff_repassessrate <- diff(repassessrate)
adfTest(diff_repassessrate)

#the acf and pacf of the repeat assessment rate
ggtsdisplay(diff_repassessrate)

#arima model for the repeat assessment rate
reprate_arima <- Arima(repassessrate, order = c(2,1,2), 
                       seasonal = list(order=c(2,0,0), period=12))
summary(reprate_arima)
checkresiduals(reprate_arima)


#the log change rate of different household types
household_chagerate <- matrix(nrow = 203, ncol = 10)
for(i in 1:10){
  for(j in 2:204){
    household_chagerate[j-1, i] <- log(nation_df[j, i+3]) - log(nation_df[j-1, i+3])
  }
}
household_chagerate <- ts(household_chagerate, 
                          start = c(2002, 5), 
                          frequency = 12)
colnames(household_chagerate) <- colnames(nation_df)[4:13]

# #plot the log change rate
# cbind('h1' = household_chagerate[, 1], 
#       'h2' = household_chagerate[, 2],
#       'h3' = household_chagerate[, 3],
#       'h4' = household_chagerate[, 4],
#       'h5' = household_chagerate[, 5],
#       'h6' = household_chagerate[, 6],
#       'h7' = household_chagerate[, 7],
#       'h8' = household_chagerate[, 8],
#       'h9' = household_chagerate[, 9],
#       'h10' = household_chagerate[, 10]) %>%
#   autoplot(facet=TRUE)

#the bar plots of the mean value and sd of change rate of different household types
plot_data_a <- data.frame(matrix(ncol = 3, nrow = 20))
colnames(plot_data_a) <- c('household', 'statistics', 'value')
for(i in 1:10){
  plot_data_a$value[i] <- round(log(nation_df[204, i+3]) - log(nation_df[1, i+3]), 4)
  plot_data_a$value[i+10] <- round(sd(household_chagerate[, i]), 4)
}
plot_data_a$household <- rep(c(1:10), 2)
plot_data_a$statistics <- c(rep('rate', 10), rep('sd', 10))

ggplot(plot_data_a,aes(x=factor(household, levels = 1:10), y=value)) + 
  geom_bar(stat = 'identity', aes(fill = statistics), position = "dodge", width = 0.5)

#application change rate arima model
app_chage <- numeric(203)
for(i in 2:204){
  app_chage[i-1] <- log(nation_df$Homeless.app[i]) - log(nation_df$Homeless.app[i-1])
}
app_chage <- ts(app_chage, start = c(2002, 5), frequency = 12)
autoplot(app_chage)
adfTest(app_chage)
app_chage_arima <- Arima(app_chage, 
                         order = c(4,0,2), 
                         seasonal = list(order=c(2,1,1), period=12)) 
checkresiduals(app_chage_arima)


#h1 arima model
adfTest(household_chagerate[, 1])
h1_arma_model <- Arima(household_chagerate[, 1], 
                       order = c(4,0,1), 
                       seasonal = list(order=c(0,0,2), period=12))
checkresiduals(h1_arma_model)
h1_ccf <- Ccf(h1_arma_model$residuals, app_chage_arima$residuals, plot = FALSE)
h1_stat <- h1_ccf$n.used * sum((h1_ccf$acf[h1_ccf$lag >=  - 1E-8 & 
                                              h1_ccf$lag <=  + 1E-8])^2)
h1_pvalue <- 1 - pchisq(h1_stat, df=1)
c(h1_stat, h1_pvalue)

#h2 arch test and arch model
adfTest(household_chagerate[, 2])
h2_arma_model <- auto.arima(household_chagerate[, 2])
checkresiduals(h2_arma_model)
h2_ccf <- Ccf(h2_arma_model$residuals, app_chage_arima$residuals, plot = FALSE)
h2_stat <- h2_ccf$n.used * sum((h2_ccf$acf[h2_ccf$lag >=  - 1E-8 & 
                                              h2_ccf$lag <=  + 1E-8])^2)
h2_pvalue <- 1 - pchisq(h2_stat, df=1)
c(h2_stat, h2_pvalue)


#h3 arch test and arch model
adfTest(household_chagerate[, 3])
h3_arma_model <- Arima(household_chagerate[, 3], 
                       order = c(4,0,4), 
                       seasonal = list(order=c(4,1,0), period=12))
checkresiduals(h3_arma_model)
h3_ccf <- Ccf(h3_arma_model$residuals, app_chage_arima$residuals, plot = FALSE)
h3_stat <- h3_ccf$n.used * sum((h3_ccf$acf[h3_ccf$lag >=  - 1E-8 & 
                                              h3_ccf$lag <=  + 1E-8])^2)
h3_pvalue <- 1 - pchisq(h3_stat, df=1)
c(h3_stat, h3_pvalue)

#h4 arch test and arch model
adfTest(household_chagerate[, 4])
h4_arma_model <- Arima(household_chagerate[, 4], 
                       order = c(4,0,1), 
                       seasonal = list(order=c(4,1,0), period=12))
checkresiduals(h4_arma_model)
h4_ccf <- Ccf(h4_arma_model$residuals, app_chage_arima$residuals, plot = FALSE)
h4_stat <- h4_ccf$n.used * sum((h4_ccf$acf[h4_ccf$lag >=  - 1E-8 & 
                                              h4_ccf$lag <=  + 1E-8])^2)
h4_pvalue <- 1 - pchisq(h4_stat, df=1)
c(h4_stat, h4_pvalue)

#h5 arch test and arch model
adfTest(household_chagerate[, 5])
h5_arma_model <- Arima(household_chagerate[, 5], 
                       order = c(4,0,4), 
                       seasonal = list(order=c(4,1,4), period=12))
checkresiduals(h5_arma_model)
h5_stat <- h5_ccf$n.used * sum((h5_ccf$acf[h5_ccf$lag >=  - 1E-8 & 
                                              h5_ccf$lag <=  + 1E-8])^2)
h5_pvalue <- 1 - pchisq(h5_stat, df=1)
c(h5_stat, h5_pvalue)

#h6 arch test and arch model
adfTest(household_chagerate[, 6])
h6_arma_model <- Arima(household_chagerate[, 6], 
                       order = c(4,0,0), 
                       seasonal = list(order=c(4,1,4), period=12))
checkresiduals(h6_arma_model)
h6_ccf <- Ccf(h6_arma_model$residuals, app_chage_arima$residuals, plot = FALSE)
h6_stat <- h6_ccf$n.used * sum((h6_ccf$acf[h6_ccf$lag >=  - 1E-8 & 
                                              h6_ccf$lag <=  + 1E-8])^2)
h6_pvalue <- 1 - pchisq(h6_stat, df=1)
c(h6_stat, h6_pvalue)

#h7 arch test and arch model
adfTest(household_chagerate[, 7])
h7_arma_model <- auto.arima(household_chagerate[, 7])
checkresiduals(h7_arma_model)
h7_ccf <- Ccf(h7_arma_model$residuals, app_chage_arima$residuals, plot = FALSE)
h7_stat <- h7_ccf$n.used * sum((h7_ccf$acf[h7_ccf$lag >=  - 1E-8 & 
                                              h7_ccf$lag <=  + 1E-8])^2)
h7_pvalue <- 1 - pchisq(h7_stat, df=1)
c(h7_stat, h7_pvalue)

#h8 arch test and arch model
adfTest(household_chagerate[, 8])
h8_arma_model <- Arima(household_chagerate[, 8], 
                       order = c(4,0,0), 
                       seasonal = list(order=c(2,1,2), period=12))
checkresiduals(h8_arma_model)
h8_ccf <- Ccf(h8_arma_model$residuals, app_chage_arima$residuals, plot = FALSE)
h8_stat <- h8_ccf$n.used * sum((h8_ccf$acf[h8_ccf$lag >=  - 1E-8 & 
                                              h8_ccf$lag <=  + 1E-8])^2)
h8_pvalue <- 1 - pchisq(h8_stat, df=1)
c(h8_stat, h8_pvalue)

#h9 arch test and arch model
adfTest(household_chagerate[, 9])
h9_arma_model <- auto.arima(household_chagerate[, 9])
checkresiduals(h9_arma_model)
h9_ccf <- Ccf(h9_arma_model$residuals, app_chage_arima$residuals, plot = FALSE)
h9_stat <- h9_ccf$n.used * sum((h9_ccf$acf[h9_ccf$lag >=  - 1E-8 & 
                                              h9_ccf$lag <=  + 1E-8])^2)
h9_pvalue <- 1 - pchisq(h9_stat, df=1)
c(h9_stat, h9_pvalue)

#h10 arch test and arch model
adfTest(household_chagerate[, 10])
h10_arma_model <- auto.arima(household_chagerate[, 10])
checkresiduals(h10_arma_model)
h10_ccf <- Ccf(h10_arma_model$residuals, app_chage_arima$residuals, plot = FALSE)
h10_stat <- h10_ccf$n.used * sum((h10_ccf$acf[h10_ccf$lag >=  - 1E-8 & 
                                              h10_ccf$lag <=  + 1E-8])^2)
h10_pvalue <- 1 - pchisq(h10_stat, df=1)
c(h10_stat, h10_pvalue)


#dynamic model of the change rate
apprate_dyn_model <- auto.arima(app_chage, 
                           xreg = household_chagerate[, 1:10])
checkresiduals(apprate_dyn_model)
summary(apprate_dyn_model)

cof_range <- matrix(ncol = 2, nrow = 16)
se <- c(0.2274,0.1100,0.2178,0.1856,0.0839,0.0813,0.0030,0.0022,0.0042,
        0.0032,0.0015,0.0029,0.0015,0.0017,0.0010,0.0011)
for(i in 1:16){
  cof_range[i, 1] <- apprate_dyn_model$coef[i] - 2*se[i]
  cof_range[i, 2] <- apprate_dyn_model$coef[i] + 2*se[i]
}


#the assessment rate of different councils
council <- levels(as.factor(council_df$Council))
assessrate_mt <- matrix(nrow = 203, ncol = 32)
for(i in 1:32){
  for(j in 2:204){
    council_data <- cbind(council_df$Applications[which(council_df$Council==council[i])], 
                          council_df$Assessments[which(council_df$Council==council[i])])
    assessrate_mt[j-1, i] <- council_data[j, 2] /  (council_data[j-1, 1]*0.55 + council_data[j, 1]*0.45)
  }
}
assessrate_mt <- data.frame(assessrate_mt)
colnames(assessrate_mt) <- council

#the total assessment rate in different councils
assessrate_mean <- numeric(32)
assessrate_sd <- numeric(32)
for(i in 1:32){
  assessrate_mean[i] <- mean(assessrate_mt[, i])
  assessrate_sd[i] <- sd(assessrate_mt[, i])
}
plot_data_b <- data.frame(value=c(assessrate_mean, assessrate_sd), 
                          council=rep(c(1:32), 2), 
                          statistics=c(rep('mean value', 32), 
                                       rep('standard deviation', 32)))
ggplot(plot_data_b, aes(x = factor(council, levels = 1:32), y = value)) + 
  geom_bar(stat = 'identity', aes(fill = statistics), position = 'dodge', width = 0.5)



#change of the assessment rate in different councils
council_chagerate <- matrix(nrow = 202, ncol = 32)
for(j in 1:32){
  for(i in 2:203){
    council_chagerate[i-1, j] <- log(assessrate_mt[i, j]) - log(assessrate_mt[i-1, j])
  }
}
council_chagerate <- ts(data.frame(council_chagerate), 
                        start = c(2002, 6), 
                        frequency = 12)
colnames(council_chagerate) <- council

# cbind('c1' = council_chagerate[, 1], 
#       'c2' = council_chagerate[, 2], 
#       'c3' = council_chagerate[, 3], 
#       'c4' = council_chagerate[, 4], 
#       'c5' = council_chagerate[, 5], 
#       'c6' = council_chagerate[, 6],
#       'c7' = council_chagerate[, 7], 
#       'c8' = council_chagerate[, 8]) %>%
#   autoplot(facet=TRUE)
# 
# cbind('c9' = council_chagerate[, 9],
#       'c10' = council_chagerate[, 10],
#       'c11' = council_chagerate[, 11],
#       'c12' = council_chagerate[, 12],
#       'c13' = council_chagerate[, 13],
#       'c14' = council_chagerate[, 14],
#       'c15' = council_chagerate[, 15],
#       'c16' = council_chagerate[, 16]) %>%
#   autoplot(facet=TRUE)
# 
# cbind('c17' = council_chagerate[, 17],
#       'c18' = council_chagerate[, 18],
#       'c19' = council_chagerate[, 19],
#       'c20' = council_chagerate[, 20],
#       'c21' = council_chagerate[, 21],
#       'c22' = council_chagerate[, 22],
#       'c23' = council_chagerate[, 23],
#       'c24' = council_chagerate[, 24]) %>%
#   autoplot(facet=TRUE)
# 
# cbind('c25' = council_chagerate[, 25],
#       'c26' = council_chagerate[, 26],
#       'c27' = council_chagerate[, 27],
#       'c28' = council_chagerate[, 28],
#       'c29' = council_chagerate[, 29],
#       'c30' = council_chagerate[, 30],
#       'c31' = council_chagerate[, 31], 
#       'c32' = council_chagerate[, 32]) %>%
#   autoplot(facet=TRUE)


#historial volatility of assessment change rate of councils 
hisvol_coun_chagerate <- numeric(32)
for(i in 1:32){
  hisvol_coun_chagerate[i] <- sd(council_chagerate[, i])*sqrt(12)
}

#the arch test and garch model
#c1 arma model, arch test and garch model
adfTest(council_chagerate[, 1])
c1_arma_model <- auto.arima(council_chagerate[, 1])
checkresiduals(c1_arma_model)
Box.test(c1_arma_model$residuals^2, lag = 12, type = 'Ljung')

#c2 arma model, arch test and garch model
adfTest(council_chagerate[, 2])
c2_arma_model <- auto.arima(council_chagerate[, 2])
checkresiduals(c2_arma_model)
Box.test(c2_arma_model$residuals^2, lag = 12, type = 'Ljung')

#c3 arma model, arch test and garch model
adfTest(council_chagerate[, 3])
c3_arma_model <- auto.arima(council_chagerate[, 3])
checkresiduals(c3_arma_model)
Box.test(c3_arma_model$residuals^2, lag = 12, type = 'Ljung')
c3_garch_model_s <- garchFit(~arma(0,1)(1,0)[12]+garch(1,1), 
                             data = council_chagerate[, 3], 
                             trace = FALSE)
summary(c3_garch_model_s)
predict(c3_garch_model_s, n.ahead=12)[12, 3]*sqrt(12)

#c4 arma model, arch test and garch model
adfTest(council_chagerate[, 4])
c4_arma_model <- auto.arima(council_chagerate[, 4])
checkresiduals(c4_arma_model)
Box.test(c4_arma_model$residuals^2, lag = 12, type = 'Ljung')

#c5 arma model, arch test and garch model
adfTest(council_chagerate[, 5])
c5_arma_model <- auto.arima(council_chagerate[, 5])
checkresiduals(c5_arma_model)
Box.test(c5_arma_model$residuals^2, lag = 12, type = 'Ljung')


#c6 arma model, arch test and garch model
adfTest(council_chagerate[, 6])
c6_arma_model <- Arima(council_chagerate[, 6], 
                       order = c(0,0,6), 
                       seasonal = list(order=c(0,0,1), period=12))
checkresiduals(c6_arma_model)
Box.test(c6_arma_model$residuals^2, lag = 12, type = 'Ljung')


#c7 arma model, arch test and garch model
adfTest(council_chagerate[, 7])
c7_arma_model <- auto.arima(council_chagerate[, 7])
checkresiduals(c7_arma_model)
Box.test(c7_arma_model$residuals^2, lag = 12, type = 'Ljung')
c7_garch_model_s <- garchFit(~arma(0,2)(2,0)[12]+garch(1,1), 
                             data = council_chagerate[, 7], 
                             trace = FALSE)
summary(c7_garch_model_s)
predict(c7_garch_model_s, n.ahead=12)[12, 3]*sqrt(12)

#c8 arma model, arch test and garch model
adfTest(council_chagerate[, 8])
c8_arma_model <- auto.arima(council_chagerate[, 8])
checkresiduals(c8_arma_model)
Box.test(c8_arma_model$residuals^2, lag = 12, type = 'Ljung')

#c9 arma model, arch test and garch model
adfTest(council_chagerate[, 9])
c9_arma_model <- auto.arima(council_chagerate[, 9])
checkresiduals(c9_arma_model)
Box.test(c9_arma_model$residuals^2, lag = 12, type = 'Ljung')
c9_garch_model_s <- garchFit(~arma(1,1)(2,1)[12]+garch(1,0), 
                             data = council_chagerate[, 9], 
                             trace = FALSE)
summary(c9_garch_model_s)
predict(c9_garch_model_s, n.ahead=12)[12, 3]*sqrt(12)

#c10 arma model, arch test and garch model
adfTest(council_chagerate[, 10])
c10_arma_model <- auto.arima(council_chagerate[, 10])
checkresiduals(c10_arma_model)
Box.test(c10_arma_model$residuals^2, lag = 12, type = 'Ljung')

#c11 arma model, arch test and garch model
adfTest(council_chagerate[, 11])
c11_arma_model <- auto.arima(council_chagerate[, 11])
checkresiduals(c11_arma_model)
Box.test(c11_arma_model$residuals^2, lag = 12, type = 'Ljung')
c11_garch_model_s <- garchFit(~arma(0,3)(2,0)[12]+garch(1,0), 
                             data = council_chagerate[, 11], 
                             trace = FALSE)
summary(c11_garch_model_s)
predict(c11_garch_model_s, n.ahead=12)[12, 3]*sqrt(12)

#c12 arma model, arch test and garch model
adfTest(council_chagerate[, 12])
c12_arma_model <- auto.arima(council_chagerate[, 12])
checkresiduals(c12_arma_model)
Box.test(c12_arma_model$residuals^2, lag = 12, type = 'Ljung')

#c13 arma model, arch test and garch model
council_chagerate[119, 13] <- 0
council_chagerate[120, 13] <- 0
adfTest(council_chagerate[, 13])
c13_arma_model <- auto.arima(council_chagerate[, 13])
checkresiduals(c13_arma_model)
Box.test(c13_arma_model$residuals^2, lag = 12, type = 'Ljung')

#c14 arma model, arch test and garch model
adfTest(council_chagerate[, 14])
c14_arma_model <- auto.arima(council_chagerate[, 14])
checkresiduals(c14_arma_model)
Box.test(c14_arma_model$residuals^2, lag = 12, type = 'Ljung')
c14_garch_model_s <- garchFit(~arma(2,1)(2,1)[12]+garch(1,1), 
                             data = council_chagerate[, 14], 
                             trace = FALSE)
summary(c14_garch_model_s)
predict(c14_garch_model_s, n.ahead=12)[12, 3]*sqrt(12)

#c15 arma model, arch test and garch model
adfTest(council_chagerate[, 15])
c15_arma_model <- auto.arima(council_chagerate[, 15])
checkresiduals(c15_arma_model)
Box.test(c15_arma_model$residuals^2, lag = 12, type = 'Ljung')

#c16 arma model, arch test and garch model
adfTest(council_chagerate[, 16])
c16_arma_model <- auto.arima(council_chagerate[, 16])
checkresiduals(c16_arma_model)
Box.test(c16_arma_model$residuals^2, lag = 12, type = 'Ljung')
c16_garch_model_s <- garchFit(~arma(0,1)(0,1)[12]+garch(1,1), 
                             data = council_chagerate[, 16], 
                             trace = FALSE)
summary(c16_garch_model_s)
predict(c16_garch_model_s, n.ahead=12)[12, 3]*sqrt(12)

#c17 arma model, arch test and garch model
adfTest(council_chagerate[, 17])
c17_arma_model <- auto.arima(council_chagerate[, 17])
checkresiduals(c17_arma_model)
Box.test(c17_arma_model$residuals^2, lag = 12, type = 'Ljung')

#c18 arma model, arch test and garch model
adfTest(council_chagerate[, 18])
c18_arma_model <- auto.arima(council_chagerate[, 18])
checkresiduals(c18_arma_model)
Box.test(c18_arma_model$residuals^2, lag = 12, type = 'Ljung')

#c19 arma model, arch test and garch model
adfTest(council_chagerate[, 19])
c19_arma_model <- auto.arima(council_chagerate[, 19])
checkresiduals(c19_arma_model)
Box.test(c19_arma_model$residuals^2, lag = 12, type = 'Ljung')

#c20 arma model, arch test and garch model
adfTest(council_chagerate[, 20])
c20_arma_model <- auto.arima(council_chagerate[, 20])
checkresiduals(c20_arma_model)
Box.test(c20_arma_model$residuals^2, lag = 12, type = 'Ljung')

#c21 arma model, arch test and garch model
adfTest(council_chagerate[, 21])
c21_arma_model <- auto.arima(council_chagerate[, 21])
checkresiduals(c21_arma_model)
Box.test(c21_arma_model$residuals^2, lag = 12, type = 'Ljung')

#c22 arma model, arch test and garch model
adfTest(council_chagerate[, 22])
c22_arma_model <- auto.arima(council_chagerate[, 22])
checkresiduals(c22_arma_model)
Box.test(c22_arma_model$residuals^2, lag = 12, type = 'Ljung')
c22_garch_model_s <- garchFit(~arma(2,2)(2,0)[12]+garch(1,1), 
                             data = council_chagerate[, 22], 
                             trace = FALSE)
summary(c22_garch_model_s)
predict(c22_garch_model_s, n.ahead=12)[12, 3]*sqrt(12)

#c23 arma model, arch test and garch model
council_chagerate[which(council_chagerate[, 23]==Inf), 23] <- 0
council_chagerate[which(council_chagerate[, 23]==-Inf), 23] <- 0
adfTest(council_chagerate[, 23])
c23_arma_model <- auto.arima(council_chagerate[, 23])
checkresiduals(c23_arma_model)
Box.test(c23_arma_model$residuals^2, lag = 12, type = 'Ljung')

#c24 arma model, arch test and garch model
adfTest(council_chagerate[, 24])
c24_arma_model <- auto.arima(council_chagerate[, 24])
checkresiduals(c24_arma_model)
Box.test(c24_arma_model$residuals^2, lag = 12, type = 'Ljung')
c24_garch_model_s <- garchFit(~arma(1,2)(1,0)[12]+garch(1,1), 
                             data = council_chagerate[, 24], 
                             trace = FALSE)
summary(c24_garch_model_s)
predict(c24_garch_model_s, n.ahead=12)[12, 3]*sqrt(12)

#c25 arma model, arch test and garch model
adfTest(council_chagerate[, 25])
c25_arma_model <- Arima(council_chagerate[, 25], 
                        order = c(4,0,3), 
                        seasonal = list(order=c(2,0,2), period=12))
checkresiduals(c25_arma_model)
Box.test(c25_arma_model$residuals^2, lag = 12, type = 'Ljung')
c25_garch_model_s <- garchFit(~arma(4,3)(2,2)[12]+garch(1,0), 
                             data = council_chagerate[, 25], 
                             trace = FALSE)
summary(c25_garch_model_s)
predict(c25_garch_model_s, n.ahead=1)*sqrt(12)

#c26 arma model, arch test and garch model
adfTest(council_chagerate[, 26])
c26_arma_model <- auto.arima(council_chagerate[, 26])
checkresiduals(c26_arma_model)
Box.test(c26_arma_model$residuals^2, lag = 12, type = 'Ljung')
c26_garch_model_s <- garchFit(~arma(0,1)(1,0)[12]+garch(1,1), 
                             data = council_chagerate[, 26], 
                             trace = FALSE)
summary(c26_garch_model_s)
predict(c26_garch_model_s, n.ahead=12)[12, 3]*sqrt(12)

#c27 arma model, arch test and garch model
council_chagerate[which(council_chagerate[, 27]==Inf), 27] <- 0
council_chagerate[which(council_chagerate[, 27]==-Inf), 27] <- 0
adfTest(council_chagerate[, 27])
c27_arma_model <- auto.arima(council_chagerate[, 27])
checkresiduals(c27_arma_model)
Box.test(c27_arma_model$residuals^2, lag = 12, type = 'Ljung')

#c28 arma model, arch test and garch model
adfTest(council_chagerate[, 28])
c28_arma_model <- auto.arima(council_chagerate[, 28])
checkresiduals(c28_arma_model)
Box.test(c28_arma_model$residuals^2, lag = 12, type = 'Ljung')

#c29 arma model, arch test and garch model
adfTest(council_chagerate[, 29])
c29_arma_model <- Arima(council_chagerate[, 29], 
                        order = c(4,0,3), 
                        seasonal = list(order=c(0,0,1), period=12))
checkresiduals(c29_arma_model)
Box.test(c29_arma_model$residuals^2, lag = 12, type = 'Ljung')

#c30 arma model, arch test and garch model
adfTest(council_chagerate[, 30])
c30_arma_model <- auto.arima(council_chagerate[, 30])
checkresiduals(c30_arma_model)
Box.test(c30_arma_model$residuals^2, lag = 12, type = 'Ljung')

#c31 arma model, arch test and garch model
adfTest(council_chagerate[, 31])
c31_arma_model <- auto.arima(council_chagerate[, 31])
checkresiduals(c31_arma_model)
Box.test(c31_arma_model$residuals^2, lag = 12, type = 'Ljung')
c31_garch_model_s <- garchFit(~arma(5,3)(2,0)[12]+garch(1,0), 
                             data = council_chagerate[, 31], 
                             trace = FALSE)
summary(c31_garch_model_s)
predict(c31_garch_model_s, n.ahead=1)[1,3]*sqrt(12)

#c32 arma model, arch test and garch model
adfTest(council_chagerate[, 32])
c32_arma_model <- auto.arima(council_chagerate[, 32])
checkresiduals(c32_arma_model)
Box.test(c32_arma_model$residuals^2, lag = 12, type = 'Ljung')


