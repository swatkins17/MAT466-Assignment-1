

# APM466 - Mathematical Theory of Finance

# Assignment 1 - Bond Yield Curves
# Sierra Watkins; 1005473685



# Import dataset - BondData.xlsx
# BondData - 360 objects, 8 variables
summary(BondData)

# install packages
library(tidyverse)
library(ggthemes)
library(reshape2)
library(dplyr)


######### Select subset of 10 bonds for 0-5 year rates #########

# ISIN's of the 10 bonds selected
ISINSubset <- c("CA135087F254", "CA135087F585", "CA135087G328", "CA135087L286", "CA135087L518",
                "CA135087H490", "CA135087J546", "CA135087J967", "CA135087K528", "CA135087K940")

BondSubSet <- BondData %>% filter(ISIN %in% ISINSubset)




######### Calculate and plot YTM #########


# Append column - days since last CPN payment
# difference between date of data recording and date of next payment, less 6 months (182 days)
BondSubSet$DaysSinceCPN <- as.numeric(182 - difftime(BondSubSet$`CPN PAYMENT`, 
                                    BondSubSet$`DATE OF PRICE`,units = c("days")))


# Dirty price = closing price (clean price) + accrued interst
BondSubSet$DirtyPrice <- BondSubSet$`CLOSING PRICE` + ((BondSubSet$COUPON * BondSubSet$DaysSinceCPN)/365)

# notional
BondSubSet$NOTIONAL <- 100+(100*BondSubSet$COUPON)


# years between price recorded and maturity date
BondSubSet$DaysToMaturity <- as.numeric(difftime(BondSubSet$`MATURITY DATE`, 
                                      BondSubSet$`DATE OF PRICE`,units = c("days"))/365)


# yields for <6 month to maturity, store these as yields for t = 0.5
BondSubSet$YTM <- ifelse(BondSubSet$DaysToMaturity <= 0.5, 
                         -log((BondSubSet$DirtyPrice)/(BondSubSet$NOTIONAL)) /
                           BondSubSet$DaysToMaturity, 0)
BondSubSet$YTM0.5 <- rep(BondSubSet$YTM[1:10],10)


# yields for 0.5y to 1 year maturity
BondSubSet$YTM <- ifelse(BondSubSet$DaysToMaturity < 1 & BondSubSet$DaysToMaturity > 0.5, 
                         (log(BondSubSet$DirtyPrice-
                               (100*BondSubSet$COUPON)
                             *exp(-BondSubSet$YTM0.5*(BondSubSet$DaysToMaturity - 0.5)))
                          - log(BondSubSet$NOTIONAL))/(-BondSubSet$DaysToMaturity),BondSubSet$YTM)

BondSubSet$YTM1 <- rep(BondSubSet$YTM[11:20],10)                     
BondSubSet$YTM1


# yields for 1 year to 1.5 year maturity
BondSubSet$YTM <- ifelse(BondSubSet$DaysToMaturity < 1.5 & BondSubSet$DaysToMaturity > 1.0, 
                         (log(BondSubSet$DirtyPrice-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM0.5*(BondSubSet$DaysToMaturity - 1.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM1*(BondSubSet$DaysToMaturity - 0.5)))
                          - log(BondSubSet$NOTIONAL))
                         /(-BondSubSet$DaysToMaturity),BondSubSet$YTM)

BondSubSet$YTM1.5 <- rep(BondSubSet$YTM[21:30],10)



# yields for 1.5 year to 2.0 year maturity
BondSubSet$YTM <- ifelse(BondSubSet$DaysToMaturity < 2.0 & BondSubSet$DaysToMaturity > 1.5, 
                         (log(BondSubSet$DirtyPrice-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM0.5*(BondSubSet$DaysToMaturity - 1.5))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM1*(BondSubSet$DaysToMaturity - 1.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM1.5*(BondSubSet$DaysToMaturity - 0.5)))
                          - log(BondSubSet$NOTIONAL))
                         /(-BondSubSet$DaysToMaturity),BondSubSet$YTM)

BondSubSet$YTM2 <- rep(BondSubSet$YTM[31:40],10)



# yields for 2.0 year to 2.5 year maturity
BondSubSet$YTM <- ifelse(BondSubSet$DaysToMaturity < 2.5 & BondSubSet$DaysToMaturity > 2.0, 
                         (log(BondSubSet$DirtyPrice-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM0.5*(BondSubSet$DaysToMaturity - 2.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM1*(BondSubSet$DaysToMaturity - 1.5))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM1.5*(BondSubSet$DaysToMaturity - 1.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM2*(BondSubSet$DaysToMaturity - 0.5))
                              )
                          - log(BondSubSet$NOTIONAL))
                         /(-BondSubSet$DaysToMaturity),BondSubSet$YTM)

BondSubSet$YTM2.5 <- rep(BondSubSet$YTM[41:50],10)



# yields for 2.5 year to 3.5 year maturity (less data available; wider date range)
BondSubSet$YTM <- ifelse(BondSubSet$DaysToMaturity < 3.5 & BondSubSet$DaysToMaturity > 2.5, 
                         (log(BondSubSet$DirtyPrice-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM0.5*(BondSubSet$DaysToMaturity - 3.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM1*(BondSubSet$DaysToMaturity - 2.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM1.5*(BondSubSet$DaysToMaturity - 1.5))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM2*(BondSubSet$DaysToMaturity - 1.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM2.5*(BondSubSet$DaysToMaturity - 0.5))
                         )
                         - log(BondSubSet$NOTIONAL))
                         /(-BondSubSet$DaysToMaturity),BondSubSet$YTM)

BondSubSet$YTM3 <- rep(BondSubSet$YTM[51:60],10)



# yields for 3.5 year to 4.0 year maturity
BondSubSet$YTM <- ifelse(BondSubSet$DaysToMaturity < 4.0 & BondSubSet$DaysToMaturity > 3.5, 
                         (log(BondSubSet$DirtyPrice-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM0.5*(BondSubSet$DaysToMaturity - 3.5))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM1*(BondSubSet$DaysToMaturity - 3.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM1.5*(BondSubSet$DaysToMaturity - 2.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM2*(BondSubSet$DaysToMaturity - 1.5))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM2.5*(BondSubSet$DaysToMaturity - 1.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM3*(BondSubSet$DaysToMaturity - 0.5))
                         )
                         - log(BondSubSet$NOTIONAL))
                         /(-BondSubSet$DaysToMaturity),BondSubSet$YTM)

BondSubSet$YTM3.5 <- rep(BondSubSet$YTM[61:70],10)



# yields for 4.0 year to 4.5 year maturity
BondSubSet$YTM <- ifelse(BondSubSet$DaysToMaturity < 4.5 & BondSubSet$DaysToMaturity > 4.0, 
                         (log(BondSubSet$DirtyPrice-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM0.5*(BondSubSet$DaysToMaturity - 4.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM1*(BondSubSet$DaysToMaturity - 3.5))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM1.5*(BondSubSet$DaysToMaturity - 3.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM2*(BondSubSet$DaysToMaturity - 2.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM2.5*(BondSubSet$DaysToMaturity - 1.5))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM3*(BondSubSet$DaysToMaturity - 1.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM3.5*(BondSubSet$DaysToMaturity - 0.5))
                         )
                         - log(BondSubSet$NOTIONAL))
                         /(-BondSubSet$DaysToMaturity),BondSubSet$YTM)

BondSubSet$YTM4 <- rep(BondSubSet$YTM[71:80],10)



# yields for 4.5 year to 5.0 year maturity
BondSubSet$YTM <- ifelse(BondSubSet$DaysToMaturity < 5.0 & BondSubSet$DaysToMaturity > 4.5, 
                         (log(BondSubSet$DirtyPrice-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM0.5*(BondSubSet$DaysToMaturity - 4.5))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM1*(BondSubSet$DaysToMaturity - 4.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM1.5*(BondSubSet$DaysToMaturity - 3.5))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM2*(BondSubSet$DaysToMaturity - 3.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM2.5*(BondSubSet$DaysToMaturity - 2.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM3*(BondSubSet$DaysToMaturity - 1.5))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM3.5*(BondSubSet$DaysToMaturity - 1.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM4*(BondSubSet$DaysToMaturity - 0.5))
                         )
                         - log(BondSubSet$NOTIONAL))
                         /(-BondSubSet$DaysToMaturity),BondSubSet$YTM)

BondSubSet$YTM4.5 <- rep(BondSubSet$YTM[81:90],10)



# yields for 5.0 year to 5.5 year maturity
BondSubSet$YTM <- ifelse(BondSubSet$DaysToMaturity < 5.5 & BondSubSet$DaysToMaturity > 5.0, 
                         (log(BondSubSet$DirtyPrice-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM0.5*(BondSubSet$DaysToMaturity - 5.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM1*(BondSubSet$DaysToMaturity - 4.5))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM1.5*(BondSubSet$DaysToMaturity - 4.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM2*(BondSubSet$DaysToMaturity - 3.5))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM2.5*(BondSubSet$DaysToMaturity - 3.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM3*(BondSubSet$DaysToMaturity - 2.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM3.5*(BondSubSet$DaysToMaturity - 1.5))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM4*(BondSubSet$DaysToMaturity - 1.0))-
                                (100*BondSubSet$COUPON)*
                                exp(-BondSubSet$YTM4.5*(BondSubSet$DaysToMaturity - 0.5))
                         )
                         - log(BondSubSet$NOTIONAL))
                         /(-BondSubSet$DaysToMaturity),BondSubSet$YTM)

BondSubSet$YTM5 <- rep(BondSubSet$YTM[91:100],10)

BondSubSet <- (BondSubSet %>% select(1:12))



# Obtained YTM data, now form plots:

YieldData <- (BondSubSet %>% select(5,4,12))
YieldData <- YieldData[order(YieldData$`DATE OF PRICE`),]
YieldData$YTM <- YieldData$YTM*100

# plot maturity vs. yield for each date recorded
ggplot(data = YieldData, 
       mapping = aes(x = YieldData$`MATURITY DATE`, YieldData$YTM, group = 
                       YieldData$`DATE OF PRICE`, color = YieldData$`DATE OF PRICE`)) + 
                     labs(title =  "Government of Canada\n5 Year Yield Curves",
                     x = "Maturity (Year)", y = "Yield (%)") + 
                     geom_point(size = 1, shape = 23) +
                      theme(panel.spacing = unit(1, "lines")) + 
                     theme(plot.title = element_text(size=20, face="bold", hjust = 0.5,
                                                     margin = margin(10, 0, 10, 0), vjust=2.5), 
                           axis.title.x = element_text(size=15, vjust=-0.35),
                           axis.title.y = element_text(size=15, vjust=0.35),
                           legend.title = element_text(size=14, face="bold")) +
                     labs(color='Date\nRecorded') + 
                     geom_smooth(method="lm", se=FALSE, fullrange=FALSE, formula = y ~ splines::bs(x, 5)) + geom_point(alpha = 0.1, aes(color = YieldData$`DATE OF PRICE`) + scale_y_continuous(labels = scales::number_format(accuracy = 0.001)))




######### Calculate and plot spot rates #########



# spot rates for <6 month to maturity
BondSubSet$SpotRate <- ifelse(BondSubSet$DaysToMaturity <= 0.5, 
                              (BondSubSet$NOTIONAL/BondSubSet$DirtyPrice)^(1/BondSubSet$DaysToMaturity)-1
                         , 0)

BondSubSet$SpotRate0.5 <- rep(BondSubSet$YTM[1:10],10)



# spot rate for 0.5y to 1y

BondSubSet$SpotRate <- ifelse(BondSubSet$DaysToMaturity < 1.0 & BondSubSet$DaysToMaturity > 0.5, 
                              (BondSubSet$NOTIONAL/(BondSubSet$DirtyPrice-
                                                      ((100*BondSubSet$COUPON)/
                                                         (1+BondSubSet$SpotRate0.5)^(BondSubSet$DaysToMaturity-0.5))^(1/BondSubSet$DaysToMaturity)))-1
                                                    ,BondSubSet$SpotRate)
BondSubSet$SpotRate
BondSubSet$SpotRate1 <- rep(BondSubSet$YTM[11:20],10)



# spot rate for 1y to 1.5y

BondSubSet$SpotRate <- ifelse(BondSubSet$DaysToMaturity < 1.5 & BondSubSet$DaysToMaturity > 1.0, 
                              (BondSubSet$NOTIONAL/(BondSubSet$DirtyPrice-
                                                      ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate0.5)^(BondSubSet$DaysToMaturity-1.0)-
                                                      ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate1)^(BondSubSet$DaysToMaturity-0.5)))
                                                      ^(1/BondSubSet$DaysToMaturity)))-1
                              ,BondSubSet$SpotRate)
BondSubSet$SpotRate
BondSubSet$SpotRate1.5 <- rep(BondSubSet$YTM[21:30],10)




# spot rate for 1.5y to 2.0y
BondSubSet$SpotRate <- ifelse(BondSubSet$DaysToMaturity < 2.0 & BondSubSet$DaysToMaturity > 1.5, 
                              (BondSubSet$NOTIONAL/(BondSubSet$DirtyPrice-
                                                      ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate0.5)^(BondSubSet$DaysToMaturity-1.5)-
                                                         ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate1)^(BondSubSet$DaysToMaturity-1.0)-
                                                            ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate1.5)^(BondSubSet$DaysToMaturity-0.5)))
                                                    ^(1/BondSubSet$DaysToMaturity))))-1,
                               BondSubSet$SpotRate)

BondSubSet$SpotRate2 <- rep(BondSubSet$YTM[31:40],10)



# spot rate for 2.0y to 2.5y
BondSubSet$SpotRate <- ifelse(BondSubSet$DaysToMaturity < 2.5 & BondSubSet$DaysToMaturity > 2.0, 
                              (BondSubSet$NOTIONAL/(BondSubSet$DirtyPrice-
                                                      ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate0.5)^(BondSubSet$DaysToMaturity-2)-
                                                         ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate1)^(BondSubSet$DaysToMaturity-1.5)-
                                                            ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate1.5)^(BondSubSet$DaysToMaturity-1)-
                                                               ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate2)^(BondSubSet$DaysToMaturity-0.5)
                                                             )))
                                                       ^(1/BondSubSet$DaysToMaturity))))-1,
                              BondSubSet$SpotRate)

BondSubSet$SpotRate2.5 <- rep(BondSubSet$YTM[41:50],10)



# spot rate for 2.5y to 3.5y
BondSubSet$SpotRate <- ifelse(BondSubSet$DaysToMaturity < 3.5 & BondSubSet$DaysToMaturity > 2.5, 
                              (BondSubSet$NOTIONAL/(BondSubSet$DirtyPrice-
                                                      ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate0.5)^(BondSubSet$DaysToMaturity-3.0)-
                                                         ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate1)^(BondSubSet$DaysToMaturity-2.0)-
                                                            ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate1.5)^(BondSubSet$DaysToMaturity-1.5)-
                                                               ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate2)^(BondSubSet$DaysToMaturity-1.0)-
                                                                  ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate2.5)^(BondSubSet$DaysToMaturity-0.5)
                                                               ))))
                                                       ^(1/BondSubSet$DaysToMaturity))))-1,
                              BondSubSet$SpotRate)

BondSubSet$SpotRate3 <- rep(BondSubSet$YTM[51:60],10)



# spot rate for 3.5y to 4.0y
BondSubSet$SpotRate <- ifelse(BondSubSet$DaysToMaturity < 4.0 & BondSubSet$DaysToMaturity > 3.5, 
                              (BondSubSet$NOTIONAL/(BondSubSet$DirtyPrice-
                                                      ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate0.5)^(BondSubSet$DaysToMaturity-3.5)-
                                                         ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate1)^(BondSubSet$DaysToMaturity-3.0)-
                                                            ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate1.5)^(BondSubSet$DaysToMaturity-2.0)-
                                                               ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate2)^(BondSubSet$DaysToMaturity-1.5)-
                                                                  ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate2.5)^(BondSubSet$DaysToMaturity-1.0)-
                                                                     ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate3)^(BondSubSet$DaysToMaturity-0.5)
                                                                  )))))
                                                       ^(1/BondSubSet$DaysToMaturity))))-1,
                              BondSubSet$SpotRate)

BondSubSet$SpotRate3.5 <- rep(BondSubSet$YTM[61:70],10)



# spot rate for 4.0y to 4.5y
BondSubSet$SpotRate <- ifelse(BondSubSet$DaysToMaturity < 4.5 & BondSubSet$DaysToMaturity > 4.0, 
                              (BondSubSet$NOTIONAL/(BondSubSet$DirtyPrice-
                                                      ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate0.5)^(BondSubSet$DaysToMaturity-4.0)-
                                                         ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate1)^(BondSubSet$DaysToMaturity-3.5)-
                                                            ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate1.5)^(BondSubSet$DaysToMaturity-3.0)-
                                                               ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate2)^(BondSubSet$DaysToMaturity-2.0)-
                                                                  ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate2.5)^(BondSubSet$DaysToMaturity-1.5)-
                                                                     ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate3)^(BondSubSet$DaysToMaturity-1.0)-
                                                                        ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate3.5)^(BondSubSet$DaysToMaturity-0.5)
                                                                     ))))))
                                                       ^(1/BondSubSet$DaysToMaturity))))-1,
                              BondSubSet$SpotRate)

BondSubSet$SpotRate4 <- rep(BondSubSet$YTM[71:80],10)



# spot rate for 4.5y to 5.0y
BondSubSet$SpotRate <- ifelse(BondSubSet$DaysToMaturity < 5.0 & BondSubSet$DaysToMaturity > 4.5, 
                              (BondSubSet$NOTIONAL/(BondSubSet$DirtyPrice-
                                                      ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate0.5)^(BondSubSet$DaysToMaturity-4.5)-
                                                         ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate1)^(BondSubSet$DaysToMaturity-4.0)-
                                                            ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate1.5)^(BondSubSet$DaysToMaturity-3.5)-
                                                               ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate2)^(BondSubSet$DaysToMaturity-3.0)-
                                                                  ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate2.5)^(BondSubSet$DaysToMaturity-2.0)-
                                                                     ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate3)^(BondSubSet$DaysToMaturity-1.5)-
                                                                        ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate3.5)^(BondSubSet$DaysToMaturity-1.0)-
                                                                           ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate4)^(BondSubSet$DaysToMaturity-0.5)
                                                                        )))))))
                                                       ^(1/BondSubSet$DaysToMaturity))))-1,
                              BondSubSet$SpotRate)

BondSubSet$SpotRate4.5 <- rep(BondSubSet$YTM[81:90],10)



# spot rate for 5.0y to 5.5y
BondSubSet$SpotRate <- ifelse(BondSubSet$DaysToMaturity < 5.5 & BondSubSet$DaysToMaturity > 5.0, 
                              (BondSubSet$NOTIONAL/(BondSubSet$DirtyPrice-
                                                      ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate0.5)^(BondSubSet$DaysToMaturity-5.0)-
                                                         ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate1)^(BondSubSet$DaysToMaturity-4.5)-
                                                            ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate1.5)^(BondSubSet$DaysToMaturity-4.0)-
                                                               ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate2)^(BondSubSet$DaysToMaturity-3.5)-
                                                                  ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate2.5)^(BondSubSet$DaysToMaturity-3.0)-
                                                                     ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate3)^(BondSubSet$DaysToMaturity-2.0)-
                                                                        ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate3.5)^(BondSubSet$DaysToMaturity-1.5)-
                                                                           ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate4)^(BondSubSet$DaysToMaturity-1.0)-
                                                                              ((100*BondSubSet$COUPON)/(1+BondSubSet$SpotRate4.5)^(BondSubSet$DaysToMaturity-0.5)
                                                                           ))))))))
                                                       ^(1/BondSubSet$DaysToMaturity))))-1,
                              BondSubSet$SpotRate)
BondSubSet$SpotRate5 <- rep(BondSubSet$YTM[91:100],10)


BondSubSet <- BondSubSet %>% select(1:13)

SpotData <- (BondSubSet %>% select(5,4,11,13))
SpotData <- SpotData[order(SpotData$`DATE OF PRICE`),]
SpotData$SpotRate <- SpotData$SpotRate*100




# plot maturity vs. yield for each date recorded
ggplot(data = SpotData, 
       mapping = aes(x = SpotData$`MATURITY DATE`, SpotData$SpotRate, group = 
                       SpotData$`DATE OF PRICE`, color = SpotData$`DATE OF PRICE`)) + 
  labs(title =  "Government of Canada\n5 Year Spot Rate Curves",
       x = "Maturity (Year)", y = "Spot Rate (%)") + 
  geom_point(size = 1, shape = 23) +
  theme(panel.spacing = unit(1, "lines")) + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5,
                                  margin = margin(10, 0, 10, 0), vjust=2.5), 
        axis.title.x = element_text(size=15, vjust=-0.35),
        axis.title.y = element_text(size=15, vjust=0.35),
        legend.title = element_text(size=14, face="bold")) +
  labs(color='Date\nRecorded') + 
  geom_smooth(method="lm", se=FALSE, fullrange=FALSE, formula = y ~ splines::bs(x, 3)) + 
  geom_point(alpha = 0.1, aes(color = SpotData$`DATE OF PRICE`) + 
               scale_y_continuous(labels = scales::number_format(accuracy = 0.001)))




######### Calculate and plot forward rates #########



ForwardData <- BondSubSet %>% select(5,11,13)
ForwardData <- ForwardData %>% filter(ForwardData$DaysToMaturity< 1.5 & ForwardData$DaysToMaturity > 1.0)


# append spot rates for each corresponding date
ForwardData$R2 <- SpotData$SpotRate[31:40]
ForwardData$R3 <- SpotData$SpotRate[51:60]
ForwardData$R4 <- SpotData$SpotRate[71:80]
ForwardData$R5 <- SpotData$SpotRate[91:100]


# append ti for each corresponding date
ForwardData$R2t <- SpotData$DaysToMaturity[31:40]
ForwardData$R3t <- SpotData$DaysToMaturity[51:60]
ForwardData$R4t <- SpotData$DaysToMaturity[71:80]
ForwardData$R5t <- SpotData$DaysToMaturity[91:100]


ForwardData$F11 <- ForwardData$SpotRate
ForwardData$F12 <- (((1+ForwardData$R2)^(ForwardData$R2t+ForwardData$DaysToMaturity))/
  (1+ForwardData$SpotRate)^(ForwardData$DaysToMaturity))^(1/ForwardData$R2t)-1
ForwardData$F13 <- (((1+ForwardData$R3)^(ForwardData$R3t+ForwardData$DaysToMaturity))/
                               (1+ForwardData$SpotRate)^(ForwardData$DaysToMaturity))^(1/ForwardData$R3t)-1
ForwardData$F14 <- (((1+ForwardData$R4)^(ForwardData$R3t+ForwardData$DaysToMaturity))/
                               (1+ForwardData$SpotRate)^(ForwardData$DaysToMaturity))^(1/ForwardData$R4t)-1
ForwardData$F15 <- (((1+ForwardData$R5)^(ForwardData$R3t+ForwardData$DaysToMaturity))/
                               (1+ForwardData$SpotRate)^(ForwardData$DaysToMaturity))^(1/ForwardData$R5t)-1

ForwardData <- ForwardData[c(1,2,3,12,13,14,15,16)]


# transpose the data to plot
ForwardData <- melt(ForwardData, id.vars = 'DATE OF PRICE' )
ForwardData <- ForwardData %>% filter(ForwardData$variable %in% c("F11", "F12", "F13", "F14", "F15"))


# convert rates to % for plotting
ForwardData$value <- ForwardData$value*100


# plot forward rates, grouped by date
ggplot(data = ForwardData, 
       mapping = aes(x = ForwardData$variable, ForwardData$value, group = 
                       ForwardData$`DATE OF PRICE`, color = ForwardData$`DATE OF PRICE`)) + 
  labs(title =  "Government of Canada\n5 Year Forward Rates",
       x = "Forward Term", y = "1 Year Forward Rate (%)") + 
  geom_point(size = 1, shape = 23) +
  theme(panel.spacing = unit(1, "lines")) + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5,
                                  margin = margin(10, 0, 10, 0), vjust=2.5), 
        axis.title.x = element_text(size=15, vjust=-0.35),
        axis.title.y = element_text(size=15, vjust=0.35),
        legend.title = element_text(size=14, face="bold")) +
  labs(color='Date\nRecorded') + 
  geom_smooth(method="lm", se=FALSE, formula = y ~ splines::bs(x, 4)) + 
  geom_point(alpha = 10, aes(color = ForwardData$`DATE OF PRICE`) + 
               scale_y_continuous(labels = scales::number_format(accuracy = 0.1)))



############## Covariance matrices for log-returns of yield and forward rates

######### Covariance matrix for log-return of yield

YieldData <- YieldData[order(YieldData$`DATE OF PRICE`,YieldData$`MATURITY DATE`),]

YieldData1 <- YieldData[1:10,]
YieldData1 <- mutate(YieldData1, LogReturn = log(YieldData1$YTM / (lag(YieldData1$YTM))))
YieldData2 <- YieldData[11:20,]
YieldData2 <- mutate(YieldData2, LogReturn = log(YieldData2$YTM / (lag(YieldData2$YTM))))
YieldData3 <- YieldData[21:30,]
YieldData3 <- mutate(YieldData3, LogReturn = log(YieldData3$YTM / (lag(YieldData3$YTM))))
YieldData4 <- YieldData[31:40,]
YieldData4 <- mutate(YieldData4, LogReturn = log(YieldData4$YTM / (lag(YieldData4$YTM))))
YieldData5 <- YieldData[41:50,]
YieldData5 <- mutate(YieldData5, LogReturn = log(YieldData5$YTM / (lag(YieldData5$YTM))))
YieldData6 <- YieldData[51:60,]
YieldData6 <- mutate(YieldData6, LogReturn = log(YieldData6$YTM / (lag(YieldData6$YTM))))
YieldData7 <- YieldData[61:70,]
YieldData7 <- mutate(YieldData7, LogReturn = log(YieldData7$YTM / (lag(YieldData7$YTM))))
YieldData8 <- YieldData[71:80,]
YieldData8 <- mutate(YieldData8, LogReturn = log(YieldData8$YTM / (lag(YieldData8$YTM))))
YieldData9 <- YieldData[81:90,]
YieldData9 <- mutate(YieldData9, LogReturn = log(YieldData9$YTM / (lag(YieldData9$YTM))))



# form matrix from returns, and generate covariance matrix:

M1 <- cov((cbind(YieldData1$LogReturn, YieldData2$LogReturn, YieldData3$LogReturn, YieldData4$LogReturn,
           YieldData5$LogReturn, YieldData6$LogReturn, YieldData7$LogReturn, YieldData8$LogReturn,
           YieldData9$LogReturn))[2:10,])



# Eigenvalues and eigenvectors of covariance matrix:
YieldCovEigenValues <- eigen(M1)



######### Covariance matrix for log-return of forward rates
ForwardData <- ForwardData[order(ForwardData$`DATE OF PRICE`,ForwardData$variable),]

ForwardData1 <- ForwardData[1:5,]
ForwardData1 <- mutate(ForwardData1, LogReturn = log(abs(ForwardData1$value / (lag(ForwardData1$value)))))
ForwardData2 <- ForwardData[6:10,]
ForwardData2 <- mutate(ForwardData2, LogReturn = log(abs(ForwardData2$value / (lag(ForwardData2$value)))))
ForwardData3 <- ForwardData[11:15,]
ForwardData3 <- mutate(ForwardData3, LogReturn = log(abs(ForwardData3$value / (lag(ForwardData3$value)))))
ForwardData4 <- ForwardData[16:20,]
ForwardData4 <- mutate(ForwardData4, LogReturn = log(abs(ForwardData4$value / (lag(ForwardData4$value)))))



# form matrix from returns, and generate covariance matrix:
M2 <- cov((cbind(ForwardData1$LogReturn, ForwardData2$LogReturn, ForwardData3$LogReturn,
                 ForwardData4$LogReturn))[2:5,])



# Eigenvalues and eigenvectors of covariance matrix:
ForwardCovEigenValues <- eigen(M2)





