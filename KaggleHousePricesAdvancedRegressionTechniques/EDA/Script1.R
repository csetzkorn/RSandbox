library(prettyR)
library(dplyr)
library(forcats) # for factors
library(ggplot2)
library(broom)

#https://www.kaggle.com/c/house-prices-advanced-regression-techniques

setwd("D:/RProjects/RSandbox/KaggleHousePricesAdvancedRegressionTechniques/Data")

# function to detect outliers
isnt_out_tukey <- function(x, k = 1.5, na.rm = TRUE) {
    quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
    iqr <- diff(quar)

    (quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr)
}

OriginalData <- read.csv(file = "train.csv", header = TRUE, sep = ",")

# for Fence missing means no fence - so recode missing
OriginalData$Fence <- fct_explicit_na(OriginalData$Fence, "NoFence")

sum(table(OriginalData$Fence))

nrow(OriginalData)

# find columns with missing values
ColumnWithMissingValues <- colnames(OriginalData)[colSums(is.na(OriginalData)) > 0]
ColumnWithMissingValues

#ClosedMatters$claimants_age[is.na(ClosedMatters$claimants_age)] <- mean(ClosedMatters$claimants_age, na.rm = T)

sum(is.na(OriginalData$LotAreaUnSq))

#table(OriginalData$LotFrontage)
#LotAreaUnSq correlated to

ggplot(OriginalData, aes(x = LotArea, y = LotFrontage)) +
    geom_point() +
    coord_cartesian(xlim = c(0, 50000), ylim = c(0, 200))


FilteredData <- OriginalData #%>%
    #select(LotFrontage, LotArea) %>%
    #filter(LotArea <= 25000)

fit <- lm(LotFrontage ~ LotArea, FilteredData)
tidy(fit)

Slope <- coef(fit)[term = 'LotArea']
Intercept <- coef(fit)[term = '(Intercept)']

OriginalData$LotFrontage[is.na(OriginalData$LotFrontage)] <- Intercept + (Slope * OriginalData$LotArea)

sum(is.na(OriginalData$LotFrontage))
ggplot(OriginalData, aes(x = LotArea, y = LotFrontage)) +
    geom_point() +
    geom_abline(intercept = Intercept, slope = Slope) +
    coord_cartesian(xlim = c(0, 50000), ylim = c(0, 200))
