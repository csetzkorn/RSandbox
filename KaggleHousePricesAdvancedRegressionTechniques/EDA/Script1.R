library(prettyR)
library(dplyr)
library(forcats) # for factors
library(ggplot2)
library(broom)
library(sqldf)

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
# Alley 
OriginalData$Alley <- fct_explicit_na(OriginalData$Alley, "NoAlley")

# impute missing LotFrontage
FilteredData <- OriginalData 
FilteredData$Outlier <- isnt_out_tukey(FilteredData$LotArea)
FilteredData = sqldf("select * from FilteredData where Outlier = 1")
fit <- lm(LotFrontage ~ LotArea, FilteredData)
tidy(fit)
Slope <- coef(fit)[term = 'LotArea']
Intercept <- coef(fit)[term = '(Intercept)']
OriginalData$LotFrontage[is.na(OriginalData$LotFrontage)] <- Intercept + (Slope * OriginalData$LotArea)[is.na(OriginalData$LotFrontage)]


# find columns with missing values
ColumnWithMissingValues <- colnames(OriginalData)[colSums(is.na(OriginalData)) > 0]
ColumnWithMissingValues
