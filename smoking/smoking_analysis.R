#'
#' # Analysis of smoking and life expectancy
#' 
#' This file is in the 'base' directory for analysis
#'  
library(yscs)

data(smoking)
?data
head(smoking)
dim(smoking)
xqplot(smoking)
xqplot(smoking, ptype = 'n')

fit <- lm( LE ~ smoking_female + smoking_male, smoking)
summary(fit)
smoking$both <- with(smoking, smoking_female + smoking_male)
fit2 <- lm( LE ~ both + smoking_female + smoking_male, smoking)
summary(fit2)
