#' ---
#' title: "Habits"
#' author: John Doe
#' date: March 22, 2005
#' output: ioslides_presentation
#' ---
#'   
#' # In the morning
#'   
#' ## Getting up | what i like to do first
#'   
#' > - Turn off alarm
#' > - Get out of bed
#' 
library(car)
Prestige
#' ----
library(latticeExtra)
#' ----
#+ results='asis',echo=FALSE
xyplot(prestige~income|type, Prestige, groups = cut(education,4),
       auto.key = T)
#' ## Linear models
#' This is a linear model:
#' ### <b> 
fit <- lm(prestige ~ income * type, Prestige)
summary(fit)
#' ### </b>
#' and that was a linear model.
#' ## Breakfast
#' 
#' - Eat eggs
#' - Drink coffee
#' 
#' # In the evening
#' 
#' ## Dinner
#' 
#' - Eat spaghetti
#' - Drink wine
#' 
#' ----
#'   
#' ![picture of spaghetti](images/spaghetti.jpg)
#' 
#' ## Going to sleep
#' 
#' - Get in bed
#' - Count sheep