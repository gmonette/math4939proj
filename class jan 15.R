#' ---
#' title: "Example of an R notebook using R Markdown"
#' author: "Your Name"
#' date: "`r format(Sys.time(), '%H:%M %d %B %Y')`"
#' output:
#'    html_document:
#'      toc: true
#'      toc_depth: 4
#' ---
#' Generated:
{{format(Sys.time(), '%H:%M %d %B %Y')}}
#' 
#' This R script in R Notebook format is a example of an R script that
#' includes data exploration and analyses in a form that you can turn into
#' an html file, or, alternatively, a pdf file or a Word document.
#' 
#' The analysis may include parts that cannot be included in the report.
#' These parts are excluded with 'chunk options' as explained below.
#' 
#' ### The General Linear Hypothesis
#' 
#' The analyses also show an example of using the GLH 
#' (General Linear Hypothesis) to test specific questions about the model.
#' 
#' The imprtance of using specified linear hypothesis is that it allows you
#' to formulate and address specific questions about the subject matter.
#' 
#' Each estimated coefficient of in a standard regression answers 
#' a specific question about the data through the model.
#' However, in most analyses, most of the questions answered by the fitted
#' coefficients of the model are of little interest **and** most of the
#' interesting questions are not answered by the fitted coefficients.
#' 
#' So you need to know how to formulate and ask meaningful questions. 
#' Not all questions can be framed with linear hypotheses but knowing how
#' to use them greatly enlarges the scope of your analyses.
#' 
#' The 'wald' function in the 'yscs' package is designed to provide
#' quick answers to frequent questions involving linear hypotheses.  
#' For more complex questions, it can be used with the 'Lfx' function that
#' helps you construct linear hypothesis matrices.
#' 
#' Linear hypotheses for models using generalized parametric splines 
#' (using the 'gsp' function) can be generated with the 'sc' function.
#' 
#' In SAS, in PROC GLM, linear hypotheses are generated with the 
#' ['ESTIMATE' and 'CONTRAST' statements](https://support.sas.com/resources/papers/proceedings11/351-2011.pdf). 
#' 
#' ## Initial setup
#' 
#' We first set up some options. This is optional.
#' 
#' If you get an error that a package is not installed
#' just install it with:
#' <!-- This is how you create a hidden comment 
#'   that is include but not shown html output -->
#' <!-- This is an example of a chunk with option: eval=FALSE -->
#+ eval=FALSE
install.packages("<name of package>")
#' <!-- And the following is a chunk we 'evaluate' 
#' but weddon't show the messy code
#' 'opts' is just an optional name for the chunk. 
#' ChunkNames, if you use them must be unique: often a pain in the neck -->
#+ opts,echo=FALSE
library(knitr)  # this is the library used to generate documents
opts_knit$set(width=100)
options(width=100)
# The 'comment' argument is the string that precedes any output in the document
opts_chunk$set(tidy=FALSE,comment='  ',fig.height=8,fig.width=10)
#'
#'
#' ## Loading some packages
#' 
#' Much of the really interesting work in R uses specialized packages. 
#' 'Mature' packages are usually uploaded to 'CRAN' and can be installed with, for example:
#+ eval=FALSE
install.packages('car')
#' Many 'maturing' packages are incubated by their authors on GitHub and
#' can be installed (after having installed the 'devtools' package) with,
#' for example:
#+ eval=FALSE
devtools::install_github('gmonette/yscs')
#' 
#' Here are the packages we will install for this script.
#' Note that you can install each package anywhere in the script before
#' you actually need to use the functions or data in the package.
#' To get information on a package, use, for example:
#+ eval=FALSE
help(package=yscs)
#' 
library(yscs) # devtools::install_github('gmonette/yscs')
library(p3d)  # devtools::install_github('gmonette/p3d')
library(lattice)
library(latticeExtra) # to combine plots together
library(gridExtra)  # for grid.arrange
library(magrittr) # for function pipelines
library(car) #'Companion to Applied Regression by John Fox
#' <!--
#' This creates a hidden comment in your html code
#' -->
#'
#' ## Exploring the Prestige data set
#' 
#' The 'Prestige' data set is in the car package so it's 
#' easy to load -- in contrast with typical data where 
#' you have to spend a lot of effort getting the data in shape
#' 
data(Prestige)  # with some packages you need to do this to 'load' the data frame
#+ eval=FALSE
?Prestige # this won't show in the document
#+
dim(Prestige)
head(Prestige)
xqplot(Prestige) # I find this invaluable for exploration but don't use it for polished reports or presentations
#'
#' ### Regression of income on education and gender
#' 
xyplot( income ~ education , Prestige)
xyplot( income ~ education | type, Prestige)
xyplot( income ~ education, Prestige, groups = type)
gd(3) # in the yscs package, uses a 'ggplot' like theme for groups
xyplot( income ~ education, Prestige, groups = type)
xyplot( income ~ education | type, Prestige)
gd_(col = 'blue', cex = 1.2, pch = 1)  # to change options when not using groups
xyplot( income ~ education | type, Prestige)
#' The followin only works interactively, so we set eval=FALSE
#+ eval=FALSE
xyplot( income ~ education, Prestige, groups = type)
trellis.focus()
zz <- panel.identify(labels=rownames(Prestige))
zz
trellis.unfocus()
#+
zz <- c(2, 90,96)
gd_(3,cex=0.9)
xyplot( income ~ education, Prestige, groups = type,
        labels = rownames(Prestige)) +
  layer( panel.text(x=x[zz],y=y[zz], labels=labels[zz],
                    cex = 2))
#+ eval=FALSE
library(p3d) # devtools::install_github('gmonette/p3d')
Init3d()
Plot3d(income ~ education + women | type, Prestige)
Id3d()
#'
#'
#' Fitting a regression:
#'
fit <- lm(income ~ education + women, Prestige)
fit
print(fit)
summary(fit)
fit.ed <- lm(income ~ education , Prestige)
summary(fit.ed)
#'
#' ### Regression diagnostics
#'
plot(fit, id.n = 7)  # number to identify
avPlots(fit, id.n = 7)
#'
#' ## Exploring expected income
#' 
#' We explore expected income as a function of gender, education and type of occupation
#'
fit <- lm( income ~ education * women * type, Prestige)
#'
#' <!--  REACHED HERE ON FRIDAY JAN 15, 2016 -->
#'
#'
#' One of my favourite exam questions is to display output
#' like this and ask "what is exact interpretation of 
#' the coefficient labelled 'women'? the coefficient 
#' labelled 'typewc'? etc.
#'
summary(fit)
#' 
#' Quick diagnostics:
#' 
plot(fit, id.n = 6)
avPlots(fit, id.n = 6)
#'
#' ### Looking at the model in 3d
#'
#+ eval=FALSE
library(p3d)
Init3d()
Plot3d(income ~ education + women | type, Prestige,
       col = c('blue','green','orange'))
Axes3d()
Id3d()
Fit3d(fit)
fitq <- update(fit, 
               . ~ type * (education * women + 
                             I(education^2) + I(women^2)))
Pop3d(6)
Fit3d(fitq) # shows how quads don't extrapolate here
#' Questions:
#' 
#' 1. How many parameters do we have? 
#' 2. How many observations? 
#' 3. Is the ratio reasonable? 
#' 4. How should we handle the large variance of residuals at the higher predicted incomes?
#' 5. Should we simplify the model?
#'
#' We will use the 'gls' (generalized least-squares) 
#' function in the 'nlme' package to incorporate 
#' heteroscedasticity in the model.
#' 
library(nlme)
?gls
Prestige$occ <- rownames(Prestige)   # make the rownames a variable
fit <- gls(income ~ education * women * type, Prestige, na.action = na.omit)
summary(fit)
plot(fit, id = .1, idLabels = ~ occ)  # default not as much as with 'lm' but fit should be the same
#       - note how we use 'id' = proportion to identify
#         instead of 'id.n' = number to identify
#       - also the 'idLabels' are given with a formula
#'
#' There is heteroscedascity and we should do something to
#' take into account the much larger variance when the
#' fitted value is large.
#' 
#' Depending on circumstances, this could be addressed in
#' many ways some of which are:
#' 
#' 1. Perhaps the large residuals are anomalous or consequences of a process not included in the model. They could be segregated or the model enlarged to predict their value.
#' 2. Perhaps the natural model should be based on a transformation of the response or of the predictors.  A transformation will sometimes restore linearity and homoscedasticity.  Find a good reference on Tukey's ladder of powers.  But don't transform mechanically. If the relationship is not linear, often you want to use a model that is not linear in the predictors, or even not linear in the parameters.
#' 3. Perhaps it makes sense to have larger variance at higher predicted values and the model should incorporate a term to allow for heteroscedasticity. This is what we pursue below with 'gls' in the 'nlme' package.
#'
fit2 <- gls(income ~ education * women * type, Prestige, 
            weights = varPower(form = ~fitted(.)),
            na.action = na.omit)
summary(fit2)
plot(fit2, id = .05, idLabels = ~ occ) # standardized residuals divided by estimated standard deviation
plot(fit2, resid(., type = 'p') ~ resid(., type = 'response'), 
     xlab = 'raw residual',
     ylab = 'standardized residual',
     id = .1, idLabels = ~ occ)
#'
#' Let's see if we can simplify the model by dropping the 3-way interaction
#' 
wald(fit2, ':.*:' ) # we use a regular expression that matches terms 
#           with two or more ':'s, i.e. 3- and higher-way interactions
wald(fit2, ':' ) # we use a regular expression that matches 
#                all interactions 
# or more ':'s
wald(fit2, 'type')  # does type add to other variables?
wald(fit2, 'women') # etc ...
wald(fit2, 'education')
#'
#' Perhaps 'type' is almost equivalent to categorical version of 'education'.
#' I will keep it nevertheless to illustrate estimation with a categorical variable.
#'  
#' Two-way interaction model:
#' 
fit3 <- gls(income ~ (education + women + type)^2, Prestige, 
            weights = varPower(form = ~fitted(.)),
            na.action = na.omit)
summary(fit3)
plot(fit3, resid(.,type='r') ~ education, id = .1, 
     idLabels = ~ occ)
plot(fit3, resid(.,type='p') ~ education, 
     id = .2, idLabels = ~occ)
#'
#' Looking at residuals is an opportunity to think of factors
#' that might have been omitted in the model.  Is there a clump of
#' residuals with a common attribute?
#'
#' ### Showing expected income with graphs
#' 
#' How much of an increase in income is associated with an additional
#' year of education?  Note that I found it hard to say: 'how much does income
#' go up when you have an extra year of education? in order to avoid the 
#' causal implications. This is quite hard and feels unnatural.
#' 
pred <- subset(Prestige, !is.na(type)) # drop occs where type is NA
# discretize gender
(20 * round(pred$women/20)) %>% unique
(20 * round(pred$women/20)) %>% tab
pred$women <- 20 * round(pred$women/20)
pred$yhat <- predict(fit3, newdata = pred)

xyplot( yhat ~ education | type, pred, 
        groups = women, type = 'l')

pred <- sortdf(pred, ~ education)
xyplot( yhat ~ education | type, pred, 
        groups = women, type = 'l')

gd(7, lwd = 2, lty = 1)
xyplot( yhat ~ education | type, pred, 
        groups = women, type = 'l',
        auto.key = T)

xyplot( yhat ~ education | type, pred, 
        groups = women, type = 'l',
        layout = c(1,3),
        auto.key = 
          list( space='right', title='% of women'))

xyplot( yhat ~ education | type, pred, 
        groups = women, type = 'l',
        ylab = 'predicted income (Cdn $ in 1970)',
        layout = c(1,3),
        auto.key = 
          list( space='right', title='% of women',
                points = FALSE, lines = TRUE))

pred$Type <- tr(pred$type, levels(pred$type),
                c("blue collar",'professional','white collar'))

pred$Type <- reorder(pred$Type, pred$education)
xyplot( yhat ~ education | Type, pred, 
        groups = women, type = 'l',
        ylab = 'predicted income (Cdn $ in 1970)',
        layout = c(1,3),
        auto.key = list( space='right', title='% of women',
                         points = FALSE, lines = TRUE),
        scales = list(y=list(alternating=F)))

#' 
#' Switching panel and grouping variables
#' 
xyplot( yhat ~ education | women, pred, groups = Type, 
        type = 'l',
        ylab = 'predicted income (Cdn $ in 1970)',
        layout = c(1,6),
        auto.key = list( space='right', 
                         points = FALSE, lines = TRUE),
        scales = list(y=list(alternating=F)))
# more intuitive colours
pred$Women <- with(pred, paste0(women, "% women"))
pred$Women <- with(pred, Women, women)
gd(col = c('blue','black','dark green'), lty = 1, lwd = 2)
xyplot( yhat ~ education | Women, pred, groups = Type, 
        type = 'l',
        ylab = 'predicted income (Cdn $ in 1970)',
        layout = c(1,6),
        auto.key = list( space='right', 
                         points = FALSE, lines = TRUE),
        scales = list(y=list(alternating=F)),
        as.table = TRUE)
#'
#' # What is 'value' of an additional year of education?
#' 
#' ## Using GLH (to come)
#' 
#' Using GLH we will be able to, among other things:
#' 
#' 1. put confidence bounds around the predicted values above
#' 2. estimated the magnitude and significance of differences between types for various combinations of gender composition and education
#' 3. estimate association between education and income for various combinations of gender and type
#' 
#' # Some R Markdown examples (This is a level 1 heading)
#' 
#' Note that headings are created with 'number signs' and the
#' level of the heading is indicated with the number of
#' 'number signs'
#' 
#' ## This is a subheading
#' 
#' This is a bullet list:
#' 
#' - item number 1
#' - item number 2
#'  
#' and this is a numbered list:
#' 
#' 1. first time
#' 1. second item (it doesn't matter what number you use in the source code.
#'  
#' This is an example of a link that happens to show you
#' where to get help on R Markdown:
#' 
#' 1. [R Markdown Cheat Sheet](https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf)
#' 2. [More documentation](http://rmarkdown.rstudio.com/)
#' 
#' You can also use LaTeX to generate equations:
#' 
#' $$f(x|\mu) = \frac{1}{\sqrt{2 \pi}}e^{-(x-\mu)^2/2}$$
#' 
#' You can also insert R output in a text paragraph: e.g.
#' 2 + 3 = `r 2+3`. Did we get that right?
#' 
#' 