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
#' <!--
#' This creates a hidden comment in your html code -- a good way to include 
#' notes you don't want to show but want to have in the document, i.e. metadata.
#' -->
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
#' The importance of using specified linear hypotheses is that it allows you
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
xqplot(Prestige) 
#' 
#' I find 'xqplot' invaluable for quick exploration but don't use it for polished reports or presentations.
#'
#' ### Regression of income on education and gender
#' 
xyplot( income ~ education , Prestige)
xyplot( income ~ education | type, Prestige)
xyplot( income ~ education, Prestige, groups = type)
gd(3) # in the yscs package, uses a 'ggplot'-like theme for groups
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
summary(fit)
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
#' One of my favourite exam questions is to display output
#' like this and ask "what is exact interpretation of 
#' the coefficient labelled 'women'? the coefficient 
#' labelled 'typewc'? etc.
#'
summary(fit)
#' 
#' Some diagnostics:
#'
#' Looking at residuals is an opportunity to think of factors
#' that might have been omitted in the model.  Is there a clump of
#' residuals with a common attribute?
#'
#' Does the model look reasonable? What should we look for?
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
#'
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
#+ eval=FALSE
?gls
#+
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
#' Let's ask whether we can consider simplifying the model by dropping the 3-way interaction
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
#' #### Two-way interaction model:
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
#' #### Model with quadratic terms in education and women
#' 
fit3q <- update(fit3, . ~ type * (education * women + I(education^2) +I(women^2)))
summary(fit3q)
#'
#' **Exercise:** Have a look at diagnostics?
#' 
#' #### Model formulas in R
#' 
#' Where in the textbook would you find the interpretation of the following
#' terms in the predictor formula:
#' 
#' 1. a categorical variable (preferably represented as a factor in R)
#' 2. a numeric variable
#' 3. an 'interaction' between numeric variables, e.g. 'education * women'
#' 4. an interaction between a numeric variable and a numeric variable
#' 5. an interaction between two categorical variables
#' 6. an interaction of a variable with itself: e.g. 'women * women', or 'women^2'
#'
#' To interpret coefficients, we need to be able to write the mathematical
#' prediction formula that corresponds to the model formula. 
#' 
#' One aspect of this is understanding the **X** matrix generated by the 
#' formula:
#' 
getX(fit3) %>% head  # top 6 rows
getX(fit3) %>% some  # from the 'car' package, prints 10 rows at random
getX(fit3q) %>% head
#'
#' The mathematical formula for 'fit3q', to pick a complex one, is
#' 
#' \[\begin{aligned}
#' \operatorname{E} (Y|W,E,T) =  
#' & {\beta _0} + {\beta _1}{T_p} + {\beta _2}{T_w} + {\beta _3}E 
#' + {\beta _4}W + {\beta _5}{E^2} + {\beta _6}{W^2} + {\beta _7}EW \\
#' &  + {\beta _8}{T_p}E + {\beta _9}{T_w}E + {\beta _{10}}{T_p}W 
#' + {\beta _{11}}{T_w}W + {\beta _{12}}{T_p}{E^2} + {\beta _{13}}{T_w}{E^2} \\
#' &  + {\beta _{14}}{T_p}{W^2} + {\beta _{15}}{T_w}{W^2} + {\beta _{16}}{T_p}EW + {\beta _{17}}{T_w}EW \\
#' \end{aligned} \]
#' 
#' where ${T_p} = 1$ if the occupation type is 'professional' and 0 otherwise and, similary for $T_w$ and 'white collar'.
#' Note that 'blue collar' is the *reference level*, i.e. the one that occupation type that correspondss to
#' $T_p = T_w = 0$.  With factors in R, the reference level is the first level of the factor, e.g.
levels(Prestige$type)
#'
#' Once we express the formula mathematically, it is easy to see how we could use the model to 
#' ask all sorts of questions. For example, what is the increase in income associated with an increase in
#' education for a professional occupation that has no women? It is
#' 
#' \[\frac{d}{{dE}}\operatorname{E} (Y|W,E,T{\left. ) \right|_{W = 0;T = p}}\]
#' 
#' which we can find by differentiating with respect to $E$ and and setting $W=0$, $T_p = 1$ and $T_w = 0$ in the 
#' the result:
#'
#' \[\begin{aligned}
#' \frac{d}{{dE}}\operatorname{E} (Y|W,E,T{\left. ) \right|_{W = 0;T = p}} =  
#' & {\beta _3} + 2{\beta _5}E + {\beta _7}W \\
#' &  + {\beta _8}{T_p} + {\beta _9}{T_w} + 2{\beta _{12}}{T_p}E + 2{\beta _{13}}{T_w}E \\ 
#' &  + {\beta _{16}}{T_p}W + {\beta _{17}}{T_w}W \\
#' \end{aligned} \]
#' 
#' We estimate this using the 'General Linear Hypothesis' as a linear combination of the vector of $\beta$s.
#' 
#' Suppose we want to estimate the 'value of an extra year' when $E = 10$, we find 
#' \[ \hat{\eta} = L \hat{\beta} = \left[ {\begin{array}{*{20}{c}}
#' 0&0&0&1&0&{20}&0&0&1&0&0&0&20&0&0&0&0&0 
#' \end{array}} \right]\left[ {\begin{array}{*{20}{c}}
#' {{\hat{\beta} _0}} \\ 
#' {{\hat{\beta} _1}} \\ 
#' {{\hat{\beta} _2}} \\ 
#' {{\hat{\beta} _3}} \\ 
#' {{\hat{\beta} _4}} \\ 
#' {{\hat{\beta} _5}} \\ 
#' {{\hat{\beta} _6}} \\ 
#' {{\hat{\beta} _7}} \\ 
#' {{\hat{\beta} _8}} \\ 
#' {{\hat{\beta} _9}} \\ 
#' {{\hat{\beta} _{10}}} \\ 
#' {{\hat{\beta} _{11}}} \\ 
#' {{\hat{\beta} _{12}}} \\ 
#' {{\hat{\beta} _{13}}} \\ 
#' {{\hat{\beta} _{14}}} \\ 
#' {{\hat{\beta} _{15}}} \\ 
#' {{\hat{\beta} _{16}}} \\ 
#' {{\hat{\beta} _{17}}} 
#' \end{array}} \right]\] 
#' 
#' To estimate the variance of $\hat{\eta}$ we use $L \hat{Var}(\hat{\beta}) L'$.
#'
#' In R, we can do this with matrix multiplication:
L <- rbind(c(0,0,0,1,0,20,0,0,1,0,0,0,20,0,0,0,0,0))
L
#' With a 'gls' fit, we extract $\hat{\beta}$ with:
coef(fit3q)  
#' So the estimate is:
L %*% coef(fit3q)
#' Wow! -- don't do it!
#'
#' Now for the variance ... but there are functions that make this easier, 'lht' in 'car' and 'wald' in 'yscs':
wald(fit3q, L)
#'
#' What could be happening?
#' 
#' Let's see the fitted model for various combination of education, women and type
#' 
pred <- expand.grid(women = seq(0,100,20), education = seq(6,18,.2), type = levels(Prestige$type))
#' 'pred' is a data frame with the Cartesian product of its arguments
dim(pred)
head(pred,10)
tail(pred,10)
pred$income.pred <- predict(fit3q, newdata = pred)
xyplot(income.pred ~ education | type, pred, groups = women, type = 'l')
xyplot(income.pred ~ education | type, pred, groups = women, type = 'l', 
       auto.key = T) + layer(panel.abline(v = 12))
gd(6, lty = 1, lwd = 2)
xyplot(income.pred ~ education | type, pred, groups = women, type = 'l', 
       auto.key = list(points = F, lines = T)) + 
  layer(panel.abline(v = 12))
#'
#' Estimating the 'value of education':
#' 
#' We print a list of expressions that generates block of the X matrix:
#' 
Lfx(fit3q)
#'
#' We can use the list of expressions to generate the design matrix over
#' the data frame 'pred'.
#'
Led <- Lfx(
  fit3q,
  list( 1,
      1 * M(type),
      1 * education,
      1 * women,
      1 * M(I(education^2)),
      1 * M(I(women^2)),
      1 * education * women,
      1 * M(type) * education,
      1 * M(type) * women,
      1 * M(type) * M(I(education^2)),
      1 * M(type) * M(I(women^2)),
      1 * M(type) * education * women ),
  pred)
dim(Led)
head(Led)
#'
#' The list of expressions can be edited easily to differentiate with
#' respect to a continuous variable.
#'
#' To get the value of education, differentiate each term with respect to education.
#' Leave expression of the form 'M(...)' as is because they generate blocks
#' of the right size for factors. Also, products of 'M' objects generate the
#' columns of the X matrix for interaction terms.
#' 
Led.deriv <- Lfx(fit3q,
           list( 0,
                 0 * M(type),
                 1 ,
                 0 * women,
                 2 * M(I(education)),
                 0 * M(I(women^2)),
                 1 * women,
                 1 * M(type) * 1,
                 0 * M(type) * women,
                 2 * M(type) * M(I(education)),
                 0 * M(type) * M(I(women^2)),
                 1 * M(type) * 1 * women ),
           pred)
ed.deriv <- walddf(fit3q, Led.deriv, data = pred)
head(ed.deriv)
xyplot( coef ~ education | type, ed.deriv, groups = women, type = 'l',
        ylab = 'elope wrt education')
xyplot( coef ~ education | type, ed.deriv, groups = women, type = 'l',
        ylab = 'elope wrt education',
        layout = c(1,3), 
        auto.key = list(space = 'right',lines=T,points=F,title = "% women")) +
  layer(panel.abline(h=0))
#'
#' Most of the individual coefficients answer questions that are of little 
#' interest because they are ***marginal*** to other terms. e.g. dropping a non-significant intercept
#' just forces the model to go through the origin which, generally, would be
#' a completely unjustified arbitrary restriction on the model
#' 
#' Dropping a main effect, e.g. 'women' that also appears in an interactionExample of 'marginal' terms that should **almost never**
#'    be dropped:
#'    1. any term that is also included in a higher-order interaction
#'    2. any term in a polynomial except the highest order term
#'    3. the intercept which we can think of as being 'included' in all other terms
#'    
#'
wald(fit3q, "2")   # are all quadratic terms jointly significant?
wald(fit3q, ":")   # are all interaction terms jointly significant?
wald(fit3q, ":.*:")   # are all 3-way interaction terms jointly significant?
wald(fit3q, "education")   # are all 3-way interaction terms jointly significant?
wald(fit3q, "women")   # are all 3-way interaction terms jointly significant?
wald(fit3q, "type")   # are all 3-way interaction terms jointly significant?
#'
#'
#' Some questions: Using this model:
#' 
#' 1. What does the coefficient for 'women' mean?
#' 2. What does the coefficient for 'women^2' mean?
#' 3. How would you estimate the increase in income associated with an extra
#'    year of education in a white collar occupation that is 40% female?
#' 4. How would you estimate the increase in income associated with an extra
#'    year of education for a professional occupation that is 20% female?
#'
#' ### Showing predicted income with graphs
#' 
#' How much of an increase in income is associated with an additional
#' year of education?  Note that I found it hard to say: 'how much does income
#' go up when you have an extra year of education? in order to avoid the 
#' causal implications. This is quite hard and feels unnatural.
#' 
#' This is a sequence of graphs showing how predicted income, using the model
#' fitted above, varies by gender
#' composition and type of occupation.
#' 
#' Since we're using 2-D graphs, we can only show the dependent variable,
#' income, as a continuous variable and one more continuous variable for the
#' x-axis. But we have two continuous predictors. Let's choose education for
#' the x-axis, which leaves the problem of what to do with the percentage of women.  
#' We need to choose a number of representative levels for percentage to use
#' as an ordinal categorical variable.
#' 
#' There are a number of approaches to show predicted values for a model. I'll
#' illustrate two of them. One way generated a data frame with crossed 
#' combinations of the predictor variable values and plot the resulting predicted
#' values.  A second way uses a data frame whose values for predictor variables
#' reflects the interdependencies in the data. We'll see both. We'll start with
#' the second method.       
#' 
#' We'll use the data but we can't use the raw values of the '% women' because
#' we want to use just a few typical levels of that variable. One approach is to
#' round the values into an appropriate number of categories.
#' 
pred <- subset(Prestige, !is.na(type)) # drop occupations where type is NA
# discretize gender
(20 * round(pred$women/20)) %>% unique
(20 * round(pred$women/20)) %>% tab
pred$women.orig <- pred$women  # save original value because we're about to overwrite them
pred$women <- 20 * round(pred$women/20)  # we need to overwrite to use the 'predict' function 
pred$income.fit3 <- predict(fit3, newdata = pred) # predicted income using model fit3 

xyplot( income.fit3 ~ education | type, pred, 
        groups = women, type = 'l')

pred <- sortdf(pred, ~ education)
xyplot( income.fit3 ~ education | type, pred, 
        groups = women, type = 'l')

gd(7, lwd = 2, lty = 1)
xyplot( income.fit3 ~ education | type, pred, 
        groups = women, type = 'l',
        auto.key = T)

xyplot( income.fit3 ~ education | type, pred, 
        groups = women, type = 'l',
        layout = c(1,3),
        auto.key = 
          list( space='right', title='% of women'))

xyplot( income.fit3 ~ education | type, pred, 
        groups = women, type = 'l',
        ylab = 'predicted income (Cdn $ in 1970)',
        layout = c(1,3),
        auto.key = 
          list( space='right', title='% of women',
                points = FALSE, lines = TRUE))

pred$Type <- tr(pred$type, levels(pred$type),
            c("blue collar",'professional','white collar'))

pred$Type <- reorder(pred$Type, pred$education)
xyplot( income.fit3 ~ education | Type, pred, 
        groups = women, type = 'l',
        ylab = 'predicted income (Cdn $ in 1970)',
        layout = c(1,3),
        auto.key = list( space='right', title='% of women',
                         points = FALSE, lines = TRUE),
        scales = list(y=list(alternating=F)))
#' 
#' Switching panel and grouping variables
#' 
xyplot( income.fit3 ~ education | women, pred, groups = Type, 
        type = 'l',
        ylab = 'predicted income (Cdn $ in 1970)',
        layout = c(1,6),
        auto.key = list( space='right', 
                         points = FALSE, lines = TRUE),
        scales = list(y=list(alternating=F)))
#' Note the gray bar in the strips that shows the level of 'women' in each 
#' panel.  It's okay for exploration, but a lay viewer won't know what it
#' means. 
#' 
#' Also the colours are "strooped" (look up "Stroop effect").
#'
#'  
pred$Women <- with(pred, paste0(women, "% women"))
gd(col = c('blue','orange','dark green'), lty = 1, lwd = 2)
xyplot( income.fit3 ~ education | Women, pred, groups = Type, 
        type = 'l',
        ylab = 'predicted income (Cdn $ in 1970)',
        layout = c(1,6),
        auto.key = list( space='right', 
                         points = FALSE, lines = TRUE),
        scales = list(y=list(alternating=F)),
        as.table = TRUE)
#'
#' Ooops! The % labels are in ***lexicographical order***. We want them in
#' order corresponding to the numerical value of the percentage.
#'
pred$Women <- with(pred, reorder(Women, women)) 
      # reorders the levels of 'Women' according to the mean of 'women'
gd(col = c('blue','orange','dark green'), lty = 1, lwd = 3)
xyplot( income.fit3 ~ education | Women, pred, groups = Type, 
        type = 'l',
        ylab = 'predicted income (Cdn $ in 1970)',
        layout = c(1,6),
        auto.key = list( space='right', 
                         points = FALSE, lines = TRUE),
        scales = list(y=list(alternating=F)),
        as.table = TRUE)
#'
#' This presentation is not very effective because it is difficult to compare
#' levels vertically. But we have too many levels to show them side by side
#' vertically.
#'
xyplot( income.fit3 ~ education | Women, 
        subset(pred, Women %in% c("0% women",'40% women','80% women')), 
        groups = Type, 
        type = 'l',
        ylab = 'predicted income (Cdn $ in 1970)',
        layout = c(3,1),
        auto.key = list( space='top', 
                         points = FALSE, lines = TRUE),
        scales = list(y=list(alternating=F)),
        as.table = TRUE)





#'
#' # What is the 'value' of an additional year of education?
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