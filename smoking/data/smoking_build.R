#'
#' Smoking and life expectancies
#' 
#' 

getwd()   # where are we? Session > Set Working Directory > To Source File Location
library(yscs)  # devtools::install_github('gmonette/yscs') # SCS tools
library(xlsx) # install.packages('xlsx') # read xls[x] files
library(magrittr) # command pipelines
library(latticeExtra) # graphics

#'
#' Using Gapminder data (self described in Excel file)
#'

smoke.file <- "smoking-download/indicator life_expectancy_at_birth.xlsx"
file.mtime(smoke.file)
z <- read.xlsx2("smoking-download/indicator life_expectancy_at_birth.xlsx", sheetIndex = 1)
head(z)
dim(z)  # too many countries
names(z)  # X1800, X1801, ... contain LEs
names(z)[1:10]
names(z)[1] <- "Country"  # replacement function -- special to R
# View(z)
#' Sometimes get blank columns and rows if there's a space somewhere in an Excel file (invisible in Excel but R sees it as data
#' 
#' Get rid of empty rows
#' 
ch <- function(x) as.character(x)  # if you're a slow typist
nchar(ch(z$Country)) # z$Country fully specified name 
#' Equivalent: z$Country, z[[1]], z[['Country']]
nchar(ch(z$Country)) 
nchar(ch(z$Country)) %>%  tab  # example of a pipeline - great in interactive programming, same as:
tab(nchar(ch(z$Country)))

z <- subset(z, nchar(ch(Country)) > 0) # in the subset function, names are evaluated in data.frame [idempotent]

dim(z) # solving the extra row problem

#' LEs are not numeric!! We should change them to numeric
#' What are they? ALWAYS DO THIS CHECK:
lapply(z, class)  # apply the same command, 'class', to each element of a list
#' All 'factors', R's way of representing categorical data for modelling
#' 
#' Want to convert all these variables to numeric. How?
#'

for( i in 2:ncol(z)) z[[i]] <- as.numeric(as.character(z[[i]]))

#' Why can't I use as.numeric(z[[i]])? Exam question!!
#' 
#' If there were ','s in the numbers we'd have to get rid of them
#' Could create a function
#' 
tonum <- function(x) {
  x <- as.character(x)
  x <- gsub(",","",x)
  x <- gsub("\\$","",x) # need to escape "$" because it has a special meaning
  as.numeric(x)
}
#' 
#' Example
#' 
zz <- c('$12,234.21','$1,2.45,6','123.45.67','not a number','NaN','Inf', NA) 
tonum(zz)
data.frame(zz, tonum(zz))
data.frame(zz, tonum(zz)) %>% lapply(class)
#'
#' Could have used:
#'
for(i in 2:ncol(z)) z[[i]] <- tonum(z[[i]])
#'
#' Note that 'tonum' is idempotent ... which means???
#' 
#' Why is it good to try to use idempotent transformations in programming?
#' 
#' 

sapply(z, class) %>% tab  # example of a pipeline
tab(sapply(z, class)) # same as this but nicer in the heat of coding
#' Note: sapply returns a vector when it can, lapply returns a list
#' 
#' ## Wide form to long form
#' 
#' The next part we can do in many ways.
#' 
#' I will start by turning this into a 'long file' with just 3 variables:
#' Country, year and LE (and a fourth 'id')
#' 
#' Use regex to change every variable name 
#' that starts with X to LE__Year
#' 
#' Values for any variable with '__' in its name are turned to
#' a 'time-varying variable.
#'  
names(z) <- sub("^X","LE__", names(z))  # ^ means beginning of string (Note: idempotent transformation)
names(z)[1:10]
zl <- tolong(z, sep = "__")
head(zl)
xqplot(zl)           # classroom photo: uniform quantiles
xqplot(zl, ptype = 'normal')  # normal quantiles
#'
#' Some questions:
#' 
#' What do you think about missing values? 
#' How could we deal with them?
#' 
names(zl) <- sub('^time$','year',names(zl))
#' Most people would do: names(zl)[2] <- 'year'
rownames(zl) <- with(zl, paste(Country,year)) 
Tab(zl, ~ Country+year) %>% c %>% tab # each combination unique
lapply(zl, class)     # check class of variables. OK
# have a look:
# windows()
xyplot(LE ~ year, zl, groups = Country, type = 'l')
gd(9)  # uses ggplot theme
xyplot(LE ~ year, zl, groups = Country, type = 'l')
trellis.focus()
panel.identify(labels = zl$Country)
trellis.unfocus()
life_expectancy <- zl


#'
#' ## Smoking
#'
#' ### Prevalence among males
#'

zmale <- read.xlsx2("smoking-download/indicator_prevalence of current tobacco use among adults (%) male.xlsx",1)
head(zmale)
# View(z.tob.male)
xqplot(zmale)  # quick look at data if not too many variables
zmale <- zmale[, 1:3]
names(zmale)[1] <- 'Country'
names(zmale) <- sub("^X", "smoking_male__", names(zmale))
head(zmale)
lapply(zmale, class)
data.frame(zmale[[2]], tonum(zmale[[2]]))
data.frame(zmale[[3]], tonum(zmale[[3]]))

for ( i in 2:3) zmale[[i]] <- tonum(zmale[[i]])
xqplot(zmale)
#'
#' ### Prevalence among females
#'

zfemale <- read.xlsx2("smoking-download/indicator_prevalence of current tobacco use among adults (%) female.xlsx",1)
dim(zfemale)
head(zfemale)   # more years !!
# View(zfemale)
xqplot(zfemale) 
names(zfemale)[1] <- 'Country'
zfemale <- zfemale[,1:4]
names(zfemale) <- sub("^X", "smoking_female__", names(zfemale))
head(zfemale)
lapply(zfemale, class)
for(i in 2:4) zfemale[[i]] <- tonum(zfemale[[i]])
xqplot(zfemale)

head(zfemale)
head(zmale)
part(zfemale$Country, zmale$Country) # note ''
zsmoke <- merge(zfemale, zmale, by = "Country") # note ''
dim(zsmoke)
head(zsmoke)
# get rid of the '' country
zsmoke <- subset(zsmoke, Country != '')
dim(zsmoke)
z <- tolong(zsmoke, sep = '__')
head(z)
names(z) <- sub('^time$', 'year', names(z))
head(z)
z <- tolong(z, sep = '_', timevar = 'sex')
# warning is OK
head(z)
xyplot(smoking ~ year | sex, z, groups = Country, type = 'b')
trellis.focus()
panel.identify(labels=z$Country)
trellis.unfocus()

sortdf(z, ~ smoking)
subset(z, Country == "Nauru")
subset(z, Country == "Russia")
sortdf(z, ~ Country)
#'
#' There is no variability by year !!!
#' 
#' This just seems to be smoking in the early 2000's
#' 
#' We'll take one that has data for both men and women
#' 
z <- subset(z, year == 2005)
#'
#' Form wide file with gender (we could have done this earlier)
#'
smoking_prevalence <- towide(z, 'Country', 'sex')
rownames(smoking_prevalence) <- with(smoking_prevalence, Country)
dim(smoking_prevalence)
names(smoking_prevalence)
smoking_prevalence <- smoking_prevalence[,grep('^id$|^year$',
                              names(smoking_prevalence), invert = TRUE)]

#'
#' ## Combine Smoking and LE
#'
life_expectancy %>% head
smoking_prevalence %>% head
part(life_expectancy$Country, smoking_prevalence$Country)

#'
#' Good approach: convert country names to 3-letter country codes ISO 3166-1
#' 
#' e.g: http://unstats.un.org/unsd/methods/m49/m49alpha.htm
#' 
#' We will just use the 180 countries that spelled consistently in both
#' files

smoking <- merge( subset(life_expectancy, year == 2005), smoking_prevalence, by = 'Country')
dim(smoking)  # kept only Countrys in both files
head(smoking)
rownames(smoking) <- smoking$Country
fit <- lm(LE ~ smoking_female + smoking_male, smoking)
summary(fit)
library(p3d)
Init3d()
Plot3d(LE ~ smoking_female + smoking_male, smoking)
Fit3d(fit)
Id3d()
fit2 <- lm(LE ~ smoking_female + I(smoking_female^2) + smoking_male + I(smoking_male^2)
           +I(smoking_male * smoking_female), smoking)
summary(fit2)
Fit3d(fit2, col = 'red')
wald(fit2, "2)")
wald(fit2, "female")
wald(fit2, "_male")

fit3 <- lm(LE ~ smoking_female + smoking_male
           +I(smoking_male * smoking_female), smoking)
Fit3d(fit3, col = 'green')
write.csv(smoking, file = 'smoking.csv')
