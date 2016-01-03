#' Life expectancy at birth: male, female and both
#' 
#' BEWARE: If running interactively, set current directory to Source File Location with
#' Menu: Session > Set Working Directory > To Source File Location
getwd()
library(yscs)
library(xlsx)
library(magrittr)
library(latticeExtra)

#'
#'  Life expectancies at birth: 
#'  male, female and both at 1990, 2000, 2012 and 2013 
#'   
zle <- read.csv("smoking-download/xmart.csv", skip = 1)
# Downloaded 2015-12-31 from WHO http://apps.who.int/gho/data/node.main.3?lang=en
# http://apps.who.int/gho/athena/data/xmart.csv?target=GHO/WHOSIS_000002,WHOSIS_000001,WHOSIS_000015&profile=crosstable&filter=COUNTRY:*;REGION:AFR;REGION:AMR;REGION:SEAR;REGION:EUR;REGION:EMR;REGION:WPR;SEX:*&x-sideaxis=COUNTRY;YEAR&x-topaxis=GHO;SEX
head(zle)
lapply(zle, class) # note that last 3 are factors 
dim(zle)
tab(zle, ~ Country + Year)
#' View(zle) 
#' note that last 3 columns have 'blanks' 
#' instead of NAs so values are turned to factor (R representation of categorical variables)
#'


#' 
#'   Life expectancy at birth
#'   
zle <- zle[, 1:5]
names(zle)[3:5] <- c('LE_both','LE_female','LE_male')
zle.long <- tolong(zle, timevar = 'sex')
head(zle.long)
lapply(zle.long, class)
xyplot(LE ~ Year | sex, zle.long, groups = Country, type = 'b') 
tab(zle.long, ~ Country) %>% c %>% Tab   # 194 countries
gd
xyplot(LE ~ Year | Country, zle.long, groups = sex, type = 'b',
       layout = c(10,10), auto.key = T) 




z <- read.xlsx2("smoking-download/indicator life_expectancy_at_birth.xlsx", sheetIndex = 1)
head(z)
dim(z)  # too many countries !!
names(z)
# View(z)
lapply(z, class)     # check class of variables: factors instead of numeric
#' Two things to fix:
#' 
#' 1. get rid of blank rows
#' 2. turn factors to numeric
#' 
z[,1] %>% as.character %>% nchar ==0 -> zeros
z <- subset(z, !zeros)
dim(z)   # better
z[[1]]
names(z)
names(z)[1] <- 'Country'
names(z)[1:10]

#' The next part we can do in many ways.
#' 
#' I will start by turning this into a 'long file' with just 3 variables:
#' Country, Year and LE
#' 
#' Use regex to change every variable name that starts with X to LE__Year
#' 
names(z) <- sub("^X","LE__", names(z))  # ^ means beginning of string (Note: idempotent transformation)
names(z)[1:10]
dl <- tolong(z, sep = "__")
dim(dl)
head(dl)
names(dl) <- sub('^time$', 'year', names(dl))
head(dl)

# have a look

xyplot(LE ~ year, dl, groups = Country, type = 'l')
gd(9)


trellis.focus()
panel.identify(labels = dl$Country)
trellis.unfocus()
z <- read.xlsx2('smoking-download/data.xlsx',1, startRow = 2)
z %>% head
names(z)[1] <- "Country"
head(z)
zz <- !(z$Country == '')
zz
zz <- cumsum(zz)
zz
z$Country2 <- zz
z$Country2
