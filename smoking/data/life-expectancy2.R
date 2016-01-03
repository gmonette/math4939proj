#' Life expectancy at birth: male, female and both
#' 
#' BEWARE: If running interactively, set current directory to Source File Location 
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
lapply(zle, class)
dim(zle)
tab(zle, ~ Country + Year)
head(zle)
# Life expectancy at birth
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

dim(z)
z %>% lapply(class)

z[,1] %>% as.character %>% nchar
z <- subset( z, z[[1]] != '')
dim(z)
head(z)
names(z)
names(z)[1] <- 'Country'
names(z) <- sub("^X","LE__", names(z))
names(z)
dl <- tolong(z, sep = "__")
dim(dl)
head(dl)
lapply(dl, class)
zz <- as.numeric(as.character(dl$LE))
cbind(dl$LE,zz)
dl$LE <- zz
xyplot(LE ~ time, dl, groups = Country, type = 'l')
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
