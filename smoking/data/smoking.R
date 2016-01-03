###
###  smoking.R: read smoking.csv prepared in smoking_build.R
###
smoking <- read.csv('smoking.csv', row.names = 1)
smoking$id <- NULL
