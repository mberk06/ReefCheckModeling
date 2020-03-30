# Author: Michael Berk, James Johndrow
# Date: Spring 2020
# Purpose: develop explanatory models of species counts using RC data

#####################
# get Helpers.R
#####################
# set wd and df based on user
if (dir.exists("/Users/michaelberk")) {
  setwd('~/Documents/Penn 2019-2020/Senior Thesis/Scripts/ReefCheckModeling/')
} else {
  setwd('~/Dropbox/Projects/Reefs/')
}

# run helpers 
source("Code/Helpers.R")

# run MICE (only run once)
# createMICE()

######################
# variable setup
######################
# get all y variables to be modeled
targetVars <- c('HC','SC','RK','NI','SNAPPER','LOBSTER','PARROTFISH','GROUPER.TOTAL',
                'BUTTERFLYFISH','PENCIL.URCHIN')
# setup data
#dfo <- loadDF()
#dfm <- loadDF(MICE=T)

# get all x-variable subsets
o <- c(subsetCols('organism'), "WATER.TEMP.AT.SURFACE")  # organism
a <- c(subsetCols('anthro'), "WATER.TEMP.AT.SURFACE") # anthropogenic
b <- c(a, o)[1:(length(c(a,o))-1)] # both

######################
# Pois/ZIP
######################
# setup xs and y (only change these lines)
df <- dfm 
xs <- a
#xs <- b[b != "AQUARIUM.FISH.COLLECTION"] # o, a, b
y <- 'NI'
df[,y] <- df[,y]*160 # perform for substrate (dividing by 160 produces strange results)

# call model
#out <- zipFunc(df, xs, y, printAll=F)
out <- poisFunc(df, xs, y, printAll=F)

######################
# RF
######################
# call model
out <- rfFunc(df, xs, y, PDP=F, printAll=F)

#extra code
#true <- out[,1]
#yPreds <- out[,2]




