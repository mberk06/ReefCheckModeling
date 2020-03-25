# Author: Michael Berk, James Johndrow
# Date: Spring 2020
# Purpose: develop macro time series forecasts using the Reef Check dataset

##################
# data setup
##################
df <- loadDF()

# pivot for days
cols <- c(subsetCols('organism'), 'DATE')
dfToPivot <- df[,cols]
pivotedDF <- pivot(d=df, grain='DATE', spread=cols)#, subsetCols('organism'), fun=sum(x))


##################
# modeling
##################