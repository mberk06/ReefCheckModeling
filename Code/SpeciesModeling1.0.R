# Author: Michael Berk, James Johndrow
# Date: Spring 2020
# Purpose: develop explanatory models of species counts using RC data

######################
# variable setup
######################
# get all y variables to be modeled
targetVars <- c('HC','SC','RK','NI','SNAPPER','LOBSTER','PARROTFISH','GROUPER.TOTAl',
                'BUTTERFLYFISH','PENCIL.URCHIN')

# get all x-variable subsets
anthro <- subsetCols('anthro')
organism <- subsetCols('organism')
both <- c(anthro, organism)

# setup data
originalDF <- loadDF()
miceDF <- loadDF(MICE=T)

######################
# ZIP
######################
# setup xs and y (only change these lines)
df <- originalDF
xs <- both # anthro, organism, both, names(miceDF)
y <- 'BUTTERFLYFISH'

# call model
out <- zipFunc(df, xs, y)
#true <- out[,1]
#yPreds <- out[,2]

# subset to non-zeros
#PREDS <- subset(data.frame(true = true, pred = yPreds), pred > 20 & pred < 45)
#cor(PREDS$true, PREDS$pred, use = 'complete.obs')

######################
# RF
######################
# setup xs and y (only change these lines)
df <- miceDF
xs <- names(miceDF) # anthro, organism, both, names(miceDF)
y <- 'HC'

# call model
out <- rfFunc(df, xs, y)
#true <- out[,1]
#yPreds <- out[,2]

