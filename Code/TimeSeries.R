# Author: Michael Berk, James Johndrow
# Date: Spring 2020
# Purpose: develop macro time series forecasts using the Reef Check dataset

##################
# data setup
##################
# setup data
originalDF <- loadDF()
miceDF <- loadDF(MICE=T)

# subset to caribbean
caribbeanCountries <- c('USA-FL','BARBADOS','MEXICO','COLOMBIA','DOMIHCCA','BELIZE','HONDURAS','ST KITTS & NEVIS',
                        'BVI','CAYMAN ISLANDS','JAMAICA','TRIHCDAD & TOBAGO','ST LUCIA','ST VINCENT & GRENADINES','GRENADA',
                        'ANTIGUA','PUERTO RICO','USVI','ANGUILLA','BARBUDA','VENEZUELA','DOMIHCCAN REPUBLIC','ARUBA','TUHCS & CAICOS',
                        'PANAMA','CUBA','BAHAMAS','HAITI','COSTA RICA','GUATEMALA')
dfCaribbean <- subset(df, df$COUNTRY %in% caribbeanCountries & df$OCEAN == 'ATLANTIC')
df <- dfCaribbean

######################
# variable setup
######################
# get all y variables to be modeled
targetVars <- c('HC','SC','RK','NI','SNAPPER','LOBSTER','PARROTFISH','GROUPER.TOTAl',
                'BUTTERFLYFISH','PENCIL.URCHIN')

######################
# ZIP
######################
# setup xs and y (only change these lines)
df <- originalDF
y <- 'BUTTERFLYFISH'

# call model and print output
out <- timeSeries(df, y, lm=F, aa=T)


