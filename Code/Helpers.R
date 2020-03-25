# Author: Michael Berk, James Johndrow
# Date: Spring 2020
# Purpose: provide modeling, subsetting, and cleaning helper functions for 
#          developing explanatory models with Reef Check data

##########################
##########################
# libraries
##########################
##########################
pacman::p_load(caTools) # train/test split
pacman::p_load(pscl) # ZIP
pacman::p_load(randomForest) # RF
pacman::p_load(stringr) # str_replace
pacman::p_load(pdp) # partial dependence plots
pacman::p_load(vip) # variable importance plots
pacman::p_load(car) # fancy scatter
pacman::p_load(SparkR) # pivot

##########################
##########################
# ZIP
##########################
##########################
zipFunc <- function(df, xs, y) {
  #' perform zero-inflated poisson regression, print model summary, and return preds
  #'
  #' @param df full dataframe with all columns (dataframe)
  #' @param xs columns to make into formula (vector character)
  #' @param y response variable (character)
  #' @return preds

  # round response variable and remove
  df[,y] <- as.integer(round(df[,y])) # convert all counts to ints
  df <- subset(df, !is.na(df[,y])) # drop na in y var
  xs <- xs[!xs %in% c(y)] # remove var from formula
  
  # create formula
  xs <- paste(xs, collapse="+")
  formula <- formula(paste(c(y, xs), collapse='~'))
  print(formula)
  
  # create train and test split
  sample = sample.split(df[,y], SplitRatio = .75)
  train = subset(df, sample == TRUE)
  test  = subset(df, sample == FALSE)
  
  # run model
  fit <- zeroinfl(formula, data = train, dist = 'poisson')
  print(summary(fit))
  
  # get correlation
  pred <- round(predict(fit, newdata=test, se.fit=TRUE,MC=2500))
  rsquare <- cor(pred, test[,y], use = "complete.obs")
  corStr = paste0(c('cor',round(rsquare, 3)), collapse = ': ')
  print(corStr)
  
  # plot output 
  plot(pred, test[,y], main=paste(c(y,'predicted against true')), xlab='yhat', ylab='y')
  abline(lm(test[,y] ~ pred), col="red") # regression line (y~x)
  text(x = 8, y = max(test[,y])-10, corStr, col = 'red', cex=0.8)#labels=paste0(c('r^2: ',rsquare)))
  qqplot(pred, test[,y])
  
  return (data.frame(y = test[,y], yPred = pred))
}

##########################
##########################
# RF
##########################
##########################
rfFunc <- function(df, xs, y) {
  #' perform random forest, print model summary, and return preds
  #'
  #' @param df full dataframe with all columns (dataframe)
  #' @param xs columns to make into formula (vector character)
  #' @param y response variable (character)
  #' @return preds

  # round response variable and remove
  if (y %in% c(subsetCols('organism'))) {
    df[,y] <- as.integer(round(df[,y])) # round all counts
  }
  df <- subset(df, !is.na(df[,y])) # drop na in y var
  xs <- xs[!xs %in% c(y)] # remove var from formula
  
  # create formula
  xs <- paste(xs, collapse="+")
  formula <- formula(paste(c(y, xs), collapse='~'))
  print(formula)
  
  # create train and test split
  sample = sample.split(df[,y], SplitRatio = .75)
  train = subset(df, sample == TRUE)
  test  = subset(df, sample == FALSE)
  
  # run model
  fit <- randomForest(formula, data = train, na.action=na.roughfix)
  print(fit)
  
  # get correlation
  pred <- round(predict(fit, newdata=test, se.fit=TRUE,MC=2500))
  rsquare <- cor(pred, test[,y], use = "complete.obs")
  corStr = paste0(c('cor',round(rsquare, 3)), collapse = ': ')
  print(corStr)
  
  # plot output 
  plot(pred, test[,y], main=paste(c(y,'predicted against true')), xlab='yhat', ylab='y')
  abline(lm(test[,y] ~ pred), col="red") # regression line (y~x)
  text(x = 8, y = max(test[,y])-10, corStr, col = 'red', cex=0.8)#labels=paste0(c('r^2: ',rsquare)))
  qqplot(pred, test[,y])
  
  # variable importance and PDP
  varImpPlot(fit)
  xs <- unlist(strsplit(xs, "[+]"))
  for (i in 1:length(xs)) {
    partialPlot(fit, train[!is.na(train[,xs[i]]),], xs[i], 
                xlab = xs[i], ylab = y,
                main = paste0(c("Partial Dependence on ", y, " vs. ", xs[i])))
  }
  
  return (data.frame(y = test[,y], yPred = pred))
}

##########################
##########################
# load data
##########################
##########################
loadDF <- function(MICE=FALSE) {
  #' load data and return
  
  # parse user
  if (dir.exists("/Users/michaelberk")) {
    setwd('~/Documents/Penn 2019-2020/Senior Thesis/Scripts/ReefCheckModeling/')
  } else {
    setwd('~/Dropbox/Projects/Reefs/')
  }
  
  # return MICEd df or non-MICED
  if (MICE) {
    load("Data/miceAnthro.RData") 
    df <- allAnthro
  } else {
    df <- read.csv('Data/df1.0.csv')
  }
  
  # clean df names
  names(df) <- ifelse(substr(names(df),nchar(names(df)),nchar(names(df)))==".",substr(names(df),1,nchar(names(df))-1),names(df))
  names(df) <- toupper(names(df))
  names(df) <- str_replace(names(df), "%", "PER")
  names(df) <- str_replace(names(df), "[(]", "")
  names(df) <- str_replace(names(df), "[)]", "")
  names(df) <- str_replace(names(df), "[-]", ".")
  names(df) <- str_replace(names(df), "[>]", "")
  names(df) <- str_replace(names(df), "[<]", "")
  if (!MICE) {df <- df[!(df['SILTATION']=='0.0' & !is.na(df['SILTATION'])),]} # remove single problem row
  
  return(df)
}

##########################
##########################
# get column name subsets
##########################
##########################
subsetCols <- function(subsetType) {
  #' return column names for specific subset
  #' 
  #'  @param subsetType key of list that is used to return col name subset (character)
  #'  @return vector of column names (character)
  
  subsets <- list()
  subsets[['substrate']] <- c("FS","HC","NI","OT","RB","RC","RK","SC","SD","SI","SP")
  subsets[['anthroAll']] <- c('Siltation', 'Dynamite Fishing?', 'Poison Fishing?', 'Aquarium fish collection', 'Harvest of inverts for food', 
                          'Harvest of inverts for curio', 'Tourist diving/snorkeling', 'Sewage pollution', 'Industrial pollution', 'Commercial fishing', 
                          'Live food fishing', 'Yachts', 'Level of other impacts?', 'Is protection enforced?', 'Level of poaching?', 'Spearfishing?', 
                          'Commercial fishing?', 'Recreational fishing?', 'Invertebrate/shell collection?', 'Anchoring?', 'Diving?')
  subsets[['anthro']] <- c("WATER.TEMP.AT.SURFACE","DYNAMITE.FISHING","POISON.FISHING","AQUARIUM.FISH.COLLECTION","HARVEST.OF.INVERTS.FOR.FOOD",
                            "HARVEST.OF.INVERTS.FOR.CURIO","TOURIST.DIVING.SNORKELING","SEWAGE.POLLUTION",
                            "INDUSTRIAL.POLLUTION","COMMERCIAL.FISHING",
                            'SILTATION','RECREATIONAL.FISHING','INVERTEBRATE.SHELL.COLLECTION', 'ANCHORING?', 
                            'DIVING?',"TRASH.FISH.NETS","TRASH.GENERAL","CORAL.DAMAGE.ANCHOR","CORAL.DAMAGE.DYNAMITE",
                            "CORAL.DAMAGE.OTHER") 
  subsets[['organismAll']] <- c('ARABIAN.BUTTERFLYFISH','ASPERGILLOSIS','BANDED.CORAL.SHRIMP','BARRACUDA',
                              'BARRAMUNDI.COD','BLACK.BAND','BLACK.BAND','BLACK.SPOTTED.GRUNT',
                              'BLACK.URCHIN','BLEACHING.PER.OF.COLONY','BLEACHING.PER.OF.POPULATION',
                              'BLUE.SEA.STAR','BLUELINE.SNAPPER','BROOMTAIL.WRASSE',
                              'BUMPHEAD.PARROT','BUTTERFLYFISH','COTS',
                              'COWRIES','DARK.BUTTERFLYFISH','DARK.BUTTERFLYFISH','DIADEMA',
                              'EDIBLE.SEA.CUCUMBER','FLAMINGO.TONGUE','GIANT.CLAM.10-20.CM',
                              'GIANT.CLAM.20-30.CM','GIANT.CLAM.30-40.CM','GIANT.CLAM.40-50.CM',
                              'GIANT.CLAM.<10.CM','GIANT.CLAM.>50.CM','GIANT.CLAM.TOTAL',
                              'GIANT.HAWKFISH','GOATFISH','GORGONIAN','GREY.GRUNT','GROUPER.30-40.CM',
                              'GROUPER.40-50.CM','GROUPER.50-60.CM','GROUPER.>60.CM','GROUPER.TOTAL',
                              'GRUNTS','HAEMULIDAE','HELMET.CONCH','HUMPHEAD.WRASSE',
                              'JACKS','KING.ANGELFISH','LIONFISH','LOBSTER','LONGFIN.BANNERFISH',
                              'MANTAS','MEXICAN.HOGFISH','MORAY.EEL','NASSAU.GROUPER.30-40.CM',
                              'NASSAU.GROUPER.40-50.CM','NASSAU.GROUPER.50-60.CM',
                              'NASSAU.GROUPER.>60.CM','NASSAU.GROUPER.TOTAL','NASSAU.GROUPER.30-40.CM',
                              'NASSAU.GROUPER.40-50.CM','NASSAU.GROUPER.50-60.CM',
                              'NASSAU.GROUPER.>60.CM','NASSAU.GROUPER.TOTAL','ORANGE.SPINE.UNICORNFISH',
                              'ORANGE.SPOTTED.GROUPER.30-40.CM','ORANGE.SPOTTED.GROUPER.40-50.CM',
                              'ORANGE.SPOTTED.GROUPER.50-60.CM','ORANGE.SPOTTED.GROUPER.>60.CM',
                              'ORANGE.SPOTTED.GROUPER.TOTAL','PARROTFISH',
                              'PEACOCK.GROUPER.30-40.CM','PEACOCK.GROUPER.40-50.CM',
                              'PEACOCK.GROUPER.50-60.CM','PEACOCK.GROUPER.>60.CM',
                              'PEACOCK.GROUPER.TOTAL','PENCIL.URCHIN','QUEEN.CONCH','SEA.FAN','SHARKS',
                              'SHORT.SPINE.URCHIN','SLATE.PENCIL.URCHIN','SNAPPER',
                              'SPIDER.CRAB','SPOTTED.GRUNT','TRIPNEUSTES','TRITON','TROCHUS','TURTLES',
                              'WHITE.BAND','WHITE.PLAGUE','WHITE.BAND','YELLOW.GOATFISH','YELLOW.TANG',
                              'YELLOWBAR.ANGELFISH','YELLOWTAIL.TANG')
  subsets[['organism']] <- c('SNAPPER','TRIPNEUSTES','TRITON','PENCIL.URCHIN','PARROTFISH','MORAY.EEL','LOBSTER','HUMPHEAD.WRASSE','GROUPER.TOTAL',
                             'DIADEMA','BUTTERFLYFISH','BUMPHEAD.PARROT','BARRAMUNDI.COD','BANDED.CORAL.SHRIMP','BLEACHING....OF.COLONY',
                             'BLEACHING....OF.POPULATION') 
  subsets[['percentile']] <- unlist(lapply(names(df), function(x) x[grepl("PERCENTILE", x)]))

  # return if in susbets
  if (subsetType %in% names(subsets)) {
    subsets[[subsetType]] <- toupper(gsub("/","\\.",gsub("\\?","\\.",gsub(" ","\\.",subsets[[subsetType]]))))
    subsets[[subsetType]] <- ifelse(substr(subsets[[subsetType]],nchar(subsets[[subsetType]]),nchar(subsets[[subsetType]]))==".",substr(subsets[[subsetType]],1,nchar(subsets[[subsetType]])-1),subsets[[subsetType]])
    subsets[[subsetType]] <- str_replace(subsets[[subsetType]], "[-]", ".")
    subsets[[subsetType]] <- str_replace(subsets[[subsetType]], "[>]", "")
    subsets[[subsetType]] <- str_replace(subsets[[subsetType]], "[<]", "")
    return(subsets[[subsetType]]) 
  } else {
    print("Incorrect subset key.")
    return(NA)
  }
}
