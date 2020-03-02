
# idea of this script: see how well some simple glms do for predicting species abundance
# and coral composition as a function of anthro factors

#######################################
#######################################
# Setup variables
#######################################
#######################################
rm(list=ls(all=T))

# set wd and df based on user
if (dir.exists("/Users/michaelberk")) {
  setwd('~/Documents/Penn 2019-2020/Senior Thesis/Scripts/ReefCheckModeling/')
  df <- read.csv('Data/df1.0.csv')
  
} else {
  setwd('~/Dropbox/Projects/Reefs/')
  df <- read.csv('Data/df1.0.csv')
}


#######################################
#######################################
# Clean data
#######################################
#######################################

# clean anthro vars
anthro.vars <- c('Siltation', 'Dynamite Fishing?', 'Poison Fishing?', 'Aquarium fish collection', 'Harvest of inverts for food', 
                 'Harvest of inverts for curio', 'Tourist diving/snorkeling', 'Sewage pollution', 'Industrial pollution', 'Commercial fishing', 
                 'Live food fishing', 'Yachts', 'Level of other impacts?', 'Is protection enforced?', 'Level of poaching?', 'Spearfishing?', 
                 'Commercial fishing?', 'Recreational fishing?', 'Invertebrate/shell collection?', 'Anchoring?', 'Diving?')

anthro.vars <- toupper(gsub("/","\\.",gsub("\\?","\\.",gsub(" ","\\.",anthro.vars))))
anthro.vars <- ifelse(substr(anthro.vars,nchar(anthro.vars),nchar(anthro.vars))==".",substr(anthro.vars,1,nchar(anthro.vars)-1),anthro.vars)

# clean df names
names(df) <- ifelse(substr(names(df),nchar(names(df)),nchar(names(df)))==".",substr(names(df),1,nchar(names(df))-1),names(df))

names(df) <- toupper(names(df))

anthro.vars <- names(df)[names(df) %in% anthro.vars]

temp.vars <-  toupper(c("Water.temp.at.surface","Water.temp.at.5m","Water.temp.at.10m"))

#names(df)[names(df) %in% temp.vars]

df$GROUPER.TOTAL <- round(df$GROUPER.TOTAL)

table(df$GROUPER.TOTAL)
hist(df$GROUPER.TOTAL[df$GROUPER.TOTAL>0])

sapply(df[,names(df) %in% c(anthro.vars,temp.vars)],function(x){length(table(x))})
n.na <- sapply(df[,names(df) %in% c(anthro.vars,temp.vars)],function(x){sum(is.na(x))})
n.na

#Grouper.model <- paste("GROUPER.TOTAL~",paste(anthro.vars,collapse='+'),paste(temp.vars,collapse='+'),sep="+") 

#fit.pois <- glm(Grouper.model,data=df,family="poisson")

#######################################
#######################################
# Create clean MICE data
#######################################
#######################################
# load package
pacman::p_load(mice)

# remove duplicate column (COMMERCIAL.FISHING)
df = df[,-51]

# impute values
anthroDF <- df[,names(df) %in% c('GROUPER.TOTAL', 'WATER.TEMP.AT.SURFACE','DYNAMITE.FISHING','POISON.FISHING','AQUARIUM.FISH.COLLECTION',
               'HARVEST.OF.INVERTS.FOR.FOOD','HARVEST.OF.INVERTS.FOR.CURIO','TOURIST.DIVING.SNORKELING',
               'SEWAGE.POLLUTION','INDUSTRIAL.POLLUTION','COMMERCIAL.FISHING','LIVE.FOOD.FISHING+YACHTS')]
mice.anthro <- mice(anthroDF)

# replace data with mice imputed data
d <- ncol(anthroDF)
for (j in 1:d) {
  if (any(is.na(anthroDF[,j]))) {
    anthroDF[is.na(anthroDF[,j]),j] <- mice.anthro$imp[[j]][,1]
  }
}

# add y columns
responseVars <- c('HC','SC','RK','TRASH.GENERAL','GROUPER.TOTAL','SNAPPER','PENCIL.URCHIN','PARROTFISH',
                  'MORAY.EEL','LOBSTER','CORAL.DAMAGE.OTHER','BUTTERFLYFISH')
allAnthro <- cbind(anthroDF, df[responseVars])

# save data
save(allAnthro, file = "miceAnthro.RData")




#######################################
#######################################
# create train/test split
#######################################
#######################################
pacman::p_load(caTools)
sample = sample.split(df$GROUPER.TOTAL, SplitRatio = .75)
train = subset(df, sample == TRUE)
test  = subset(df, sample == FALSE)



#######################################
#######################################
# Poisson
#######################################
#######################################
pacman::p_load(mice)

# GROUPER TOTAL
fit.pois <- glm(GROUPER.TOTAL~WATER.TEMP.AT.SURFACE+DYNAMITE.FISHING+POISON.FISHING+AQUARIUM.FISH.COLLECTION+HARVEST.OF.INVERTS.FOR.FOOD+HARVEST.OF.INVERTS.FOR.CURIO+
TOURIST.DIVING.SNORKELING+SEWAGE.POLLUTION+INDUSTRIAL.POLLUTION+COMMERCIAL.FISHING+LIVE.FOOD.FISHING+YACHTS,
                data=train, subset=df$GROUPER.TOTAL>0,family='poisson')

summary(fit.pois)

# perform zero inflated pois
zip.fit <- zeroinfl(GROUPER.TOTAL ~ WATER.TEMP.AT.SURFACE+DYNAMITE.FISHING+POISON.FISHING+AQUARIUM.FISH.COLLECTION+HARVEST.OF.INVERTS.FOR.FOOD+HARVEST.OF.INVERTS.FOR.CURIO+
                      TOURIST.DIVING.SNORKELING+SEWAGE.POLLUTION+INDUSTRIAL.POLLUTION+COMMERCIAL.FISHING+LIVE.FOOD.FISHING+YACHTS, 
                      data = train)

summary(zip.fit)

############ Create train and test subsets ###########
# train
train.subset <- train[,names(train) %in% c('GROUPER.TOTAL', 'WATER.TEMP.AT.SURFACE','DYNAMITE.FISHING','POISON.FISHING','AQUARIUM.FISH.COLLECTION',
               'HARVEST.OF.INVERTS.FOR.FOOD','HARVEST.OF.INVERTS.FOR.CURIO','TOURIST.DIVING.SNORKELING',
               'SEWAGE.POLLUTION','INDUSTRIAL.POLLUTION','COMMERCIAL.FISHING','LIVE.FOOD.FISHING+YACHTS')]
train.subset <- train.subset[,-11]

impute.data <- mice(train.subset,m=5)

# replace data with mice imputed data
trainImputed <- train.subset
d <- ncol(train.subset)
for (j in 1:d) {
  if (any(is.na(trainImputed[,j]))) {
    trainImputed[is.na(trainImputed[,j]),j] <- impute.data$imp[[j]][,1]
  }
}

# test
test.subset <- test[,names(df) %in% c('GROUPER.TOTAL', 'WATER.TEMP.AT.SURFACE','DYNAMITE.FISHING','POISON.FISHING','AQUARIUM.FISH.COLLECTION',
               'HARVEST.OF.INVERTS.FOR.FOOD','HARVEST.OF.INVERTS.FOR.CURIO','TOURIST.DIVING.SNORKELING',
               'SEWAGE.POLLUTION','INDUSTRIAL.POLLUTION','COMMERCIAL.FISHING','LIVE.FOOD.FISHING+YACHTS')]
test.subset <- test.subset[,-11]

impute.data <- mice(test.subset,m=5)

# replace data with mice imputed data
testImputed <- test.subset
d <- ncol(test.subset)
for (j in 1:d) {
  if (any(is.na(testImputed[,j]))) {
    testImputed[is.na(testImputed[,j]),j] <- impute.data$imp[[j]][,1]
  }
}

############# Model with imputed vals
# zero inflated
zip.fit.imp1 <- zeroinfl(GROUPER.TOTAL ~ WATER.TEMP.AT.SURFACE+DYNAMITE.FISHING+POISON.FISHING+AQUARIUM.FISH.COLLECTION+HARVEST.OF.INVERTS.FOR.FOOD+HARVEST.OF.INVERTS.FOR.CURIO+
                      TOURIST.DIVING.SNORKELING+SEWAGE.POLLUTION+INDUSTRIAL.POLLUTION+COMMERCIAL.FISHING, 
                    data = trainImputed)

# test (check if can predict with NA - else, impute testing data also)
test = subset(testImputed, DYNAMITE.FISHING != '1.0')
pred <- predict(zip.fit.imp1, test)
cor(pred, test$GROUPER.TOTAL) # 0.049

summary(pred)
summary(test$GROUPER.TOTAL)

#############
###############
###############
##############
##############
# RKC
fit.pois <- glm(RK~WATER.TEMP.AT.SURFACE+DYNAMITE.FISHING+POISON.FISHING+AQUARIUM.FISH.COLLECTION+HARVEST.OF.INVERTS.FOR.FOOD+HARVEST.OF.INVERTS.FOR.CURIO+
                  TOURIST.DIVING.SNORKELING+SEWAGE.POLLUTION+INDUSTRIAL.POLLUTION+COMMERCIAL.FISHING+LIVE.FOOD.FISHING+YACHTS,
                data=train, subset=df$RK>0,family='poisson')

summary(fit.pois)

# perform zero inflated pois
zip.fit <- zeroinfl(RK ~ WATER.TEMP.AT.SURFACE+DYNAMITE.FISHING+POISON.FISHING+AQUARIUM.FISH.COLLECTION+HARVEST.OF.INVERTS.FOR.FOOD+HARVEST.OF.INVERTS.FOR.CURIO+
                      TOURIST.DIVING.SNORKELING+SEWAGE.POLLUTION+INDUSTRIAL.POLLUTION+COMMERCIAL.FISHING+LIVE.FOOD.FISHING+YACHTS, 
                    data = train)

summary(zip.fit)

############ Create train and test subsets ###########
# train
train.subset <- train[,names(df) %in% c('RK', 'WATER.TEMP.AT.SURFACE','DYNAMITE.FISHING','POISON.FISHING','AQUARIUM.FISH.COLLECTION',
                                        'HARVEST.OF.INVERTS.FOR.FOOD','HARVEST.OF.INVERTS.FOR.CURIO','TOURIST.DIVING.SNORKELING',
                                        'SEWAGE.POLLUTION','INDUSTRIAL.POLLUTION','COMMERCIAL.FISHING','LIVE.FOOD.FISHING+YACHTS')]
train.subset <- train.subset[,-11]

impute.data <- mice(train.subset,m=5)

# replace data with mice imputed data
trainImputed <- train.subset
d <- ncol(train.subset)
for (j in 1:d) {
  if (any(is.na(trainImputed[,j]))) {
    trainImputed[is.na(trainImputed[,j]),j] <- impute.data$imp[[j]][,1]
  }
}

# test
test.subset <- test[,names(df) %in% c('RK', 'WATER.TEMP.AT.SURFACE','DYNAMITE.FISHING','POISON.FISHING','AQUARIUM.FISH.COLLECTION',
                                      'HARVEST.OF.INVERTS.FOR.FOOD','HARVEST.OF.INVERTS.FOR.CURIO','TOURIST.DIVING.SNORKELING',
                                      'SEWAGE.POLLUTION','INDUSTRIAL.POLLUTION','COMMERCIAL.FISHING','LIVE.FOOD.FISHING+YACHTS')]
test.subset <- test.subset[,-11]

impute.data <- mice(test.subset,m=5)

# replace data with mice imputed data
testImputed <- test.subset
d <- ncol(test.subset)
for (j in 1:d) {
  if (any(is.na(testImputed[,j]))) {
    testImputed[is.na(testImputed[,j]),j] <- impute.data$imp[[j]][,1]
  }
}

############# Model with imputed vals
# zero inflated
zip.fit.imp1 <- zeroinfl(RK ~ WATER.TEMP.AT.SURFACE+DYNAMITE.FISHING+POISON.FISHING+AQUARIUM.FISH.COLLECTION+HARVEST.OF.INVERTS.FOR.FOOD+HARVEST.OF.INVERTS.FOR.CURIO+
                           TOURIST.DIVING.SNORKELING+SEWAGE.POLLUTION+INDUSTRIAL.POLLUTION+COMMERCIAL.FISHING, 
                         data = trainImputed)

# test (check if can predict with NA - else, impute testing data also)
test = subset(testImputed, DYNAMITE.FISHING != '1.0')
pred <- predict(zip.fit.imp1, test)
cor(pred, test$RK) 

summary(pred)
summary(test$RK)


##################################
##################################
# RF
##################################
##################################
# load packages
pacman::p_load(randomForest)
pacman::p_load(caTools)
pacman::p_load(party)

# clean y var
noNAGrouperDF <- subset(df, !is.na(df$GROUPER.TOTAL)) # drop na in y var

# train/test split
sample = sample.split(noNAGrouperDF$GROUPER.TOTAL, SplitRatio = .75)
train = subset(noNAGrouperDF, sample == TRUE)
test  = subset(noNAGrouperDF, sample == FALSE)

# NOTE - SILTATION IS ALL NA
# create formula
anthroString = paste(anthro.vars[2:length(anthro.vars)], collapse=' + ')
formula = as.formula(paste("GROUPER.TOTAL ~ ",anthroString, sep = ""))

# model (rough NA imputation)
rfRough <- randomForest(
  formula,
  data=train,
  na.action=na.roughfix
)

rfRough # var explained: 12% (NA in y removed)
summary(rfRough) 

# get pred and accuracy (CANNOT PRED BECAUSE OF NA)
#pred = predict(rfRough, newdata=subset(test,select=-c(GROUPER.TOTAL)))
#cor(pred, test$GROUPER.TOTAL)

# get output tree
getTree(rfRough, 1, labelVar=TRUE)

# plot output
#visRF <- cforest(formula, data = train)
#plot(rfRough, type="simple")

# model (RF NA imputation)
#imputeDFTrain <- rfImpute(formula, noNAGrouperDF)
#imputeDFTest <- rfImpute(formula, noNAGrouperDF)

rfImpute <- randomForest(
  formula,
  data=imputeDFTrain
)

rfImpute # 9.5% variance explained
summary(rfImpute)

# get pred
pred = predict(rfImpute, newdata=subset(imputeDFTest,select=-c(GROUPER.TOTAL)))
cor(pred, imputeDFTest$GROUPER.TOTAL) # 0.522
#getTree(rfImpute, 1, labelVar=TRUE)





