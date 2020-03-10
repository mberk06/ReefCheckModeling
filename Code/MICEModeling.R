# Develop anthropogentic models that explain and forecast health indicators using a MICED dataset

#########################
#########################
# Load and setup df (MICEd)
#########################
#########################
# read in RData
load("Data/miceAnthro.RData") 
df <- allAnthro
summary(df)

# determine response variable and convert to ints
names(df)
responseVar <- 'LOBSTER'
df[,responseVar] <- round(df[,responseVar])

# drop NA in y var
df <- subset(df, !is.na(df[,responseVar])) # drop na in y var

# create formula 
anthroVars <- c("WATER.TEMP.AT.SURFACE","DYNAMITE.FISHING","POISON.FISHING","AQUARIUM.FISH.COLLECTION","HARVEST.OF.INVERTS.FOR.FOOD",
                           "HARVEST.OF.INVERTS.FOR.CURIO","TOURIST.DIVING.SNORKELING","SEWAGE.POLLUTION",
                           "INDUSTRIAL.POLLUTION","COMMERCIAL.FISHING") 

xs <- paste(anthroVars, collapse="+")
formula <- formula(paste(c(responseVar, xs), collapse='~'))

# create train and test split
pacman::p_load(caTools)
sample = sample.split(df[,responseVar], SplitRatio = .75)
train = subset(df, sample == TRUE)
test  = subset(df, sample == FALSE)

#########################
#########################
# Poisson Reg
#########################
#########################
# fit glm poisson 
pois.fit <- glm(formula, data=train[train$AQUARIUM.FISH.COLLECTION!="",],family='poisson')
summary(pois.fit)

# get preds
poisPred <- predict(pois.fit, newdata=test)
cor(poisPred, test[,responseVar], use = "complete.obs") # 0.057


# perform zero inflated pois (NOT WORKING DUE TO COLLINEARITY OF VARIABLES - col 4 and col 1-3)
zip.fit <- zeroinfl(formula, data = train, dist = 'poisson')
summary(zip.fit)

# get preds
poisPred <- predict(zip.fit, newdata=test)
cor(poisPred, test[,responseVar], use = "complete.obs")

#########################
#########################
# RF
#########################
#########################
pacman::p_load(randomForest)

# model (rough NA imputation)
rfRough <- randomForest(
  formula,
  data=train
)

# evaluate
rfRough 

# get cor of preds 
pred <- predict(rfRough, newdata = test)
cor(pred, test[,responseVar], use = "complete.obs") #0.14











