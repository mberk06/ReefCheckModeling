# Develop models of reef health based on species present

#######################################
#######################################
# Setup variables
#######################################
#######################################
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
# Clean data and create carribean dataset
#######################################
#######################################

# clean anthro vars
organisms <- c('ARABIAN BUTTERFLYFISH','ASPERGILLOSIS','BANDED CORAL SHRIMP','BARRACUDA',
              'BARRAMUNDI COD','BLACK BAND','BLACK BAND','BLACK SPOTTED GRUNT',
              'BLACK URCHIN','BLEACHING (% OF COLONY)','BLEACHING (% OF POPULATION)',
              'BLUE SEA STAR','BLUELINE SNAPPER','BROOMTAIL WRASSE',
              'BUMPHEAD PARROT','BUTTERFLYFISH','COTS',
              'COWRIES','DARK BUTTERFLYFISH','DARK BUTTERFLYFISH','DIADEMA',
              'EDIBLE SEA CUCUMBER','FLAMINGO TONGUE','GIANT CLAM 10-20 CM',
              'GIANT CLAM 20-30 CM','GIANT CLAM 30-40 CM','GIANT CLAM 40-50 CM',
              'GIANT CLAM <10 CM','GIANT CLAM >50 CM','GIANT CLAM TOTAL',
              'GIANT HAWKFISH','GOATFISH','GORGONIAN','GREY GRUNT','GROUPER 30-40 CM',
              'GROUPER 40-50 CM','GROUPER 50-60 CM','GROUPER >60 CM','GROUPER TOTAL',
              'GRUNTS','HAEMULIDAE','HELMET CONCH','HUMPHEAD WRASSE',
              'JACKS','KING ANGELFISH','LIONFISH','LOBSTER','LONGFIN BANNERFISH',
              'MANTAS','MEXICAN HOGFISH','MORAY EEL','NASSAU GROUPER 30-40 CM',
              'NASSAU GROUPER 40-50 CM','NASSAU GROUPER 50-60 CM',
              'NASSAU GROUPER >60 CM','NASSAU GROUPER TOTAL','NASSAU GROUPER 30-40 CM',
              'NASSAU GROUPER 40-50 CM','NASSAU GROUPER 50-60 CM',
              'NASSAU GROUPER >60 CM','NASSAU GROUPER TOTAL','ORANGE SPINE UNICORNFISH',
              'ORANGE SPOTTED GROUPER 30-40 CM','ORANGE SPOTTED GROUPER 40-50 CM',
              'ORANGE SPOTTED GROUPER 50-60 CM','ORANGE SPOTTED GROUPER >60 CM',
              'ORANGE SPOTTED GROUPER TOTAL','PARROTFISH',
              'PEACOCK GROUPER 30-40 CM','PEACOCK GROUPER 40-50 CM',
              'PEACOCK GROUPER 50-60 CM','PEACOCK GROUPER >60 CM',
              'PEACOCK GROUPER TOTAL','PENCIL URCHIN','QUEEN CONCH','SEA FAN','SHARKS',
              'SHORT SPINE URCHIN','SLATE PENCIL URCHIN','SNAPPER',
              'SPIDER CRAB','SPOTTED GRUNT','TRIPNEUSTES','TRITON','TROCHUS','TURTLES',
              'WHITE BAND','WHITE PLAGUE','WHITE BAND','YELLOW GOATFISH','YELLOW TANG',
              'YELLOWBAR ANGELFISH','YELLOWTAIL TANG')


organisms <- toupper(gsub("/","\\.",gsub("\\?","\\.",gsub(" ","\\.",organisms))))
organisms <- ifelse(substr(organisms,nchar(organisms),nchar(organisms))==".",substr(organisms,1,nchar(organisms)-1),organisms)

# clean df names
names(df) <- ifelse(substr(names(df),nchar(names(df)),nchar(names(df)))==".",substr(names(df),1,nchar(names(df))-1),names(df))
names(df) <- toupper(names(df))

# get organisms subset
organisms <- names(df)[names(df) %in% organisms]

# susbet organsisms to desired list
organisms <- c('SNAPPER','TRIPNEUSTES','TRITON','PENCIL.URCHIN','PARROTFISH','MORAY.EEL','LOBSTER','HUMPHEAD.WRASSE','GROUPER.TOTAL',
               'HAEMULIDAE','GIANT.CLAM.TOTAL','EDIBLE.SEA.CUCUMBER','DIADEMA','COTS','BUTTERFLYFISH','BUMPHEAD.PARROT','BARRAMUNDI.COD',
               'BANDED.CORAL.SHRIMP')

errorOrganisms <- c("HAEMULIDAE",'GIANT.CLAM.TOTAL','EDIBLE.SEA.CUCUMBER','DIADEMA','COTS')
organisms <- organisms [! organisms %in% errorOrganisms]


##############################
##############################
# Clean anthro
##############################
##############################
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

# drop bad row
df <- df[!(df[v]=='0.0' & !is.na(df[v])),]

# get caribbean subset
caribbeanCountries <- c('USA-FL','BARBADOS','MEXICO','COLOMBIA','DOMINICA','BELIZE','HONDURAS','ST KITTS & NEVIS',
                        'BVI','CAYMAN ISLANDS','JAMAICA','TRINIDAD & TOBAGO','ST LUCIA','ST VINCENT & GRENADINES','GRENADA',
                        'ANTIGUA','PUERTO RICO','USVI','ANGUILLA','BARBUDA','VENEZUELA','DOMINICAN REPUBLIC','ARUBA','TURKS & CAICOS',
                        'PANAMA','CUBA','BAHAMAS','HAITI','COSTA RICA','GUATEMALA')
dfCaribbean <- subset(df, df$COUNTRY %in% caribbeanCountries & df$OCEAN == 'ATLANTIC')

##############################
##############################
# Set up DF
##############################
##############################
# SET CORRECT DF (Caribbean or not)
df <- df
#df <- df[df$OCEAN == "PACIFIC" & df$GROUPER.TOTAL != 0,]
#df <- dfCaribbean

# clean response
responseVar <- 'GROUPER.TOTAL'
df[,responseVar] <- round(df[,responseVar]) # Note: only run with counts

df <- subset(df, !is.na(df[,responseVar])) # drop na in y var
organisms <- organisms[!organisms %in% c(responseVar)] # remove var from formula

# create formula
xs <- paste(organisms, collapse="+")
formula <- formula(paste(c(responseVar, xs), collapse='~'))

# create train and test split
pacman::p_load(caTools)
sample = sample.split(df[,responseVar], SplitRatio = .75)
train = subset(df, sample == TRUE)
test  = subset(df, sample == FALSE)

###########################
# Poisson
###########################
# perform zero inflated pois
pacman::p_load(pscl)
zip.fit <- zeroinfl(formula, data = train, dist = 'poisson')
summary(zip.fit)

# get correlation
zipPred <- predict(zip.fit, newdata=test)
cor(zipPred, test[,responseVar], use = "complete.obs")
summary(zipPred)
summary(test[,responseVar])

# plot output 
plot(zipPred, test[,responseVar])
qqplot(zipPred, test[,responseVar])

###########################
# RF
###########################
# create model
pacman::p_load(randomForest)
rfRough <- randomForest(
  formula,
  data=train,
  na.action=na.roughfix
)
rfRough 
rfRough$importance

# get correlation
rfPred <- predict(rfRough, newdata=test)
cor(rfPred, test[,responseVar], use = "complete.obs")
summary(rfPred)
summary(test[,responseVar])

# plot output 
plot(rfPred, test[,responseVar])
qqplot(rfPred, test[,responseVar])

###########################
# TT-specific
###########################
# subset data
TT <- subset(df, df$COUNTRY == 'TRINIDAD & TOBAGO')

# create formula
xs <- paste(organisms[9], collapse="+")
formula <- formula(paste(c(responseVar, xs), collapse='~'))
 
# perform zero inflated pois
zip.fit <- zeroinfl(formula, data = TT, dist = 'poisson')
summary(zip.fit)
  
  
  
  