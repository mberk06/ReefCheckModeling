
# idea of this script: see how well some simple glms do for predicting species abundance
# and coral composition as a function of anthro factors

rm(list=ls(all=T))
setwd('~/Dropbox/Projects/Reefs/')

# read data
df <- read.csv('Data/df1.0.csv')


anthro.vars <- c('Siltation', 'Dynamite Fishing?', 'Poison Fishing?', 'Aquarium fish collection', 'Harvest of inverts for food', 
                 'Harvest of inverts for curio', 'Tourist diving/snorkeling', 'Sewage pollution', 'Industrial pollution', 'Commercial fishing', 
                 'Live food fishing', 'Yachts', 'Level of other impacts?', 'Is protection enforced?', 'Level of poaching?', 'Spearfishing?', 
                 'Commercial fishing?', 'Recreational fishing?', 'Invertebrate/shell collection?', 'Anchoring?', 'Diving?')

anthro.vars <- toupper(gsub("/","\\.",gsub("\\?","\\.",gsub(" ","\\.",anthro.vars))))
anthro.vars <- ifelse(substr(anthro.vars,nchar(anthro.vars),nchar(anthro.vars))==".",substr(anthro.vars,1,nchar(anthro.vars)-1),anthro.vars)

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

fit.pois <- glm(GROUPER.TOTAL~WATER.TEMP.AT.SURFACE+DYNAMITE.FISHING+POISON.FISHING+AQUARIUM.FISH.COLLECTION+HARVEST.OF.INVERTS.FOR.FOOD+HARVEST.OF.INVERTS.FOR.CURIO+
TOURIST.DIVING.SNORKELING+SEWAGE.POLLUTION+INDUSTRIAL.POLLUTION+COMMERCIAL.FISHING+LIVE.FOOD.FISHING+YACHTS,
                data=df, subset=df$GROUPER.TOTAL>0,family='poisson')


summary(fit.pois)

pacman::p_load(pscl)

zip.fit <- zeroinfl(GROUPER.TOTAL ~ WATER.TEMP.AT.SURFACE+DYNAMITE.FISHING+POISON.FISHING+AQUARIUM.FISH.COLLECTION+HARVEST.OF.INVERTS.FOR.FOOD+HARVEST.OF.INVERTS.FOR.CURIO+
                      TOURIST.DIVING.SNORKELING+SEWAGE.POLLUTION+INDUSTRIAL.POLLUTION+COMMERCIAL.FISHING+LIVE.FOOD.FISHING+YACHTS, 
                      data = df)


summary(zip.fit)

pacman::p_load(mice)

df.subset <- df[,names(df) %in% c('GROUPER.TOTAL', 'WATER.TEMP.AT.SURFACE','DYNAMITE.FISHING','POISON.FISHING','AQUARIUM.FISH.COLLECTION',
               'HARVEST.OF.INVERTS.FOR.FOOD','HARVEST.OF.INVERTS.FOR.CURIO','TOURIST.DIVING.SNORKELING',
               'SEWAGE.POLLUTION','INDUSTRIAL.POLLUTION','COMMERCIAL.FISHING','LIVE.FOOD.FISHING+YACHTS')]
df.subset <- df.subset[,-11]

impute.data <- mice(df.subset,m=5)

df1 <- df.subset
d <- ncol(df.subset)
for (j in 1:d) {
  if (any(is.na(df1[,j]))) {
    df1[is.na(df1[,j]),j] <- impute.data$imp[[j]][,1]
  }
}

zip.fit.imp1 <- zeroinfl(GROUPER.TOTAL ~ WATER.TEMP.AT.SURFACE+DYNAMITE.FISHING+POISON.FISHING+AQUARIUM.FISH.COLLECTION+HARVEST.OF.INVERTS.FOR.FOOD+HARVEST.OF.INVERTS.FOR.CURIO+
                      TOURIST.DIVING.SNORKELING+SEWAGE.POLLUTION+INDUSTRIAL.POLLUTION+COMMERCIAL.FISHING, 
                    data = df1)
