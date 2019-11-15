setwd("/Users/hewishwang/Desktop/stat_479_tree/628useGuide")

var=c('RestaurantsAttire','RestaurantsGoodForGroups','DriveThru','OutdoorSeating',
      'RestaurantsTableService','WheelchairAccessible','Smoking','ByAppointmentOnly',
      'HappyHour','BusinessAcceptsCreditCards','BYOBCorkage','HasTV','BikeParking',
      'Corkage','DogsAllowed','GoodForKids','RestaurantsTakeOut','BusinessAcceptsBitcoin',
      'RestaurantsPriceRange2','WiFi','RestaurantsReservations','RestaurantsDelivery',
      'NoiseLevel','Caters','CoatCheck','Alcohol','GoodForDancing','wednesday','hipster',
      'saturday','live','brunch','trendy','garage','latenight','thursday','gluten.free',
      'intimate','classy','lot','dj','valet','karaoke','background_music','sunday','divey',
      'romantic','breakfast','vegetarian','touristy','monday','validated','dinner','upscale',
      'street','friday','casual','dessert','lunch','tuesday')

oneLevel=c("AcceptsInsurance","BYOB","AgesAllowed","halal","no_music","soy.free",
           "dairy.free","kosher","vegan","jukebox","video")

sig.var=c("RestaurantsAttire","DriveThru","RestaurantsTableService","BusinessAcceptsCreditCards"
          ,"BikeParking","CoatCheck","NoiseLevel","trendy","casual")

dat=read.csv("attributes.csv",header = T)

# write GUIDE description file
k <- ncol(dat)
roles <- rep("n",k)
c.vars <- var
roles[names(dat) %in% c.vars] <- "c"
x.vars <- c("X","business_id",oneLevel)
roles[names(dat) %in% x.vars] <- "x"
d.var <- "stars"
roles[names(dat) %in% d.var] <- "d"
write("attributes.csv",file="desc_atr.txt")
write("NA",file="desc_atr.txt",append=TRUE)
write("2",file="desc_atr.txt",append=TRUE)
write.table(cbind(1:k,names(dat),roles),file="desc_atr.txt",
            row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)


# Importance score
scr.data <- read.table("importance scores.txt",header=TRUE)
scr.data=scr.data[which(scr.data$Score>1),]
#scr.data=scr.data[1:20,]
par(mar=c(5,6,2,1),las=1,cex.axis=0.7)
barplot(scr.data$Score,names.arg=scr.data$Variable,col="cyan",
        horiz=TRUE,xlab="Importance scores")
abline(v=1,col="red",lty=2)
#graphics.off()
impt.var=as.character(scr.data$Variable)
impt.var=c("stars",impt.var)

# linear model
lmDat=read.csv("attributesInR.csv",header = T)

modDat=lmDat[,colnames(lmDat) %in% impt.var]
lmMod=lm(stars~.,data=modDat)
summary(lmMod)
lmR1=lm(stars~RestaurantsAttire+DriveThru+RestaurantsTableService+BusinessAcceptsCreditCards
        +BikeParking+CoatCheck+NoiseLevel+trendy+casual,data=modDat)
summary(lmR1)
# anova1=aov(stars~RestaurantsAttire+DriveThru+RestaurantsTableService+BusinessAcceptsCreditCards
#         +BikeParking+CoatCheck+NoiseLevel+trendy+casual,data=modDat)
# summary(anova1)
# 
# library(nnet)
# glmR1=multinom(stars~RestaurantsAttire+DriveThru+RestaurantsTableService+BusinessAcceptsCreditCards
#         +BikeParking+CoatCheck+NoiseLevel+trendy+casual,family = ,data=modDat)
# summary(glmR1)


mNoise=lm(stars~NoiseLevel,data=modDat)
summary(mNoise)
mRA=lm(stars~RestaurantsAttire,data=modDat)
summary(mRA)
mDT=lm(stars~DriveThru,data=modDat)
summary(mDT)
mRT=lm(stars~RestaurantsTableService,data=modDat)
summary(mRT)
mBAC=lm(stars~BusinessAcceptsCreditCards,data=modDat)
summary(mBAC)
mBP=lm(stars~BikeParking,data=modDat)
summary(mBP)
mCC=lm(stars~CoatCheck,data=modDat)
summary(mCC)
mTrendy=lm(stars~trendy,data=modDat)
summary(mTrendy)
mCasual=lm(stars~casual,data=modDat)
summary(mCasual)
