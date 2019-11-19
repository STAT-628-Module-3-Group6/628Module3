#setwd("/Users/hewishwang/Desktop/stat_479_tree/628useGuide")
dat=read.csv("attributes.csv",header = T)

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
row=nrow(dat)
missingTooMuch=c()

for(i in 1:length(var)){
  if(sum(is.na(dat[,var[i]]))/row > 0.5) {
    missingTooMuch=append(missingTooMuch,var[i])
  }
}


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
scr.data=scr.data[1:10,]
par(mar=c(5,8.5,2,1),las=1,cex.axis=0.7)
barplot(scr.data$Score,names.arg=scr.data$Variable,col="cyan",
        horiz=TRUE,xlab="Importance scores")
abline(v=1,col="red",lty=2)


# ANOVA and multiple comparison
lmDat=read.csv("attributesInR.csv",header = T)
library(agricolae)
mCasual=aov(stars~casual,data=lmDat)
summary(mCasual)
outCasual=LSD.test(mCasual,"casual",p.adj="bonferroni" )
outCasual$group
plot(outCasual,ylim =c(3,4),xlab="casual")


mDinner=aov(stars~dinner,data=lmDat)
summary(mDinner)
outDinner=LSD.test(mDinner,"dinner",p.adj="bonferroni" )
outDinner$group
plot(outDinner,ylim =c(3,4))

mLunch=aov(stars~lunch,data=lmDat)
summary(mLunch)
outLunch=LSD.test(mLunch,"lunch",p.adj="bonferroni" )
outLunch$group
plot(outLunch,ylim =c(3,4),xlab="lunch")

mNoise=aov(stars~NoiseLevel,data=lmDat)
summary(mNoise)
outNoise=LSD.test(mNoise,"NoiseLevel",p.adj="bonferroni" )
outNoise$group
plot(outNoise,ylim = c(2.9,3.6),xlab="NoiseLevel")

mBrunch=aov(stars~brunch,data=lmDat)
summary(mBrunch)
outBrunch=LSD.test(mBrunch,"brunch",p.adj="bonferroni" )
outBrunch$group
plot(outBrunch,ylim =c(3,3.8))

mLatenight=aov(stars~latenight,data=lmDat)
summary(mLatenight)
outLatenight=LSD.test(mLatenight,"latenight",p.adj="bonferroni" )
outLatenight$group
plot(outLatenight,ylim =c(3,3.8))

mBreakfast=aov(stars~breakfast,data=lmDat)
summary(mBreakfast)
outBreakfast=LSD.test(mBreakfast,"breakfast",p.adj="bonferroni" )
outBreakfast$group
plot(outBreakfast,ylim =c(3,3.8))

mTrendy=aov(stars~trendy,data=lmDat)
summary(mTrendy)
outTrendy=LSD.test(mTrendy,"trendy",p.adj="bonferroni" )
outTrendy$group
plot(outTrendy,ylim =c(3,3.8))

mIntimate=aov(stars~intimate,data=lmDat)
summary(mIntimate)
outIntimate=LSD.test(mIntimate,"intimate",p.adj="bonferroni" )
outIntimate$group
plot(outIntimate)

mGroup=aov(stars~RestaurantsGoodForGroups,data=lmDat)
summary(mGroup)
outGroup=LSD.test(mGroup,"RestaurantsGoodForGroups",p.adj="bonferroni" )
outGroup$group
plot(outGroup,ylim = c(3,4),xlab="RestaurantsGoodForGroups")


# Take examples for slides
par(mfrow=c(1,2),mar=c(4,2,4,0.1))
plot(outCasual,ylim =c(3,4),xlab="casual")
plot(outNoise,ylim = c(2.9,3.6),xlab="NoiseLevel")


# Recommendation system about attributes

# If casual/trendy/intimate is NA, "You should tell what is your restaurant's ambience."
# If casual/trendy/intimate is False, "You should make your restaurant's ambience casual/trendy/intimate."
# If dinner/lunch/brunch/latenight/breakfast is NA, "You should provide your GoodForMeal information."
# If dinner is False, "You should provide dinner for customers."
# If brunch/latenight/breakfast is True, "You should pay more attention on lunch and dinner."
# If NoiseLevel is NA, "You should provide your NoiseLevel information"
# If NoiseLevel is loud/very_loud, "You should make your restaurant more quiet."
# If RestaurantsGoodForGroups is NA, "You should tell whether your restaurant is good for groups or not."
# If RestaurantsGoodForGroups is False, "You should provide big tables for groups customers."
# "You can determine whether the advice makes sense or not, since these are suggestions formed from data."


# Recommendation system about reviews

# “Your $high_food is fond of customers, so you should keep the flavor. ”
# “Your $low_food more or less has a bad effect on your Yelp rating, so you should improve its recipe or remove $low_food from your menu.”
# If service is 0, “Your service lowers your Yelp rating, so you should improve your waiters’ service level.”
