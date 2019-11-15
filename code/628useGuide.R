setwd("/Users/hewishwang/Desktop/stat_479_tree")
var=c('RestaurantsAttire',
'RestaurantsGoodForGroups',
'DriveThru',
'OutdoorSeating',
'RestaurantsTableService',
'WheelchairAccessible',
'Smoking',
'ByAppointmentOnly',
'HappyHour',
'BusinessAcceptsCreditCards',
'BYOBCorkage',
'HasTV',
'BikeParking',
'Corkage',
'DogsAllowed',
'GoodForKids',
'RestaurantsTakeOut',
'BusinessAcceptsBitcoin',
'RestaurantsPriceRange2',
'WiFi',
'RestaurantsReservations',
'RestaurantsDelivery',
'NoiseLevel',
'Caters',
'CoatCheck',
'Alcohol',
'GoodForDancing',
'wednesday',
'hipster',
'saturday',
'live',
'brunch',
'trendy',
'garage',
'latenight',
'thursday',
'gluten.free',
'intimate',
'classy',
'lot',
'dj',
'valet',
'karaoke',
'background_music',
'sunday',
'divey',
'romantic',
'breakfast',
'vegetarian',
'touristy',
'monday',
'validated',
'dinner',
'upscale',
'street',
'friday',
'casual',
'dessert',
'lunch',
'tuesday')
oneLevel=c("AcceptsInsurance","BYOB","AgesAllowed","halal","no_music","soy.free",
           "dairy.free","kosher","vegan","jukebox","video")

dat=read.csv("attributes.csv",header = T)

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
