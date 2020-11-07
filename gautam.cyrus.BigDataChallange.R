library(stringr)
#Loading the data
fastFood <- read.csv(file = 'C:/Users/F.R.I.E.N.D.S/Desktop/BigDataChallange/fastFoodRestaurant.csv')

#check the column
names(fastFood)

#make your own table by deleting certain colum from the main data and changing the column name
fastFoodClean <- data.frame(name = fastFood$name, dateEstablished = fastFood$dateAdded, address = fastFood$address, city = fastFood$city, country = fastFood$country)

#allFastFoodRestName clean table
fastFoodClean
names(fastFoodClean)

#trying to clean the establised data information. For now the date is displayed as "2019-05-19T23:58:05Z". 
#I have to remove the everything after T. To do that i am using a single date first.
date1 <- fastFoodClean$dateEstablished[1]
date1
gsub('T.*', '', date1)

#Removing everything after T
allDate <- fastFoodClean$dateEstablished
fastFoodClean$dateEstablished
fastFoodCleanDate<-gsub("T.*","",allDate)
fastFoodCleanDate

#creating a allFastFoodRestName table with the improved date
fastFoodClean <- data.frame(name = fastFood$name, dateEstablished = fastFoodCleanDate, address = fastFood$address, city = fastFood$city, country = fastFood$country)
fastFoodClean

#Table showing how many Fast food chains are in each city
head(fastFoodClean$city)
totalRestaurantEachCity <- table(fastFoodClean$city)
head(totalRestaurantEachCity)

allFastFoodRestName <- fastFoodClean$name
df <- data.frame(allFastFoodRestName)
df
#For KFC
df$allFastFoodRestName[which(df$allFastFoodRestName == "Kentucky Fried Chicken" |df$allFastFoodRestName == "KFC - Kentucky Fried Chicken"
             |df$allFastFoodRestName == "KFC Kentucky Fried Chicken"|df$allFastFoodRestName == "KFC AW"|df$allFastFoodRestName == "Kfc")] = "KFC"
df
#For Mcdonalds
df$allFastFoodRestName[which(df$allFastFoodRestName == "McDonalds Family Restaurant"| df$allFastFoodRestName == "McDonalds Family Restaurant"
             | df$allFastFoodRestName == "McDonalds"|df$allFastFoodRestName == "Mc Donald's")] = "McDonald's"

#For Mucho Gusto
df$allFastFoodRestName[which(df$allFastFoodRestName == "Mucho Gusto Mexican Kitchen")] = "Mucho Gusto"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Mr Sub Sandwiches")] = "Mr Submarine"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Moes Southwest Grill")] = "Moe's Southwest Grill"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Melt Bar and Grilled")] = "Melt Bar & Grilled"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Mcalister's Deli"| df$allFastFoodRestName == "McAlisters Deli")] = "McAlister's Deli"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Marys Pizza Shack")] = "Mary's Pizza Shack"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Mambo Grill and Tapas")] = "Mambo Grill Tapas"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Mai Tai Restaurant")] = "Mai-Tai Restaurant"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Long John Silvers")] = "Long John Silver's"
df$allFastFoodRestName[which(df$allFastFoodRestName == "L L Hawaiian Barbecue"| df$allFastFoodRestName == "L L Hawaiian Barbeque")] = "L & L Hawaiian Barbecue"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Krystal Burgers")] = "Krystal"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Killer Burger")] = "Killer Burgers"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Jimmy Johns")] = "Jimmy John's"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Jersey Mikes Subs")] = "Jersey Mike's Subs"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Jerry's Subs and Pizza")] = "Jerry's Subs & Pizza"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Jasons Deli")] = "Jason's Deli"
df$allFastFoodRestName[which(df$allFastFoodRestName == "InNOut Burger")] = "In-N-Out Burger"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Hunan Lion Gourmet Chinese")] = "Hunan Lion Gourmet Chinese Dining"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Hot Dog On A Stick")] = "Hot Dog on a Stick"
df$allFastFoodRestName[which(df$allFastFoodRestName == "HomeTown Buffet")] = "Hometown Buffet"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Hardee's/red Burrito"| df$allFastFoodRestName == "Hardee's/Red Burrito")] = "Hardees Red Burrito"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Hardee's Restaurants")] = "Hardee's Restaurant"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Guthries")] = "Guthrie's"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Great Wall Chinese Restaurant")] = "Great Wall Of China Restaurant"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Golden Palace Restaurant")] = "Golden Palace"
df$allFastFoodRestName[which(df$allFastFoodRestName == "George's Gyros Spot 2")] = "George's Gyros Spot"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Full Moon Bar B Que")] = "Full Moon Bar-B-Que"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Frisch's Big Boy Restaurant")] = "Frisch's Big Boy"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Freddys Frozen Custard Steakburgers"| df$allFastFoodRestName == "Freddy's Frozen Custard Steakburgers")] = "Freddy's Frozen Custard & Steakburgers"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Foxs Pizza Den")] = "Fox's Pizza Den"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Five Guys Burgers Fries")] = "Five Guys Burgers & Fries"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Fireplace Restaurant Lounge")] = "Fireplace Restaurant & Lounge"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Farlows on the Water")] = "Farlow's On The Water"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Famous Daves")] = "Famous Dave's"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Emidio & Sons Italian Restaurant")] = "Emidio Sons Italian Restaurant"

data.frame(name = table(df)[1:1000,], frequency = table(df)[,1:1000])
numberOfRestaurantTable <- table(df)
numberOfRestaurantTable[,1]
plot(numberOfRestaurantTable)
?axis()
