#Installing various packages to use in our program.
install.packages('maps')
install.packages('mapproj')
install.packages("biscale")
install.packages('leaflet')
install.packages('plyr')
install.packages('dplyr')
install.packages('devtools')
install.packages('Rtools')
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("hadley/lazyeval")
devtools::install_github("hadley/dplyr")

#Calling the installed packages
library(dplyr)
library(plyr)
library(leaflet)
library(biscale)
library(stringr)
library(maps)
library(mapproj)
library(ggplot2)

#Loading the data of Obesity rate and Fast Foood Restaurant
obesity <- read.csv(file = 'C:/Users/F.R.I.E.N.D.S/Desktop/BigDataChallange/Obesity in different countries at different time-cleaned.csv')

#----------------------------------For Obesity Rate----------------------------------
# Since our Obesity data has all the countries data, we filter the data
# of the USA using the filter function.
obesityUSA <- obesity %>% filter(Country == "United States of America")
# %>% passes the left hand side of the operator to the first argument of the 
# right hand side of the operator

# checking the column name of the filtered data
names(obesityUSA)

# Creating a table containing Year, Obesity rate and Sex.
obesityUSA <- obesityUSA %>% group_by(Year, Obesity...., Sex)%>%
  dplyr::summarise(count = n())

# Filtering the obesity rate of male 
maleObesity <- obesityUSA %>% filter(Sex == 'Male')
# Filtering the obesity rate of Female
FemaleObesity <- obesityUSA %>% filter(Sex == 'Female')
# Filtering the obesity rate of both sexes
bothSexesObesity <- obesityUSA %>% filter(Sex == 'Both sexes')

# Removing everything after the rate of Obesity. For eg: our obesity
# rate is given like: 10.3 [20.7, 20.3], so extracting just the rate i.e.10.3
# from all sexes

# Extracting the rate of Obesity in Men
maleObesity$Obesity.... <- gsub(' .*', '', maleObesity$Obesity....)
# Extracting the rate of obesity in Female
FemaleObesity$Obesity.... <- gsub(' .*', '', FemaleObesity$Obesity....)
# Extracting the rate of obesity in bothsexes
bothSexesObesity$Obesity.... <- gsub(' .*', '', bothSexesObesity$Obesity....)


# Creating space to include three graph in one figure. The par function
# takes one argument mfrow that signifies how many rows and column are require.
# Since we require 3 rows and 1 column value for our 3 graphs, mfrow is (3, 1)
par(mfrow=c(3,1))
# Plotting the rate of Male Obesity vs Year graph
plot(maleObesity$Year, maleObesity$Obesity...., ylim = c(1, 50), ylab = 'Male Obesity %', xlab = NA )
# Plotting the rate of Female Obesity vs Year graph
plot(FemaleObesity$Year, FemaleObesity$Obesity...., ylim = c(1, 50), ylab = 'Female Obesity %', xlab = NA )
# Plotting the rate of obesity in both sexes vs the year graph
plot(bothSexesObesity$Year, bothSexesObesity$Obesity...., ylim = c(1, 50), ylab = 'Both Sexes Obesity %', xlab = 'Year' )

# Creating a space for one graph only to compare the change in obesity rate
# between male and female in one graph.
par(mfrow = c(1, 1))
# Plotting male obesity vs year graph
plot(maleObesity$Year,maleObesity$Obesity....,type="l",col="red")
#Adding the rate of Female Obesity in the graph above
lines(maleObesity$Year,FemaleObesity$Obesity....,col="green")
# Creating an index to distinguish between two graph.
legend("topleft",
       c("Male Obesity","Female Obesity"),
       fill=c("red","green")
)
#----------------------------------------------------------------------------------------------------------------------------


#---------------------------------For Fast Food Restaurant--------------------------------------------------------------------

# Loading the Fast Food restaurant graph
fastFood <- read.csv(file = 'C:/Users/F.R.I.E.N.D.S/Desktop/BigDataChallange/fastFoodRestaurant.csv')
# check the column of our data
names(fastFood)

# Making your own new table by deleting certain column from the main data and changing the column name
fastFoodClean <- data.frame(name = fastFood$name, dateEstablished = fastFood$dateAdded, address = fastFood$address, city = fastFood$city, country = fastFood$country)

# Checking the new table and its column name
fastFoodClean
names(fastFoodClean)

# Trying to clean the establised data information. For now the date is displayed as "2019-05-19T23:58:05Z". 
# Removing everything after T from the above example by using a single date first.

# The first date in the tabel
date1 <- fastFoodClean$dateEstablished[1]
# Removing everything after T using gsub function. In this case, gsub takes 
# three argument.first argument takes the what you want to remove, second 
# argument takes what you want to replace it with(in this case nothing) and 
# third it takes the name of the list containing the data
gsub('T.*', '', date1)


# Applying the above proceess to Remove everything after T from rest of the date.
# All the date from out data
allDate <- fastFoodClean$dateEstablished
# Applying the gsub function to all our data
fastFoodCleanDate<-gsub("T.*","",allDate)
# Checking the change
fastFoodCleanDate


# Creating another table with the improved date from above
fastFoodClean <- data.frame(name = fastFood$name, dateEstablished = fastFoodCleanDate, address = fastFood$address, city = fastFood$city, country = fastFood$country)

# Creating a table that shows how many Fast food chains are in each city
totalRestaurantEachCity <- table(fastFoodClean$city)
head(totalRestaurantEachCity)

# Creating a list of all the fast food restaurant available in our data 
allFastFoodRestName <- fastFoodClean$name

# Creating a data frame of all the fast food restaurant to count the total number
# of each retaurant
df <- data.frame(allFastFoodRestName)
df

# While looking through the data of all the restaurant name, we found 
# something interesting. Even though some restaurants in the data were the same,
# they were counted as different. It was becasuse the restaurant 
# were spelled differently.
# For eg: KFC and kfc were counted differently. Therefore, in order to
# correct these spelling mistake we use the 'which' function that takes
# the name of the wrongly spelled restaurant and equals(changes) it to
# the correct spelling. 

# Correcting the spelling error for KFC. 
df$allFastFoodRestName[which(df$allFastFoodRestName == "Kentucky Fried Chicken" |df$allFastFoodRestName == "KFC - Kentucky Fried Chicken"
             |df$allFastFoodRestName == "KFC Kentucky Fried Chicken"|df$allFastFoodRestName == "KFC AW"|df$allFastFoodRestName == "Kfc")] = "KFC"
# Correcting the spelling error for Mcdonalds
df$allFastFoodRestName[which(df$allFastFoodRestName == "McDonalds Family Restaurant"| df$allFastFoodRestName == "McDonalds Family Restaurant"
             | df$allFastFoodRestName == "McDonalds"|df$allFastFoodRestName == "Mc Donald's"|df$allFastFoodRestName == "Mcdonalds")] = "McDonald's"
# Similarly, Correcting the spelling error for other restaurants.
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

# Creating a table with all the corrected restaurants name.
numberOfTotalRestaurant <- table(df)
# Checking the corrected table
numberOfTotalRestaurant
# Trying to plot all the graph of all the restaurant with their numbers. 
# Since the list of restaurant was big this graph won't be useful, but
# you can still make use this to see that some restaurant are opened more
# than others.
plot(table(df))


# Cleaning the date of establishment more to get just the
# years when the restaurants were established
date1 <- fastFoodClean$dateEstablished
# Retriving just the year from our date. 
yearOfEstablishment <- gsub('-.*', '', date1)

# creating a frequency table with years from 2014 to 2019
# and the total number of restaurants
yearOfEstablishmentTable <- table(yearOfEstablishment)

# creating barplot to visualize the above table
barplot(yearOfEstablishmentTable, xlab = 'Years', ylab = 'Number of New Restaurants', col = 'firebrick', las = 1, ylim = c(0, 3000), main = 'New Fast Food Restaurants Openining From 2014-19')

plot(yearOfEstablishmentTable, type = 'l')


# Filtering the main data according to the country. If the fast food restaurant 
# is not in the USA it is removed from the table 
USA <- fastFood %>% filter(country == "US")

# Creating a table with city, province, longitutde and latitude
USA <- USA %>% group_by(city, province, longitude, latitude) %>%
  dplyr::summarise(count = n()) %>%
  arrange(desc(count))

# Specifying my own color to use
myColor <- colorNumeric(palette = 'RdYlBu', domain = c(1:2300))

# Using the leaflet package and CartoDB provider to plot all the places
# restaurant in the map of USA. Magnifying the graph shows that there are
# more restaurant in the west and east side than the middle.
USA %>%
  leaflet() %>%
  addProviderTiles('CartoDB') %>%
  addCircleMarkers(radius = 1, color = ~myColor(count))

