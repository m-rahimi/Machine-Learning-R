closeAllConnections()
rm(list = ls())

library(data.table)
library(ggplot2)

#setwd("/work/Rproject/Data_Incubator")
setwd("C:/Users/Amin/Documents/R/Rproject/Data_Incubator/Q2")
data <- fread('Historic_Secured_Property_Tax_Rolls.csv',  colClasses = "character")

# select the latest assessment
data$`Closed Roll Fiscal Year` <- as.integer(data$`Closed Roll Fiscal Year`)
block <- data[,.(.N),by=.(`Block and Lot Number`)][order(-N)]
df <- data.table(matrix(ncol = 43, nrow = 0))
colnames(df) <- colnames(data)
for (ii in 1:1000){
  df <- rbind(df,data[`Block and Lot Number`==block[ii]$`Block and Lot Number`][`Closed Roll Fiscal Year` == max(`Closed Roll Fiscal Year`)])
  cat(ii, '\n')
}

#df <- fread('assessment.csv',  colClasses = "character")

#Q1 What fraction of assessments are for properties of the most common class?
classcode <- data[,.(.N),by=.(`Property Class Code`)][order(-N)]
classcode[1]  # D is the most common class

# assessment for each block should be sum of assessed fixtures, improvment, land, and personal properties
# I am not sure this assumption is correct !!!!!
data$assessment <- as.numeric(data$`Closed Roll Assessed Fixtures Value`) +
                   as.numeric(data$`Closed Roll Assessed Improvement Value`) +
                   as.numeric(data$`Closed Roll Assessed Land Value`) +
                  as.numeric(data$`Closed Roll Assessed Personal Prop Value`)

data <- data[!is.na(data$assessment)]
# group the class and calculate sum of assessment for each class
ass_class <- data[,.(.N,sum=sum(assessment)), by=.(`Property Class Code`)][order(-N)]
total_ass <- sum(ass_class$sum)
ass_class[1]$sum / total_ass # 0.292147 the ratio of most common class assesssment to total assessment

#Q2 What is the median assessed improvement value, considering only non-zero assessments?
median(as.numeric(df[assessment != 0]$`Closed Roll Assessed Improvement Value`))  # 205321.5

#Q3 Calculate the average improvement value (using only non-zero assessments) in each neighborhood.
# What is the difference between the greatest and least average values?
ave_improvement <- df[,.(.N,ave=mean(as.numeric(`Closed Roll Assessed Improvement Value`))), by=.(`Neighborhood Code`)][order(-ave)]
ave_improvement[1]$ave - ave_improvement[dim(ave_improvement)[1]]$ave  # 6362537

#Q4 What is the yearly growth rate of Land Values over the years covered by this data? 
land <- data[,.(`Closed Roll Fiscal Year`, `Closed Roll Assessed Land Value`)]
land$`Closed Roll Assessed Land Value` <- as.numeric(land$`Closed Roll Assessed Land Value`)
land <- land[`Closed Roll Assessed Land Value` > 100] # not only zero but also avoid small value which are not trustable
land$`Closed Roll Fiscal Year` <- land$`Closed Roll Fiscal Year` - min(land$`Closed Roll Fiscal Year`) #change the origin of year

#Using log make calculation easy -> log(P) = rt + log(P0)
land$`Closed Roll Assessed Land Value` <- log(land$`Closed Roll Assessed Land Value`)
# plot the data
ggplot(data = land[sample(.N,10000)], aes(x = `Closed Roll Fiscal Year`, y=`Closed Roll Assessed Land Value`)) +
  geom_point() 
# it looks averaging all land values hide the growth rate. the complete model should apply to each land seperatly
model <- lm(data = land, formula = `Closed Roll Assessed Land Value` ~ `Closed Roll Fiscal Year`)
summary(model)
# Intercept = 11.67516 and slop = 0.04189

#Q5 We can use the property locations to estimate the areas of the neighborhoods. 
#Represent each as an ellipse with semi-axes given by a single standard deviation of the longitude and latitude. 
#What is the area, in square kilometers, of the largest neighborhood measured in this manner? Be sure to filter out invalid coordinates.
location <- df[,.(Location, `Neighborhood Code`)][Location != ""]
location$lat <- as.numeric(sapply(location$Location, function(x) strsplit(x, "[(, )]")[[1]][2]))
location$lon <- as.numeric(sapply(location$Location, function(x) strsplit(x, "[(, )]")[[1]][4]))

location <- location[,.(mean_lat=mean(lat),mean_lon=mean(lon),sd_lat=sd(lat),sd_lon=sd(lon)), by=(`Neighborhood Code`)]
# what I need to do is to calculate the distance between mean and mean+sd which gives me radius in x and y
# in fact mean is the center of ellips and mean+sd reperest the border; distance between these two points gives radius

distance <- function(lat1, lon1, lat2, lon2) { # calculate distance between two points based on the GPS data (latitude and longatude)
  p = 0.017453292519943295 # Math.PI / 180
  a = 0.5 - cos((lat2 - lat1) * p)/2 + cos(lat1 * p) * cos(lat2 * p) * (1 - cos((lon2 - lon1) * p))/2;
  return(12742 * asin(sqrt(a))) # // 2 * R; R = 6371 km
}

area <- c() # accumulate the R1*R2 which reperesnt the area 
for (ii in 1:dim(location)[1]){
  loc <- location[ii]
  loc[,lat := mean_lat + sd_lat][,lon := mean_lon + sd_lon]
  R1 <- distance(loc$mean_lat,loc$mean_lon,loc$lat,loc$mean_lon)
  R2 <- distance(loc$mean_lat,loc$mean_lon,loc$mean_lat,loc$lon)
  area <- c(area, R1*R2)
}

max(area) * pi # 16.4688 the area of the largest neighborhood


#Q6 What is the difference between the average number of units in buildings build in or after 1950, 
#   and that for buildings built before 1950?
unit_year <- df[,.(`Year Property Built`,`Number of Units`)]
unit_year$`Year Property Built` <- as.numeric(unit_year$`Year Property Built`)
unit_year$`Number of Units` <- as.numeric(unit_year$`Number of Units`)

mean(unit_year[`Year Property Built` >= 1950]$`Number of Units`) - mean(unit_year[`Year Property Built` < 1950]$`Number of Units`) # 0.05111592

#Q7 Considering only properties with non-zero numbers of bedrooms and units, calculate the average number of bedrooms per unit in each zip code. 
# Use the ratio of the means instead of the mean of the ratio. What is this ratio in the zip code where it achieves its maximum?
bed_unit_zip <- df[,.(`Number of Bedrooms`,`Number of Units`, `Zipcode of Parcel`)]
bed_unit_zip <- bed_unit_zip[`Zipcode of Parcel` != ""][`Number of Units` != "0"][`Number of Bedrooms` != "0"]

for (name in names(bed_unit_zip)){ 
  bed_unit_zip[[name]] <- as.numeric(bed_unit_zip[[name]]) #conver to numeric
}

#calculate the mean of bedrooms and mean of unit in each zip code
bed_unit_zip <- bed_unit_zip[, .(ave_bed=mean(`Number of Bedrooms`), ave_unit=mean(`Number of Units`)), by=.(`Zipcode of Parcel`)]

bed_unit_zip[, ratio := ave_bed/ave_unit][order(-ratio)][1]$ratio # 3.80756 largest ratio of mean bedrooms to mean units


#Q8 Estimate how built-up each zip code is by comparing the total property area to the total lot area. 
# What is the largest ratio of property area to surface area of all zip codes?
pro_lot <- df[,.(`Property Area in Square Feet`,`Lot Area`, `Zipcode of Parcel`)]
pro_lot <- pro_lot[`Zipcode of Parcel` != ""]

for (name in names(pro_lot)){ 
  pro_lot[[name]] <- as.numeric(pro_lot[[name]]) #conver to numeric
}
pro_lot <- pro_lot[!is.na(pro_lot$`Lot Area`)]

pro_lot <- pro_lot[,surface:=`Property Area in Square Feet`+`Lot Area`] # surface area is sum of the property area and lot area
pro_lot <- pro_lot[, .(sum_pro=sum(`Property Area in Square Feet`), sum_surface=sum(surface)), by=.(`Zipcode of Parcel`)]
 
pro_lot[, ratio := sum_pro / sum_surface][order(-ratio)][1]$ratio # 0.9250366 largest ratio of property area to surface area
