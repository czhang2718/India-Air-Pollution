library(rsconnect)

#----------------------------------------------actually run-----------------------------------------------
vars <- c(
  "SO2" = "so2",
  "NO2" = "no2",
  "Suspended Particulate Matter" = "spm",
  "All" = "all"
)

# write.csv(polldat_avg, "pd_avg.csv")
polldat = read.csv("temp_pd.csv", header=TRUE)
polldat_avg <- read.csv("pd_avg.csv", header=TRUE)
polldat_avg$city = as.character(polldat_avg$city)
geocities = read.csv("gcities.csv", header=TRUE)

# polldat$lat = as.numeric(polldat$lat)
# polldat$lng = as.numeric(polldat$lng)
# polldat$radius = as.numeric(polldat$radius)


#-----------------------------------------------preprocess-------------------------------------------------
#format data
# yrs = {}
# for(i in 1990:2030){
#   yrs = c(yrs, toString(i))
# }
# 
# colnames <- c("city", "so2", "no2", "spm", "year", "date")
# for(i in 1:length(colnames)){
#   names(polldat)[i] = colnames[i]
# }
# polldat$year = as.factor(substr(polldat$year, 1, 4))
# 
# 
# 
# #processes data
# dat <- read.csv("data.csv", stringsAsFactors=FALSE)
# polldat <- data.frame(dat$location, dat$so2, dat$no2, dat$spm, dat$date, dat$date, stringsAsFactors=FALSE)
# polldat = polldat[1:435739,]    #remove missing data
cities={}




#get list of cities in data
for(i in 2:dim(polldat_avg)[1]){
  if(polldat_avg$city[i]!="" && !(polldat_avg$city[i]%in% cities)) cities = c(cities, polldat_avg$city[i])
}
# cities.df = as.data.frame(cities)
# 
# 
# 
# 
# #geocode using NEW data
# geocities=data.frame(matrix(ncol=3))
# colnames(geocities) = c("city", "lat", "long")
# 
# geodata <- read.csv(file="geodata.csv", header = TRUE)
# for(i in 1:dim(geodata)[1]){
#   if(geodata$city[i] %in% cities){
#     loc = as.character(geodata$city[i])
#     geocities = rbind(geocities, c(loc, geodata$lat[i], geodata$lng[i]))
#     cities= cities[which(cities!= loc)]
#   }
#   if(geodata$admin[i] %in% cities){
#     loc = as.character(geodata$admin[i])
#     geocities = rbind(geocities, c(loc, geodata$lat[i], geodata$lng[i]))
#     cities= cities[which(cities!= loc)]
#   }
# }
# 
# 
# 
# 
# 
# #------------------------------------------------------------------------------------------------------
# 
# #geocode using OLD data
# getcoords <- function(row){
#   coords={}
#   latlong={}
#   for(i in 1: length(row)){
#     if(!grepl("^\\s*$", row[i])&& check.numeric(row[i])){
#       coords=c(coords, row[i])
#     }
#   }
#   # print(coords)
#   for(ele in coords){
#     if(as.numeric(ele)!= round(as.numeric(ele))) {
#       latlong = c(latlong, ele)}
#   }
#   return (latlong)
# }
# 
# 
# 
# #geocode coordinates, maybe radius/area
# library(varhandle)
# geocode <- read.delim("IN.txt", header=FALSE, sep="", stringsAsFactors=FALSE)
# geocode = as.data.frame(geocode)
# #get geocoded rows of cities
# # geocities=data.frame(matrix(ncol=3))
# # colnames(geocities) = c("city", "lat", "long")
# # cities[which(cities=="Eluru")] <- "Eluru"
# # cities[which(cities=="Ananthapur")] <- "Ananthapuram"
# 
# for(i in 1:dim(geocode)[1]){
# 
#   if((geocode[i,3] !="" && geocode[i,3] %in% cities) && length(getcoords(as.character(geocode[i,])))==2){
#     thiscity = geocode[i, 3]
#     if(!thiscity %in% geocities$city){
#       geocities = rbind(geocities, c(thiscity, getcoords(as.character(geocode[i,]))))
#     }
#     
#     cities= cities[which(cities!= thiscity)]       #remove from list
#   }
# 
#   if((geocode[i,4] !="" && geocode[i,4] %in% cities) && length(getcoords(as.character(geocode[i,])))==2){
#     thiscity = geocode[i, 4]
#     if(!thiscity %in% geocities$city){
#       geocities = rbind(geocities, c(thiscity, getcoords(as.character(geocode[i,]))))
#     }
#     cities= cities[which(cities!= geocode[i, 4])]
#   }
# 
#   if((geocode[i,5] !="" && geocode[i,5] %in% cities) && length(getcoords(as.character(geocode[i,])))==2){
#     thiscity = geocode[i, 5]
#     if(!thiscity %in% geocities$city){
#       geocities = rbind(geocities, c(thiscity, getcoords(as.character(geocode[i,]))))
#     }
#     cities= cities[which(cities!= geocode[i, 5])]}
# }
# 
# #write.csv(geocities, file="gcities.csv")
# 
# 
# #-------------------------------------------------------------------------------------------------------
# 
# 
# computes the average of numeric vector
# average <- function(nums){
#   total <- 0
#   count <- 0
#   for(i in 1: length(nums)){
#     if(is.numeric(nums[i]) & !is.na(nums[i])){
#       print(nums[i])
#       count = count+1
#       total <- total + nums[i]
#     }
#   }
#   return (total/count)
# }

# 
# #get averages for each contaminant by year, all cities
# avgso2 <- {}
# avgno2 <- {}
# avgspm <- {}
# for(i in 1987:2015){
#   nums <- polldat[which(as.numeric(as.character(polldat$year))==i),]
#   tempso2 <- nums[which(!is.null(nums$so2)), 2]
#   tempno2 <- nums[which(!is.null(nums$no2)), 3]
#   tempspm <- nums[which(!is.null(nums$spm)), 4]
# 
#   avgso2 = c(avgso2, average(tempso2))
#   avgno2 <- c(avgno2, average(tempno2))
#   avgspm <- c(avgspm, average(tempspm))
# }

# 
# 
# #get averages for each year for one city
# #TODO
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #move geocode to polldat
# polldat[,"lat"] <-90
# polldat[, "lng"] <- 10
# area = 42.7*1000
# polldat[, "radius"] <- (area/pi)^.5
# for(city in geocities$city){
#   polldat$lat[which(polldat$city==city)] = geocities$lat[which(geocities$city==city)]
#   polldat$lng[which(polldat$city==city)] = geocities$long[which(geocities$city==city)]
# }
# 
# 
# population
#remember to ADD SPACE AFTER CITY NAME IN DATA

# 
# popdata <- read.csv("real_pop.csv", header = TRUE)
# popdata$city = as.character(popdata$city)
# # popdata$population = as.numeric(popdata$population)
# polldat_avg[,"pop"] <- 0
# for(i in 1: length(popdata$city)){
#   # print(city)
#   city = substr(popdata$city[i], 1, nchar(popdata$city[i])-1)   #remove random whitespace at the end of every name
#   if(city %in% polldat_avg$city){
#     polldat_avg$pop[which(polldat_avg$city==city)] = as.numeric(popdata$population[i])
#   }
# }


# #geocode remaining 56 cities
# other_cit <- read.csv("manual_cities.csv", header = TRUE)
# toAppend <- data.frame(other_cit$city, other_cit$lat, other_cit$long)
# colnames(toAppend) = c("city", "lat", "long")
# geocities <- rbind(geocities, toAppend)

#speed up
# polldat_avg <- data.frame(matrix(ncol=8))
# newcols = c(colnames(polldat)[1:5], colnames(polldat)[7:9])
# colnames(polldat_avg) = newcols
# for(city in cities){
#   subset <- polldat[which(polldat$city==city),]
#   tempyrs <- {}
#   for(x in subset$year){
#     if(!(x %in% tempyrs)) tempyrs = c(tempyrs, x)
#   }
#   
#   for(year in tempyrs){
#     print("new start ;)")
#     print(year)
#     polldat_avg = rbind(polldat_avg, c(city, average(subset$so2[which(subset$year==year)]),
#                                        average(subset$no2[which(subset$year==year)]), average(subset$spm[which(subset$year==year)]),
#                                        year, geocities$lat[which(geocities$city==city)], geocities$long[which(geocities$city==city)],
#                                        average(subset$radius[which(subset$year==year)])))
#   }
# }


#make avg col on polldat for input all

# for(i in 1:length(polldat_avg$so2)){
#   polldat_avg$avg[i] = average(c(polldat_avg$so2[i], polldat_avg$no2[i], polldat_avg$spm[i]))
# }
# 
# write.csv(polldat_avg, "pd_avg.csv")

# polldat_avg$date <- as.Date(ISOdate(polldat_avg$year, 1, 1))