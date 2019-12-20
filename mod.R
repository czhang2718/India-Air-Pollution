data = read.csv("pdavg_copy.csv", header=TRUE)
colnames(data) = c("city", "so2", "no2", "spm", "year")
data$year = as.numeric(data$year)
data$city = as.character(data$city)

#get list of cities
cities={}
for(i in 1:dim(data)[1]){
  if(data$city[i]!="" && !(data$city[i]%in% cities)) cities = c(cities, data$city[i])
}



#test for one city, for 2016
temp = data.frame(matrix(ncol=1, nrow=0))
for(j in 33:42) temp = rbind(temp, j)
colnames(temp) = c("year")


tempdat = data[1:2,]

for(cit in cities){
  print(cit)
  pred_so2=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  pred_no2=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  pred_spm=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  
  if(dim(data[which(!is.na(data$so2) & data$city==cit),])[1]>0){
    mod_so2 = lm(so2 ~ year, data = data[which(!is.na(data$so2) & data$city==cit),])
    pred_so2 = predict(mod_so2, temp)
  }
    
  
  if(dim(data[which(!is.na(data$no2) & data$city==cit),])[1]>0){
    mod_no2 = lm(no2 ~ year, data = data[which(!is.na(data$no2) & data$city==cit),])
    pred_no2 = predict(mod_no2, temp)
  }
    
  print (dim(data[which(!is.na(data$spm) & data$city==cit),])[1])
  if(dim(data[which(!is.na(data$spm) & data$city==cit),])[1]>0){
    mod_spm = lm(spm ~ year, data = data[which(!is.na(data$spm) & data$city==cit),])
    pred_spm = predict(mod_spm, temp)
  }
  
  
  for(i in 1:10){
    tempdat = rbind(tempdat, c(cit, max(c(pred_so2[i], 0)),  max(c(pred_no2[i], 0)),  max(c(pred_spm[i], 0)), i+32))
  }
  
  
}

tempdat$year = as.numeric(tempdat$year)-3

write.csv(tempdat, "predicted.csv")


citydat = read.csv("pd_tot.csv", header=TRUE)

for(i in 1:dim(tempdat)[1]){
  loc = tempdat$city[i]
  tempdat$lat[i] = citydat$lat[which(citydat$city==loc)][1]
  tempdat$lng[i] = citydat$lng[which(citydat$city==loc)][1]
  tempdat$radius[i] = citydat$radius[which(citydat$city==loc)][1]
  tempdat$avg[i] = average(c(as.numeric(tempdat$so2[i]), as.numeric(tempdat$no2[i]), as.numeric(tempdat$spm[i])))

  tempdat$pop[i] = citydat$pop[which(citydat$city==loc)][1]
}
tempdat$date = as.Date(ISOdate(tempdat$year, 1, 1))

average <- function(nums){
  total <- 0
  count <- 0
  for(i in 1: length(nums)){
    if(is.numeric(nums[i]) & !is.na(nums[i])){
      print(nums[i])
      count = count+1
      total <- total + nums[i]
    }
  }
  return (total/count)
}

