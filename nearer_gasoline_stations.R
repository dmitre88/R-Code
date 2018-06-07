#' Estimate the number of near stations within a specified radius
#'
#' The near stations are considered within a radius based on the euclidean distance.
#' 
#' @param data1 A data frame with the variables latitude (lat), longitude (lon), and retailers identifier (id).
#' @param e A value used to find all the neighbors within the specified radius (e=0.02 equals a radius of ~1.7 km)
#'
#' @return The function returns the data frame used as input, with the following new variables: near_stations (number of near stations),
nearer_gasoline_stations<-function(data1, e=0.01){
  
  #Create the output variable
  data1$near_stations<-NULL
  
  pb<-txtProgressBar(min=1, max=nrow(data1), style=3) #to keep track of the progress
  for (i in 1:nrow(data1)){
    
    #Extract the i-ID data
    station_i<-as.numeric((data1[i,c("id", "lon", "lat")]))
    
    #Subset that is within an e-distance. It is not included the station_id. They are only keep the id, lon, and lat variables.
    data_subset<-data1[data1$lon>(station_i[2]-e) & data1$lon<(station_i[2]+e) & 
                         data1$lat>(station_i[3]-e) & data1$lat<(station_i[3]+e), c("id", "lon", "lat")]
    data_subset<-data_subset[!data_subset$id%in%station_i[1],]
    
    #If there are no other stations near, capture 0
    if(nrow(data_subset)==0) { 
      data1[i, "near_stations"]<-0
      next
    }
    
    #Estimate the euclidean distance and keep those observations within a radius of "e"
    data_subset$euclidean_dist<-dist(rbind(station_i[2:3], data_subset[,c("lon", "lat")]))[1:nrow(data_subset)]
    data_subset<-data_subset[data_subset$euclidean_dist<e,]
    data1[i, "near_stations"]<-nrow(data_subset)
    
    #To keep track of the progress
    setTxtProgressBar(pb, i)    
  }
  close(pb)
  
  #Return the dataframe with he new variables
  return(data1)
}
