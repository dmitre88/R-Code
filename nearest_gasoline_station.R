#' Estimate the driving time to the nearest gasoline station
#'
#' Identify the nearest gasoline station based on the driving time. It uses the route gmapsdistance library, which uses the 
#' Google Maps API. As Google restricts the number of requests per day, the function performs the gmapsdistance for the selected
#' rows (begin row until the final row). Note: if the function displays an error, it has to be used the gmapdistance library from
#' github, not from the CRAN. (see https://github.com/rodazuero/gmapsdistance/issues/17)
#' devtools::install_github("rodazuero/gmapsdistance@058009e8d77ca51d8c7dbc6b0e3b622fb7f489a2")
#' 
#' @param data1 A data frame with the variables latitude (lat), longitude (lon), and retailers identifier (id).
#' @param e A value used to find all the neighbors within the specified euclidean distance (e=0.02 equals a radius of ~1.7 km)
#' @param begin_row A value used to select the first row the data1 on to which perform the gmapsdistance.
#' @param final_row A value used to select the last row the data1 on to which perform the gmapsdistance.
#' @param count A value used to perform the gmapsdistance function over "n" stations until it finds the shortest one.
#' @param api The API key provided by Google Maps.
#'
#' @return The function returns the data frame used as input, with the following new variables: minutes (driving time in minutes),
#' km (driving distance in kilometers), euclidean_dist (coordinate distance), nearest_id (nearest retailer identifier).
nearest_gasoline_station<-function(data1, e=0.02, begin_row=1, final_row=800, count=2, api=""){
  
  #Check for variables needed
  if (!"minutes"%in%names(data1)) {data1$minutes<-NA} #create the time variable if it doesnt exist
  if (!"km"%in%names(data1)) {data1$km<-NA} #create the km variable in case if it doesnt exist
  if (!"euclidean_dist"%in%names(data1)) {data1$euclidean_dist<-NA} #create the km variable if it doesnt exist
  if (!"nearest_id"%in%names(data1)) {data1$nearest_id<-NA} #create the nearest_id variable if it doesnt exist
  
  #to keep the API within limits
  route_counter=0
  
  set.api.key(api) #Set api key
  pb<-txtProgressBar(min=begin_row, max=final_row, style=3) #to keep track of the progress
  for (i in begin_row:final_row){
    
    #Extract the i-ID data
    station_i<-as.numeric((data1[i,c("id", "lon", "lat")]))
    
    #Subset that is within a e-distance. It is not included the station_id. They are only keep the id, lon, and lat variables.
    data_subset<-data1[data1$lon>(station_i[2]-e) & data1$lon<(station_i[2]+e) & 
                         data1$lat>(station_i[3]-e) & data1$lat<(station_i[3]+e), c("id", "lon", "lat")]
    data_subset<-data_subset[!data_subset$id%in%station_i[1],]
    
    #If there is no other station near, capture NA and continue to the next iteration, else continue.
    if(nrow(data_subset)==0) { data1[i,c("minutes", "km", "euclidean_dist", "nearest_id")]<-NA }
    if(nrow(data_subset)==0) { next }
    
    #Estimate the euclidean distance
    data_subset$euclidean_dist<-dist(rbind(station_i[2:3], data_subset[,c("lon", "lat")]))[1:nrow(data_subset)]
    data_subset<-data_subset[order(data_subset$euclidean_dist),]
    
    #Driving distance and time between the i observation and the rest of the subset
    data_subset$km<-NA
    data_subset$minutes<-NA
    count<-min(count, nrow(data_subset))
    counter=0
    for (j in 1:nrow(data_subset)){
      
      #To avoid API limits
      if (route_counter==9) { 
        route_counter=0
        Sys.sleep(2)
      }
      
      #Route between points (the dep_date has to be some time in the future)
      route_df<-suppressMessages(
        gmapsdistance(
          origin=paste0(station_i[3], "+", station_i[2]),
          destination=paste0(data_subset[j, "lat"], "+", data_subset[j, "lon"]),
          mode="driving", 
          dep_date = "2018-05-31", 
          dep_time = "17:00:00"
        )
      ) 
      
      
      #To keep count of the API requests
      route_counter=route_counter+1
      
      data_subset[j,"minutes"]<-route_df[[1]]/60 #the output is in minutes
      data_subset[j,"km"]<-route_df[[2]]/1000 #the output is in meters
      #break the loop if the driving time increases as the eucledian distance increases (it has to increased n-times)
      if (j==1) {next} #only can compare the distance from the 2nd observation onwards
      if ( (data_subset[j,"minutes"]-data_subset[j-1,"minutes"])>0 ) {counter=counter+1} #if it is farther, +=1 to counter
      if (counter==count) {break}
    }
    
    #Copy the data from nearest station
    data_subset<-data_subset[order(data_subset$minutes),]
    data1[i,c("minutes","km","euclidean_dist","nearest_id")]<-data_subset[1,c("minutes","km","euclidean_dist","id")]
    
    #To keep track of the progress
    setTxtProgressBar(pb, i)    
  }
  close(pb)
  
  #Return the dataframe with the new variables
  return(data1)
}
