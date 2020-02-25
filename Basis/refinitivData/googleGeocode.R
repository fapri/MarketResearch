library(ggmap)

register_google(key = "AIzaSyB93jX1hpjRAf99FBE8ElvVMBsTT14lpYA")


results <- data.frame(address=character(),
                 lat=double(), 
                 long=double(), 
                 stringsAsFactors=FALSE) 

for (row in 1:nrow(spotOnly)) {
  if(row != 72) {
    address = toString(spotOnly[row, "geoFormatAddress"])
    
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    
    
    result = list(address=address, lat=geo_reply$results[[1]]$geometry$location$lat , long=geo_reply$results[[1]]$geometry$location$lng)
    results = rbind(results, result, stringsAsFactors=FALSE)
  }
}

saveRDS(results, "allGoogle.rds")