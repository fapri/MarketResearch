library(ggmap)

register_google(key = "AIzaSyDc2iCQbl7evj8fah3v_phnkwcJQnYoPCQ")


results <- data.frame(address=character(),
                 lat=double(), 
                 long=double(), 
                 stringsAsFactors=FALSE) 

for (row in 1:nrow(notWorking)) {
  if(row != 24) {
    address = toString(notWorking[row, "geoFormatAddress"])
    
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    
    
    result = list(address=address, lat=geo_reply$results[[1]]$geometry$location$lat , long=geo_reply$results[[1]]$geometry$location$lng)
    results = rbind(results, result, stringsAsFactors=FALSE)
  }
}

saveRDS(results, "moreMatches.rds")