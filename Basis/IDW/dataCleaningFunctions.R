# Cleaning Eikon Data 
# Functions for manually editing data


cleanCorn1 = function() {
  # Remove terminals from other states
  spotOnly = spotOnly[-which(spotOnly$instrument == "CORNADMMCN-C1"),]
  spotOnly = spotOnly[-which(spotOnly$instrument == "CORNADMSNV-C1"),]
  
  # Fix Carrolton  zip code
  spotOnly$phoneNumber[which(spotOnly$instrument == "CORNRCGCAR2-C1")] = '"Carrollton, MO - 64633.  Phone - 660-5422-412"'
  # Fix Morehouse zip code
  spotOnly$phoneNumber[which(spotOnly$instrument == "CORNBCTMRH-C1")] = '"Morehouse, MO - 63868.  Phone - 573-6679-921"'
  
  # Fixed row where data was offset
  rowToFix = which(spotOnly$instrument == "CORNGFGBTH-C1")
  spotOnly$phoneNumber[rowToFix] = '"Bethany, MO - 64424, 660-425-3014"'
  spotOnly$address[rowToFix] = "31019 E 260th Ave"
  spotOnly$terminalName[rowToFix] = "Gage Fertilizer and Grain"
  
  # ALL OF THESE ADDRESSES WERE WRONG
  # Novelty ADM
  spotOnly$address[which(spotOnly$instrument == "CORNADMNOV-C1")] = "65031 State Hwy 15"
  # Pattonsburg MFA
  spotOnly$address[which(spotOnly$instrument == "CORNMFAPBG-C1")] = "204 1st St"
  # Unionville MFA
  spotOnly$address[which(spotOnly$instrument == "CORNMFAUNV-C1")] = "520 S 23rd St"
  # Hopkins Green Plains
  spotOnly$address[which(spotOnly$instrument == "CORNGPLHPN-C1")] = "200 N Railroad St"
  # Harrisonville Roth Heriford
  spotOnly$address[which(spotOnly$instrument == "CORNRHFHRL-C1")] = "32503 MO-2"
  # Corder Ray Carroll
  spotOnly$address[which(spotOnly$instrument == "CORNRCGDER-C1")] = "26194 MO-20"
  # Adrian West Central Ag
  spotOnly$address[which(spotOnly$instrument == "CORNWCAADR-C1")] = "438 County Rd 11002"
  # Baring ADM
  spotOnly$address[which(spotOnly$instrument == "CORNADMBIN-C1")] = "101 2nd St"
  # Blackburn Central Missouri AgriService
  spotOnly$address[which(spotOnly$instrument == "CORNCAEBCB-C1")] = "215 W Park St"
  # Caruthersville Bunge
  spotOnly$address[which(spotOnly$instrument == "CORNBNGCRT-C1")] = "100 Ward Ave"
  # Caruthersville Consolidated Grain and Barge
  spotOnly$address[which(spotOnly$instrument == "CORNCGBCRT-C1")] = "2073 Co Rd 337"
  # Centerview West Central Ag
  spotOnly$address[which(spotOnly$instrument == "CORNWCACNV-C1")] = "103 N Main St"
  # La Plata ADM
  spotOnly$address[which(spotOnly$instrument == "CORNADMLAP-C1")] = "131 E Moore St"
  # Louisiana Bunge
  spotOnly$address[which(spotOnly$instrument == "CORNBNGLOU-C1")] = "1035 MO-79"
  # Lucerne	MB Grain
  spotOnly$address[which(spotOnly$instrument == "CORNPSFLUC-C1")] = "14680 US Hwy 136"
  # Malta Bend Central Missouri AgriService
  spotOnly$address[which(spotOnly$instrument == "CORNCAEMBN-C1")] = "1 E Pacific St"
  # Newtown Fowler Elevator
  spotOnly$address[which(spotOnly$instrument == "CORNFWLNTN-C1")] = "301 E Broadway St"
  # Slater Central Missouri AgriService
  spotOnly$address[which(spotOnly$instrument == "CORNCAESLT-C1")] = "305 Industrial Blvd,"
  # Slater Ray Carroll Grain	
  spotOnly$address[which(spotOnly$instrument == "CORNRCGSLT-C1")] = "29261 North Highway 240"
  # Baring Nemo Feed
  spotOnly$address[which(spotOnly$instrument == "CORNNMFBIN-C1")] = "50726 State Hwy 15"
  # Lancaster MFA
  spotOnly$address[which(spotOnly$instrument == "CORNMFALNC-C1")] = "13975 US Hwy 63"
  # Braymer Consumers
  spotOnly$address[which(spotOnly$instrument == "CORNCNMBYM-C1")] = "100 Railroad street"
  # Craig Grain
  spotOnly$address[which(spotOnly$instrument == "CORNCGICRA-C1")] = "102 Main St"
  # Concordia MFA
  spotOnly$address[which(spotOnly$instrument == "CORNMFACNC-C1")] = "601 S Main St"
  # Lamar MFA
  spotOnly$address[which(spotOnly$instrument == "CORNMFALMR-C1")] = "1901 Hwy Kk"
  # Charleston Consolidated Grain and Barge
  spotOnly$address[which(spotOnly$instrument == "CORNCGBCHO-C1")] = "6720 N Hwy K"
  # Burlington Junction MFA
  spotOnly$address[which(spotOnly$instrument == "CORNMFABRL-C1")] = "1101 W Main St"
  
  # # Template for fixing addresses
  # # 
  # spotOnly$address[which(spotOnly$instrument == "-C1")] = ""
  
  return(spotOnly)
}

cleanCorn2 = function(){
  # Fix Brunswick Ray Carroll lat/long
  finalSet[which(finalSet$instrument == "CORNRCGBRU-C1"), c("lat", "long")] = c(39.430464, -93.147535)
  # Fix Laredo MFA lat/long
  finalSet[which(finalSet$instrument == "CORNMFALRD-C1"), c("lat", "long")] = c(40.026754, -93.444296)
  # Fix Burlington Junction MFA lat/long
  finalSet[which(finalSet$instrument == "CORNMFABRL-C1"), c("lat", "long")] = c(40.445430, -95.077368)
  # Fix Carrolton lat/long
  finalSet[which(finalSet$instrument == "CORNRCGCAR2-C1"), c("lat", "long")] = c(39.313495, -93.376687)
  
  return(finalSet)
}









cleanSoybean1 = function() {
  # Remove terminals from other states
  spotOnly = spotOnly[-which(spotOnly$instrument == "SOYBADMMCN-C1"),]
  
  # Fix Carrolton zip code
  spotOnly$phoneNumber[which(spotOnly$instrument == "SOYBRCGCAR2-C1")] = '"Carrollton, MO - 64633.  Phone - 660-5422-412"'
  # Fix Morehouse zip code
  spotOnly$phoneNumber[which(spotOnly$instrument == "SOYBBCTMRH-C1")] = '"Morehouse, MO - 63868.  Phone - 573-6679-921"'

  # Fixed row where data was duplicated
  spotOnly = spotOnly[-which(spotOnly$phoneNumber == "660-425-3014"),]
  
  # ALL OF THESE ADDRESSES WERE WRONG
  # Novelty ADM
  spotOnly$address[which(spotOnly$instrument == "SOYBADMNOV-C1")] = "65031 State Hwy 15"
  # Pattonsburg MFA
  spotOnly$address[which(spotOnly$instrument == "SOYBMFAPBG-C1")] = "204 1st St"
  # Unionville MFA
  spotOnly$address[which(spotOnly$instrument == "SOYBMFAUNV-C1")] = "520 S 23rd St"
  # Harrisonville Roth Heriford
  spotOnly$address[which(spotOnly$instrument == "SOYBRHFHRL-C1")] = "32503 MO-2"
  # Corder Ray Carroll
  spotOnly$address[which(spotOnly$instrument == "SOYBRCGDER-C1")] = "26194 MO-20"
  # Adrian West Central Ag
  spotOnly$address[which(spotOnly$instrument == "SOYBWCAADR-C1")] = "438 County Rd 11002"
  # Baring ADM
  spotOnly$address[which(spotOnly$instrument == "SOYBADMBIN-C1")] = "101 2nd St"
  # Blackburn Central Missouri AgriService
  spotOnly$address[which(spotOnly$instrument == "SOYBCAEBCB-C1")] = "215 W Park St"
  # Caruthersville Bunge
  spotOnly$address[which(spotOnly$instrument == "SOYBBNGCRT-C1")] = "100 Ward Ave"
  # Caruthersville Consolidated Grain and Barge
  spotOnly$address[which(spotOnly$instrument == "SOYBCGBCRT-C1")] = "2073 Co Rd 337"
  # Centerview West Central Ag
  spotOnly$address[which(spotOnly$instrument == "SOYBWCACNV-C1")] = "103 N Main St"
  # La Plata ADM
  spotOnly$address[which(spotOnly$instrument == "SOYBADMLAP-C1")] = "131 E Moore St"
  # Louisiana Bunge
  spotOnly$address[which(spotOnly$instrument == "SOYBBNGLOU-C1")] = "1035 MO-79"
  # Malta Bend Central Missouri AgriService
  spotOnly$address[which(spotOnly$instrument == "SOYBCAEMBN-C1")] = "1 E Pacific St"
  # Newtown Fowler Elevator
  spotOnly$address[which(spotOnly$instrument == "SOYBFWLNTN-C1")] = "301 E Broadway St"
  # Slater Central Missouri AgriService
  spotOnly$address[which(spotOnly$instrument == "SOYBCAESLT-C1")] = "305 Industrial Blvd,"
  # Slater Ray Carroll Grain	
  spotOnly$address[which(spotOnly$instrument == "SOYBRCGSLT-C1")] = "29261 North Highway 240"
  # Lancaster MFA
  spotOnly$address[which(spotOnly$instrument == "SOYBMFALNC-C1")] = "13975 US Hwy 63"
  # Braymer Consumers
  spotOnly$address[which(spotOnly$instrument == "SOYBCNMBYM-C1")] = "100 Railroad street"
  # Craig Grain
  spotOnly$address[which(spotOnly$instrument == "SOYBCGICRA-C1")] = "102 Main St"
  # Concordia MFA
  spotOnly$address[which(spotOnly$instrument == "SOYBMFACNC-C1")] = "601 S Main St"
  # Lamar MFA
  spotOnly$address[which(spotOnly$instrument == "SOYBMFALMR-C1")] = "1901 Hwy Kk"
  # Charleston Consolidated Grain and Barge
  spotOnly$address[which(spotOnly$instrument == "SOYBCGBCHO-C1")] = "6720 N Hwy K"
  # Burlington Junction MFA
  spotOnly$address[which(spotOnly$instrument == "SOYBMFABRL-C1")] = "1101 W Main St"
  
  # # Template for fixing addresses
  # # 
  # spotOnly$address[which(spotOnly$instrument == "-C1")] = ""
  
  return(spotOnly)
}

cleanSoybean2 = function(){
  # Fix Brunswick Ray Carroll lat/long
  finalSet[which(finalSet$instrument == "SOYBRCGBRU-C1"), c("lat", "long")] = c(39.430464, -93.147535)
  # Fix Laredo MFA lat/long
  finalSet[which(finalSet$instrument == "SOYBMFALRD-C1"), c("lat", "long")] = c(40.026754, -93.444296)
  # Fix Burlington Junction MFA lat/long
  finalSet[which(finalSet$instrument == "SOYBMFABRL-C1"), c("lat", "long")] = c(40.445430, -95.077368)
  # Fix Carrolton lat/long
  finalSet[which(finalSet$instrument == "SOYBRCGCAR2-C1"), c("lat", "long")] = c(39.313495, -93.376687)
  
  return(finalSet)
}



