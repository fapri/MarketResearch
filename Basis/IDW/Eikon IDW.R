# Cleaning Eikon Data


# Load libraries
library(readr)
library(tmap)
library(tmaptools)
library(stringr)

# Load data
allBasis = read_csv("Basis/refinitivData/allBasis.csv")
spotOnly = read_csv("Basis/refinitivData/spotOnly.csv", 
                    col_types = cols(X1 = col_skip()))

# Remove extra columns
spotOnly = subset(spotOnly, select = -c(GEN_TEXT16, Location))

# Cahnge column names
colnames(spotOnly) = c("instrument", "contractName", "basis", "date", "terminalName", "address", "county", "cropType", "phoneNumber")

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
spotOnly$address[which(spotOnly$instrument == "CORNMFACNC-C1")] = "601 S Main St, 64020"
# Lamar MFA
spotOnly$address[which(spotOnly$instrument == "CORNMFALMR-C1")] = "1901 Hwy Kk, 64759"




# # Template for fixing addresses
# # 
# spotOnly$address[which(spotOnly$instrument == "-C1")] = ""




# Get zip codes
spotOnly$zipCode = str_extract(spotOnly$phoneNumber, "\\d{5}")

# Paste zip codes to address to send to geocoder
spotOnly$geoFormatAddress = paste(spotOnly$address, spotOnly$zipCode, sep = ", ")

# Get lat/lon from geocoder OSM
x = data.frame(geocode_OSM(spotOnly$geoFormatAddress))



# # Get lat/lon from geocoder GOOGLE
# geocode(spotOnly$geoFormatAddress)



# Get rows that didn't geocode
notWorking = spotOnly[which(!spotOnly$geoFormatAddress %in% x$query), ]



