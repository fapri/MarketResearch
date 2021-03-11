# Emily Scully, with adjustments
# Started  March 31, 2020
# Revisions from May 15, 2020
# February 9, 2021: Fixed formatting issues and added feature to save images
# Project: Decoupling Data Organization and Tables

# Load libraries
library(readxl)
library(readr)
library(dplyr)
library(svDialogs)
library(stringr)
library(testit)
library(tidyr)
library(flextable)
library(officer)
library(tidyverse)

# Load data from markdown file
#setwd('C:/Users/gerlts/downloads')
# setwd('D:/Art/Decoupling/2018 OCE/LitRev/DataArticles/RCode_Tables')
DcData = read_excel("DecouplingLitDataNew.xlsx",
                    col_names = FALSE)
# # From R file
# DcData = read_excel("Decoupling/Updates/DecouplingLitDataNew.xlsx",
#                     col_names = FALSE)

# Renumber columns
colnames(DcData) = paste0("c", seq(from = 1, to = ncol(DcData), by = 1))

# Remove NA rows
DcData = DcData[rowSums(is.na(DcData)) != ncol(DcData),]

# Carry "notes" titles down to rows with notes
for (row in 1:(nrow(DcData) - 1)) {
  if (length(grep("Notes", DcData$c2[row])) > 0 & is.na(DcData$c1[row + 1]) & is.na(DcData$c2[row + 1])) {
    DcData$c2[row + 1] = "Notes"
  }
}

# Clean data
DcData = DcData[-grep("Notes", DcData$c2), ]
DcData$c1[grep("Nature of supply variable", DcData$c2)] = "Nature of supply variable"
DcData$c2[grep("Nature of supply variable", DcData$c2)] = NA
DcData$c1[grep("Cross effects among crops other than", DcData$c2)] = "Other cross effects"
DcData$c2[grep("Cross effects among crops other than", DcData$c2)] = NA

# fills in main categories so they can be matched to subcategories and to remove NAs
for (row in 1:(nrow(DcData) - 1)) {
  if (!is.na(DcData$c1[row]) & is.na(DcData$c1[row + 1])) {
    DcData$c1[row + 1] = DcData$c1[row]
  }
}

# Remove rows with only NAs
DcData = DcData[-(which(is.na(DcData$c2[9:nrow(DcData)])) + 8), ]

# removes () and any info in them
DcData$c2 = gsub("\\s*\\([^\\)]+\\)","", as.character(DcData$c2))

# removes punctuation
DcData$c1 = gsub("[[:punct:]]+","", DcData$c1)
DcData$c2 = gsub("[[:punct:]]+","", DcData$c2)

# formatting/standardizing
DcData$c1 = str_to_title(DcData$c1)
DcData$c2 = str_to_title(DcData$c2)
DcData$c1 = gsub(" ", "", DcData$c1)
DcData$c2 = gsub(" ", "", DcData$c2)

# Shortening names that are excessively long
DcData$c1[grep("ProgramToWhichResultsRelate", DcData$c1)] = "Program"
DcData$c1[grep("NatureOfSupplyVariable", DcData$c1)] = "NatureOfSupply"
DcData$c2[grep("Peerreviewed", DcData$c2)] = "PeerReviewed"
DcData$c2[grep("EstimatedMarketData", DcData$c2)] = "EstMarketData"
DcData$c2[grep("EstimatedSurveyData", DcData$c2)] = "EstSurveyData"
DcData$c2[grep("EstimatedBalancedPanelData", DcData$c2)] = "EstBalPanelData"
DcData$c2[grep("EstimatedUnbalancedPanelData", DcData$c2)] = "EstUnbalPanelData"
DcData$c2[grep("AllOfUnitedStates", DcData$c2)] = "AllOfUS"
DcData$c2[grep("SomePartOfUnitedStates", DcData$c2)] = "PartOfUS"
DcData$c2[grep("IfSoStateTwodigitPostCode", DcData$c2)] = "StatePostCode"
DcData$c2[grep("OtherCountryOrCountries", DcData$c2)] = "OtherCountries"
DcData$c2[grep("ListCropOrCrops", DcData$c2)] = "CropOrCrops"
DcData$c2[grep("PercentChangeInOutputPerunitPaymentDollarOrEuro", DcData$c2)] = "PctChangeOutputPerunitPmt"
DcData$c2[grep("RatioOfPaymentImpactToMarketImpact", DcData$c2)] = "RatioOfPmtImpToMarketImp"

# merge the two categories of labels together, separated by _
DcData$c3 = paste(DcData$c1, DcData$c2, sep = "_")

# Convert the data to a list for key/value access
tDf = t(DcData)
colnames(tDf) = row.names(tDf) = NULL

# Converts each data frame row into a list
# This is helpful for being able to call the variables by 
# their key, rather than col/row notation
DcList = lapply(seq_len(ncol(tDf[4:nrow(tDf), ])), function(i) tDf[4:nrow(tDf), ][,i])
names(DcList) = tDf[3, ]

# Convert to numeric where applicable
DcList = lapply(DcList, function(col) {
  if (!has_warning(as.numeric(as.character(col)))) {
    as.numeric(as.character(col))
  } else {
    col
  }
})

# Create temp vector of the avenues
# Originally  "UpdatingAndExpectations_", but ideally will change name to "ExpectationsofBaseUpdating_"
# Originally  "All_", but ideally will change name to "Non-Specific_"
#             
tempAvenues = c("PriceEffect_",
                "RiskReduction_",
                "RiskAndWealth_",
                "UpdatingAndExpectations_",
                "OtherEffects_",
                "All_")

# Create vector of values needed for table
factors = c("Relavancy",
            "PCOPUP",
            "PI2MI",
            "ElasMarket",
            "ElasPayment")

# match each avenue to all factors
avenues = vector()
for (i in seq(length(tempAvenues))) {
  avenues = c(avenues, paste0(tempAvenues[i], factors))
}

# Convert to a list of lists for key/value access
templateList = list()
for (i in avenues) {
  templateList[[i]] = list("data" = NA, "studyIndex" = NA,"count" = NA, "studyCount" = NA, "median" = NA,
                           "simpleAvg" = NA, "studyWeightAvg" = NA, "studyAiWeightAvg" = NA)
}

# Initialize lists
subLists = list("usNat" = templateList,
                "cornBelt" = templateList,
                "otherRegion" = templateList,
                "estPS" = templateList,
                "estMD" = templateList,
                "simuOrTheory" = templateList,
                "all" = templateList,
                "allCrossEffect" = templateList,
                "allCrops" = templateList,
                "oneCrop" = templateList)

# usNat
# cornBelt
# otherRegion
# estPS
# estMD
# simuOrTheory
# all
# allCrossEffect
# allCrops
# oneCrop

####################### CALCULATIONS #######################

# Needed if you run the R code outside of the rmarkdown
# # Set program choices for user
# programChoices = c("All during period",
#                    "Crop insurance",
#                    "ARC",
#                    "PLC",
#                    "SCO",
#                    "CCP",
#                    "ACRE",
#                    "Market loss assistance",
#                    "Fixed direct payment (contract payment)",
#                    "Marketing Loan program",
#                    "Milk Income Loss Contract (MILC)",
#                    "Margin Protection Program",
#                    "Pre-1996 US policy",
#                    "Other"
# )
# 
# # Set supply choices for user
# supplyChoices = c("Area of a Crop or Crops",
#                   "Yield",
#                   "Production of a Crop or Crops")
# 
# # Dialogue boxes for user input
# # selectedProgramText = dlgList(programChoices, preselect = NULL, multiple = FALSE,
# #                           title = "Program Type")$res
# selectedProgramText = "All during period"
# 
# # Dialogue boxes for user input
# # selectedSupplyText = dlgList(supplyChoices, preselect = NULL, multiple = FALSE,
# #                          title = "Supply Type")$res
# selectedSupplyText = "Area of a Crop or Crops"

# Convert user selection to list key name for access by name
selectedProgram = switch(selectedProgramText,
                         "All during period" = "Program_AllDuringPeriod",
                         "Crop insurance" = "Program_CropInsurance",
                         "ARC" = "Program_AgriculturalRiskCoverage",
                         "PLC" = "Program_PriceLossCoverate",
                         "SCO" = "Program_SupplementalCoverageOption",
                         "CCP" = "Program_CountercyclicalPayments",
                         "ACRE"  = "Program_AverageCropRevenueElection",
                         "Market loss assistance" = "Program_MarketLossAssistance",
                         "Fixed direct payment (contract payment)" = "Program_FixedDirectPayment",
                         "Marketing Loan program" = "Program_MarketingLoanProgram",
                         "Milk Income Loss Contract (MILC)" = "Program_MilkIncomeLossContract",
                         "Margin Protection Program" = "Program_MarginProtectionProgram",
                         "Pre-1996 US policy" = "Program_Pre1996UsPolicy",
                         "Other" = "Program_Other"
)

# Convert user selection to list key name for access by name
selectedSupply = switch(selectedSupplyText,
                        "Area of a Crop or Crops" = "area",
                        "Yield" = "yield",
                        "Production of a Crop or Crops" = "production"
)

# Extract dummy variable for what type of program is used
programMaster = DcList[[selectedProgram]]

# Convert NAs to 0 for selected supply type
areaFunction <- function(x) {
  result <- NULL
  
  if (x == 'area') {
    result <- replace_na(DcList[["NatureOfSupply_AreaOfACrop"]],0) +
      replace_na(DcList[["NatureOfSupply_AreaOfAllOrManyCrops"]],0)
  }
  
  if (x == 'yield') result <- replace_na(DcList[["NatureOfSupply_Yield"]], 0)
  
  if (x == 'production') {
    result <- replace_na(DcList[["NatureOfSupply_ProductionOfACrop"]],0) +
      replace_na(DcList[["NatureOfSupply_ProductionOfAllCrops"]],0)
  }
  result <- sapply(result, function(y) ifelse(y>0,1,NA))
  
  return(result)
}
areaMaster <- areaFunction(selectedSupply)

# Convert NA to 0 for other cross effects
DcList[["OtherCrossEffects_IsThisColumnACrosseffect"]] = 
  replace_na(DcList[["OtherCrossEffects_IsThisColumnACrosseffect"]], 0)

# Get cross effect indexes
noCrossEffect = as.numeric(gsub(0, NA, 1 - DcList[["OtherCrossEffects_IsThisColumnACrosseffect"]]))
crossEffect = as.numeric(gsub(0, NA, DcList[["OtherCrossEffects_IsThisColumnACrosseffect"]]))

# Create sublists of the calculations
calcLists = function(subList, programId, tableType) {
  # Extra factors are other conditions to consider, such as studies covering all of the US
  if (programId == "all") {
    extraFactors = 1
    crossEffectFactor = noCrossEffect
  } else if (programId == "estPS") { # Estimation: Panel or survey
    panelSurveySum = replace_na(DcList[["Method_EstSurveyData"]], 0) + 
      replace_na(DcList[["Method_EstBalPanelData"]], 0) + 
      replace_na(DcList[["Method_EstUnbalPanelData"]], 0)
    extraFactors = as.numeric(gsub(0, NA, panelSurveySum))
    crossEffectFactor = noCrossEffect
  } else if (programId == "estMD") { # Estimation: Market Data
    extraFactors = DcList[["Method_EstMarketData"]]
    crossEffectFactor = noCrossEffect
  } else if (programId == "simuOrTheory") { # Method: Simulation or Theory
    simuOrTheorySum = replace_na(DcList[["Method_Simulation"]], 0) + 
      replace_na(DcList[["Method_Theory"]], 0)
    extraFactors = as.numeric(gsub(0, NA, simuOrTheorySum))
    crossEffectFactor = noCrossEffect
  } else if (programId == "usNat") { # Us national
    extraFactors = DcList[["Region_AllOfUS"]]
    crossEffectFactor = noCrossEffect
  } else if (programId == "cornBelt") { 
    extraFactors = DcList[["Region_CornBelt"]]
    crossEffectFactor = noCrossEffect
  } else if (programId == "otherRegion") { # Other Regions
    # ORIGINALLY exclude the US and Corn Belt (Region_CornBelt), 
    #          but instead exclude US and part of US (PartOfUS)
    otherRelavancy = replace_na(DcList[["Region_AllOfUS"]], 0) + 
      replace_na(DcList[["Region_PartOfUS"]], 0)
    extraFactors = as.numeric(gsub(0, NA, (1 - otherRelavancy)))
    crossEffectFactor = noCrossEffect
  } else if (programId == "allCrossEffect") { 
    extraFactors = 1
    crossEffectFactor = crossEffect
  } else if (programId == "allCrops") { 
    extraFactors = 1
    if (tableType == "area") {
      #extraFactors = DcList[["NatureOfSupply_AreaOfAllOrManyCrops"]]
      crossEffectFactor = 1
    } else if (tableType == "production") {
      #extraFactors = DcList[["NatureOfSupply_ProductionOfAllCrops"]]
      crossEffectFactor = 1
    } else if (tableType == "yield") {
      #extraFactors = NA
      crossEffectFactor = NA
    }
  } else if (programId == "oneCrop") { 
    extraFactors = 1
    if (tableType == "area") {
      #extraFactors = DcList[["NatureOfSupply_AreaOfACrop"]]
      crossEffectFactor = 1
    } else if (tableType == "production") {
      #extraFactors = DcList[["NatureOfSupply_ProductionOfACrop"]]
      crossEffectFactor = 1
    } else if (tableType == "yield") {
      #extraFactors = DcList[["NatureOfSupply_Yield"]]
      crossEffectFactor = 1
    }
  }
  
  # Pull relavancy and apply it to the PCOPUP and PI2MI data
  for (i in grep("Relavancy", names(subList))) {
    avenue = which((sub("\\_.*", "", names(subList))) == sub("\\_.*", "", names((subList[i]))))
    
    Relavancy = grep("Relavancy", names(subList[avenue]))
    PCOPUP = grep("PCOPUP", names(subList[avenue]))
    PI2MI = grep("PI2MI", names(subList[avenue]))
    ElasMarket = grep("ElasMarket", names(subList[avenue]))
    ElasPayment = grep("ElasPayment", names(subList[avenue]))
    
    subList[[avenue[Relavancy]]][["data"]] = programMaster * areaMaster * extraFactors * crossEffectFactor *
      DcList[[which((sub(".*_", "", names(DcList))) == sub("\\_.*", "", names((subList[i]))))]]
    subList[[avenue[PCOPUP]]][["data"]] = subList[[avenue[Relavancy]]]$data * (DcList[["ImpactOfPayments_ChangeInOutputPerPayment"]])
    subList[[avenue[PI2MI]]][["data"]] = subList[[avenue[Relavancy]]]$data * DcList[["Ratio_RatioOfPmtImpToMarketImp"]]
    subList[[avenue[ElasMarket]]][["data"]] = subList[[avenue[Relavancy]]]$data * DcList[["Elasticities_Market"]]
    subList[[avenue[ElasPayment]]][["data"]] = subList[[avenue[Relavancy]]]$data * DcList[["Elasticities_Payment"]]
    
    dataIncludedIndexPCOPUP = rep(NA, times = length(subList[[avenue[Relavancy]]]$data))
    dataIncludedIndexPI2MI = rep(NA, times = length(subList[[avenue[Relavancy]]]$data))
    dataIncludedIndexElasMarket = rep(NA, times = length(subList[[avenue[Relavancy]]]$data))
    dataIncludedIndexElasPayment = rep(NA, times = length(subList[[avenue[Relavancy]]]$data))
    
    dataIncludedIndexPCOPUP[which(!is.na(subList[[avenue[PCOPUP]]]$data))] = 1
    dataIncludedIndexPI2MI[which(!is.na(subList[[avenue[PI2MI]]]$data))] = 1
    dataIncludedIndexElasMarket[which(!is.na(subList[[avenue[ElasMarket]]]$data))] = 1
    dataIncludedIndexElasPayment[which(!is.na(subList[[avenue[ElasPayment]]]$data))] = 1
    
    subList[[avenue[Relavancy]]]$studyIndex = subList[[avenue[Relavancy]]]$data * DcList[["Study_NA"]]
    subList[[avenue[PCOPUP]]]$studyIndex = dataIncludedIndexPCOPUP * DcList[["Study_NA"]]
    subList[[avenue[PI2MI]]]$studyIndex = dataIncludedIndexPI2MI * DcList[["Study_NA"]]
    subList[[avenue[ElasMarket]]]$studyIndex = dataIncludedIndexElasMarket * DcList[["Study_NA"]]
    subList[[avenue[ElasPayment]]]$studyIndex = dataIncludedIndexElasPayment * DcList[["Study_NA"]]
  }
  return(subList)
}
#WT: revised above for change to table by replacing the following row with the ImpactOfPayments_ChangeOutputPerPmt
#subList[[avenue[PCOPUP]]][["data"]] = subList[[avenue[Relavancy]]]$data * (DcList[["ImpactOfPayments_PctChangeOutputPerunitPmt"]] * 100)


# Run the function for all the sublists
for (i in seq_len(length(subLists))) {
  subLists[[i]] = calcLists(subLists[[i]], names(subLists)[i], selectedSupply)
}

# Calculates the number of observations triggered and averages the values
calcStats = function(subList) {
  for (i in names(subList)) {
    
    countIndex = which(!is.na(subList[[i]][["data"]]))
    subList[[i]]$count = length(countIndex)
    subList[[i]]$studyCount = length(unique(na.omit(subList[[i]][["studyIndex"]][countIndex])))
    
    if (subList[[i]]$count > 0) {
      subList[[i]]$simpleAvg = mean(subList[[i]][["data"]], na.rm = TRUE)
      subList[[i]]$median = median(subList[[i]][["data"]], na.rm = TRUE)
      
      subList[[i]]$studyWeightAvg = na.omit(data.frame("index" = subList[[i]][["studyIndex"]], 
                                                       "data" = subList[[i]][["data"]])) %>% 
        group_by(index) %>% 
        summarise(average = mean(data, na.rm = TRUE)) %>%
        summarise(newAvg = mean(average, na.rm = TRUE)) %>%
        pull(newAvg)
      
      subList[[i]]$studyAiWeightAvg = NA
      
    } else {
      # Ensure case where there are studies available but no data
      # do not get included in the table
      subList[[i]]$count = 0
      subList[[i]]$studyCount = 0
      
      subList[[i]]$simpleAvg = NA
      subList[[i]]$median = NA
      subList[[i]]$studyWeightAvg = NA
      subList[[i]]$studyAiWeightAvg = NA
    }
  }
  return(subList)
}

# Run the function for all the sublists
for (i in seq_len(length(subLists))) {
  subLists[[i]] = calcStats(subLists[[i]])
}


###############################################################


# Template for loading data into the table
# tableTemplate = read_excel("Decoupling/Updates/tableTemplate.xlsx", col_names = TRUE, sheet = 1)
tableTemplate = read_excel("tableTemplate.xlsx", col_names = TRUE, sheet = 1)
tableTemplate = as.data.frame(tableTemplate)

# column pattern
# 1 Price Effect
# 2 Risk Reduction
# 3 Risk and Wealth
# 4 Expectations of Base Updating
# 5 Other 
# 6 Not Specific

# ORIGINALLY rounded to 2 digits. Try 3 digits here
nearZero = function(value) {
  if (is.na(value)) {
    return(NA)
  } else if (round(value, digits = 3) == 0) {
    return("~0")
  } else {
    return(round(value, digits = 3))
  }
}

# Get appropriate column
cols1 =  grep(pattern = "1", x = tableTemplate[1, ]) # obs/simple avg
cols2 =  grep(pattern = "2", x = tableTemplate[1, ]) # study/study weighted avg
cols3 =  grep(pattern = "3", x = tableTemplate[1, ]) # median/Ai weighted avg

for (name in names(subLists)) {
  PCOPUPindexes = grep(pattern = "PCOPUP", x = names(subLists[[name]]))
  PI2MIindexes = grep(pattern = "PI2MI", x = names(subLists[[name]]))
  ElasMarketindexes = grep(pattern = "ElasMarket", x = names(subLists[[name]]))
  ElasPaymentindexes = grep(pattern = "ElasPayment", x = names(subLists[[name]]))
  
  # Gets the rows for PCOPUP
  PCOPUProw1 = grep(pattern = paste0(name, 1), x = tableTemplate$index)[1]
  PCOPUProw2 = grep(pattern = paste0(name, 2), x = tableTemplate$index)[1]
  # Gets the rows for PI2MI
  PI2MIrow1 = grep(pattern = paste0(name, 1), x = tableTemplate$index)[2]
  PI2MIrow2 = grep(pattern = paste0(name, 2), x = tableTemplate$index)[2]
  # Gets the rows for ElasMarket
  ElasMarketrow1 = grep(pattern = paste0(name, 1), x = tableTemplate$index)[3]
  ElasMarketrow2 = grep(pattern = paste0(name, 2), x = tableTemplate$index)[3]
  # Gets the rows for ElasPayment
  ElasPaymentrow1 = grep(pattern = paste0(name, 1), x = tableTemplate$index)[4]
  ElasPaymentrow2 = grep(pattern = paste0(name, 2), x = tableTemplate$index)[4]
  
  equalityOfLength = all(sapply(list(length(subLists[[name]][PCOPUPindexes]), 
                                     length(subLists[[name]][PI2MIindexes]),
                                     length(subLists[[name]][ElasMarketindexes]), 
                                     length(subLists[[name]][ElasPaymentindexes]),
                                     length(cols1), length(cols2), 
                                     length(cols3)), function(x) 
                                       x == length(subLists[[name]][PCOPUPindexes])))
  
  if (equalityOfLength) {
    for (i in seq_len(length(subLists[[name]][PCOPUPindexes]))) {
      
      # Fill values for case count and study count
      if (subLists[[name]][[PCOPUPindexes[i]]]$count == 0 & subLists[[name]][[PCOPUPindexes[i]]]$studyCount == 0) {
        tableTemplate[PCOPUProw1, cols1[i]] = NA
        tableTemplate[PCOPUProw1, cols2[i]] = NA
      } else {
        tableTemplate[PCOPUProw1, cols1[i]] = subLists[[name]][[PCOPUPindexes[i]]]$count
        tableTemplate[PCOPUProw1, cols2[i]] = subLists[[name]][[PCOPUPindexes[i]]]$studyCount
      }
      
      if (subLists[[name]][[PI2MIindexes[i]]]$count == 0 & subLists[[name]][[PI2MIindexes[i]]]$studyCount == 0) {
        tableTemplate[PI2MIrow1, cols1[i]] = NA
        tableTemplate[PI2MIrow1, cols2[i]] = NA
      } else {
        tableTemplate[PI2MIrow1, cols1[i]] = subLists[[name]][[PI2MIindexes[i]]]$count
        tableTemplate[PI2MIrow1, cols2[i]] = subLists[[name]][[PI2MIindexes[i]]]$studyCount
      }
      
      if (subLists[[name]][[ElasMarketindexes[i]]]$count == 0 & subLists[[name]][[ElasMarketindexes[i]]]$studyCount == 0) {
        tableTemplate[ElasMarketrow1, cols1[i]] = NA
        tableTemplate[ElasMarketrow1, cols2[i]] = NA
      } else {
        tableTemplate[ElasMarketrow1, cols1[i]] = subLists[[name]][[ElasMarketindexes[i]]]$count
        tableTemplate[ElasMarketrow1, cols2[i]] = subLists[[name]][[ElasMarketindexes[i]]]$studyCount
      }
      
      if (subLists[[name]][[ElasPaymentindexes[i]]]$count == 0 & subLists[[name]][[ElasPaymentindexes[i]]]$studyCount == 0) {
        tableTemplate[ElasPaymentrow1, cols1[i]] = NA
        tableTemplate[ElasPaymentrow1, cols2[i]] = NA
      } else {
        tableTemplate[ElasPaymentrow1, cols1[i]] = subLists[[name]][[ElasPaymentindexes[i]]]$count
        tableTemplate[ElasPaymentrow1, cols2[i]] = subLists[[name]][[ElasPaymentindexes[i]]]$studyCount
      }
      
      # cell c
      tableTemplate[PCOPUProw1, cols3[i]] = nearZero(subLists[[name]][[PCOPUPindexes[i]]]$median)
      tableTemplate[PI2MIrow1, cols3[i]] = nearZero(subLists[[name]][[PI2MIindexes[i]]]$median)
      tableTemplate[ElasMarketrow1, cols3[i]] = nearZero(subLists[[name]][[ElasMarketindexes[i]]]$median)
      tableTemplate[ElasPaymentrow1, cols3[i]] = nearZero(subLists[[name]][[ElasPaymentindexes[i]]]$median)
      
      # cell d
      tableTemplate[PCOPUProw2, cols1[i]] = nearZero(subLists[[name]][[PCOPUPindexes[i]]]$simpleAvg)
      tableTemplate[PI2MIrow2, cols1[i]] = nearZero(subLists[[name]][[PI2MIindexes[i]]]$simpleAvg)
      tableTemplate[ElasMarketrow2, cols1[i]] = nearZero(subLists[[name]][[ElasMarketindexes[i]]]$simpleAvg)
      tableTemplate[ElasPaymentrow2, cols1[i]] = nearZero(subLists[[name]][[ElasPaymentindexes[i]]]$simpleAvg)
      
      # cell e
      tableTemplate[PCOPUProw2, cols2[i]] = nearZero(subLists[[name]][[PCOPUPindexes[i]]]$studyWeightAvg)
      tableTemplate[PI2MIrow2, cols2[i]] = nearZero(subLists[[name]][[PI2MIindexes[i]]]$studyWeightAvg)
      tableTemplate[ElasMarketrow2, cols2[i]] = nearZero(subLists[[name]][[ElasMarketindexes[i]]]$studyWeightAvg)
      tableTemplate[ElasPaymentrow2, cols2[i]] = nearZero(subLists[[name]][[ElasPaymentindexes[i]]]$studyWeightAvg)
      
      # cell f
      tableTemplate[PCOPUProw2, cols3[i]] = NA
      tableTemplate[PI2MIrow2, cols3[i]] = NA
      tableTemplate[ElasMarketrow2, cols3[i]] = NA
      tableTemplate[ElasPaymentrow2, cols3[i]] = NA
      
    }
  } else {
    stop("Sublist length and column length are not equal. Make sure that any added avenues 
         for one (either sublist or table) were also added properly to the other.")
  }
} 



###############################################################

# Delete column
tableTemplate = subset(tableTemplate, select = -(index))

# Delete row
tableTemplate = tableTemplate[-1,]

# Dynamic program identifier - necessary for the table header
# WT: try remove 5/28, but necessary?
# tableTemplate[c(1, 36), ] = NA

tableTemplate = tableTemplate[rowSums(is.na(tableTemplate)) != ncol(tableTemplate), ]

rownames(tableTemplate) = NULL

# Function to create and format the flex tables
makeTables = function(myft) {
  # Merge duplicate columns, row-wise
  myft = merge_at(myft, i = 1, j = 1:10)
  myft = merge_at(myft, i = 2, j = 2:3)
  # Method
  myft = merge_at(myft, i = 9, j = 2:3)
  # All
  myft = merge_at(myft, i = 16, j = 2:3)
  # Nature of Effect
  myft = merge_at(myft, i = 19, j = 2:3)
  myft = merge_v(myft, j = 3)
  
  # Rename columns
  # These cannot be put into the template because the column names would not be unique
  # originally "Updating and expectations" for V13-v15, but ideally want "Expectations of Base Updating"
  # originally "All" for V19-v21, but ideally want "Non-Specific"
  myft = set_header_labels(myft, values = list(V1 = " ",
                                               V2 = " ",
                                               V3 = " ",
                                               V4 = "Price Effect",
                                               V5 = "Price Effect",
                                               V6 = "Price Effect",
                                               V7 = "Risk Reduction",
                                               V8 = "Risk Reduction",
                                               V9 = "Risk Reduction",
                                               V10 = "Risk and Wealth",
                                               V11 = "Risk and Wealth",
                                               V12 = "Risk and Wealth",
                                               V13 = "Expectations of \nBase Updating",
                                               V14 = "Expectations of \nBase Updating",
                                               V15 = "Expectations of \nBase Updating",
                                               V16 = "Other Effect",
                                               V17 = "Other Effect",
                                               V18 = "Other Effect",
                                               V19 = "All",
                                               V20 = "All",
                                               V21 = "All"))
  
  # myft = border_inner(myft, border = fp_border(color = "black", width = 1))
  
  # Merge duplicate columns in the header
  myft = merge_h(myft, part = "header")
  
  # Align some rows to the left
  myft = align(myft, i = c(1, 2, 9, 16, 19), j = 1:2, align = "left")
  
  # Fix the column widths
  myft = width(myft, j = 1:3, width = 1.25)
  myft = width(myft, j = 4:21, width = 0.60)
  
  # Align title center
  myft = align(myft, align = "center", part = "header")
  
  # Align some data to the left
  myft = align(myft, j = c(6, 9, 12, 15, 18, 21), align = "left", part = "body")
  
  # Align some data to the center
  myft = align(myft, j = c(5, 8, 11, 14, 17, 20), align = "center", part = "body")
  
  # Text styling
  myft = style(myft, i = c(1), pr_t = fp_text(color = "black", bold = TRUE), part = "body")
  myft = style(myft, pr_t = fp_text(color = "black", bold = TRUE, font.size = 10), part = "header")
  
  # Use to create dynamic header
  #    WT: removed 5/28
  #  myft = set_caption(myft, paste0("Avenue of Payment Impact on ", 
  #                                  selectedSupplyText))
  
  tableRowBorders = c(2, 4, 6, 8, 9, 11, 13, 15, 16, 18, 19, 21, 23, 25)
  
  myft = hline(myft, i = tableRowBorders, j = 2:21, border = fp_border(color = "black", width = 1))
  
  myft = hline(myft, i = 24, j = 3, border = fp_border(color = "black", width = 2))
  myft = hline(myft, i = 25, border = fp_border(color = "black", width = 2))
  
  myft = bg(myft, bg = "white", part = "all")
  
  return(myft)
}

tablePCOPUP = makeTables(flextable(tableTemplate[1:25,]))
tablePI2MI = makeTables(flextable(tableTemplate[26:50,]))
tableElasMarket = makeTables(flextable(tableTemplate[51:75,]))
tableElasPayment = makeTables(flextable(tableTemplate[76:100,]))


program = paste(selectedProgram, selectedSupply, sep = "_")

if(savePCOPUPStatus == 1) {
  save_as_image(tablePCOPUP, paste0(program, "_PCOPUP.jpg"), zoom = 1, expand = 10)
}
if(savePI2MIStatus == 1) {
  save_as_image(tablePI2MI, paste0(program, "_PI2MI.jpg"), zoom = 1, expand = 10)
}
if(saveElasMarketStatus == 1) {
  save_as_image(tableElasMarket, paste0(program, "_ElasMarket.jpg"), zoom = 1, expand = 10)
}
if(saveElasPaymentStatus == 1) {
  save_as_image(tableElasPayment, paste0(program, "_ElasPayment.jpg"), zoom = 1, expand = 10)
}






