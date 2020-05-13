# Emily Scully
# Created March 31, 2020
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
DcData = read_excel("DecouplingLitDataNew.xlsx", 
                    col_names = FALSE)
# # From R file
# DcData = read_excel("Decoupling/DecouplingLitDataNew.xlsx",
#                     col_names = FALSE)

# Renumber columns
colnames(DcData) = paste0("c", seq(from = 1, to = ncol(DcData), by = 1))

# Remove NA rows
DcData = DcData[rowSums(is.na(DcData)) != ncol(DcData),]

# Carry "notes" titles down to rows with notes
for (row in 1:(nrow(DcData) - 1)) {
  if (length(grep("Notes", DcData$c2[row])) > 0 & 
      is.na(DcData$c1[row + 1]) & 
      is.na(DcData$c2[row + 1])) {
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

# formatting/standardizing the text
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
tDcData = t(DcData)
colnames(tDcData) = row.names(tDcData) = NULL

# Converts each data frame row into a list
# This is helpful for being able to call the variables by 
# their key, rather than col/row notation
DcList = lapply(seq_len(ncol(tDcData[4:nrow(tDcData), ])), function(i) {
  tDcData[4:nrow(tDcData), ][,i]
  })
names(DcList) = tDcData[3, ]

# Convert to numeric where applicable
DcList = lapply(DcList, function(col) {
  if (!has_warning(as.numeric(as.character(col)))) {
    as.numeric(as.character(col))
  } else {
    col
  }
})

# Create temp vector of the avenues
tempAvenues = c("PriceEffect_",
                "RiskReduction_",
                "RiskAndWealth_",
                "UpdatingAndExpectations_",
                "Other_",
                "All_")

# Create vector of values needed for table
factors = c("Relavancy",
            "PCOPUP",
            "PI2MI")

# Match each avenue to all factors
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

####################### CALCULATIONS #######################

# # Needed if you run the R code outside of the rmarkdown
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
# selectedProgramText = dlgList(programChoices, preselect = NULL, multiple = FALSE,
#                           title = "Program Type")$res
# # selectedProgramText = "Fixed direct payment (contract payment)"
# 
# # Dialogue boxes for user input
# selectedSupplyText = dlgList(supplyChoices, preselect = NULL, multiple = FALSE,
#                          title = "Supply Type")$res
# # selectedSupplyText = "Area of a Crop or Crops"

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

# Convert NA to 0
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
  }
  
  # Estimation: Panel or survey
  else if (programId == "estPS") {
    panelSurveySum = replace_na(DcList[["Method_EstSurveyData"]], 0) + 
      replace_na(DcList[["Method_EstBalPanelData"]], 0) + 
      replace_na(DcList[["Method_EstUnbalPanelData"]], 0)
    extraFactors = as.numeric(gsub(0, NA, panelSurveySum))
    crossEffectFactor = noCrossEffect
  } 
  
  # Estimation: Market Data
  else if (programId == "estMD") {
    extraFactors = DcList[["Method_EstMarketData"]]
    crossEffectFactor = noCrossEffect
  }
  
  # Method: Simulation or Theory
  else if (programId == "simuOrTheory") {
    simuOrTheorySum = replace_na(DcList[["Method_Simulation"]], 0) + 
      replace_na(DcList[["Method_Theory"]], 0)
    extraFactors = as.numeric(gsub(0, NA, simuOrTheorySum))
    crossEffectFactor = noCrossEffect
  }
  
  # Us national
  else if (programId == "usNat") {
    extraFactors = DcList[["Region_AllOfUS"]]
    crossEffectFactor = noCrossEffect
  } 
  
  # Corn belt
  else if (programId == "cornBelt") {
    extraFactors = DcList[["Region_CornBelt"]]
    crossEffectFactor = noCrossEffect
  } 
  
  # Other Regions
  else if (programId == "otherRegion") {
    # exclude the US and Corn Belt
    otherRelavancy = replace_na(DcList[["Region_AllOfUS"]], 0) + 
      replace_na(DcList[["Region_CornBelt"]], 0)
    extraFactors = as.numeric(gsub(0, NA, (1 - otherRelavancy)))
    crossEffectFactor = noCrossEffect
  }
 
  # Effect: cross effect
  else if (programId == "allCrossEffect") {
    extraFactors = 1
    crossEffectFactor = crossEffect
  } 
  
  # Effect: all crops
  else if (programId == "allCrops") {
    # What supply gets displayed depends on the type of table
    if (tableType == "area") {
      extraFactors = DcList[["NatureOfSupply_AreaOfAllOrManyCrops"]]
      crossEffectFactor = 1
    }
    
    else if (tableType == "production") {
      extraFactors = DcList[["NatureOfSupply_ProductionOfAllCrops"]]
      crossEffectFactor = 1
    }
    
    # Yield for all crops is never considered and always NA
    else if (tableType == "yield") {
      extraFactors = NA
      crossEffectFactor = NA
    }
  } 
  
  # Effect: one crop
  else if (programId == "oneCrop") {
    # What supply gets displayed depends on the type of table
    if (tableType == "area") {
      extraFactors = DcList[["NatureOfSupply_AreaOfACrop"]]
      crossEffectFactor = 1
    }
    
    else if (tableType == "production") {
      extraFactors = DcList[["NatureOfSupply_ProductionOfACrop"]]
      crossEffectFactor = 1
    } 
    
    else if (tableType == "yield") {
      extraFactors = DcList[["NatureOfSupply_Yield"]]
      crossEffectFactor = 1
    }
  }
  
  # Pull relavancy and apply it to the PCOPUP and PI2MI data
  for (i in grep("Relavancy", names((subList)))) {
    # Identify the avenue type for the index
    avenue = which((sub("\\_.*", "", names(subList))) == sub("\\_.*", "", names((subList[i]))))
    
    # Isolate the list index of each factor from the selected avenue
    Relavancy = grep("Relavancy", names(subList[avenue]))
    PCOPUP = grep("PCOPUP", names(subList[avenue]))
    PI2MI = grep("PI2MI", names(subList[avenue]))
    
    # Determines which indexes should be included in the calculations
    # These are based on the premise that NA * 1 = NA and NA * 0 = NA
    subList[[avenue[Relavancy]]][["data"]] = programMaster * extraFactors * crossEffectFactor *
      DcList[[which((sub(".*_", "", names(DcList))) == sub("\\_.*", "", names((subList[i]))))]]
    
    # Extracts the PCOPUP and PI2MI values only for relevant indexes
    subList[[avenue[PCOPUP]]][["data"]] = subList[[avenue[Relavancy]]]$data * (DcList[["ImpactOfPayments_PctChangeOutputPerunitPmt"]] * 100)
    subList[[avenue[PI2MI]]][["data"]] = subList[[avenue[Relavancy]]]$data * DcList[["Ratio_RatioOfPmtImpToMarketImp"]]

    # Extracts the study number only for relevant indexes
    subList[[avenue[Relavancy]]]$studyIndex = subList[[avenue[PCOPUP]]]$studyIndex = 
      subList[[avenue[PI2MI]]]$studyIndex = subList[[avenue[Relavancy]]]$data * DcList[["Study_NA"]]
  }
  return(subList)
}

# Run the function for all the sublists
for (i in seq_len(length(subLists))) {
  subLists[[i]] = calcLists(subLists[[i]], names(subLists)[i], selectedSupply)
}

# Calculates the number of observations triggered and averages the values
calcStats = function(subList) {
  for (i in names(subList)) {
    
    # Calculate number of observations and number of studies
    subList[[i]]$count = length(which(!is.na(subList[[i]][["data"]])))
    subList[[i]]$studyCount = length(unique(na.omit(subList[[i]][["studyIndex"]])))
    
    if (subList[[i]]$count > 0) {
      # Calculate mean and median
      subList[[i]]$simpleAvg = mean(subList[[i]][["data"]], na.rm = TRUE)
      subList[[i]]$median = median(subList[[i]][["data"]], na.rm = TRUE)
      
      # Calculate study weighted average
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

####################### TABLE LOADING #######################

# Template for loading data into the table
# tableTemplate = read_excel("Decoupling/tableTemplate.xlsx", col_names = TRUE, sheet = 1)
tableTemplate = read_excel("tableTemplate.xlsx", col_names = TRUE, sheet = 1)
tableTemplate = as.data.frame(tableTemplate)

# Function to replace values that round to zero with ~0
nearZero = function(value) {
  if (is.na(value)) {
    return(NA)
  } else if (round(value, digits = 2) == 0) {
    return("~0")
  } else {
    return(round(value, digits = 2))
  }
}

# Get appropriate columns
cols1 =  grep(pattern = "1", x = tableTemplate[1, ]) # obs/simple avg
cols2 =  grep(pattern = "2", x = tableTemplate[1, ]) # study/study weighted avg
cols3 =  grep(pattern = "3", x = tableTemplate[1, ]) # median/Ai weighted avg

for (name in names(subLists)) {
  # Get list indexes for PCOPUP and PI2MI
  PCOPUPindexes = grep(pattern = "PCOPUP", x = names(subLists[[name]]))
  PI2MIindexes = grep(pattern = "PI2MI", x = names(subLists[[name]]))

  # Gets the rows for PCOPUP
  PCOPUProw1 = grep(pattern = paste0(name, 1), x = tableTemplate$index)[1]
  PCOPUProw2 = grep(pattern = paste0(name, 2), x = tableTemplate$index)[1]
  # Gets the rows for PI2MI
  PI2MIrow1 = grep(pattern = paste0(name, 1), x = tableTemplate$index)[2]
  PI2MIrow2 = grep(pattern = paste0(name, 2), x = tableTemplate$index)[2]
  
  # Ensure that all variables have the same length
  # This is an okay check, but other errors can still slip through it
  equalityOfLength = all(sapply(list(length(subLists[[name]][PCOPUPindexes]), 
                                     length(subLists[[name]][PI2MIindexes]),
                                     length(cols1), length(cols2), 
                                     length(cols3)), function(x) 
                                       x == length(subLists[[name]][PCOPUPindexes])))
  
  if (equalityOfLength) {
    for (i in seq_len(length(subLists[[name]][PCOPUPindexes]))) {

      # Load cells a and b for PCOPUP
      if (subLists[[name]][[PCOPUPindexes[i]]]$count == 0 & subLists[[name]][[PCOPUPindexes[i]]]$studyCount == 0) {
        tableTemplate[PCOPUProw1, cols1[i]] = NA
        tableTemplate[PCOPUProw1, cols2[i]] = NA
      } else {
        tableTemplate[PCOPUProw1, cols1[i]] = subLists[[name]][[PCOPUPindexes[i]]]$count
        tableTemplate[PCOPUProw1, cols2[i]] = subLists[[name]][[PCOPUPindexes[i]]]$studyCount
      }
      
      # Load cells a and b for PI2MI
      if (subLists[[name]][[PI2MIindexes[i]]]$count == 0 & subLists[[name]][[PI2MIindexes[i]]]$studyCount == 0) {
        tableTemplate[PI2MIrow1, cols1[i]] = NA
        tableTemplate[PI2MIrow1, cols2[i]] = NA
      } else {
        tableTemplate[PI2MIrow1, cols1[i]] = subLists[[name]][[PI2MIindexes[i]]]$count
        tableTemplate[PI2MIrow1, cols2[i]] = subLists[[name]][[PI2MIindexes[i]]]$studyCount
      }
      
      # Load cell c
      tableTemplate[PCOPUProw1, cols3[i]] = nearZero(subLists[[name]][[PCOPUPindexes[i]]]$median)
      tableTemplate[PI2MIrow1, cols3[i]] = nearZero(subLists[[name]][[PI2MIindexes[i]]]$median)
      
      # Load cell d
      tableTemplate[PCOPUProw2, cols1[i]] = nearZero(subLists[[name]][[PCOPUPindexes[i]]]$simpleAvg)
      tableTemplate[PI2MIrow2, cols1[i]] = nearZero(subLists[[name]][[PI2MIindexes[i]]]$simpleAvg)
      # Load cell e
      tableTemplate[PCOPUProw2, cols2[i]] = nearZero(subLists[[name]][[PCOPUPindexes[i]]]$studyWeightAvg)
      tableTemplate[PI2MIrow2, cols2[i]] = nearZero(subLists[[name]][[PI2MIindexes[i]]]$studyWeightAvg)
      # Load cell f
      tableTemplate[PCOPUProw2, cols3[i]] = NA
      tableTemplate[PI2MIrow2, cols3[i]] = NA
    }
  } else {
    stop("Sublist length and column length are not equal. Make sure that any added avenues 
         for one (either sublist or table) were also added properly to the other.")
  }
} 

####################### TABLE PRINTING ####################### 

# Delete index column
tableTemplate = subset(tableTemplate, select = -(index))

# Dynamic program identifier
tableTemplate[c(1, 36), ] = selectedProgramText

# Function to create and format the flex tables
makeTables = function(myft) {
  # Merge duplicate columns, row-wise
  myft = merge_at(myft, i = 1)
  myft = merge_at(myft, i = 2, j = 1:8)
  myft = merge_at(myft, i = 3, j = 2:3)
  myft = merge_at(myft, i = 13, j = 2:3)
  myft = merge_at(myft, i = 23, j = 2:3)
  myft = merge_at(myft, i = 26, j = 2:3)
  myft = merge_v(myft, j = 3)
  
  # Rename columns
  # These cannot be put into the template because the column names would not be unique
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
                                               V13 = "Updating and Expectations",
                                               V14 = "Updating and Expectations",
                                               V15 = "Updating and Expectations",
                                               V16 = "Other",
                                               V17 = "Other",
                                               V18 = "Other",
                                               V19 = "All",
                                               V20 = "All",
                                               V21 = "All"))

  # Merge duplicate columns in the header
  myft = merge_h(myft, part = "header")
  
  # Align some rows to the left
  myft = align(myft, i = c(1, 2, 3, 13, 23, 26), j = 1:2, align = "left")
  
  # # # Fix the column widths
  myft = width(myft, j = 1:3, width = 1.25)
  myft = width(myft, j = 4:21, width = 0.60)
  
  # Align title center
  myft = align(myft, align = "center", part = "header")
  
  # Align some data to the left
  myft = align(myft, j = c(6, 9, 12, 15, 18, 21), align = "left", part = "body")
  
  # Align some data to the center
  myft = align(myft, j = c(5, 8, 11, 14, 17, 20), align = "center", part = "body")
  
  # Text styling
  myft = style(myft, i = c(1, 2), pr_t = fp_text(color = "black", bold = TRUE), part = "body")
  myft = style(myft, pr_t = fp_text(color = "black", bold = TRUE, font.size = 10), part = "header")
  
  # Use to create dynamic header
  myft = set_caption(myft, paste0("Avenue of Payment Impact on ", 
                                  selectedSupplyText))
  
  
  tableRowBorders = c(3, 5, 9, 11, 13, 15, 19, 21, 23, 25, 27, 29, 32, 35)
  
  myft = hline(myft, i = tableRowBorders, j = 2:21, border = fp_border(color = "black", width = 1))
  
  myft = hline(myft, i = 34, j = 3, border = fp_border(color = "black", width = 2))
  myft = hline(myft, i = 35, border = fp_border(color = "black", width = 2))
  
  return(myft)
}

# Make and store tables separately
tablePCOPUP = makeTables(flextable(tableTemplate[1:35,]))
tablePI2MI = makeTables(flextable(tableTemplate[36:70,]))

