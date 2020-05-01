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

# Load data
DcData = read_excel("Decoupling/DecouplingLitDataNew.xlsx", 
                    col_names = FALSE)

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
tempAvenues = c("PriceEffect_",
                "RiskReduction_",
                "RiskAndWealth_",
                "UpdatingAndExpectations_",
                "ExemptionsOrExclusions_",
                "Other_",
                "All_")

# Create vector of values needed for table
factors = c("Relavancy",
            "PCOPUP",
            "PI2MI")

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
# Region and Own effect
usNat = cornBelt = otherRegion = templateList
# Methods and own effect
estPS = estMD = simuOrTheory = templateList
# All and own effect
all = templateList
# Nature of effect
allCrossEffect = allCrops = oneCrop = templateList

subLists = list("usNat" = usNat,
                "cornBelt" = cornBelt,
                "otherRegion" = otherRegion,
                "estPS" = estPS,
                "estMD" = estMD,
                "simuOrTheory" = simuOrTheory,
                "all" = all,
                "allCrossEffect" = allCrossEffect,
                "allCrops" = allCrops,
                "oneCrop" = oneCrop)

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

###############################################################

# Set program choices for user
programChoices = c("All during period",
                   "Crop insurance",
                   "ARC",
                   "PLC",
                   "SCO",
                   "CCP",
                   "ACRE",
                   "Market loss assistance",
                   "Fixed direct payment (contract payment)",
                   "Marketing Loan program",
                   "Milk Income Loss Contract (MILC)",
                   "Margin Protection Program",
                   "Pre-1996 US policy",
                   "Other"
)

# Set supply choices for user
supplyChoices = c("Area of a Crop or Crops",
                  "Yield",
                  "Production of a Crop or Crops")

# # Dialogue boxes for user input
# selectedProgramText = dlgList(programChoices, preselect = NULL, multiple = FALSE,
#                           title = "Program Type")$res
selectedProgramText = "Fixed direct payment (contract payment)"

# # Dialogue boxes for user input
# selectedSupplyText = dlgList(supplyChoices, preselect = NULL, multiple = FALSE,
#                          title = "Supply Type")$res
selectedSupplyText = "Area of a Crop or Crops"

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
# Extract dummy variable for what type of supply is used


# Convert NA to 0
DcList[["OtherCrossEffects_IsThisColumnACrosseffect"]] = 
  replace_na(DcList[["OtherCrossEffects_IsThisColumnACrosseffect"]], 0)

# Get cross effect indexes
noCrossEffect = 1 - DcList[["OtherCrossEffects_IsThisColumnACrosseffect"]]
crossEffect = DcList[["OtherCrossEffects_IsThisColumnACrosseffect"]]




# subList = allCrops
# programId = "allCrops"
# tableType = selectedSupply



# i = 1
# subList = subLists[[i]]
# programId = names(subLists)[i]
# tableType = selectedSupply


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
    if (tableType == "area") {
      extraFactors = DcList[["NatureOfSupply_AreaOfAllOrManyCrops"]]
      crossEffectFactor = 1
    }
    
    else if (tableType == "production") {
      extraFactors = DcList[["NatureOfSupply_ProductionOfAllCrops"]]
      crossEffectFactor = 1
    }
    
    else if (tableType == "yield") {
      extraFactors = 0
      crossEffectFactor = 0
    }
  } 
  
  # Effect: one crop
  else if (programId == "oneCrop") {
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
    avenue = which((sub("\\_.*", "", names(subList))) == sub("\\_.*", "", names((subList[i]))))
    
    Relavancy = grep("Relavancy", names(subList[avenue]))
    PCOPUP = grep("PCOPUP", names(subList[avenue]))
    PI2MI = grep("PI2MI", names(subList[avenue]))
    
    subList[[avenue[Relavancy]]][["data"]] = programMaster * extraFactors * crossEffectFactor *
      DcList[[which((sub(".*_", "", names(DcList))) == sub("\\_.*", "", names((subList[i]))))]]
    subList[[avenue[PCOPUP]]][["data"]] = subList[[avenue[Relavancy]]]$data * DcList[["ImpactOfPayments_PctChangeOutputPerunitPmt"]]
    subList[[avenue[PI2MI]]][["data"]] = subList[[avenue[Relavancy]]]$data * DcList[["Ratio_RatioOfPmtImpToMarketImp"]]

    subList[[avenue[Relavancy]]]$studyIndex = subList[[avenue[PCOPUP]]]$studyIndex = 
      subList[[avenue[PI2MI]]]$studyIndex = subList[[avenue[Relavancy]]]$data * DcList[["Study_NA"]]
  }
  return(subList)
}

# Run the function for all the sublists
for (i in seq_len(length(subLists))) {
  subLists[[i]] = calcLists(subLists[[i]], names(subLists)[i], selectedSupply)
}

# i = 1
# subList = subLists[[i]]

# Calculates the number of observations triggered and averages the values
calcStats = function(subList) {
  for (i in names(subList)) {
    subList[[i]]$count = length(which(!is.na(subList[[i]][["data"]])))
    
    subList[[i]]$studyCount = length(unique(na.omit(subList[[i]][["studyIndex"]])))
    
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
tableTemplate = read_excel("Decoupling/tableTemplate.xlsx", col_names = TRUE, sheet = 2)
tableTemplate = as.data.frame(tableTemplate)

# Dynamic program identifier
tableTemplate[1, ] = selectedProgramText

# Create the flex table
myft = flextable(tableTemplate)

# Merge duplicate columns, row-wise
myft = merge_at(myft, i = 1)
myft = merge_at(myft, i = 2, j = 1:8)
myft = merge_at(myft, i = 3, j = 2:3)
myft = merge_at(myft, i = 13, j = 2:3)
myft = merge_at(myft, i = 23, j = 2:3)
myft = merge_at(myft, i = 26, j = 2:3)
myft = merge_at(myft, i = 36, j = 1:6)
myft = merge_at(myft, i = 37, j = 2:3)
myft = merge_at(myft, i = 47, j = 2:3)
myft = merge_at(myft, i = 57, j = 2:3)
myft = merge_at(myft, i = 60, j = 2:3)
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

# myft = border_inner(myft, border = fp_border(color = "black", width = 1))

# Merge duplicate columns in the header
myft = merge_h(myft, part = "header")

# Align some rows to the left
myft = align(myft, i = c(1, 2, 3, 13, 23, 26, 36,37, 47, 57, 60), j = 1:2, align = "left")

# # # Fix the column widths
myft = width(myft, j = 1:3, width = 1.25)
myft = width(myft, j = 4:21, width = 0.5)

# Align title center
myft = align(myft, align = "center", part = "header")

# Align some data to the left
myft = align(myft, j = c(6, 9, 12, 15, 18, 21), align = "left", part = "body")

# Align some data to the center
myft = align(myft, j = c(5, 8, 11, 14, 17, 20), align = "center", part = "body")

# Text styling
myft = style(myft, i = c(1, 2, 36), pr_t = fp_text(color = "black", bold = TRUE), part = "body")
myft = style(myft, pr_t = fp_text(color = "black", bold = TRUE, font.size = 10), part = "header")

# Use to create dynamic header
myft = set_caption(myft, paste0("Avenue of Payment Impact on ", 
                                selectedSupplyText, 
                                ", Number of Observations. and Simple Average"))


tableRowBorders = c(3,5,9,11,13,15,19,21,25,26,29,31,37,
                    39,43,45,47,49,53,55,59,60,63,65)

myft = hline(myft, i = tableRowBorders, j = 2:21, border = fp_border(color = "black", width = 1))

myft = hline(myft, i = 34,border = fp_border(color = "black", width = 2))




padding(myft, padding.bottom = 75, i = 35)


myft






demo_loop <- system.file(package = "flextable", "examples/rmd", "loop_docx.Rmd")
rmd_file <- tempfile(fileext = ".Rmd")
file.copy(demo_loop, to = rmd_file, overwrite = TRUE)
#> [1] TRUE
rmd_file # R Markdown document used for demo
#> [1] "/var/folders/08/2qdvv0q95wn52xy6mxgj340r0000gn/T//RtmpBwTk5k/file10d241e913d76.Rmd"
if(require("rmarkdown", quietly = TRUE)){
  render(input = rmd_file, output_format = "word_document", output_file = "loop_docx.docx")
}

demo_loop <- system.file(package = "flextable", "examples/rmd", "loop_html.Rmd")
rmd_file <- tempfile(fileext = ".Rmd")
file.copy(demo_loop, to = rmd_file, overwrite = TRUE)
#> [1] TRUE
rmd_file # R Markdown document used for demo
#> [1] "/var/folders/08/2qdvv0q95wn52xy6mxgj340r0000gn/T//RtmpBwTk5k/file10d24515a1da7.Rmd"
if(require("rmarkdown", quietly = TRUE)){
  render(input = rmd_file, output_format = "html_document", output_file = "loop_html.html")
}







ft1 <- myft
tf <- tempfile(fileext = ".docx")
save_as_docx(ft1, path = tf)



