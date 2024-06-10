library(data.table)
library(stringr)
library(dplyr)
library(tidyr)
library(rstudioapi)
library(openxlsx)

# Set working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# Get list of available files containing trendtext to be evaluated
trend_files <- list.files("data", full.names = TRUE, 
                          pattern = "SEACAR_AnalysisResults_")

# Create datatable to store abbreviation and habitat associations
hab_df <- data.table("abbrev" = c("_OY","_NEK","_SAV","_WC"),
                     "full_name" = c("oyster", "nekton", "sav", "wc"))

# Functions ----
# Function to generate expectedText values (Oyster)
checkOysterTrends <- function(modelEstimate, lowConfidence, upConfidence){
  increasing <- modelEstimate > 0
  trendPresent <- ifelse(lowConfidence < 0 & upConfidence < 0, TRUE, 
                         ifelse(lowConfidence > 0 & upConfidence > 0, TRUE, FALSE))
  trendStatus <- "no significant change"
  if(trendPresent){
    trendStatus <- ifelse(increasing, "increase", "decrease")
  }
  return(trendStatus)
}

# Function to generate expectedText values (SAV)
checkSAVTrends <- function(p, LME_Slope, SufficientData){
  if(SufficientData){
    if(is.na(LME_Slope)){
      return("Model did not fit the available data")
    } else {
      increasing <- LME_Slope > 0
      trendPresent <- p <= 0.05
      trendStatus <- "no significant change"
      if(trendPresent){
        trendStatus <- ifelse(increasing, "increase", "decrease")
      }          
    }
  } else {
    trendStatus <- "Insufficient data to calculate trend"
  }
  return(trendStatus)
}

# Function to generate expectedText values (WC)
checkWCTrends <- function(p, SennSlope){
  increasing <- SennSlope > 0
  trendPresent <- p < 0.05
  trendStatus <- "no significant change"
  if(trendPresent){
    trendStatus <- ifelse(increasing, "increase", "decrease")
  }          
  return(trendStatus)
}

# Function to extract yearMin and yearMax from TrendText column
extractYears <- function(text){
  year_string <- str_extract_all(text, "\\d{4} and \\d{4}")[[1]]
  return(data.frame(yearMin = as.numeric(str_split(year_string, " and ")[[1]][1]),
                    yearMax = as.numeric(str_split(year_string, " and ")[[1]][2])))
}

# Create empty list to populate with desired dataframes
data_storage <- list()

# Begin loop ----
# loop through each file / habitat
for(i in 1:nrow(hab_df)){
  abbrev <- hab_df[i]$abbrev
  full_name <- hab_df[i]$full_name
  
  # Subset for desired file
  file <- str_subset(trend_files, abbrev)
  # Store filenames for display in worksheet
  hab_df[i, `:=` (fileName = tail(str_split(file, "/")[[1]],1))]
  
  # Read in file, set as data.table object
  df <- openxlsx::read.xlsx(file)
  setDT(df)
  
  # Store original dataframe for reference
  data_storage[["original"]][[paste0(full_name, "_original")]] <- df
  
  # Declare column name to check for NA values
  na_check <- ifelse(full_name=="oyster", "Intercept", "TrendText")
  # Select only values with model results
  df <- df[!is.na(get(na_check)), ]
  
  # Apply expectedText functions to their respective
  if(full_name=="oyster"){
    df <- df %>% 
      rowwise() %>%
      mutate(expectedText = expectedText(ModelEstimate, LowerConfidence, UpperConfidence),
             containsExpectedText = str_detect(TrendText, expectedText),
             years = list(extractYears(TrendText))) %>%
      unnest(years) %>%
      mutate(yearMinMatchEarliestLiveDate = ifelse(yearMin==EarliestLiveDate, TRUE, FALSE),
             yearMaxMatchLatestLiveDate = ifelse(yearMax==LatestLiveDate, TRUE, FALSE))
  } else if(full_name=="nekton"){
    df <- df %>% 
      rowwise() %>%
      mutate(expectedMean = paste0("was ",as.character(round(Mean,2))),
             containsExpectedMean = str_detect(TrendText, expectedMean),
             expectedMin = paste0("minimum of ",as.character(round(Min,1))),
             containsExpectedMin = str_detect(TrendText, expectedMin),
             expectedMax = paste0("maximum of ",as.character(round(Max,1))),
             containsExpectedMax = str_detect(TrendText, expectedMax),
             years = list(extractYears(TrendText))) %>%
      unnest(years) %>%
      mutate(yearMinMatchEarliestYear = ifelse(yearMin==EarliestYear, TRUE, FALSE),
             yearMaxMatchLatestYear = ifelse(yearMax==LatestYear, TRUE, FALSE))
  } else if(full_name=="sav"){
    df <- df %>%
      rowwise() %>%
      mutate(expectedText = checkSAVTrends(p, LME_Slope, SufficientData),
             containsExpectedText = str_detect(TrendText, expectedText),
             years = list(extractYears(TrendText))) %>%
      unnest(years) %>%
      mutate(yearMinMatchEarliestYear = ifelse(yearMin==EarliestYear, TRUE, FALSE),
             yearMaxMatchLatestYear = ifelse(yearMax==LatestYear, TRUE, FALSE))
  } else if(full_name=="wc"){
    df <- df %>%
      rowwise() %>%
      mutate(expectedText = checkWCTrends(p, SennSlope),
             containsExpectedText = str_detect(TrendText, expectedText),
             years = list(extractYears(TrendText))) %>%
      unnest(years) %>%
      mutate(yearMinMatchEarliestYear = ifelse(yearMin==EarliestYear, TRUE, FALSE),
             yearMaxMatchLatestYear = ifelse(yearMax==LatestYear, TRUE, FALSE))
  }
  
  # Store "checked" dataframe to display results
  data_storage[["checked"]][[paste0(full_name, "_checked")]] <- df
}

# Add descriptive statement to first worksheet
textStatement <- "'_checked' tabs are subsets of the original dataframes 
containing only the entries with model results. New columns have been 
added to check for TrendText accuracy."
fileNameTextStatement <- rbind(hab_df, 
                               data.table("abbrev" = c(NA,textStatement),
                                          "full_name" = NA,
                                          "fileName" = NA))

# Create a combined output for all data tables to be featured in workbook
combined_output <- c(list("fileNames" = fileNameTextStatement), 
                     data_storage[["original"]],
                     data_storage[["checked"]])
# File output name
file_output <- paste0("output/trendText_checked_", Sys.Date(), ".xlsx")

# Save to xlsx
openxlsx::write.xlsx(combined_output, file=file_output,
                     headerStyle = createStyle(textDecoration = "BOLD"))