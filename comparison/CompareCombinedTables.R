library(data.table)
library(stringr)
library(dplyr)
library(tidyr)
library(arsenal)
library(glue)
library(ggplot2)
library(viridis)
library(extrafont)
library(knitr)
library(kableExtra)
library(readxl)
library(openxlsx)
library(rstudioapi)
library(tictoc)
library(lubridate)

# Set working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# Load in SEACAR combined tables
source("../seacar_data_location.R")

# archive subfolder must contain a folder with the date of combined tables
# Ideal workflow upon new combined table export is to place old export files 
# from SEACARdata into new archive subfolder (with name of export as folder name)
# before running unzip.R to place new combined exports into SEACARdata
# New files in /SEACARdata/
# Old files in /SEACARdata/archive/YYYY-Mmm-DD, with old_file_date declared as the date below

old_file_date <- "2024-Jul-11"

new_files <- list.files(seacar_data_location, full.names = TRUE)
old_files <- list.files((paste0(seacar_data_location,"/archive/",old_file_date)), full.names = TRUE)

# discrete files
wq_disc_files <- str_subset(str_subset(new_files, "Combined_WQ_WC_NUT_"), "_cont_", negate = TRUE)
wq_disc_files_old <- str_subset(str_subset(old_files, "Combined_WQ_WC_NUT_"), "_cont_", negate = TRUE)

# continuous files
wq_cont_files <- str_subset(str_subset(new_files, "Combined_WQ_WC_NUT_"), "_cont_", negate = FALSE)
wq_cont_files_old <- str_subset(str_subset(old_files, "Combined_WQ_WC_NUT_"), "_cont_", negate = FALSE)

# habitat files
new_hab_files <- str_subset(new_files, "All_")
old_hab_files <- str_subset(old_files, "All_")

# Determine whether to collect & include VQ data in reports (too many pages)
collect_vq_data <- TRUE

# Determines whether or not flag_overviews are shown in report
# Export (SEACARQAQCFlagCode_Overview.xlsx) is still created
show_flag_overview <- FALSE

# Display file names in report (otherwise dates in summary table may be sufficient)
file_name_display <- FALSE

# Variable to show Column Differences in Export or not
show_columns <- FALSE

## Variables set to FALSE, script will change to TRUE if conditions met
columns_differ <- FALSE
programs_differ <- FALSE
species_differ <- FALSE

# Empty datatables / data directories to be filled
data_directory <- list()

# Colorize function to enable color throughout report (LaTeX format)
colorize <- function(x, color) {sprintf("\\textcolor{%s}{%s}", color, x)}

# Functions to compare between exports
# Compare columns between exports
compare_columns <- function(data_old, data_new, old_file_short, new_file_short,
                            habitat,param){
  if(length(names(data_old))!=length(names(data_new))){
    print(paste0("There is a difference in the number of columns for ", 
                 new_file_short, " vs. ", old_file_short))
    oldColumns <- data.table(
      "Column" = sort(names(data_old)),
      "ColumnType" = sapply(data_old, class)
    )
    newColumns <- data.table(
      "Column" = sort(names(data_new)),
      "ColumnType" = sapply(data_new, class)
    )
    
    # data_directory[["column_compare"]][[habitat]][[param]][['oldColumns']] <- oldColumns
    # data_directory[["column_compare"]][[habitat]][[param]][['newColumns']] <- newColumns
    
    return(list("oldColumns" = oldColumns, "newColumns" = newColumns))
    
  }
}

# Compare programs between exports
compare_programs <- function(data_old, data_new, old_file_short, new_file_short,
                             habitat, param){
  
  if(length(sort(data_old[, unique(ProgramID)]))!=
     length(sort(data_new[, unique(ProgramID)]))){
    
    old_programs <- sort(unique(data_old$ProgramID))
    new_programs <- sort(unique(data_new$ProgramID))
    
    diff_df <- data.table()
    diff <- data.table()
    if(length(setdiff(new_programs,old_programs))){
      diff <- data.table("pid" = setdiff(new_programs,old_programs),
                         "export" = "new")
      diff_df <- rbind(diff_df, diff)
      print("Program difference in new export")
    } else if(length(setdiff(old_programs,new_programs))){
      diff <- data.table("pid" = setdiff(old_programs,new_programs),
                         "export" = "old")
      diff_df <- rbind(diff_df, diff)
      print("Program difference in old export")
    }
    
    diff_df <- diff_df %>%
      mutate(color_code = ifelse(export=="old", "red", "green"))
    
    colorize_program <- function(pid, color_code) {colorize(pid, color_code)}
    
    new_programs_color <- unlist(lapply(new_programs, function(pid) {
      ifelse(pid %in% diff_df$pid, colorize_program(pid, diff_df$color_code), pid)
    }))
    
    old_programs_color <- unlist(lapply(old_programs, function(pid) {
      ifelse(pid %in% diff_df$pid, colorize_program(pid, diff_df$color_code), pid)
    }))
    
    print(paste0("There is a difference in the number of ProgramIDs for ",
                 new_file_short, " vs. ", old_file_short))
    
    return(list("old_programs" = old_programs_color, "new_programs" = new_programs_color, "diff_df" = diff_df))
  }
}

# Provide counts of data by program for each parameter
program_counts <- function(data_old, data_new, habitat, param, quadsize="None"){
  if(!habitat=="Species"){
    program_count_table <- merge(
      data_old %>% 
        group_by(ProgramID, ProgramName) %>% 
        summarise(nOld = n(),
                  LatestDateOld = substr(max(SampleDate), 1, 10)),
      data_new %>% 
        group_by(ProgramID, ProgramName) %>% 
        summarise(nNew = n(),
                  LatestDateNew = substr(max(SampleDate), 1, 10))) %>%
      mutate(difference = nNew - nOld)
    program_count_table$parameter <- param
  } else if(quadsize=="Yes"){
    program_count_table <- merge(
      data_old %>% 
        group_by(ProgramID, ProgramName, ParameterName, QuadSize_m2) %>% 
        summarise(nOld = n(),
                  LatestDateOld = substr(max(SampleDate), 1, 10)),
      data_new %>% 
        group_by(ProgramID, ProgramName, ParameterName, QuadSize_m2) %>% 
        summarise(nNew = n(),
                  LatestDateNew = substr(max(SampleDate), 1, 10))) %>%
      mutate(difference = nNew - nOld)
  } else {
    program_count_table <- merge(
      data_old %>% 
        group_by(ProgramID, ProgramName, ParameterName) %>% 
        summarise(nOld = n(),
                  LatestDateOld = substr(max(SampleDate), 1, 10)),
      data_new %>% 
        group_by(ProgramID, ProgramName, ParameterName) %>% 
        summarise(nNew = n(),
                  LatestDateNew = substr(max(SampleDate), 1, 10))) %>%
      mutate(difference = nNew - nOld)
  }
  # Apply abbreviations to ProgramName to reduce size within tables
  # Also remove & from ProgramNames (thanks LaTeX)
  program_count_table <- program_count_table %>%
    mutate(ProgramName = case_when(
      str_detect(ProgramName, "Aquatic Preserve Continuous Water Quality Monitoring") ~ 
        str_replace(ProgramName, "Aquatic Preserve Continuous Water Quality Monitoring", "APCWQM"),
      str_detect(ProgramName, "Aquatic Preserves Continuous Water Quality Monitoring") ~ 
        str_replace(ProgramName, "Aquatic Preserves Continuous Water Quality Monitoring", "APCWQM"),
      str_detect(ProgramName, "Water Management District Continuous Water Quality Programs") ~ 
        str_replace(ProgramName, "Water Management District Continuous Water Quality Programs", "WMD CWQP"),
      str_detect(ProgramName, "Water Quality Monitoring Program") ~ 
        str_replace(ProgramName, "Water Quality Monitoring Program", "WQMP"),
      str_detect(ProgramName, "National Estuarine Research Reserve System-Wide Monitoring Program") ~ 
        str_replace(ProgramName, "National Estuarine Research Reserve System-Wide Monitoring Program", "NERR SWMP"),
      str_detect(ProgramName, " & ") ~ 
        str_replace(ProgramName, " & ", " and "),
      TRUE ~ ProgramName  # Keep other programs unchanged
    ))
  return(program_count_table)
}

# Run quantiles and grab quantile data
grab_quantiles <- function(df, habitat, param, quadsize="None", type="quantile"){
  
  if(type=="quantile"){
    grab_val_low <- as.name("LowQuantile")
    grab_val_high <- as.name("HighQuantile")
  } else if(type=="threshold"){
    grab_val_low <- as.name("LowThreshold")
    grab_val_high <- as.name("HighThreshold")
  }
  
  if(quadsize=="None"){
    data <- df[ParameterName==param, ]
    # Use ThresholdID for Total Nitrogen (use All, not Calculated)
    if(param=="Total Nitrogen"){
      quant_low_value <- db_thresholds[ThresholdID==31, get(grab_val_low)]
      quant_high_value <- db_thresholds[ThresholdID==31, get(grab_val_high)]
    } else if(param=="Count" & habitat=="Coral"){
      quant_low_value <- db_thresholds[ThresholdID==88, get(grab_val_low)]
      quant_high_value <- db_thresholds[ThresholdID==88, get(grab_val_high)]
      sg1_include <- c("Corallimorpharians", "Milleporans", "Octocorals", "Others", "Porifera", "Scleractinians", "NULL")
      data <- data[SpeciesGroup1 %in% sg1_include, ]
    } else {
      quant_low_value <- db_thresholds[ParameterName==param & CombinedTable==habitat, get(grab_val_low)]
      quant_high_value <- db_thresholds[ParameterName==param & CombinedTable==habitat, get(grab_val_high)]
    }
  } else {
    if(quadsize=="NA"){
      quant_low_value <- db_thresholds[ParameterName==param & CombinedTable==habitat & is.na(QuadSize_m2), get(grab_val_low)]
      quant_high_value <- db_thresholds[ParameterName==param & CombinedTable==habitat & is.na(QuadSize_m2), get(grab_val_high)]
      data <- df[ParameterName==param & is.na(QuadSize_m2), ]
    } else {
      quant_low_value <- db_thresholds[ParameterName==param & CombinedTable==habitat & QuadSize_m2==quadsize, get(grab_val_low)]
      quant_high_value <- db_thresholds[ParameterName==param & CombinedTable==habitat & QuadSize_m2==quadsize, get(grab_val_high)]
      data <- df[ParameterName==param & QuadSize_m2==quadsize, ]
    }
  }
  
  subset_low <- data[ResultValue < quant_low_value, ]
  subset_low$q_subset <- "low"
  
  subset_high <- data[ResultValue > quant_high_value, ]
  subset_high$q_subset <- "high"
  
  combined_subset <- bind_rows(subset_low, subset_high)
}

# Provides overview tables for SEACAR_QAQCFlagCode by ProgramID & Parameter
flag_overview <- function(data_new, return = "wide"){
  flag_data <- data_new %>%
    mutate(flags = str_split(SEACAR_QAQCFlagCode, "/")) %>%
    unnest(flags)
  
  # Summarise by program
  data <- flag_data %>%
    group_by(flags, ParameterName, ProgramID) %>%
    summarise(n = n(), .groups="keep") %>%
    arrange(ParameterName, flags) %>%
    filter(flags %in% c("1Q","2Q","3Q","4Q","5Q","8Q","15Q","16Q","17Q","18Q","19Q")) # filter(flags %in% c("1Q","2Q","3Q","4Q","5Q","8Q","15Q","16Q","17Q"))
    
  # Collect program totals by parameter
  totals <- data_new %>%
    group_by(ParameterName, ProgramID) %>%
    summarise(n_total_prog = n(), .groups="keep")
  
  data_totals <- merge(data, totals, by = c("ProgramID","ParameterName"))
  data_totals <- data_totals %>%
    mutate(pct_flagged = round((n/n_total_prog)*100,2)) %>%
    arrange(flags)
  
  # Pivot wide for better display in report
  wide_table <- data_totals %>%
    select(-pct_flagged) %>%
    pivot_wider(names_from = flags, values_from = n, values_fill = 0) %>%
    arrange(ParameterName, ProgramID)
  
  # Wide format return (USE THIS)
  if(return=="wide"){
    return(as.data.table(wide_table))
  }
  # Long format return
  if(return=="long"){
    return(as.data.table(data_totals))
  }
  
  # Grab 15Q values
  if(return=="fifteen"){
    return(as.data.table(flag_data %>% filter(flags=="15Q")))
  }
}

# Species Crosswalk check
sp_crosswalk <- setDT(read_xlsx("data/SEACAR_Metadata.xlsx", sheet = "Mthd_SpeciesCrosswalk", skip=5))
sp_crosswalk[is.na(sp_crosswalk)] <- "NULL"

sp_match <- function(commonID, group1, group2, habitat){
  match <- sp_crosswalk[CommonIdentifier==commonID & Group1==group1 & Group2==group2 & Habitat==habitat, ]
  return(nrow(match)!=0)
}

check_species <- function(data_new, habitat){
  species <- setDT(data_new %>% group_by(CommonIdentifier, SpeciesGroup1, SpeciesGroup2) %>% reframe())
  species[is.na(species)] <- "NULL"
  species$Habitat <- habitat
  # Apply function
  species <- setDT(
    species %>% rowwise() %>% 
      mutate(match = sp_match(CommonIdentifier, SpeciesGroup1, SpeciesGroup2, Habitat))
  )
  if(nrow(species[match==FALSE])>0){
    print("At least 1 species mismatch detected")
  } else {
    print("no species mismatch")
  }
}

# Function to check for Jan 1st Dates (DB will default to Jan 1st, helps to identify errors)
# Apply to Discrete and "Species" only (not Continuous)
jan_dates <- function(data_new){data_new[month(SampleDate) == 1 & day(SampleDate) == 1]}

## Import database thresholds
## Latest file available at:
## https://github.com/FloridaSEACAR/IndicatorQuantiles/blob/main/output/ScriptResults/Database_Thresholds.xlsx
# Date of thresholds file to use (this should reflect the same file USF used)
# i.e. the previous iteration of Database_Thresholds
thresh_date <- "20240813"
db_threshold_file <- paste0("Database_Thresholds_", thresh_date, ".xlsx")
db_thresholds <- read_xlsx(paste0("../../IndicatorQuantiles/output/ScriptResults/", db_threshold_file), skip=6)
db_thresholds <- db_thresholds %>% 
  filter(!IndicatorName=="Acreage") %>%
  select(ThresholdID, ParameterID, ParameterName, Habitat, CombinedTable, 
         IndicatorID, IndicatorName, ExpectedValues, LowThreshold, HighThreshold, 
         LowQuantile, HighQuantile, QuadSize_m2)
setDT(db_thresholds)

# Select which habitats to include in report
habitats <- c("Discrete", "Continuous", "Species")
# habitats <- c("Continuous")

# Begin Discrete processing
tic()
if("Discrete" %in% habitats){
  habitat <- "Discrete WQ"
  comparison_table <- data.table()
  program_count_table <- data.table()
  # wq_disc_files[c(2,3,16)]
  for(file in wq_disc_files){
    # Shortened file names for later use
    new_file_short <- tail(str_split(file, "/")[[1]],1)
    new_file_short_split <- str_split(new_file_short, "Combined_WQ_WC_NUT_")[[1]][2]
    new_file_shorter <- str_split(paste(tail(str_split(str_split(new_file_short, "Combined_WQ_WC_NUT_")[[1]][2], "-")[[1]],3),collapse = "-"),".txt")[[1]][1]
    
    # pattern to grab relevant "old" file
    pattern <- str_split(str_split(new_file_short, "NUT_")[[1]], "-")[[2]][1]
    pattern <- ifelse(pattern=="Dissolved_Oxygen", paste0(pattern,"-"), pattern)
    
    # Grab "old" export file, file_short
    old_file <- str_subset(wq_disc_files_old, pattern)
    old_file_short <- tail(str_split(old_file, "/")[[1]],1)
    old_file_short_split <- str_split(old_file_short, "Combined_WQ_WC_NUT_")[[1]][2]
    old_file_shorter <- str_split(paste(tail(str_split(str_split(old_file_short, "Combined_WQ_WC_NUT_")[[1]][2], "-")[[1]],3),collapse = "-"),".txt")[[1]][1]
    
    # Read in data frame for each combined data export
    print(paste0("Reading in: ", new_file_short))
    data_new <- fread(file, sep='|', na.strings = "NULL")
    data_new <- data_new[MADup==1, ]
    # Read in old data
    print(paste0("Reading in: ", old_file_short))
    data_old <- fread(old_file, sep='|', na.strings = "NULL")
    data_old <- data_old[MADup==1, ]
    
    # Full ParameterName for a given file
    param <- data_new[, unique(ParameterName)]
    
    # Record filenames for display in report
    data_directory[[habitat]][["new_file_name"]][[param]] <- new_file_shorter
    data_directory[[habitat]][["old_file_name"]][[param]] <- old_file_shorter
    
    data_table <- data.table(
      "parameter" = param,
      "oldFile" = old_file_shorter,
      "newFile" = new_file_shorter,
      "nDataOld" = nrow(data_old),
      "nDataNew" = nrow(data_new)
    )
    
    data_table[ , `:=` (difference = nDataNew - nDataOld)]
    
    comparison_table <- bind_rows(comparison_table, data_table)
    
    ##### Comparison checks #### ----
    ## The following are intended to check for inconsistencies between data exports
    
    ### Compare columns and class types
    ## If they have different number of columns, list them in table below
    data_directory[[habitat]][["column_compare"]][[param]] <- compare_columns(
      data_old[Include==1, ], data_new[Include==1, ], old_file_short, new_file_short, habitat, param)
    
    ### Compare programs between exports
    ## If they have different lengths, record which programs are included/not included
    data_directory[[habitat]][["program_compare"]][[param]] <- compare_programs(
      data_old[Include==1, ], data_new[Include==1, ], old_file_short, new_file_short, habitat, param)
    
    ### Provide counts of data by program by parameter
    program_count_table <- bind_rows(program_count_table, program_counts(
      data_old[Include==1, ], data_new[Include==1, ], habitat, param))
    
    ### Grab quantile data
    data_directory[[habitat]][["quantile"]][[param]] <- grab_quantiles(data_new[Include==1, ], habitat, param, type="quantile")
    
    ### Grab threshold data
    data_directory[[habitat]][["threshold"]][[param]] <- grab_quantiles(data_new, habitat, param, type="threshold")
    
    ### Provide overview of QAQC flags
    data_directory[[habitat]][["flag_overview_wide"]][[param]] <- flag_overview(data_new[Include==1, ], return="wide")
    
    ### Grab values that fall outside of Expected values (15Q check)
    data_directory[[habitat]][["fifteenQ"]][[param]] <- flag_overview(data_new[Include==1, ], return="fifteen")
    
    ### Collect Jan 1st values
    data_directory[[habitat]][["jan_dates"]][[param]] <- jan_dates(data_new[Include==1, ])
    
    ### The following collects statistics about ValueQualifiers and includes them in the report
    if(collect_vq_data==TRUE){
      ## Begin Value Qualifier Data Collection ----
      # List of included value qualifiers
      vq_list <- data_new[, unique(ValueQualifier)]
      
      # Some VQ are combined (i.e. "AIQ"), split them
      mod_data <- bind_rows(
        data_new[!is.na(ValueQualifier) & ValueQualifierSource=="STORET_WIN", ] %>%
          mutate(ValueQualifier = str_split(ValueQualifier, "")) %>%
          unnest(ValueQualifier), 
        data_new[!ValueQualifierSource=="STORET_WIN", ]
      )
      
      # VQ overview table
      vq_table <- mod_data %>%
        filter(!is.na(ValueQualifier)) %>%
        group_by(ValueQualifier, ValueQualifierSource) %>%
        summarise(n = n(), .groups = "keep") %>%
        select(ValueQualifierSource, ValueQualifier, n) %>%
        arrange(ValueQualifierSource, ValueQualifier)
      
      vq_program_table <- mod_data %>%
        group_by(ProgramID, ProgramName, ValueQualifier, 
                 ValueQualifierSource, ActivityType) %>%
        summarise(n_vq_data = n(), .groups = "keep") %>%
        arrange(ValueQualifier)
      
      # append individual tables into data_directory
      if(nrow(vq_table)>0){data_directory[[habitat]][["vq_table"]][[param]] <- vq_table}
      if(nrow(vq_program_table)>0){data_directory[[habitat]][["vq_program_table"]][[param]] <- vq_program_table}
    }
  }
  
  data_directory[[habitat]][["comparison_table"]] <- comparison_table
  data_directory[[habitat]][["program_count_table"]] <- program_count_table
}
toc()

# Begin Continuous processing
tic()
if("Continuous" %in% habitats){
  habitat <- "Continuous WQ"
  comparison_table <- data.table()
  program_count_table <- data.table()
  
  cont_params <- c("Dissolved_Oxygen","Dissolved_Oxygen_Saturation","pH",
                   "Salinity", "Turbidity", "Water_Temperature")
  
  for (p in cont_params){
    data_old_combined <- data.table()
    data_new_combined <- data.table()
    for (region in c("_NW","_NE","_SW","_SE")){
      par_reg_pattern <- paste0(p, region)
      region_files <- str_subset(wq_cont_files, par_reg_pattern)
      for (new_file in region_files){
        # Grab "new" export file, file_short
        new_file_short <- tail(str_split(new_file, "/")[[1]],1)
        new_file_short_split <- str_split(new_file_short, "Combined_WQ_WC_NUT_cont_")[[1]][2]
        new_file_shorter <- str_split(paste(tail(str_split(str_split(new_file_short, "Combined_WQ_WC_NUT_cont_")[[1]][2], "-")[[1]],3),collapse = "-"),".txt")[[1]][1]
        
        # Grab "old" export file, file_short
        old_file <- str_subset(wq_cont_files_old, par_reg_pattern)
        old_file_short <- tail(str_split(old_file, "/")[[1]],1)
        old_file_short_split <- str_split(old_file_short, "Combined_WQ_WC_NUT_")[[1]][2]
        old_file_shorter <- str_split(paste(tail(str_split(str_split(old_file_short, "Combined_WQ_WC_NUT_")[[1]][2], "-")[[1]],3),collapse = "-"),".txt")[[1]][1]
        
        # Read in data frame for each combined data export
        print(paste0("Reading in: ", new_file_short))
        data_new <- fread(new_file, sep='|', na.strings = "NULL")
        data_new <- data_new[MADup==1, ]
        # Read in old data
        print(paste0("Reading in: ", old_file_short))
        data_old <- fread(old_file, sep='|', na.strings = "NULL")
        data_old <- data_old[MADup==1, ]
        
        # Combine data by parameter for all regions
        data_old_combined <- bind_rows(data_old_combined, data_old)
        data_new_combined <- bind_rows(data_new_combined, data_new)
        
        # Full ParameterName for a given file
        param <- data_new[, unique(ParameterName)]
        
        # Record filenames for display in report
        data_directory[[habitat]][["new_file_name"]][[param]] <- new_file_shorter
        data_directory[[habitat]][["old_file_name"]][[param]] <- old_file_shorter
        
        data_table <- data.table(
          "parameter" = param,
          "region" = str_split(region,"_")[[1]][2],
          "oldFile" = old_file_shorter,
          "newFile" = new_file_shorter,
          "nDataOld" = nrow(data_old),
          "nDataNew" = nrow(data_new)
        )
        
        data_table[ , `:=` (difference = nDataNew - nDataOld)]
        data_table[ , `:=` (pctChange = round((difference / nDataOld)*100,2))]
        
        comparison_table <- bind_rows(comparison_table, data_table)
      }
    }
    
    ##### Comparison checks #### ----
    ## The following are intended to check for inconsistencies between data exports
    
    ### Compare columns and class types
    ## If they have different number of columns, list them in table below
    data_directory[[habitat]][["column_compare"]][[param]] <- compare_columns(
      data_old_combined[Include==1, ], data_new_combined[Include==1, ], old_file_short, new_file_short, habitat, param)
    
    ### Compare programs between exports
    ## If they have different lengths, record which programs are included/not included
    data_directory[[habitat]][["program_compare"]][[param]] <- compare_programs(
      data_old_combined[Include==1, ], data_new_combined[Include==1, ], old_file_short, new_file_short, habitat, param)
    
    ### Provide counts of data by program by parameter
    program_count_table <- bind_rows(program_count_table, program_counts(
      data_old_combined[Include==1, ], data_new_combined[Include==1, ], habitat, param))
    
    ### Grab quantile data
    data_directory[[habitat]][["quantile"]][[param]] <- grab_quantiles(data_new_combined[Include==1, ], habitat, param, type="quantile")
    
    ### Grab threshold data
    data_directory[[habitat]][["threshold"]][[param]] <- grab_quantiles(data_new_combined, habitat, param, type="threshold")
    
    ### Provide overview of QAQC flags
    data_directory[[habitat]][["flag_overview_wide"]][[param]] <- flag_overview(data_new_combined[Include==1, ], return="wide")
    
    ### Grab values that fall outside of Expected values (15Q check)
    data_directory[[habitat]][["fifteenQ"]][[param]] <- flag_overview(data_new_combined[Include==1, ], return="fifteen")
    
  }
  data_directory[[habitat]][["comparison_table"]] <- comparison_table
  data_directory[[habitat]][["program_count_table"]] <- program_count_table
}
toc()

# Begin "Species" processing (SAV, Coral, Oyster, CW, Nekton)
tic()
if("Species" %in% habitats){
  comparison_table <- data.table()
  program_count_table <- data.table()
  
  for(i in 1:length(new_hab_files)){
    file <- new_hab_files[i]
    
    # Shortened file names for later use
    new_file_short <- tail(str_split(file, "/")[[1]],1)
    new_file_shorter <- str_split(paste(tail(str_split(str_split(new_file_short, "Parameters")[[1]][2], "-")[[1]],3),collapse = "-"),".txt")[[1]][1]
    
    # pattern to grab relevant "old" file (pattern = habitat name)
    pattern <- str_split(str_split(new_file_short, "All_")[[1]][2], "_Parameters")[[1]][1]
    
    # Match habitat name with format of db_thresholds, i.e. CORAL to Coral
    habitat <- ifelse(pattern %in% c("OYSTER","CORAL","NEKTON"), str_to_title(pattern), pattern)
    
    # Grab "old" export file, file_short
    old_file <- str_subset(old_hab_files, pattern)
    old_file_short <- tail(str_split(old_file, "/")[[1]],1)
    old_file_shorter <- str_split(paste(tail(str_split(str_split(old_file_short, "Parameters")[[1]][2], "-")[[1]],3),collapse = "-"),".txt")[[1]][1]
    
    # Save file names for use in report
    data_directory[["Species"]][["new_file_name"]][[habitat]] <- new_file_short
    data_directory[["Species"]][["old_file_name"]][[habitat]] <- old_file_short
    
    # Read in data frame for each combined data export
    print(paste0("Reading in: ", new_file_short))
    data_new <- fread(file, sep='|', na.strings = "NULL")
    data_new <- data_new[MADup==1, ]
    # Read in old data
    print(paste0("Reading in: ", old_file_short))
    data_old <- fread(old_file, sep='|', na.strings = "NULL")
    data_old <- data_old[MADup==1, ]
    
    data_table <- data.table(
      "habitat" = habitat,
      "oldFile" = old_file_shorter,
      "newFile" = new_file_shorter,
      "nDataOld" = nrow(data_old),
      "nDataNew" = nrow(data_new)
    )
    
    data_table[ , `:=` (difference = nDataNew - nDataOld)]
    
    comparison_table <- bind_rows(comparison_table, data_table)
    
    ##### Comparison checks #### ----
    ## The following are intended to check for inconsistencies between data exports
    
    ### Compare columns and class types
    ## If they have different number of columns, list them in table below
    data_directory[["Species"]][["column_compare"]][[habitat]] <- compare_columns(
      data_old[Include==1, ], data_new[Include==1, ], old_file_short, new_file_short, habitat, param)
    
    ### Compare programs between exports
    ## If they have different lengths, record which programs are included/not included
    data_directory[["Species"]][["program_compare"]][[habitat]] <- compare_programs(
      data_old[Include==1, ], data_new[Include==1, ], old_file_short, new_file_short, habitat, param)
    
    ### Provide counts of data by program by parameter
    
    qsize <- ifelse(habitat=="Oyster", "Yes", "None")
    
    p_count_df <- program_counts(data_old[Include==1, ], data_new[Include==1, ], "Species", param, quadsize=qsize)
    p_count_df$habitat <- habitat
    program_count_table <- bind_rows(program_count_table, p_count_df)
    
    params <- unique(data_new$ParameterName)
    for(param in params){
      
      # Split by quad size
      if(habitat=="Oyster" & param %in% c("Shell Height","Number of Oysters Counted - Total",
                                          "Number of Oysters Counted - Live","Number of Oysters Counted - Dead")){
        for(q in unique(data_new$QuadSize_m2)){
          param_q <- paste0(param, "(",q,")")
          q <- ifelse(is.na(q), "NA", q)
          ### Grab quantile data
          data_directory[["Species"]][["quantile"]][[habitat]][[param_q]] <- grab_quantiles(data_new[Include==1, ], habitat, param, quadsize=q, type="quantile")
          data_directory[["Species"]][["threshold"]][[habitat]][[param_q]] <- grab_quantiles(data_new, habitat, param, quadsize=q, type="threshold")
        }
      } else {
        ### Grab quantile data
        data_directory[["Species"]][["quantile"]][[habitat]][[param]] <- grab_quantiles(data_new[Include==1, ], habitat, param, quadsize="None", type="quantile")
        data_directory[["Species"]][["threshold"]][[habitat]][[param]] <- grab_quantiles(data_new, habitat, param, quadsize="None", type="threshold")
      }
    }
    
    ### Provide overview of QAQC flags
    data_directory[["Species"]][["flag_overview_wide"]][[habitat]] <- flag_overview(data_new[Include==1, ], return="wide")
    
    ### Grab values that fall outside of Expected values (15Q check)
    data_directory[["Species"]][["fifteenQ"]][[habitat]] <- flag_overview(data_new[Include==1, ], return="fifteen")
    
    ### Collect Jan 1st values
    data_directory[["Species"]][["jan_dates"]][[habitat]] <- jan_dates(data_new[Include==1, ])
  }
  
  data_directory[["Species"]][["comparison_table"]] <- comparison_table
  data_directory[["Species"]][["program_count_table"]] <- program_count_table
  
}
toc()

file_out <- paste0("Comparison_Report_",gsub("-","",Sys.Date()))

# Render reports
rmarkdown::render(input = "ReportTemplate.Rmd",
                  output_format = "pdf_document",
                  output_file = paste0("output/",file_out,".pdf"),
                  clean = TRUE)
unlink(paste0(file_out,".md"))
unlink(paste0(file_out,".log"))
unlink(paste0(file_out,".txt"))

# Export Excel overview tables
vq_results <- bind_rows(data_directory[["Discrete WQ"]][["vq_program_table"]], 
                        .id = "Parameter")

# Combine tables from data_directory
flag_results <- data.table()
program_count_results <- data.table()
filename_summary <- data.table()
for(i in names(data_directory)){
  # Handles "species" separately to account for habitat names
  if(i=="Species"){
    flag_results_df <- bind_rows(data_directory[[i]][["flag_overview_wide"]], 
                                 .id="Habitat") %>% 
      rename("Total Program Data" = n_total_prog)
    program_counts_df <- bind_rows(data_directory[[i]][["program_count_table"]]) %>%
      rename(parameter = ParameterName,
             Habitat = habitat)
    
    # Grab file names to include in export
    newFileName <- bind_rows(data_directory[[i]][["new_file_name"]])
    oldFileName <- bind_rows(data_directory[[i]][["old_file_name"]])
    # Add file names for each habitat
    fileNameSummary <- program_counts_df %>%
      rowwise() %>%
      mutate(oldFileName = oldFileName[[Habitat]],
             newFileName = newFileName[[Habitat]]) %>%
      group_by(Habitat, oldFileName, newFileName) %>%
      summarise()
  } else {
    flag_results_df <- bind_rows(data_directory[[i]][["flag_overview_wide"]]) %>% 
      rename("Total Program Data" = n_total_prog) %>%
      mutate(Habitat = i)
    program_counts_df <- bind_rows(data_directory[[i]][["program_count_table"]]) %>%
      mutate(Habitat = i)
    # Grab file names to include in export
    newFileName <- bind_rows(data_directory[[i]][["new_file_name"]])
    oldFileName <- bind_rows(data_directory[[i]][["old_file_name"]])
    # Add file names for each parameter
    fileNameSummary <- program_counts_df %>%
      rowwise() %>%
      mutate(oldFileName = oldFileName[[parameter]],
             newFileName = newFileName[[parameter]]) %>%
      group_by(parameter, Habitat, oldFileName, newFileName) %>%
      summarise()
  }
  flag_results <- bind_rows(flag_results, flag_results_df)
  program_count_results <- bind_rows(program_count_results, program_counts_df)
  filename_summary <- bind_rows(filename_summary, fileNameSummary)
}

rm(program_counts_df, flag_results_df, fileNameSummary)

# flag_results <- flag_results %>% select(
#   ProgramID, ParameterName, Habitat, 
#   "Total Program Data", "1Q", "8Q", "15Q", "16Q", "17Q")

# Create list in format "WorksheetName" = datatable
ws <- list("Filename Summary" = filename_summary, 
           "Program Differences" = program_count_results,
           "SEACAR QAQCFlag Overview" = flag_results,
           "ValueQualifiers by Program" = vq_results)

openxlsx::write.xlsx(ws,
                     file=paste0("output/Comparison_Report_Export_",
                                 gsub("-","",Sys.Date()), ".xlsx"),
                     headerStyle = createStyle(textDecoration = "BOLD"),
                     colWidths= "auto")
