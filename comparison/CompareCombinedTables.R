library(data.table)
library(stringr)
library(dplyr)
library(tidyr)
# library(dfCompare)
# library(compareDF)
library(arsenal)
library(glue)
library(ggplot2)
library(viridis)
library(extrafont)
library(knitr)
library(kableExtra)

# Load in SEACAR combined tables
source("seacar_data_location.R")

# archive subfolder must contain a folder with the date of combined tables
# Ideal workflow upon new combined table export is to place old export files 
# from SEACARdata into new archive subfolder (with name of export as folder name)
# before running unzip.R to place new combined exports into SEACARdata
# New files in /SEACARdata/
# Old files in /SEACARdata/archive/YYYY-Mmm-DD, with old_file_date declared as the date below

old_file_date <- "2024-Jan-10"

new_files <- list.files(seacar_data_location, full.names = TRUE)
old_files <- list.files((paste0(seacar_data_location,"archive/",old_file_date)), full.names = TRUE)

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
collect_vq_data <- FALSE

# Display file names in report (otherwise dates in summary table may be sufficient)
file_name_display <- FALSE

# Test variable to delete a column to check column comparison functionality
# test_columns <- FALSE

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
    
    # data_directory[["program_compare"]][[habitat]][[param]][["old_programs"]] <- old_programs_color
    # data_directory[["program_compare"]][[habitat]][[param]][["new_programs"]] <- new_programs_color
    # data_directory[["program_compare"]][[habitat]][[param]][["diff_df"]] <- diff_df
    
    print(paste0("There is a difference in the number of ProgramIDs for ",
                 new_file_short, " vs. ", old_file_short))
    
    return(list("old_programs" = old_programs_color, "new_programs" = new_programs_color, "diff_df" = diff_df))
  }
}

# Provide counts of data by program for each parameter
program_counts <- function(data_old, data_new, habitat, param){

  if(!habitat=="Species"){
    program_count_table <- merge(
      data_old %>% group_by(ProgramID) %>% summarise(nOld = n()),
      data_new %>% group_by(ProgramID) %>% summarise(nNew = n())) %>%
      mutate(difference = nNew - nOld)
    program_count_table$parameter <- param
  } else {
    program_count_table <- merge(
      data_old %>% group_by(ProgramID, ParameterName) %>% summarise(nOld = n()),
      data_new %>% group_by(ProgramID, ParameterName) %>% summarise(nNew = n())) %>%
      mutate(difference = nNew - nOld)
  }
  
  return(program_count_table)
}

# Select which habitats to include in report
habitats <- c("Discrete", "Continuous", "Species")
# habitats <- c("Species")

# Begin Discrete processing
if("Discrete" %in% habitats){
  habitat <- "Discrete"
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
    # Read in old data
    print(paste0("Reading in: ", old_file_short))
    data_old <- fread(old_file, sep='|', na.strings = "NULL")
    
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
      data_old, data_new, old_file_short, new_file_short, habitat, param)
    
    ### Compare programs between exports
    ## If they have different lengths, record which programs are included/not included
    data_directory[[habitat]][["program_compare"]][[param]] <- compare_programs(
      data_old, data_new, old_file_short, new_file_short, habitat, param)
    
    ### Provide counts of data by program by parameter
    program_count_table <- bind_rows(program_count_table, program_counts(
      data_old, data_new, habitat, param))
    
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
        group_by(ProgramID, ProgramName, ValueQualifier, ValueQualifierSource) %>%
        summarise(n_vq_data = n(), .groups = "keep") %>%
        arrange(ValueQualifier)
      
      # append individual tables into data_directory
      data_directory[[habitat]][["vq_table"]][[param]] <- vq_table
      data_directory[[habitat]][["vq_program_table"]][[param]] <- vq_program_table
    }
  }
  
  data_directory[[habitat]][["comparison_table"]] <- comparison_table
  data_directory[[habitat]][["program_count_table"]] <- program_count_table
}

# Begin Continuous processing
if("Continuous" %in% habitats){
  habitat <- "Continuous"
  comparison_table <- data.table()
  program_count_table <- data.table()
  
  cont_params <- c("Dissolved_Oxygen","Dissolved_Oxygen_Saturation","pH", 
                   "Salinity", "Turbidity", "Water_Temperature")
  
  for (p in cont_params){
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
        # Read in old data
        print(paste0("Reading in: ", old_file_short))
        data_old <- fread(old_file, sep='|', na.strings = "NULL")
        
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
        
        ##### Comparison checks #### ----
        ## The following are intended to check for inconsistencies between data exports
        
        ### Compare columns and class types
        ## If they have different number of columns, list them in table below
        data_directory[[habitat]][["column_compare"]][[param]] <- compare_columns(
          data_old, data_new, old_file_short, new_file_short, habitat, param)
        
        ### Compare programs between exports
        ## If they have different lengths, record which programs are included/not included
        data_directory[[habitat]][["program_compare"]][[param]] <- compare_programs(
          data_old, data_new, old_file_short, new_file_short, habitat, param)
        
        ### Provide counts of data by program by parameter
        program_count_table <- bind_rows(program_count_table, program_counts(
          data_old, data_new, habitat, param))
        
      }
    }
  }
  
  data_directory[[habitat]][["comparison_table"]] <- comparison_table
  data_directory[[habitat]][["program_count_table"]] <- program_count_table
  
}

# Begin "Species" processing (SAV, Coral, Oyster, CW, Nekton)
if("Species" %in% habitats){
  comparison_table <- data.table()
  program_count_table <- data.table()
  
  for(i in 1:length(new_hab_files)){
    file <- new_hab_files[i]
    
    # Shortened file names for later use
    new_file_short <- tail(str_split(file, "/")[[1]],1)
    new_file_shorter <- str_split(paste(tail(str_split(str_split(new_file_short, "Parameters")[[1]][2], "-")[[1]],3),collapse = "-"),".txt")[[1]][1]
    
    # pattern to grab relevant "old" file
    pattern <- str_split(str_split(new_file_short, "All_")[[1]][2], "_Parameters")[[1]][1]
    # pattern is habitat name
    habitat <- pattern
    
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
    # Read in old data
    print(paste0("Reading in: ", old_file_short))
    data_old <- fread(old_file, sep='|', na.strings = "NULL")
    
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
      data_old, data_new, old_file_short, new_file_short, habitat, param)
    
    ### Compare programs between exports
    ## If they have different lengths, record which programs are included/not included
    data_directory[["Species"]][["program_compare"]][[habitat]] <- compare_programs(
      data_old, data_new, old_file_short, new_file_short, habitat, param)
    
    params <- unique(data_new$ParameterName)
    for(param in params){
      ### Provide counts of data by program by parameter
      p_count_df <- program_counts(data_old, data_new, "Species", param)
      p_count_df$habitat <- habitat
      program_count_table <- bind_rows(program_count_table, p_count_df)
    }
    
  }
  
  data_directory[["Species"]][["comparison_table"]] <- comparison_table
  data_directory[["Species"]][["program_count_table"]] <- program_count_table
  
}

file_out <- paste0(old_file_shorter, "-vs-", new_file_shorter,"_Report")

# Render reports
rmarkdown::render(input = "comparison/ReportTemplate.Rmd",
                  output_format = "pdf_document",
                  output_file = paste0(file_out,".pdf"),
                  clean = TRUE)
unlink(paste0(file_out,".md"))