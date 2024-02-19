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

# archive subfolder must contain only 1 set of each combined table
# Ideal workflow upon new combined table export is to delete previous files from
# archive subfolder, place old export files from SEACARdata into archive before
# running unzip.R to place new combined exports into SEACARdata
# New files in /SEACARdata/
# Old files in /SEACARdata/archive

files <- list.files(seacar_data_location, full.names = TRUE)
old_files <- list.files((paste0(seacar_data_location,"archive/")), full.names = TRUE)

wq_disc_files <- str_subset(str_subset(files, "Combined_WQ_WC_NUT_"), "_cont_", negate = TRUE)
wq_disc_files_old <- str_subset(str_subset(old_files, "Combined_WQ_WC_NUT_"), "_cont_", negate = TRUE)

# Determine whether to collect & include VQ data in reports (too many pages)
collect_vq_data <- FALSE

## Variables set to FALSE, script will change to TRUE if conditions met
columns_differ <- FALSE
programs_differ <- FALSE

data_directory <- list()
old_new_data <- data.table()
program_result_table <- data.frame(ProgramID = integer(), stringsAsFactors = FALSE)

# wq_disc_files[c(2,3,16)]

for(file in wq_disc_files){
  # Shortened file names for later use
  new_file_short <- tail(str_split(file, "/")[[1]],1)
  new_file_shorter <- str_split(paste(tail(str_split(str_split(new_file_short, "Combined_WQ_WC_NUT_")[[1]][2], "-")[[1]],3),collapse = "-"),".txt")[[1]][1]
  
  # pattern to grab relevant "old" file
  pattern <- str_split(str_split(new_file_short, "NUT_")[[1]], "-")[[2]][1]
  # if(pattern=="Dissolved_Oxygen"){
  #   pattern <- paste0(pattern,"-")
  # }
  pattern <- ifelse(pattern=="Dissolved_Oxygen", paste0(pattern,"-"), pattern)
  
  # Grab "old" export file, file_short
  old_file <- str_subset(wq_disc_files_old, pattern)
  old_file_short <- tail(str_split(old_file, "/")[[1]],1)
  old_file_shorter <- str_split(paste(tail(str_split(str_split(old_file_short, "Combined_WQ_WC_NUT_")[[1]][2], "-")[[1]],3),collapse = "-"),".txt")[[1]][1]
  
  # Read in data frame for each combined data export
  print(paste0("Reading in: ", new_file_short))
  data_new <- fread(file, sep='|', na.strings = "NULL")
  # Read in old data
  print(paste0("Reading in: ", old_file_short))
  data_old <- fread(old_file, sep='|', na.strings = "NULL")
  
  # Full ParameterName for a given file
  param <- data_new[, unique(ParameterName)]
  
  data_table <- data.table(
    "parameter" = param,
    "oldFile" = old_file_shorter,
    "newFile" = new_file_shorter,
    "nDataOld" = nrow(data_old),
    "nDataNew" = nrow(data_new)
  )
  
  data_table[ , `:=` (difference = nDataNew - nDataOld)]
  
  old_new_data <- bind_rows(old_new_data, data_table)
  
  # Create list of programs and their datasets
  new_programs <- data_new %>%
    distinct(ProgramID, ParameterName) %>% arrange(ProgramID) %>%
    mutate(ParameterName = paste0(ParameterName, "-new"))
  old_programs <- data_old %>%
    distinct(ProgramID, ParameterName) %>% arrange(ProgramID) %>%
    mutate(ParameterName = paste0(ParameterName, "-old"))
  
  program_result_table <- bind_rows(program_result_table, rbind(new_programs, old_programs))
  
  # compare <- arsenal::comparedf(data_old, data_new)
  # data_directory[[param]][["comparison"]] <- compare
  
  ##### Comparison checks #### ----
  ## The following are intended to check for inconsistencies between data exports
  
  ### Compare columns and class types
  ## If they have different number of columns, list them in table below
  if(length(names(data_old))!=length(names(data_new))){
    print(paste0("There is a difference in the number of columns for ", new_file_short, " vs. ", old_file_short))
    column_compare <- data.table(
      "oldColumns" = sort(names(data_old)),
      "oldColumnsType" = sapply(data_old, class),
      "newColumns" = sort(names(data_new)),
      "newColumnsType" = sapply(data_new, class)
    )
    data_directory[["column_compare"]][[param]] <- column_compare
  }
  
  ### Compare programs between exports
  ## If they have different lengths, record which programs are included/not included
  if(length(sort(data_old[, unique(ProgramID)]))!=length(sort(data_new[, unique(ProgramID)]))){
    print(paste0("There is a difference in the number of ProgramIDs for ", new_file_short, " vs. ", old_file_short))
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
    
    data_directory[["program_compare"]][[param]][["old_programs"]] <- old_programs
    data_directory[["program_compare"]][[param]][["new_programs"]] <- new_programs
    data_directory[["program_compare"]][[param]][["diff_df"]] <- diff_df
    
  }
  
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
    data_directory[["vq_table"]][[param]] <- vq_table
    data_directory[["vq_program_table"]][[param]] <- vq_program_table 
    
    
  }
  
}

# program_result_table$Present <- 1
# programs_wide <- program_result_table %>%
#   pivot_wider(names_from = ProgramID, values_from = Present, values_fill = list(Present = 0)) %>%
#   arrange(ParameterName)

file_out <- "Combined_WQ_WC_NUT_Discrete"

# Render reports
rmarkdown::render(input = "comparison/ReportTemplate.Rmd",
                  output_format = "pdf_document",
                  output_file = paste0(file_out,".pdf"),
                  clean = TRUE)
unlink(paste0(file_out,".md"))