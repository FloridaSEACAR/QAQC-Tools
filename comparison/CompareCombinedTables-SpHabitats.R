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

new_files <- list.files(seacar_data_location, full.names = TRUE)
old_files <- list.files((paste0(seacar_data_location,"archive/2023-Oct-11")), full.names = TRUE)

new_hab_files <- str_subset(new_files, "All_")
old_hab_files <- str_subset(old_files, "All_")

# Determine whether to collect & include VQ data in reports (too many pages)
collect_vq_data <- FALSE

# Test variable to delete a column to check column comparison functionality
test_columns <- FALSE

## Variables set to FALSE, script will change to TRUE if conditions met
columns_differ <- FALSE
programs_differ <- FALSE
species_differ <- FALSE

data_directory <- list()
habitat_data <- data.table()
program_result_table <- data.frame(ProgramID = integer(), stringsAsFactors = FALSE)

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
  data_directory[["new_file_name"]][[habitat]] <- new_file_short
  data_directory[["old_file_name"]][[habitat]] <- old_file_short
  
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
  
  habitat_data <- bind_rows(habitat_data, data_table)
  
  # Column Differences ----
  if(length(names(data_old))!=length(names(data_new))){
    print(paste0("There is a difference in the number of columns for ", new_file_short, " vs. ", old_file_short))
    oldColumns <- str_replace_all(sort(names(data_old)),"_","-")
    newColumns <- str_replace_all(sort(names(data_new)),"_","-")
    
    diff_df <- data.table()
    diff <- data.table()
    if(length(setdiff(newColumns,oldColumns))){
      diff <- data.table("column" = setdiff(newColumns,oldColumns),
                         "export" = "new")
      diff_df <- rbind(diff_df, diff)
      print("Column difference in new export")
    } else if(length(setdiff(oldColumns,newColumns))){
      diff <- data.table("column" = setdiff(oldColumns,newColumns),
                         "export" = "old")
      diff_df <- rbind(diff_df, diff)
      print("Column difference in old export")
    }
    
    diff_df <- diff_df %>%
      mutate(color_code = ifelse(export=="old", "red", "green"))
    
    colorize_column <- function(col, color_code) {colorize(col, color_code)}
    
    newColumns_color <- unlist(lapply(newColumns, function(col) {
      ifelse(col %in% diff_df$column, colorize_program(col, diff_df$color_code), col)
    }))
    
    oldColumns_color <- unlist(lapply(oldColumns, function(col) {
      ifelse(col %in% diff_df$column, colorize_program(col, diff_df$color_code), col)
    }))
    
    data_directory[["column_compare"]][[habitat]][['oldColumns']] <- oldColumns_color
    data_directory[["column_compare"]][[habitat]][['newColumns']] <- newColumns_color
  }
  
  # Program Differences ----
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
    
    colorize_program <- function(pid, color_code) {colorize(pid, color_code)}
    
    new_programs_color <- unlist(lapply(new_programs, function(pid) {
      ifelse(pid %in% diff_df$pid, colorize_program(pid, diff_df$color_code), pid)
    }))
    
    old_programs_color <- unlist(lapply(old_programs, function(pid) {
      ifelse(pid %in% diff_df$pid, colorize_program(pid, diff_df$color_code), pid)
    }))
    
    data_directory[["program_compare"]][[param]][["old_programs"]] <- old_programs_color
    data_directory[["program_compare"]][[param]][["new_programs"]] <- new_programs_color
    data_directory[["program_compare"]][[param]][["diff_df"]] <- diff_df
    
  }
  # Species Differences ----
  if(!habitat=="Oyster"){
    if(length(sort(data_old[, unique(CommonIdentifier)]))!=length(sort(data_new[, unique(CommonIdentifier)]))){
      print(paste0("There is a difference in the number of Species for ", new_file_short, " vs. ", old_file_short))
      old_species <- sort(unique(data_old$CommonIdentifier))
      new_species <- sort(unique(data_new$CommonIdentifier))
      
      diff_df <- data.table()
      diff <- data.table()
      if(length(setdiff(new_species,old_species))){
        diff <- data.table("sp" = setdiff(new_species,old_species),
                           "export" = "new")
        diff_df <- rbind(diff_df, diff)
        print("Species difference in new export")
      } else if(length(setdiff(old_species,new_species))){
        diff <- data.table("sp" = setdiff(old_species,new_species),
                           "export" = "old")
        diff_df <- rbind(diff_df, diff)
        print("Species difference in old export")
      }
      
      diff_df <- diff_df %>%
        mutate(color_code = ifelse(export=="old", "red", "green"))
      
      data_directory[["species_compare"]][[habitat]][["diff_df"]] <- diff_df
    }
  }
  
}

file_out <- paste0(old_file_shorter, "-vs-", new_file_shorter,"_SpeciesHabitats_Report")

# Render reports
rmarkdown::render(input = "comparison/ReportTemplate-SpHabitats.Rmd",
                  output_format = "pdf_document",
                  output_file = paste0(file_out,".pdf"),
                  clean = TRUE)
unlink(paste0(file_out,".md"))