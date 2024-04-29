library(xlsx)
library(data.table)
library(dplyr)

#List data files
seacardat <- list.files(seacar_data_location, full.names = TRUE)
# Subset for species habitat files only
habitat_files <- str_subset(seacardat, "All_")
wq_files <- str_subset(seacardat, "Combined_WQ_WC_NUT")
wq_files_disc <- str_subset(wq_files, "_cont_", negate=TRUE)

# empty table to store results
## SPECIES
data_log <- data.table()
for(file in habitat_files){
  
  short_file <- tail(str_split(file, "/")[[1]],1)
  
  dat <- fread(file, sep='|')
  
  df <- dat %>% 
    dplyr::group_by(ParameterName) %>%
    dplyr::summarise(n = n())
  df$file_name <- short_file
  
  data_log <- rbind(data_log, df)
  
}

write.xlsx(data_log, file="output/parameters_in_exports.xlsx", row.names = FALSE)

# empty table to store results
## DISCRETE
data_log <- data.table()
for(file in wq_files_disc){
  
  short_file <- tail(str_split(file, "/")[[1]],1)
  
  dat <- fread(file, sep='|')
  
  df <- dat %>% 
    dplyr::group_by(ParameterName) %>%
    dplyr::summarise(n = n())
  df$file_name <- short_file
  
  data_log <- rbind(data_log, df)
  
}

write.xlsx(data_log, file="output/parameters_in_exports_discrete.xlsx", row.names = FALSE)