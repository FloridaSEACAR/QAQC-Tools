library(data.table)
library(stringr)
library(dplyr)
library(openxlsx)

# Loop through all combined tables to find unique speciesgroup1 and 2 groupings
files <- list.files("C:/SEACAR Data/SEACARdata", 
                    full.names = TRUE,
                    pattern = ".txt")
hab_files <- str_subset(files, "All_")

species_groups <- data.table()
for(file in str_subset(hab_files, "OYSTER", negate=T)){
  df <- fread(file, sep='|', na.strings="NULL")
  
  sp_group <- df %>% 
    group_by(IndicatorID, IndicatorName, ParameterID, ParameterName, 
             Group1ID, SpeciesGroup1, Group2ID, SpeciesGroup2) %>% 
    summarise()
  
  sp_group$habitat <- tail(str_split(file, "/")[[1]],1)
  species_groups <- rbind(species_groups, sp_group)
  
}

write.xlsx(species_groups, paste0("data/species_groups_", gsub("-","_",Sys.Date()),".xlsx"))
