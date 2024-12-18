# Species Crosswalk check
# Used to verify that Mthd_SpeciesCrosswalk tab on SEACAR Metadata matches data

# New combined table file locations
new_files <- list.files(seacar_data_location, full.names = TRUE)

# Current Metadata file
sp_crosswalk <- setDT(read_xlsx("data/SEACAR_Metadata_20241206.xlsx", sheet = "Mthd_SpeciesCrosswalk", skip=5))

# Old Metadata file (useful for checking how species groups have changed)
# sp_crosswalk <- setDT(read_xlsx("data/SEACAR_Metadata_20241001.xlsx", sheet = "Mthd_SpeciesCrosswalk", skip=5))

# Convert to text-based NULL for ease of comparison
sp_crosswalk[is.na(sp_crosswalk)] <- "NULL"

# Function to return TRUE/FALSE if there is a match for CommonID, Group1, Group2, Habitat
sp_match <- function(commonID, group1, group2, habitat){
  match <- sp_crosswalk[CommonIdentifier==commonID & Group1==group1 & Group2==group2 & Habitat==habitat, ]
  return(nrow(match)!=0)
}

# Function to apply sp_match function
# If return = "statement" it will return a printed output notifying of species mismatch
# If return = "data" it will return ALL CommonID, Group1, Group2, Hab combinations and their match value
check_species <- function(data_new, return = "statement"){
  species <- setDT(data_new %>% group_by(CommonIdentifier, SpeciesGroup1, SpeciesGroup2, Habitat) %>% reframe())
  species[is.na(species)] <- "NULL"
  # Apply function
  species <- setDT(
    species %>% rowwise() %>% 
      mutate(match = sp_match(CommonIdentifier, SpeciesGroup1, SpeciesGroup2, Habitat))
  )
  # Create print statement
  statement <- ifelse(nrow(species[match==FALSE])>0, 
                      "At least 1 species mismatch detected", 
                      "no species mismatch")
  if(return=="statement"){
    print(statement)
  } else if(return=="data"){
    return(species)
  }
}

# Cross-ref between previous Metadata species crosswalk and current exports to identify species differences
habs <- c("CORAL","CW","NEKTON","SAV")
sp_check <- data.table()
for(h in habs){
  #Locate habitat file and load
  df <- fread(str_subset(new_files, h), sep='|', na.strings = "NULL")
  species <- check_species(data_new = df, return = "data")
  sp_check <- bind_rows(sp_check, species)
}

fwrite(sp_check, file="output/species_check_20241217.csv")
