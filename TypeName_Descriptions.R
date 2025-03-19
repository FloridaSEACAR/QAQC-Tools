# Script to extract the Method Metadata type names and their corresponding column descriptions from
# SEACAR_Metadata.xlsx files and save them as an .xlsx file.
#
# Author: Stephen R. Durham, PhD
#         Office of Resilience and Coastal Protection
#         Florida Department of Environmental Protection
#         Stephen.Durham@FloridaDEP.gov
#
# Date: 3/14/2025
#
# Note: The final output as of 3/19/2025 does not have Coral/Coral Reef parameter IDs or names for 
# the nekton type_names because although they are column names in the Col_Coral worksheet, they do
# not appear in the Ref_MethodMetadata worksheet. Similarly, the SurveyMethod type_name for the 
# nekton-related parameter IDs in the Coral/Coral Reef habitat require some post-processing to
# remove four duplicate rows and add the column description from Col_Nekton.


library(tidyverse)
library(data.table)
library(openxlsx)
library(here)

metadata_path <- here::here("data/SEACAR_Metadata_20250313.xlsx")

#Load 'Ref_MethodMetadata' sheet
refmm <- setDT(read.xlsx(metadata_path, sheet = "Ref_MethodMetadata", startRow = 7, colNames = T))
refmm <- janitor::clean_names(refmm)

#Keep distinct combinations of habitat and type name
refmm2 <- distinct(refmm[, .(habitat, parameter_id, parameter_name, method_metadata_type_id, type_name)])

#Load and combine column description metadata sheets
colsheets <- c("Col_WaterQuality_Discrete",
               "Col_WaterQuality_Continuous",
               "Col_SAV",
               "Col_Oyster",
               "Col_Nekton",
               "Col_Acreage",
               "Col_Coral",
               "Col_CoastalWetlands")

coldefs <- lapply(colsheets, function(x){
  col_x <- setDT(read.xlsx(metadata_path, sheet = x, startRow = ifelse(str_detect(x, "Acreage"), 9, 7), colNames = T))
  col_x <-janitor::clean_names(col_x)
  col_x[, sheet := x]
  
  return(col_x)
  
})

coldefs2 <- rbindlist(coldefs)

#Add 'habitat' column to coldefs2
coldefs2[,`:=` (habitat = fcase(str_detect(sheet, "WaterQuality|Nekton"), "Water Column",
                                str_detect(sheet, "SAV"), "Submerged Aquatic Vegetation",
                                str_detect(sheet, "Oyster"), "Oyster/Oyster Reef",
                                str_detect(sheet, "Coral"), "Coral/Coral Reef",
                                str_detect(sheet, "CoastalWetlands"), "Coastal Wetlands",
                                default = NA_character_),
                nekton = ifelse(str_detect(sheet, "Nekton"), 1, 0))]

coldefs3 <- distinct(coldefs2[column_name %in% refmm2$type_name, .(habitat, nekton, type_name = column_name, description)])

nektypes <- intersect(coldefs3[habitat == "Water Column", unique(type_name)], coldefs3[habitat == "Coral/Coral Reef", unique(type_name)])
nekpars <- intersect(refmm2[habitat == "Water Column", unique(parameter_name)], refmm2[habitat == "Coral/Coral Reef", unique(parameter_name)])
nektypes_id <- refmm2[str_detect(habitat, "Water|Coral") & 
                        parameter_name %in% nekpars &
                        type_name %in% nektypes & 
                        !(method_metadata_type_id %in% refmm2[parameter_name == "Water Temperature", method_metadata_type_id]), 
                      max(method_metadata_type_id), 
                      by = list(habitat, type_name)]

refmm2[, nekton := ifelse(method_metadata_type_id %in% nektypes_id$V1, 1, 0), by = .I]
refmm2[str_detect(habitat, "Coral") & !(parameter_id %in% c(79, 80)), nekton := 0]


#Add parameter ID and name to coldefs
coldefs4 <- merge(coldefs3[type_name %in% refmm2$type_name, ], distinct(refmm2[, .(habitat, nekton, parameter_id, parameter_name, type_name)]), by = c("habitat", "nekton", "type_name"), all.x = T)
coldefs4[str_detect(habitat, "Coral") & parameter_id %in% c(79, 80), nekton := 1]

#Add non-nekton "SurveyMethod" type name manually because it is not in the current column definitions
smeth <- coldefs4[habitat == "Water Column" & type_name == "ActivityType", ]
smeth[, `:=` (type_name = "SurveyMethod", description = "Indicates whether the record is from a continuous or discrete water sampling program.")]
coldefs4 <- rbind(coldefs4, smeth)

#Add type ID to coldefs
coldefs5 <- merge(coldefs4, distinct(refmm2[, .(habitat, nekton, parameter_id, parameter_name, method_metadata_type_id, type_name)]), by = c("habitat", "nekton", "parameter_id", "parameter_name", "type_name"), all = T)
setcolorder(coldefs5, c("habitat", "nekton", "parameter_id", "parameter_name", "method_metadata_type_id", "type_name", "description"))
setorder(coldefs5, habitat, nekton, parameter_id, method_metadata_type_id)

#Save the result as an .xlsx file
write.xlsx(coldefs5,
           here::here(paste0("TypeName_Descriptions_", Sys.Date(), ".xlsx")),
           asTable = T,
           colNames = T,
           firstRow = T,
           colWidths = "auto")
