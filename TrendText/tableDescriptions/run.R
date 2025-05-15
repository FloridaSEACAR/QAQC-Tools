library(rstudioapi)
library(knitr)
library(openxlsx)
knitr::opts_chunk$set(warning = FALSE, echo = FALSE, message = FALSE)

wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# The lines below render the individual reports and provide an output file
rmarkdown::render("SAV_TableDescriptions.Rmd")
rmarkdown::render("WC_Discrete_TableDescriptions.Rmd")
rmarkdown::render("WC_Continuous_TableDescriptions.Rmd")
rmarkdown::render("WC_Nekton_TableDescriptions.Rmd")
rmarkdown::render("Coral_TableDescriptions.Rmd")
rmarkdown::render("CW_TableDescriptions.Rmd")
rmarkdown::render("Oyster_TableDescriptions.Rmd")

# That output file is then loaded to produce these combined reports
descTable <- setDT(read.xlsx(paste0("output/Atlas_Descriptions_", 
                                    gsub("_","-",(Sys.Date())), ".xlsx")))

# Ensure that "None" entries in "SamplingFrequency" column are rendered as NA in final output
descTable$SamplingFrequency[descTable$SamplingFrequency=="None"] <- NA

# Add ParameterVisId values in case they are needed for Atlas deployment
figCaps <- openxlsx::read.xlsx("data/AtlasFigureCaptions_Final.xlsx") %>%
  select(-c("Website", "FigureCaptions"))
figCaps$IndicatorName[figCaps$IndicatorName=="Percent Cover" & 
                        figCaps$HabitatName=="Submerged Aquatic Vegetation"] <- "Percent Cover (by species)"

# Combine ParameterVisId into final output file
descTable <- merge(descTable, figCaps, all.x = T)

# Add MA AreaID into final output file
MA_All <- fread("data/ManagedArea.csv")
# Temporary re-naming of Southeast Florida Coral Reef Ecosystem Conservation Area & St. Andrews
descTable$ManagedAreaName[descTable$ManagedAreaName=="Southeast Florida Coral Reef Ecosystem Conservation Area"] <- "Kristin Jacobs Coral Aquatic Preserve"
descTable$ManagedAreaName[descTable$ManagedAreaName=="St. Andrews State Park Aquatic Preserve"] <- "St. Andrews Aquatic Preserve"

descTable <- merge(MA_All[, c("ManagedAreaName", "AreaID")], descTable, by = "ManagedAreaName", all.y = T)

# Export output file
write.xlsx(descTable,
           file = paste0("output/Atlas_Descriptions_",
                         gsub("_","-",(Sys.Date())), ".xlsx"),
           asTable = T)

# Import WebsiteParameters.csv
websiteParams <- fread("data/WebsiteParameters.csv")
# Correct order of websiteParams to match format from the Atlas
websiteParams <- websiteParams %>% 
  arrange(factor(IndicatorName, levels = c("Nutrients","Water Quality","Water Clarity")),
          factor(ParameterName, levels = c("Total Nitrogen","Total Phosphorus",
                                           "Dissolved Oxygen", "Dissolved Oxygen Saturation", "Salinity", "Water Temperature", "pH",
                                           "Turbidity", "Total Suspended Solids", "Chlorophyll a, Uncorrected for Pheophytin",
                                           "Chlorophyll a, Corrected for Pheophytin", "Secchi Depth", "Colored Dissolved Organic Matter"))) %>%
  filter(Website==1)
setDT(websiteParams)

rmarkdown::render("main.Rmd", output_file = paste0("output/allTableDescriptions_", Sys.Date(), ".docx"))
