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

# That output file is then loaded to produce these combined reports
descTable <- setDT(read.xlsx(paste0("output/Atlas_Descriptions_", 
                                    gsub("_","-",(Sys.Date())), ".xlsx")))
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
