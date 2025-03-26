library(rstudioapi)
library(knitr)
library(openxlsx)
knitr::opts_chunk$set(warning = FALSE, echo = FALSE, message = FALSE)

wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# The lines below render the indiviudal reports and provide an output file
rmarkdown::render("WC_Discrete_Trend_Text.Rmd")
rmarkdown::render("WC_Continuous_Trend_Text.Rmd")

# That output file is then loaded to produce these combined reports
descTable <- setDT(read.xlsx(paste0("output/Atlas_Descriptions_", 
                                    gsub("_","-",(Sys.Date())), ".xlsx")))

# Correct order of websiteParams to match format from the Atlas
websiteParams <- websiteParams %>% 
  arrange(factor(IndicatorName, levels = c("Nutrients","Water Quality","Water Clarity")),
          factor(ParameterName, levels = c("Total Nitrogen","Total Phosphorus",
                                           "Dissolved Oxygen", "Dissolved Oxygen Saturation", "Salinity", "Water Temperature", "pH",
                                           "Turbidity", "Total Suspended Solids", "Chlorophyll a, Uncorrected for Pheophytin",
                                           "Chlorophyll a, Corrected for Pheophytin", "Secchi Depth", "Colored Dissolved Organic Matter"))) %>%
  filter(Website==1)
setDT(websiteParams)

rmarkdown::render("main.Rmd")