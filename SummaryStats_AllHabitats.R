source("../SEACAR_data_location.R")
all_files <- list.files(seacar_data_location, full=T, pattern = ".txt")

library(future)
library(future.apply)
plan(multisession, workers = 4)

summary_list <- future_lapply(all_files, function(file){
  df <- data.table::fread(file, select = c("Habitat", "IndicatorName", "ProgramID", "ProgramName", "ParameterName", "ResultValue"))
  if(nrow(df)==0) return(NULL)
  
  df[, .(
    min = min(ResultValue),
    median = median(ResultValue),
    max = max(ResultValue),
    sd = sd(ResultValue),
    n = .N
  ), by = .(Habitat, IndicatorName, ProgramID, ProgramName, ParameterName)]
})

plan(sequential)

summary_results <- rbindlist(summary_list, use.names = TRUE, fill = TRUE) %>%
  mutate(across(where(is.numeric), \(x) round(x, digits = 2)))

openxlsx::write.xlsx(summary_results, asTable = T, file = "output/AllHabitats_summary_stats.xlsx")
