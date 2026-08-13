library(dplyr)
library(stringr)
library(knitr)
library(data.table)

files <- list.files(seacar_data_location, full=T, pattern = ".txt")

results <- data.frame()
ma_results <- data.frame()
for(file in files){
  df <- fread(file, sep = "|", na.strings="NULL")
  if(nrow(df)==0) next
  df2 <- df %>% 
    group_by(ProgramID, ProgramName, Habitat, ParameterName) %>% 
    reframe(
      n = n(),
      minSampleDate = min(SampleDate),
      maxSampleDate = max(SampleDate)
    )
  results <- bind_rows(results, df2)
  rm(df2)
  if(!str_detect(file, "_cont_")){
    df3 <- df %>% 
      group_by(AreaID, ManagedAreaName, Habitat) %>% 
      reframe(n = n())
    df3$AreaID <- as.character(df3$AreaID)
    ma_results <- bind_rows(ma_results, df3)
    rm(df3)    
  }
  rm(df)
}

prog_matrix <- openxlsx2::wb_to_df("C:/Users/Hill_T/Downloads/SEACAR Program Matrix (11).xlsx", show_formula = TRUE) %>%
  rowwise() %>%
  mutate(`Program Name` = str_split_1(`Program Name`, pattern = '\\"')[[4]]) %>%
  select(ID, `Program Name`, `Start Year`, `End Year`, Frequency, `Managing Entity`, Habitats, `SEACAR Citation`) %>%
  rowwise() %>%
  mutate(me = stringr::str_split_1(`Managing Entity`, ";")[[1]],
         rcp = ifelse(str_detect(me, "Office of Resilience"), TRUE, FALSE),
         cite_year = str_extract(`SEACAR Citation`, "(?<=\\()\\d{4}(?=\\))")) %>%
  tidyr::separate_rows("Habitats", sep = ", ")

results_grouped <- results %>% 
  group_by(ProgramID, ProgramName, Habitat) %>%
  reframe(n = sum(n),
          params = paste(unique(ParameterName), collapse = "|"),
          minDate = min(minSampleDate),
          maxDate = max(maxSampleDate))

combined <- prog_matrix %>% 
  left_join(
    results_grouped, 
    join_by(
      "ID" == "ProgramID",
      "Habitats" == "Habitat",
      "Program Name" == "ProgramName"
    )
  ) %>%
  mutate(
    maxDateYear = year(maxDate),
    mismatch.Data.vs.Cite = maxDateYear!=cite_year,
    DataOlderThan1Year = (as.POSIXct(Sys.Date()) - as.POSIXct(maxDate)) >= 365
  ) %>% 
  rename("ProgramID" = "ID") %>% 
  select(ProgramID, `Program Name`, everything(), -me)

openxlsx::write.xlsx(combined, file = "output/ProgramMatrix_RCP_CitationDates.xlsx", asTable=T)

### stacked bar chart
library(ggplot2)
na_results <- ma_results %>% 
  filter(is.na(ManagedAreaName)) %>% 
  group_by(Habitat) %>% 
  reframe(n = sum(n))
na_results$inMA <- "Non-MA"

non_na_results <- ma_results %>% 
  filter(!is.na(ManagedAreaName)) %>% 
  group_by(Habitat) %>% 
  reframe(n = sum(n))
non_na_results$inMA <- "MA"

ma_results_grouped <- bind_rows(non_na_results, na_results)
ma_results_grouped$inMA <- factor(ma_results_grouped$inMA, levels = c("Non-MA", "MA"))

options(scipen = 999)

ggplot(ma_results_grouped, aes(x = Habitat, y = n, fill = inMA)) +
  geom_col() +
  labs(title = "Number of data entries by Habitat",
       x = "Habitat",
       y = "Number of data entries") +
  SEACAR::SEACAR_plot_theme()
