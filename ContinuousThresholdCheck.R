## Begin Threshold check script
all_params <- c("Dissolved Oxygen","Dissolved Oxygen Saturation","pH","Salinity","Turbidity","Water Temperature")

files <- list.files(seacar_data_location, full.names = TRUE)
wq_files <- files[str_detect(files, "Combined_WQ_WC_NUT_")]
wq_cont_files <- str_subset(wq_files, "_cont_")

# Thresholds to check against
# 4000 High threshold for Turbidity is highest of 4000 vs 1000 (YSI vs 6600)
ref_thresholds <- data.table("ParameterName" = all_params,
                             "LowThreshold" = c(0, 0, 2, 0, 0, -5),
                             "HighThreshold" = c(50, 500, 14, 70, 4000, 45))

# To store results
threshold_data <- list()

# Loop through parameters
for (p in all_params){
  # Create parameter for file path pattern recognition
  par <- str_replace_all(p, " ", "_")
  
  # Create exception so DOS and DO are kept separate
  if(p == "Dissolved Oxygen"){
    pattern <- "gen_NE|gen_NW|gen_SE|gen_SW"
    files <- str_subset(wq_cont_files, pattern)
  } else {
    files <- str_subset(wq_cont_files, par)
  }
  
  # Declare low and high thresholds from ref_thresholds dataframe
  low_threshold <- ref_thresholds[ParameterName==p, LowThreshold]
  high_threshold <- ref_thresholds[ParameterName==p, HighThreshold]
  
  # data frame to store results (for each parameter)
  data_frame <- data.frame()
  
  # read each file in
  for(file in files){
    
    data <- fread(file, sep='|')
    
    # filter for high and low thresholds
    data <- data[(ResultValue < low_threshold) | (ResultValue > high_threshold), ]
    # sometimes VQ NAs are perceived as character, force to integer
    data$ValueQualifier <- as.integer(data$ValueQualifier)
    
    # Record threshold_type in new column
    data[ResultValue < low_threshold, `:=` (threshold_type = "low")]
    data[ResultValue > high_threshold, `:=` (threshold_type = "high")]
    
    # combine each region file for a given parameter into a single dataframe
    data_frame <- bind_rows(data_frame, data)
  }
  
  # record data results for each parameter
  threshold_data[[p]] <- data_frame
}

# combine all data results for all parameters
threshold_data_combined <- bind_rows(threshold_data, .id = "parameter")

# group together and produce summary statistics table
group_df <- threshold_data_combined %>% 
  group_by(ProgramID, ProgramName, parameter, threshold_type) %>%
  summarise(n_data = n(),
            SEACAR_QAQCFlagCode = list(unique(SEACAR_QAQCFlagCode)),
            Include = list(unique(Include)), .groups="keep")