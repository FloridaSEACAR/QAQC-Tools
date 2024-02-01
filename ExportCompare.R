# Script to compare DDI exports to identify changes
# Author: Stephen R. Durham, PhD
# Date: Fall 2023
# Heading info and some basic commenting added 2/1/2024 -SRD

#Load packages
library(tidyverse)
library(data.table)
library(compareDF)
library(sf)
library(mapview)
library(openxlsx)

#Create objects for the names of the new and old files to be compared, as well as the relevant geospatial files
# newer_fn <- here::here("SEACARdata/All_Oyster_Parameters-2023-Oct-11.txt") #File name of newer export
# older_fn <- here::here("SEACARdata/All_Oyster_Parameters-2023-Sep-05.txt") #File name of older export
newer_fn <- here::here("SEACARdata/All_SAV_Parameters-2023-Oct-12.txt") #File name of newer export
older_fn <- here::here("SEACARdata/All_SAV_Parameters-2023-Jun-05.txt") #File name of older export
alllocs_fn <- here::here("mapping/SampleLocations12dec2023/seacar_dbo_vw_SampleLocation_Point.shp") #Latest shapefile of SEACAR sample locations (for MADup check)
orcp_fn <- here::here("mapping/orcp_all_sites/orcp_all_sites/ORCP_Managed_Areas.shp") #Latest shapefile of ORCP managed area boundaries (for MADup check)

#Load the data files
newer <- fread(newer_fn, sep = "|", na.string = "NULL")
older <- fread(older_fn, sep = "|", na.string = "NULL")
alllocs <- st_read(alllocs_fn)
orcp <- st_read(orcp_fn)

#Make sure the geospatial data are valid and share a common CRS
alllocs <- st_make_valid(alllocs)
alllocs <- st_transform(alllocs, crs = 4326)
orcp <- st_make_valid(orcp)
orcp <- st_transform(orcp, crs = 4326)

#Identify sample locations within overlapping managed area boundaries
mas <- sort(unique(c(newer$ManagedAreaName, older$ManagedAreaName)))
dup2mas <- c("Apalachicola Bay Aquatic Preserve", "Boca Ciega Bay Aquatic Preserve", "Guana River Marsh Aquatic Preserve")
overlaps <- st_intersection(subset(orcp, orcp$LONG_NAME %in% mas))
overlaps2 <- overlaps[, -which(names(overlaps) == "origins")]
overlaps2 <- subset(overlaps2, overlaps2$n.overlaps == 2 & str_detect(overlaps2$LONG_NAME, "Nature", negate = TRUE))
dup2locs <- alllocs[overlaps2, , op = st_intersects]
# mapview(subset(orcp, orcp$LONG_NAME %in% mas), col.regions = "dodgerblue", layer.name = "MAs") +
#   mapview(overlaps2, col.regions = "firebrick", layer.name = "Overlap") +
#   mapview(subset(dup2locs, dup2locs$LocationID %in% newer[MADup == 1, unique(LocationID)]), col.regions = "green", layer.name = "Dup1 Locs") +
#   mapview(subset(dup2locs, dup2locs$LocationID %in% newer[MADup == 2, unique(LocationID)]), col.regions = "purple", layer.name = "Dup2 Locs")

#Assign new, consistent MADup values for both exports
newer[, MADup := NULL]
newer[LocationID %in% dup2locs$LocationID, MADup := ifelse(ManagedAreaName %in% dup2mas, 2, 1), by = ManagedAreaName]
newer[is.na(MADup), MADup := 1]

older[, MADup := NULL]
older[LocationID %in% dup2locs$LocationID, MADup := ifelse(ManagedAreaName %in% dup2mas, 2, 1), by = ManagedAreaName]
older[is.na(MADup), MADup := 1]

#Check for general inconsistencies between the files (MADup values, column names, number of rows)
if(FALSE %in% c(newer[LocationID %in% dup2locs$LocationID & MADup == 2, sort(unique(ManagedAreaName))] %in% dup2mas)) stop(paste0("MADup values are incorrect in ", newer_fn, "."))
if(FALSE %in% c(older[LocationID %in% dup2locs$LocationID & MADup == 2, sort(unique(ManagedAreaName))] %in% dup2mas)) stop(paste0("MADup values are incorrect in ", older_fn, "."))
if(all.equal(colnames(newer), colnames(older)) == FALSE) warning("Column names in these two exports are not the same.")
if(nrow(newer) > nrow(older)) warning(paste0("The newer export has ", nrow(newer) - nrow(older), " more rows than the older export."))
if(nrow(newer) < nrow(older)) warning(paste0("The newer export has ", nrow(older) - nrow(newer), " fewer rows than the older export."))

#Create new data.tables for comparison without the RowID variable
newer2 <- newer[, -c("RowID")]
older2 <- older[, -c("RowID")]


#Compare two oyster export files-------------------------------------------
if(str_detect(newer_fn, "Oyster")){
  
  #Get habitat and export dates
  habitat <- "Oyster"
  newdate <- str_sub(newer_fn, str_locate(newer_fn, "-202.-")[[1]] + 1, -5)
  olddate <- str_sub(older_fn, str_locate(older_fn, "-202.-")[[1]] + 1, -5)
  
  #Make sure ordering is the same in the old and new files
  setorder(newer2, ParameterName, MADup, ProgramID, ProgramName, DataFileName, SampleDate, QuadIdentifier, ProgramLocationID, ReefIdentifier, AreaID, ResultValue, LiveDate, LiveDate_Qualifier, UniversalReefID)
  newer2[, obs_ind := seq(1, .N), by = list(ParameterName, MADup, ProgramID, ProgramName, DataFileName, SampleDate, QuadIdentifier, ProgramLocationID, ReefIdentifier, AreaID)]
  setcolorder(newer2, c("ParameterName", "MADup", "ProgramID", "ProgramName", "DataFileName", "SampleDate", "QuadIdentifier", "ProgramLocationID", "ReefIdentifier", "AreaID", "obs_ind", "ResultValue", "LiveDate", "LiveDate_Qualifier", "UniversalReefID", "HabitatClassification", "MinimumSizeMeasured_mm", "NumberMeasured_n", "PercentLiveMethod", "QuadSize_m2", "SurveyMethod"))
  
  setorder(older2, ParameterName, MADup, ProgramID, ProgramName, DataFileName, SampleDate, QuadIdentifier, ProgramLocationID, ReefIdentifier, AreaID, ResultValue, LiveDate, LiveDate_Qualifier, UniversalReefID)
  older2[, obs_ind := seq(1, .N), by = list(ParameterName, MADup, ProgramID, ProgramName, DataFileName, SampleDate, QuadIdentifier, ProgramLocationID, ReefIdentifier, AreaID)]
  setcolorder(older2, c("ParameterName", "MADup", "ProgramID", "ProgramName", "DataFileName", "SampleDate", "QuadIdentifier", "ProgramLocationID", "ReefIdentifier", "AreaID", "obs_ind", "ResultValue", "LiveDate", "LiveDate_Qualifier", "UniversalReefID", "HabitatClassification", "MinimumSizeMeasured_mm", "NumberMeasured_n", "PercentLiveMethod", "QuadSize_m2", "SurveyMethod"))
  
  #Create the comparison data table using the compareDF package
  comp <- compare_df(df_new = newer2, df_old = older2, group_col = c("ParameterName", "MADup", "ProgramID", "ProgramName", "DataFileName", "SampleDate", "QuadIdentifier", "ProgramLocationID", "ReefIdentifier", "AreaID", "obs_ind"), change_markers = c("new", "old", "unchanged"))
  
  # grpn <- sort(table(comp$comparison_df$grp), decreasing = TRUE)
  # head(grpn)
  # table(grpn)
  
  compdf <- setDT(comp$comparison_df, keep.rownames = TRUE)
  compdf[, rn := as.integer(rn)]
  compdf_wide <- setDT(create_wide_output(comp), keep.rownames = TRUE)
  compdf_wide[, rn := as.integer(rn)]
  diffnum <- setDT(comp$comparison_table_diff_numbers, keep.rownames = TRUE)
  diffnum[, rn := as.integer(rn)]
  
  
  diffnum2 <- diffnum[chng_type == 1, .(ResultValue = sum(ResultValue),
                                        LiveDate = sum(LiveDate),
                                        LiveDate_Qualifier = sum(LiveDate_Qualifier),
                                        UniversalReefID = sum(UniversalReefID),
                                        NumberMeasured_n = sum(NumberMeasured_n),
                                        QuadSize_m2 = sum(QuadSize_m2))]
  
  changes_oy <- data.table(Habitat = habitat,
                           OlderFileShort = str_sub(older_fn, max(str_locate_all(older_fn, "/")[[1]]) + 1, -1),
                           NewerFileShort = str_sub(newer_fn, max(str_locate_all(older_fn, "/")[[1]]) + 1, -1),
                           OlderExportDate = olddate,
                           NewerExportDate = newdate,
                           OlderNRows = nrow(older),
                           NewerNRows = nrow(newer),
                           ColumnName = names(diffnum2),
                           NumberChanges = unlist(diffnum2[1]),
                           OlderFileLong = older_fn,
                           NewerFileLong = newer_fn,
                           ComparisonDate = Sys.Date(),
                           ScriptFile = str_sub(rstudioapi::getSourceEditorContext()$path, max(str_locate_all(rstudioapi::getSourceEditorContext()$path, "/")[[1]]) + 1, -1))
  
  
  #Summarize metadata variables for the changes in each column
  changes_oy[, `:=` (DelRows_N = nrow(diffnum[eval(as.name(ColumnName)) > 0 & grp == 1, ]),
                     AddRows_N = nrow(diffnum[eval(as.name(ColumnName)) > 0 & grp == 2, ]),
                     Updates_N = nrow(diffnum[eval(as.name(ColumnName)) > 0 & grp == 0, ])), by = ColumnName]
  changes_oy[, `:=` (DelRows_Prog = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 1, rn], sort(unique(ProgramID))]),
                     AddRows_Prog = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 2, rn], sort(unique(ProgramID))]),
                     Updates_Prog = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 0, rn], sort(unique(ProgramID))]),
                     DelRows_Par = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 1, rn], sort(unique(ParameterName))]),
                     AddRows_Par = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 2, rn], sort(unique(ParameterName))]),
                     Updates_Par = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 0, rn], sort(unique(ParameterName))]),
                     DelRows_Yr = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 1, rn], sort(unique(Year))]),
                     DelRows_Mo = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 1, rn], sort(unique(Month))]),
                     AddRows_Yr = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 2, rn], sort(unique(Year))]),
                     AddRows_Mo = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 2, rn], sort(unique(Month))]),
                     Updates_Yr = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 0, rn], sort(unique(Year))]),
                     Updates_Mo = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 0, rn], sort(unique(Month))]),
                     DelRows_MA = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 1, rn], sort(unique(ManagedAreaName))]),
                     AddRows_MA = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 2, rn], sort(unique(ManagedAreaName))]),
                     Updates_MA = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 0, rn], sort(unique(ManagedAreaName))])), by = ColumnName]
  changes_oy[which(str_detect(changes_oy, "integer(0)|character(0)"))] <- NA
  replace(changes_oy, changes_oy == "integer(0)", NA)
  changes_oy[changes_oy == "integer(0)"] <- NA
  changes_oy[changes_oy == "character(0)"] <- NA
  
  changes_oy <- melt(changes_oy, measure.vars = patterns("^Del|^Add|^Updates"))
  changes_oy[, ChangeType := factor(fcase(str_detect(variable, "^Del"), "deleted",
                                          str_detect(variable, "^Add"), "added",
                                          str_detect(variable, "^Update"), "updated"), levels = c("deleted", "added", "updated"))]
  changes_oy[, variable := factor(fcase(str_detect(variable, "_N"), "N Row(s)",
                                        str_detect(variable, "_Prog"), "Program(s)",
                                        str_detect(variable, "_Par"), "Parameter(s)",
                                        str_detect(variable, "_Yr"), "Year(s)",
                                        str_detect(variable, "_Mo"), "Month(s)",
                                        str_detect(variable, "_MA"), "Managed Area(s)"), levels = c("N Row(s)", "Program(s)", "Parameter(s)", "Year(s)", "Month(s)", "Managed Area(s)"))]
  setnames(changes_oy, c("variable", "value"), c("ChangedRowVar", "ChangedRowVal"))
  setcolorder(changes_oy, c("ComparisonDate", "ScriptFile", "Habitat", "OlderFile", "NewerFile", "OlderExportDate", "NewerExportDate", "OlderNRows", "NewerNRows", "ColumnName", "NRowsChanged", "ChangedRowVar", "ChangeType", "ChangedRowVal"))
  setorder(changes_oy, ComparisonDate, ScriptFile, Habitat, OlderFile, NewerFile, OlderExportDate, NewerExportDate, OlderNRows, NewerNRows, ColumnName, NRowsChanged, ChangedRowVar, ChangeType)
  
  
  

  #Compare two SAV export files-------------------------------------------
} else if(str_detect(newer_fn, "SAV")){
  habitat <- "SAV"
  newdate <- str_sub(newer_fn, str_locate(newer_fn, "-202.-")[[1]] + 1, -5)
  olddate <- str_sub(older_fn, str_locate(older_fn, "-202.-")[[1]] + 1, -5)
  
  setorder(newer2, ParameterName, MADup, ProgramID, ProgramName, DataFileName, SampleDate, QuadIdentifier, ProgramLocationID, SiteIdentifier, AreaID, CommonIdentifier, ResultValue)
  newer2[, obs_ind := seq(1, .N), by = list(ParameterName, MADup, ProgramID, ProgramName, DataFileName, SampleDate, QuadIdentifier, ProgramLocationID, SiteIdentifier, AreaID, CommonIdentifier)]
  setcolorder(newer2, c("ParameterName", "MADup", "ProgramID", "ProgramName", "DataFileName", "SampleDate", "QuadIdentifier", "ProgramLocationID", "SiteIdentifier", "AreaID", "CommonIdentifier", "obs_ind", "ResultValue"))
  
  setorder(older2, ParameterName, MADup, ProgramID, ProgramName, DataFileName, SampleDate, QuadIdentifier, ProgramLocationID, SiteIdentifier, AreaID, CommonIdentifier, ResultValue)
  older2[, obs_ind := seq(1, .N), by = list(ParameterName, MADup, ProgramID, ProgramName, DataFileName, SampleDate, QuadIdentifier, ProgramLocationID, SiteIdentifier, AreaID, CommonIdentifier)]
  setcolorder(older2, c("ParameterName", "MADup", "ProgramID", "ProgramName", "DataFileName", "SampleDate", "QuadIdentifier", "ProgramLocationID", "SiteIdentifier", "AreaID", "CommonIdentifier", "obs_ind", "ResultValue"))
  
  comp <- compare_df(df_new = newer2, df_old = older2, group_col = c("ParameterName", "MADup", "ProgramID", "ProgramName", "DataFileName", "SampleDate", "QuadIdentifier", "ProgramLocationID", "SiteIdentifier", "AreaID", "CommonIdentifier", "obs_ind"), change_markers = c("new", "old", "unchanged"))
  
  # grpn <- sort(table(comp$comparison_df$grp), decreasing = TRUE)
  # head(grpn)
  # table(grpn)
  
  compdf <- setDT(comp$comparison_df, keep.rownames = TRUE)
  compdf[, rn := as.integer(rn)]
  compdf_wide <- setDT(create_wide_output(comp), keep.rownames = TRUE)
  # compdf_wide[, rn := as.integer(rn)]
  diffnum <- setDT(comp$comparison_table_diff_numbers, keep.rownames = TRUE)
  diffnum[, rn := as.integer(rn)]
  
  diffnum2 <- diffnum[chng_type == 1, .(ParameterName = sum(ParameterName),
                                        ProgramID = sum(ProgramID),
                                        ProgramName = sum(ProgramName),
                                        DataFileName = sum(DataFileName),
                                        SampleDate = sum(SampleDate),
                                        QuadIdentifier = sum(QuadIdentifier),
                                        ProgramLocationID = sum(ProgramLocationID),
                                        SiteIdentifier = sum(SiteIdentifier),
                                        AreaID = sum(AreaID),
                                        CommonIdentifier = sum(CommonIdentifier),
                                        obs_ind = sum(obs_ind),
                                        ResultValue = sum(ResultValue),
                                        LocationID = sum(LocationID),
                                        Year = sum(Year),
                                        Month = sum(Month),
                                        ManagedAreaName = sum(ManagedAreaName),
                                        Region = sum(Region),
                                        SpeciesName = sum(SpeciesName),
                                        GenusName = sum(GenusName),
                                        SpeciesGroup1 = sum(SpeciesGroup1),
                                        Drift_Attached = sum(Drift_Attached),
                                        SamplingMethod1 = sum(SamplingMethod1),
                                        SamplingMethod2 = sum(SamplingMethod2),
                                        ReportingLevel = sum(ReportingLevel),
                                        QuadSize_m2 = sum(QuadSize_m2),
                                        Grid = sum(Grid),
                                        Depth_M = sum(Depth_M))]
  
  changes_sav <- data.table(Habitat = habitat,
                        # OlderFileShort = str_sub(older_fn, max(str_locate_all(older_fn, "/")[[1]]) + 1, -1),
                        # NewerFileShort = str_sub(newer_fn, max(str_locate_all(newer_fn, "/")[[1]]) + 1, -1),
                        OlderFile = str_sub(older_fn, max(str_locate_all(older_fn, "/")[[1]]) + 1, -1),
                        NewerFile = str_sub(newer_fn, max(str_locate_all(newer_fn, "/")[[1]]) + 1, -1),
                        OlderExportDate = olddate,
                        NewerExportDate = newdate,
                        OlderNRows = nrow(older),
                        NewerNRows = nrow(newer),
                        ColumnName = names(diffnum2),
                        NRowsChanged = unlist(diffnum2[1]),
                        # OlderFileLong = older_fn,
                        # NewerFileLong = newer_fn,
                        ComparisonDate = as.character(Sys.Date()),
                        ScriptFile = str_sub(rstudioapi::getSourceEditorContext()$path, max(str_locate_all(rstudioapi::getSourceEditorContext()$path, "/")[[1]]) + 1, -1))
  
  
  #Summarize metadata variables for the changes in each column
  changes_sav[, `:=` (DelRows_N = nrow(diffnum[eval(as.name(ColumnName)) > 0 & grp == 1, ]),
                      AddRows_N = nrow(diffnum[eval(as.name(ColumnName)) > 0 & grp == 2, ]),
                      Updates_N = nrow(diffnum[eval(as.name(ColumnName)) > 0 & grp == 0, ])), by = ColumnName]
  changes_sav[, `:=` (DelRows_Prog = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 1, rn], sort(unique(ProgramID))]),
                      AddRows_Prog = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 2, rn], sort(unique(ProgramID))]),
                      Updates_Prog = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 0, rn], sort(unique(ProgramID))]),
                      DelRows_Par = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 1, rn], sort(unique(ParameterName))]),
                      AddRows_Par = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 2, rn], sort(unique(ParameterName))]),
                      Updates_Par = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 0, rn], sort(unique(ParameterName))]),
                      DelRows_Yr = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 1, rn], sort(unique(Year))]),
                      DelRows_Mo = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 1, rn], sort(unique(Month))]),
                      AddRows_Yr = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 2, rn], sort(unique(Year))]),
                      AddRows_Mo = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 2, rn], sort(unique(Month))]),
                      Updates_Yr = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 0, rn], sort(unique(Year))]),
                      Updates_Mo = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 0, rn], sort(unique(Month))]),
                      DelRows_MA = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 1, rn], sort(unique(ManagedAreaName))]),
                      AddRows_MA = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 2, rn], sort(unique(ManagedAreaName))]),
                      Updates_MA = list(compdf[rn %in% diffnum[eval(as.name(ColumnName)) > 0 & grp == 0, rn], sort(unique(ManagedAreaName))])), by = ColumnName]
  changes_sav[which(str_detect(changes_sav, "integer(0)|character(0)"))] <- NA
  replace(changes_sav, changes_sav == "integer(0)", NA)
  changes_sav[changes_sav == "integer(0)"] <- NA
  changes_sav[changes_sav == "character(0)"] <- NA
  
  changes_sav2 <- melt(changes_sav, measure.vars = patterns("^Del|^Add|^Updates"))
  changes_sav2[, ChangeType := factor(fcase(str_detect(variable, "^Del"), "deleted",
                                            str_detect(variable, "^Add"), "added",
                                            str_detect(variable, "^Update"), "updated"), levels = c("deleted", "added", "updated"))]
  changes_sav2[, variable := factor(fcase(str_detect(variable, "_N"), "N Row(s)",
                                          str_detect(variable, "_Prog"), "Program(s)",
                                          str_detect(variable, "_Par"), "Parameter(s)",
                                          str_detect(variable, "_Yr"), "Year(s)",
                                          str_detect(variable, "_Mo"), "Month(s)",
                                          str_detect(variable, "_MA"), "Managed Area(s)"), levels = c("N Row(s)", "Program(s)", "Parameter(s)", "Year(s)", "Month(s)", "Managed Area(s)"))]
  setnames(changes_sav2, c("variable", "value"), c("ChangedRowVar", "ChangedRowVal"))
  setcolorder(changes_sav2, c("ComparisonDate", "ScriptFile", "Habitat", "OlderFile", "NewerFile", "OlderExportDate", "NewerExportDate", "OlderNRows", "NewerNRows", "ColumnName", "NRowsChanged", "ChangedRowVar", "ChangeType", "ChangedRowVal"))
  setorder(changes_sav2, ComparisonDate, ScriptFile, Habitat, OlderFile, NewerFile, OlderExportDate, NewerExportDate, OlderNRows, NewerNRows, ColumnName, NRowsChanged, ChangedRowVar, ChangeType)

   
  
  #Compare two coral export files-------------------------------------------
} else if(str_detect(newer_fn, "CORAL")){
  habitat <- "CORAL"
  newdate <- str_sub(newer_fn, str_locate(newer_fn, "-202.-")[[1]] + 1, -5)
  olddate <- str_sub(older_fn, str_locate(older_fn, "-202.-")[[1]] + 1, -5)
  
  #Compare two nekton export files-------------------------------------------
} else if(str_detect(newer_fn, "NEKTON")){
  habitat <- "NEKTON"
  newdate <- str_sub(newer_fn, str_locate(newer_fn, "-202.-")[[1]] + 1, -5)
  olddate <- str_sub(older_fn, str_locate(older_fn, "-202.-")[[1]] + 1, -5)
  
  #Compare two coastal wetlands export files-------------------------------------------
} else if(str_detect(newer_fn, "CW")){
  habitat <- "CW"
  newdate <- str_sub(newer_fn, str_locate(newer_fn, "-202.-")[[1]] + 1, -5)
  olddate <- str_sub(older_fn, str_locate(older_fn, "-202.-")[[1]] + 1, -5)
  
  #Compare two continuous WQ export files-------------------------------------------
} else if(str_detect(newer_fn, "Combined_WQ_WC_NUT_cont_")){
  habitat <- "WC_cont"
  newdate <- str_sub(newer_fn, str_locate(newer_fn, "-202.-")[[1]] + 1, -5)
  olddate <- str_sub(older_fn, str_locate(older_fn, "-202.-")[[1]] + 1, -5)
  
  #Compare two discrete WQ export files-------------------------------------------
} else if(str_detect(newer_fn, "Combined_WQ_WC_NUT_")){
  habitat <- "WC_disc"
  newdate <- str_sub(newer_fn, str_locate(newer_fn, "-202.-")[[1]] + 1, -5)
  olddate <- str_sub(older_fn, str_locate(older_fn, "-202.-")[[1]] + 1, -5)
  
}

#Format and export the comparison results as an Excel file
headStyle <- createStyle(textDecoration = "bold")
bodyStyle <- createStyle(wrapText = TRUE)
changeswb <- createWorkbook()
addWorksheet(wb = changeswb, sheetName = "AllComparisons")
writeDataTable(wb = changeswb, sheet = "AllComparisons", x = changes_sav2, headerStyle = headStyle, keepNA = T, na.string = "NA", colNames = T, withFilter = T, bandedRows = F)
addStyle(wb = changeswb, sheet = "AllComparisons", style = bodyStyle, cols = c(1:ncol(changes_sav2)), rows = c(2:nrow(changes_sav2)), gridExpand = TRUE)
setColWidths(wb = changeswb, sheet = "AllComparisons", cols = c(1:(ncol(changes_sav2) - 1)), widths = "auto")
setColWidths(wb = changeswb, sheet = "AllComparisons", cols = 14, widths = 60)
saveWorkbook(wb = changeswb, file = here::here(paste0("ExportFileComparisons_", Sys.Date(), ".xlsx")), overwrite = T)








# #MADup looks to be inconsistently applied (one reason why comparing oyster exports has been so difficult) - Look at sample locations to confirm
# oydat <- fread(here::here("SEACARdata/All_Oyster_Parameters-2023-Jun-05.txt"), sep = "|", na.string = "NULL")
# oydat2 <- fread(here::here("SEACARdata/All_Oyster_Parameters-2023-Sep-05.txt"), sep = "|", na.string = "NULL")
# oydat3 <- fread(here::here("SEACARdata/All_Oyster_Parameters-2023-Oct-11.txt"), sep = "|", na.string = "NULL")
# 
locs <- st_read(here::here("mapping/SampleLocations26jan2023/seacar_dbo_vw_SampleLocation_Point.shp"))
mas <- st_read(here::here("mapping/orcp_all_sites/orcp_all_sites/ORCP_Managed_Areas.shp"))

locs <- st_make_valid(locs)
locs <- st_transform(locs, crs = 4326)
mas <- st_make_valid(mas)
mas <- st_transform(mas, crs = 4326)

mapview(subset(mas, mas$LONG_NAME == "Guana Tolomato Matanzas National Estuarine Research Reserve"), col.regions = "gold", alpha = 0.8, layer.name = "GTMNERR") +
  mapview(subset(mas, mas$LONG_NAME == "Guana River Marsh Aquatic Preserve"), col.regions = "pink", alpha = 1, layer.name = "GRMAP") +
  mapview(subset(locs, locs$LocationID %in% oydat[MADup == 1 & ManagedAreaName %in% c("Guana Tolomato Matanzas National Estuarine Research Reserve", "Guana River Marsh Aquatic Preserve"), unique(LocationID)]), col.regions = "dodgerblue", layer.name = "Jun05, MADup = 1") +
  mapview(subset(locs, locs$LocationID %in% oydat[MADup == 2 & ManagedAreaName %in% c("Guana Tolomato Matanzas National Estuarine Research Reserve", "Guana River Marsh Aquatic Preserve"), unique(LocationID)]), col.regions = "firebrick", layer.name = "Jun05, MADup = 2") +
  mapview(subset(locs, locs$LocationID %in% oydat2[MADup == 1 & ManagedAreaName %in% c("Guana Tolomato Matanzas National Estuarine Research Reserve", "Guana River Marsh Aquatic Preserve"), unique(LocationID)]), col.regions = "purple", layer.name = "Sep05, MADup = 1") +
  mapview(subset(locs, locs$LocationID %in% oydat2[MADup == 2 & ManagedAreaName %in% c("Guana Tolomato Matanzas National Estuarine Research Reserve", "Guana River Marsh Aquatic Preserve"), unique(LocationID)]), col.regions = "green", layer.name = "Sep05, MADup = 2") +
  mapview(subset(locs, locs$LocationID %in% oydat3[MADup == 1 & ManagedAreaName %in% c("Guana Tolomato Matanzas National Estuarine Research Reserve", "Guana River Marsh Aquatic Preserve"), unique(LocationID)]), col.regions = "white", layer.name = "Oct11, MADup = 1") +
  mapview(subset(locs, locs$LocationID %in% oydat3[MADup == 2 & ManagedAreaName %in% c("Guana Tolomato Matanzas National Estuarine Research Reserve", "Guana River Marsh Aquatic Preserve"), unique(LocationID)]), col.regions = "black", layer.name = "Oct11, MADup = 2")

# 
# #Re-do the MADup variable
# dupsdat <- distinct(oydat3[, .(LocID = LocationID, MAname = ManagedAreaName)])
# dups <- dupsdat[, .(ma_n = .N), by = LocID]
# dups[ma_n == 2, `:=` (madup1 = ifelse(str_detect(oydat3[LocationID == LocID, unique(ManagedAreaName)][1], "Bend|Estero|Indian|Jensen|Lemon|Loxahatchee|Mosquito|Nassau|Nature|Sound|Martins|Tomoka|Yellow"), 1, 0),
#                       madup2 = ifelse(oydat3[LocationID == LocID, unique(ManagedAreaName)][2] %in% c("Apalachicola Bay Aquatic Preserve", "Boca Ciega Bay Aquatic Preserve", "Guana River Marsh Aquatic Preserve"), 1, 0)), by = LocID]
# dups[ma_n == 2 & madup1 == 0 & madup2 == 0, madup1 := 1]
# dups[is.na(madup1) & is.na(madup2), `:=` (madup1 = 1, madup2 = 0)]
# 
# oydat3[, MADup := ifelse(dups[LocID == LocationID, madup1] == 1, 1, 2), by = LocationID]
# oydat2[, MADup := ifelse(dups[LocID == LocationID, madup1] == 1, 1, 2), by = LocationID]
# 
# newer <- oydat3 #Re-run export comparison above
# older <- oydat2 #Re-run export comparison above



