# QAQC-Tools

The purpose of this repository is to act as a store of any scripts relevent to the internal QAQC process within SEACAR.
Many scripts contain the variable "seacar_data_location" which points to the location where SEACAR combined tables can be found. Must be set by the user.

## /comparison/
* Script intended to highlighted differences in structure between Combined export tables
* Generates LaTeX-based .PDF reports outlining differences
* Currently highlights differences between exports for the following:
    + number of records (data entries)
    + number of programs
    + number of columns

## ContinuousThresholdCheck.R

* Loops through the data location (specified using the variable *seacar_data_location*) and looks for relevant Continuous combined tables
* Applies thresholds as specified in the *Ref_WQ_Include* sheet of *SEACAR_DatExportFile_Metadata* (hard-coded, future solutions can include updating via Metadata file directly)
* Stores the results that fall above or below the given threshold for each parameter, listing them in table format in *threshold_data_combined*
* Summary results are viewed using `View(group_df)`
* Unique *SEACAR_QAQCFlagCode* values are provided to see which QAQCFlagCodes are being applied by the database
* Assuming the thresholds and their respective QAQCFlagCodes are being applied correctly on the database side, *threshold_data_combined* should return 0 results, and *group_df* will return an error upon compiling

## unzip.R

* unzip.R can be used to unzip the Combined Export Table files
* Script should be placed into the folder where your most recent files have been downloaded
* *downloaddate* variable should be set to the date listed in the .zip files using the format "YYYY-MM-DD"
* *folder_name* variable is set to determine where unzipped files should be placed. Default is "SEACARdata"
* .zip files are removed upon completion. Comment out lines 25 and 29 to keep .zip files

## ParameterList.R
* Generates a .xlsx file that records which parameters are included in Combined Export Tables and records the number of records and the data file used.

## species_crosswalk_check.R
* Generates a .csv file outlining each Habitat and CommonIdentifier, SpeciesGroup1, SpeciesGroup2 combinations available in latest combined table exports.
* Requires the latest SEACAR_Metdata.xlsx file as an input to verify correct implementation of *Mthd_SpeciesCrosswalk* tab in latest exports.
* `match` column (TRUE or FALSE) is created to confirm correct implementation. If all values are TRUE, the species crosswalk has been implemented correctly.