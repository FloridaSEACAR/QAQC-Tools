# QAQC-Tools

The purpose of this repository is to act as a store of any scripts relevent to the internal QAQC process within SEACAR.

## ContinuousThresholdCheck.R

* Loops through the data location (specified using the variable *seacar_data_location*) and looks for relevant Continuous combined tables
* Applies thresholds as specified in the *Ref_WQ_Include* sheet of *SEACAR_DatExportFile_Metadata* (hard-coded, future solutions can include updating via Metadata file directly)
* Stores the results that fall above or below the given threshold for each parameter, listing them in table format in *threshold_data_combined*
* Summary results are viewed using `View(group_df)`
* Unique *SEACAR_QAQCFlagCode* values are provided to see which QAQCFlagCodes are being applied by the database
* Assuming the thresholds and their respective QAQCFlagCodes are being applied correctly on the database side, *threshold_data_combined* should return 0 results, and *group_df* will return an error upon compiling