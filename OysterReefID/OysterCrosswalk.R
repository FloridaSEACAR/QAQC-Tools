library(stringr)
library(data.table)
library(sf)
library(tidyverse)
library(doFuture)
library(tictoc)
library(doRNG)
library(smoothr)

file_in <- str_subset(
  list.files("C:/SEACAR Data/SEACARdata/", full.names = TRUE), 
  "OYSTER")
oysterraw <- fread(file_in, sep="|", na.strings=c("NULL"))
oysterraw2 <- pivot_wider(oysterraw, names_from="ParameterName",
                          values_from="ResultValue")
setDT(oysterraw2)
setnames(oysterraw2, c("Density", "Percent Live", "Shell Height",
                       "Number of Oysters Counted - Live",
                       "Number of Oysters Counted - Dead",
                       "Number of Oysters Counted - Total", "Reef Height"),
         c("Density_m2", "PercentLive_pct", "ShellHeight_mm",
           "Number_of_Oysters_Counted_Live_Count",
           "Number_of_Oysters_Counted_Dead_Count",
           "Number_of_Oysters_Counted_Total_Count",
           "ReefHeight_mm"))
oysterraw2[, ObsIndex := seq(1:nrow(oysterraw2))]

oysterraw <- oysterraw2
rm(oysterraw2)

######################## -----
#Load spatial data files for RCP managed areas, SEACAR sample locations and
#FWC statewide oyster reef layer
fwcoymap <- st_read("C:/SEACAR Data/SEACARshapes/Oyster_Beds_in_Florida/Oyster_Beds_in_Florida.shp")
rcp <- st_read("C:/SEACAR Data/SEACARshapes/orcp_all_sites/ORCP_Managed_Areas.shp")
oysamplelocs <- st_read("C:/SEACAR Data/SEACARshapes/SampleLocations5dec2024/seacar_dbo_vw_SampleLocation_Point.shp")
oysterprogs <- unique(oysterraw$ProgramID)
oysamplelocs <- subset(oysamplelocs, oysamplelocs$ProgramID %in% oysterprogs)

# # Temporary fix for 2 samples with incorrect coordinates
# oysamplelocs$Latitude_D[oysamplelocs$ProgramLoc=="FL14"] <- 30.05787
# oysamplelocs$Longitude_[oysamplelocs$ProgramLoc=="FL14"] <- -85.5384
# oysamplelocs$Latitude_D[oysamplelocs$ProgramLoc=="ML35"] <- 28.991211
# oysamplelocs$Longitude_[oysamplelocs$ProgramLoc=="ML35"] <- -80.887575

##### Generate combined oyster shapefile (all_oysters) using all historical files
oyster_shapes <- list.files("C:/SEACAR Data/SEACARshapes/AllOyster", pattern = ".shp", full.names = T)
oyster_shapes <- str_subset(oyster_shapes, ".xml", negate = T)
# Some shapes are too large and give erroneous results
shapes_to_skip <- c("SurficialGeology selection.shp", "ANERRmaps_oysters.shp")

shape_directory <- list()
for(shape in oyster_shapes){
  name <- tail(str_split(shape, "/")[[1]],1)
  if(name %in% shapes_to_skip) next
  print(paste0("Processing shapefile: ", name))
  df <- st_read(shape)
  df$geometry <- st_make_valid(df$geometry)
  df <- st_transform(df, crs=4326)
  
  shape_directory[[name]] <- df %>% mutate(across(names(df)[names(df)!="geometry"], as.character))
}

all_oysters <- bind_rows(shape_directory, .id="id")
all_oysters$geometry <- st_make_valid(all_oysters$geometry)
# Create OBJECTID from RowID (to mimic format of fwc oyster map)
all_oysters <- all_oysters %>% mutate(OBJECTID = row_number())
##### End all_oysters creation

#Make sure spatial data are in the same projection
rcp_m <- st_transform(rcp, 32119)
fwcoymap_m <- st_transform(fwcoymap, 32119)
oysamplelocs_m <- st_transform(oysamplelocs, 32119)
all_oysters_m <- st_transform(all_oysters, 32119)

# Create oyster map file for RCP managed areas
fwcoymap_m_rcp <- fwcoymap_m[rcp_m, , op=st_intersects]
# all_oysters for RCP managed areas
all_oysters_m_rcp <- all_oysters_m[rcp_m, , op=st_intersects]

oyster_locs <- oysterraw %>% 
  group_by(LocationID, ProgramID, ProgramLocationID, 
           OriginalLongitude, OriginalLatitude) %>%
  summarise(.groups = "keep")

### Mean/Median reef size by OIMMP boundaries -----
oimmp <- st_read("C:/SEACAR Data/SEACARshapes/OIMMP_Boundaries/OIMMPRegions.shp")
oimmp <- st_transform(oimmp, crs = st_crs(fwcoymap_m_rcp))
# spatial join to assign regions to sample locations
fwcoymap_with_regions <- st_join(fwcoymap_m_rcp, oimmp, join = st_within)
# Preview
# mapView(fwcoymap_with_regions %>% filter(is.na(Region))) + mapview(oimmp)

# Generate mean and median reef sizes by region
reef_summary <- fwcoymap_with_regions %>%
  rename(reefSize = Shapearea,
         oimmp_Region = Region) %>%
  group_by(oimmp_Region) %>%
  summarise(
    mean_reefsize = mean(reefSize, na.rm = TRUE),
    median_reefsize = median(reefSize, na.rm = TRUE)
  ) %>% st_drop_geometry() %>% as.data.table()
reef_summary[is.na(oimmp_Region), oimmp_Region := "NA"]
fwrite(reef_summary, "output/oimmp_reef_summary.csv")

#####
## Create a crosswalk of connected reefs between Oyster Beds in Florida and all_oysters
# Find overlaps between fwcoymap_m_rcp and all_oysters_m_rcp using st_intersection
overlaps <- st_intersection(fwcoymap_m_rcp, all_oysters_m_rcp)

# Check for empty geometries and filter them out
overlaps <- overlaps %>%
  filter(!st_is_empty(geometry))

# Create a crosswalk that lists the overlapping reefs and assigns a UniversalReefID
crosswalk <- overlaps %>%
  st_drop_geometry() %>%  # Drop geometry to work with attribute data
  select(fwcoymap_id = OBJECTID, all_oysters_id = OBJECTID.1) %>%  # Adjust to match reef ID column names
  distinct() %>%  # Ensure unique pairs of overlapping reefs
  mutate(UniversalReefID = row_number())  # Assign a unique UniversalReefID for each overlap

crosswalk <- crosswalk %>%
  mutate(fwcoymap_id = as.character(fwcoymap_id))

# Find overlaps within the all_oysters_m_rcp shapefile itself
internal_overlaps <- st_intersection(all_oysters_m_rcp, all_oysters_m_rcp)

# Filter out empty geometries and self-overlaps (where the same reef intersects itself)
internal_overlaps <- internal_overlaps %>%
  filter(!st_is_empty(geometry) & OBJECTID != OBJECTID.1)

# Create a secondary crosswalk for internal overlaps in all_oysters_m_rcp
internal_crosswalk <- internal_overlaps %>%
  st_drop_geometry() %>%
  select(all_oysters_id1 = OBJECTID, all_oysters_id2 = OBJECTID.1) %>%
  distinct()

# Integrate the internal overlaps into the main crosswalk
# This step involves finding any reefs in all_oysters_m_rcp that overlap with one another and
# making sure they share the same UniversalReefID if they also overlap with reefs in fwcoymap_m_rcp.

# First, create a lookup table for all_oysters_id1 and all_oysters_id2 using the initial crosswalk
lookup <- crosswalk %>%
  select(all_oysters_id, UniversalReefID) %>%
  distinct()

# Merge the internal_crosswalk with the lookup table to propagate the UniversalReefID
internal_crosswalk_expanded <- internal_crosswalk %>%
  left_join(lookup, by = c("all_oysters_id1" = "all_oysters_id"), relationship = "many-to-many") %>%
  left_join(lookup, by = c("all_oysters_id2" = "all_oysters_id"), relationship = "many-to-many", suffix = c(".1", ".2"))

# Combine the UniversalReefID values from both joins and assign new IDs where needed
internal_crosswalk_expanded <- internal_crosswalk_expanded %>%
  mutate(
    UniversalReefID = coalesce(UniversalReefID.1, UniversalReefID.2, max(crosswalk$UniversalReefID, na.rm = TRUE) + row_number())
  ) %>%
  select(all_oysters_id1, all_oysters_id2, UniversalReefID)

# Combine internal_crosswalk with the main crosswalk
combined_crosswalk <- bind_rows(
  crosswalk %>% select(fwcoymap_id, all_oysters_id, UniversalReefID),
  internal_crosswalk_expanded %>% 
    mutate(fwcoymap_id = NA_character_) %>% 
    select(fwcoymap_id, all_oysters_id = all_oysters_id1, UniversalReefID)
)

# Remove duplicates to create a final crosswalk
combined_crosswalk <- combined_crosswalk %>%
  distinct()

# Convert to data.table
setDT(combined_crosswalk)
write.csv(combined_crosswalk, "output/reef_crosswalk_final.csv", row.names = FALSE)
#####

#Create dataframe of oyster sample locations within RCP managed areas
#that will be used to crosswalk reefIDs from different programIDs
reefcrosswalk_rcp <- st_join(oysamplelocs_m, rcp_m["LONG_NAME"],
                             join=st_intersects)
setnames(reefcrosswalk_rcp, "LONG_NAME", "SITE_NAME")

reefcrosswalk_rcp <- subset(reefcrosswalk_rcp,
                            !is.na(reefcrosswalk_rcp$SITE_NAME))

#Need to make sure that samples outside of MA boundaries but taken from reefs
#that are partially within the MA boundaries are included. 
reefcrosswalk_oymap <- st_join(oysamplelocs_m, fwcoymap_m_rcp["OBJECTID"],
                               join=st_intersects)
st_geometry(reefcrosswalk_rcp) <- NULL
reefcrosswalk_rcp <- dplyr::left_join(reefcrosswalk_oymap, reefcrosswalk_rcp)

# Remove samples that fall outside of FL by Western-most longitude
reefcrosswalk_rcp <- reefcrosswalk_rcp %>% filter(Longitude_ > -87.38)

# Assign oimmp regionID to oyster sample locations
reefcrosswalk_rcp_regions <- st_join(reefcrosswalk_rcp, oimmp, join = st_within)
reefcrosswalk_rcp_regions$Region[is.na(reefcrosswalk_rcp_regions$Region)] <- "NA"
reefcrosswalk_rcp_regions <- reefcrosswalk_rcp_regions %>%
  select(-c(Shape_Leng, Shape_Area)) %>% rowwise() %>%
  mutate(
    meanReefSize = reef_summary[oimmp_Region == Region, mean_reefsize],
    medianReefSize = reef_summary[oimmp_Region == Region, median_reefsize]
  ) %>% 
  rename(oimmp_Region = Region)
# View results
# mapView(reefcrosswalk_rcp_regions, zcol = "oimmp_Region")

##### Begin matching process----
### Function to find closest reef
find_closest_reef <- function(reef_shapefile, reefcrosswalk, buffer = 20){
  
  # Calculate distances in a vectorized way for all rows
  distances_matrix <- st_distance(reef_shapefile, reefcrosswalk)
  
  # For each sample, find the index of the minimum distance in the corresponding row
  min_indices <- apply(distances_matrix, 2, which.min)
  
  # Assign the OBJECTID of the closest feature based on the minimum distance
  reefcrosswalk$closest <- reef_shapefile$OBJECTID[min_indices]
  # Record metadata values
  if("METADATA" %in% names(reef_shapefile)){
    reefcrosswalk$closest_metadata <- reef_shapefile$METADATA[min_indices]
  } else {
    reefcrosswalk$closest_metadata <- reef_shapefile$id[min_indices]
  }
  
  # Create a logical matrix to check if each row in reefcrosswalk is within BUFFER units of the corresponding closest OBJECTID
  is_within_distance <- st_is_within_distance(reefcrosswalk, reef_shapefile, dist = buffer, sparse = FALSE)
  
  # Convert the logical matrix into a vector indicating match or no match for each sample
  reefcrosswalk$match_cat <- ifelse(rowSums(is_within_distance) > 0, reefcrosswalk$closest, "no match")
  
  reefcrosswalk$match_metadata <- ifelse(
    rowSums(is_within_distance) > 0, 
    reefcrosswalk$closest_metadata, 
    NA
  )
  
  return(reefcrosswalk %>% select(-closest_metadata))
}

#### Determine buffer size
medianReefSize <- median(fwcoymap_m_rcp$Shapearea)
medianReefDistance <- round(sqrt(medianReefSize),2)
# Median reef distance as buffer (14.21)
buffer_distance <- medianReefDistance

# Apply function (takes time)
# uses fwcoymap_m_rcp as reef shapefile, finds matches with reefcrosswalk_rcp
# This is the first pass, function is then applied to all_oysters to find further matches

tic()
reefcrosswalk_matched <- find_closest_reef(
  reef_shapefile = fwcoymap_m_rcp,
  reefcrosswalk = reefcrosswalk_rcp,
  buffer = buffer_distance
)
toc()

# reefcrosswalk_matched_combined <- list()
# tic()
# for(reg in unique(reefcrosswalk_rcp_regions$oimmp_Region)){
#   if(reg=="NA") next
#   reg_crosswalk <- reefcrosswalk_rcp_regions %>% filter(oimmp_Region==reg)
#   buffer_dist <- unique(reg_crosswalk %>% pull(medianReefSize))
#   reefcrosswalk_matched_temp <- find_closest_reef(
#     reef_shapefile = fwcoymap_m_rcp,
#     reefcrosswalk = reg_crosswalk,
#     buffer = buffer_dist
#   )
#   reefcrosswalk_matched_combined[[reg]] <- reefcrosswalk_matched_temp
#   print(paste0("Finished region: ", reg))
# }
# toc()
# reefcrosswalk_matched <- bind_rows(reefcrosswalk_matched_combined)

print(paste0("Total rows: ", nrow(reefcrosswalk_matched)))
print(paste0("Unmatched samples: ", nrow(reefcrosswalk_matched %>% filter(match_cat=="no match"))))
print(paste0("Matched samples: ", nrow(reefcrosswalk_matched %>% filter(match_cat!="no match"))))

# Copy newly-created match_cat column into new column
reefcrosswalk_matched$fwc_match_cat <- reefcrosswalk_matched$match_cat
reefcrosswalk_matched$fwc_match_metadata <- reefcrosswalk_matched$match_metadata

# Apply function again, this time cross-checking with all_oysters shapefile
tic()
reefcrosswalk_matched_all <- find_closest_reef(
  reef_shapefile = all_oysters_m_rcp,
  reefcrosswalk = reefcrosswalk_matched,
  buffer = buffer_distance
)
toc()

# reefcrosswalk_matched_combined <- list()
# tic()
# for(reg in unique(reefcrosswalk_matched$oimmp_Region)){
#   if(reg=="NA") next
#   reg_crosswalk <- reefcrosswalk_matched %>% filter(oimmp_Region==reg)
#   buffer_dist <- unique(reg_crosswalk %>% pull(medianReefSize))
#   reefcrosswalk_matched_temp <- find_closest_reef(
#     reef_shapefile = all_oysters_m_rcp,
#     reefcrosswalk = reg_crosswalk,
#     buffer = buffer_dist
#   )
#   reefcrosswalk_matched_combined[[reg]] <- reefcrosswalk_matched_temp
#   print(paste0("Finished region: ", reg))
# }
# toc()
# reefcrosswalk_matched_all <- bind_rows(reefcrosswalk_matched_combined)

# Copy newly-created match_cat column into new column
reefcrosswalk_matched_all$all_oy_match_cat <- reefcrosswalk_matched_all$match_cat
reefcrosswalk_matched_all$all_oy_match_metadata <- reefcrosswalk_matched_all$match_metadata

matched <- reefcrosswalk_matched_all %>% filter(!match_cat=="no match")
print(paste0("Number of samples total: ", nrow(reefcrosswalk_matched_all)))
print(paste0("Number of samples matched (All Oy Map): ", 
             nrow(matched), 
             " (",
             round(nrow(matched)/nrow(reefcrosswalk_matched_all)*100,1),
             "%)"))

# Create new column to show matched, unmatched values on map
# Update matchFrom column for newly matched samples
reefcrosswalk_matched_all <- reefcrosswalk_matched_all %>%
  mutate(newMatch = ifelse(match_cat=="no match", "no match", "match"))

new_crosswalk <-
  reefcrosswalk_matched_all %>% 
    group_by(fwc_match_cat, all_oy_match_cat, LocationID) %>% 
    summarise()

# AssignID function with hierarchical logic
assignID <- function(fwc_id, all_oy_id, crosswalk){
  # Try using only fwcoymap_id
  uid <- crosswalk[fwcoymap_id==fwc_id, UniversalReefID]
  if (length(uid) > 0) {
    return(uid[1])
  }
  # Try to match using all_oysters_id
  uid <- crosswalk[all_oysters_id == all_oy_id, UniversalReefID]
  if (length(uid) > 0) {
    return(uid[1])
  }
  # Try to match both fwcoymap_id and all_oysters_id together
  uid <- crosswalk[fwcoymap_id==fwc_id & all_oysters_id==all_oy_id, UniversalReefID]
  # If a match is found, return it
  if(length(uid)>0){
    return(uid[1])
  }
  # If none of the above match, return NA
  return(NA)
}

new_crosswalk2 <- new_crosswalk %>% 
  rowwise() %>%
  mutate(UniversalReefID = assignID(
    fwc_id = fwc_match_cat,
    all_oy_id = all_oy_match_cat,
    crosswalk = combined_crosswalk
  )) %>% 
  ungroup()

cat(paste0("Number of values total: ", nrow(new_crosswalk2), "  \n"))
cat(paste0("Number of values with URID: ", nrow(new_crosswalk2 %>% filter(!is.na(UniversalReefID))), "  \n"))
cat(paste0("Number of values without URID: ", nrow(new_crosswalk2 %>% filter(is.na(UniversalReefID))), "  \n"))

# 958 Samples remain without an assigned UniversalReefID
View(new_crosswalk2 %>% filter(is.na(UniversalReefID)))

#### Begin process for assigning these values UniversalReefIDs while determining when these values should be grouped
# I used test.R from here on to test different scenarios

# Use the original new_crosswalk2, which includes all samples, both matched and unmatched
unmatched_samples <- new_crosswalk2

# Convert unmatched_samples to an sf object if it's not already
if (!inherits(unmatched_samples, "sf")) {
  unmatched_samples <- st_as_sf(unmatched_samples, coords = c("longitude", "latitude"), crs = 4326)
}

# Buffer all points by 100 meters and find intersecting buffers
buffers <- st_buffer(unmatched_samples, dist = buffer_distance)
intersections <- st_intersects(buffers)

# Initialize group_index for grouping UniversalReefID values
group_index <- integer(length = nrow(unmatched_samples))
current_group <- max(unmatched_samples$UniversalReefID, na.rm = TRUE) + 1

# Loop through each point to assign group indices based on existing UniversalReefID or create a new group
for (i in seq_along(intersections)) {
  if (!is.na(unmatched_samples$UniversalReefID[i])) {
    # If the current point has a UniversalReefID, assign it to the group
    group_index[intersections[[i]]] <- unmatched_samples$UniversalReefID[i]
  } else {
    # Check if any of the intersecting points already have an assigned UniversalReefID
    intersecting_ids <- unique(group_index[intersections[[i]]])
    intersecting_ids <- intersecting_ids[intersecting_ids > 0]  # Filter out zeros (unassigned)
    
    if (length(intersecting_ids) > 0) {
      # Assign the existing UniversalReefID from intersecting points
      group_index[intersections[[i]]] <- intersecting_ids[1]  # Choose the first intersecting ID
    } else {
      # If no intersecting UniversalReefID is found, assign a new group index
      group_index[intersections[[i]]] <- current_group
      current_group <- current_group + 1
    }
  }
}

# Map the group indices to UniversalReefID values
group_mapping <- data.frame(
  group_index = unique(group_index),
  new_UniversalReefID = unique(group_index)
)

# Join the group mapping back to unmatched_samples to assign new UniversalReefID values
unmatched_samples <- unmatched_samples %>%
  mutate(group_index = group_index) %>%
  left_join(group_mapping, by = "group_index") %>%
  mutate(UniversalReefID = coalesce(UniversalReefID, new_UniversalReefID)) %>%
  select(-new_UniversalReefID, -group_index)

# Update the original new_crosswalk2 dataframe with the new UniversalReefID values
unmatched_samples_non_spatial <- st_drop_geometry(unmatched_samples) %>%
  select(LocationID, UniversalReefID)

new_crosswalk2 <- new_crosswalk2 %>%
  left_join(unmatched_samples_non_spatial, by = "LocationID") %>%
  mutate(UniversalReefID = coalesce(UniversalReefID.x, UniversalReefID.y)) %>%
  select(-UniversalReefID.x, -UniversalReefID.y)

saveRDS(new_crosswalk2, file = paste0("output/new_crosswalk2_", gsub("_","-",Sys.Date()),".rds"))

map3 <- 
  mapView(new_crosswalk2, zcol = "UniversalReefID") +
  mapView(fwcoymap_m_rcp, color = "blue", col.regions="darkgreen") +
  mapView(all_oysters_m_rcp %>% select(geometry, METADATA, OBJECTID), color = "blue", col.regions="darkred")

mapshot(
  map3, url = paste0("output/matched_reefs_", gsub("_","-",Sys.Date()), ".html")
)

##### Creation of new polygon shapefile to show new reefID associations
# Group by UniversalReefID and create polygons for each reef using buffer
# Create a small buffer around each point and then dissolve them to form a polygon
# Buffer each point by buffer_distance, then union
reef_polygons <- new_crosswalk2 %>%
  group_by(UniversalReefID) %>%
  summarise(
    geometry = st_union(st_buffer(geometry, dist = buffer_distance))
  ) %>%
  ungroup()

# Smooth polygons to normalize appearance
reef_polygons_smoothed <- smooth(reef_polygons, method = "ksmooth", smoothness = 10)
# Ensure the resulting geometries are of type MULTIPOLYGON
reef_polygons_smoothed <- st_cast(reef_polygons_smoothed, "MULTIPOLYGON")

# Save the resulting shapefile
st_write(reef_polygons_smoothed, paste0("output/new_shapes/reef_polygons_", gsub("_","-",(Sys.Date())),".shp"), append=FALSE)

# View final polygons
map4 <- mapView(reef_polygons_smoothed)
mapshot(map4, url = paste0("output/final_reefs_", gsub("_","-",(Sys.Date())), ".html"))

# Final crosswalk sent to Claude 12-04-2024
reefcrosswalk_final <- new_crosswalk2 %>% st_drop_geometry() %>%
  select(LocationID, UniversalReefID)
fwrite(reefcrosswalk_final, file = "output/reef_crosswalk_final_12-04-24.csv")

# Explore OIMMP boundaries
oysamplelocs_m_regions <- st_join(oysamplelocs_m, oimmp, join = st_within)

crosswalk_with_regions <- merge(
  oysamplelocs_m_regions %>% 
    st_drop_geometry(), 
  new_crosswalk2 %>% 
    select(LocationID, UniversalReefID) %>% 
    st_drop_geometry()
)

oimmp_reef_overview <- crosswalk_with_regions %>% 
  group_by(Region) %>%
  mutate(`n_reefs_in_region` = length(unique(UniversalReefID))) %>%
  ungroup() %>%
  group_by(Region, ProgramID, n_reefs_in_region) %>%
  reframe(`n_UniversalReefID` = length(unique(UniversalReefID)),
          `percentage_of_all_URID` = round(n_UniversalReefID / length(unique(crosswalk_with_regions$UniversalReefID))*100, 2)) %>%
  rename("OIMMP Region" = "Region")

oimmp_reef_overview$`percentage_of_region_URID` <- round(
  oimmp_reef_overview$`n_UniversalReefID` / oimmp_reef_overview$`n_reefs_in_region` * 100, 
  2)

oimmp_reef_overview <- merge(
  oimmp_reef_overview, 
  oysterraw %>% group_by(ProgramID, ProgramName) %>% summarise())

oimmp_reef_overview <- oimmp_reef_overview %>% select(
  "OIMMP Region", n_reefs_in_region, ProgramID, ProgramName, n_UniversalReefID, percentage_of_all_URID, percentage_of_region_URID
)

openxlsx::write.xlsx(oimmp_reef_overview, file = "OIMMP_UniversalReefID_Overview.xlsx")
