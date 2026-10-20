### The purpose of this script is to identify oyster sampling locations
### which have not currently been assigned a UniversalReefID value
### This script utilizes FWC Oyster Beds in Florida, SEACAR Oyster data export, 
### and previously-created UniversalReefID crosswalk to cross-reference these 
### points and assign UID values. 
### The first check is for "direct" matches: when the sample location falls directly on the reef
### The second check is for "buffered" matches: when there is no match with any reef, apply buffer and identify nearby reefs
### If a "buffered" point matches with more than 1 reef, and therefore inconclusive, a new UID is created and designated as "seacar_designated"
### We will record whether the match is "direct" or "buffered"
### If they match with nearby reefs, they inherit
### those UID values, if they do not match with previous reefs, a new UID is created.
### Written with AI-assistance (OpenAI GPT-5.6 Sol)
library(sf)
library(mapview)
library(data.table)
library(dplyr)
library(rstudioapi)
library(stringr)
library(purrr)
library(tidyverse)
library(igraph)

wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

###############################################################################
# 1. Settings and import relevant files.
###############################################################################

# Today's date for any new UID assignments
current_run_date <- Sys.Date()

# Projected Florida CRS in meters for all spatial analyses.
analysis_crs <- st_crs(6439)

# Distance used for:
# 1. Buffered searches for nearby reef polygons.
# 2. Grouping unmatched point locations.
buffer_distance_m <- 14.21

# When a buffered search finds multiple reefs, accept the closest reef when
# it is within this distance of the sample point. (currently 1m)
buffered_ambiguity_accept_distance_m <- 1

# When TRUE, create all review tables and then stop before UID assignment.
# When FALSE, spatially ambiguous points are processed as unmatched points and
# buffered UID conflicts are processed as UID merges.
stop_on_spatial_review <- FALSE
stop_on_buffered_uid_conflict <- FALSE

# Maximum separation allowed when testing whether polygons from adjacent
# versions represent a shifted version of the same reef.
cross_version_tolerance_m <- 14.21

# A positive-area intersection larger than this is considered definite
# cross-version equivalence.
cross_version_min_intersection_area_m2 <- 0.01

# For polygons that do not directly overlap, their buffered polygons must
# overlap by at least this proportion of the smaller buffered polygon.
cross_version_min_buffered_overlap_ratio <- 0.25

# Historical polygons can be linked automatically through positive-area
# overlap. Shifted historical matching is disabled by default.
allow_shifted_historical_equivalence <- FALSE

# Duplicate vertices within this distance are removed.
# Units are meters because analysis_crs is projected.
duplicate_vertex_tolerance_m <- 0.001

# Load in latest SEACAR Oyster dataset
file_in <- str_subset(list.files("C:/SEACAR Data/SEACARdata/", full.names = TRUE), "OYSTER")
oysterraw <- fread(file_in, sep="|", na.strings="NULL")

# Previously, SEACAR sampling locations shapefile was imported as reference.
# Now, all unique sampling locations are derived from oysterraw (rename cols to maintain previous naming convention)
oy_sample_locs <- oysterraw %>%
  select(
    LocationID, 
    ProgramID,
    ProgramLoc = ProgramLocationID,
    Latitude_D = OriginalLatitude,
    Longitude_ = OriginalLongitude
  ) %>%
  distinct() %>%
  st_as_sf(
    coords = c(
      "Longitude_",
      "Latitude_D"
    ),
    crs = 4326,
    remove = FALSE
  )

# Validation of oysterraw coordinates
location_coordinate_validation <- oysterraw %>%
  filter(!is.na(LocationID)) %>%
  distinct(LocationID, OriginalLatitude, OriginalLongitude) %>%
  count(LocationID, name = "n_coordinate_pairs") %>%
  filter(n_coordinate_pairs != 1)

if(nrow(location_coordinate_validation) > 0){
  stop(
    "One or more LocationID values are associated with ",
    "multiple coordinate pairs. Review location_coordinate_validation."
  )
}

# Preserve the incoming point CRS for the final output.
output_crs <- st_crs(oy_sample_locs)

# Function to transform metadata to include sourcedate information if not available
rename_metadata <- function(SOURCEDATE, METADATA){
  ifelse((str_detect(METADATA, SOURCEDATE)|str_detect(METADATA, gsub("-", "_", SOURCEDATE))), METADATA, paste0(METADATA, "_", SOURCEDATE))
}
# Download latest Oyster Beds in Florida shapefile
fwc_new <- st_read("C:/SEACAR Data/SEACARshapes/Oyster_Beds_in_Florida/Oyster_Beds_in_Florida.shp") %>%
  st_transform(crs = analysis_crs) %>% st_make_valid()
# Bring in old fwc map(s) for reference
fwc_2024 <- st_read("C:/SEACAR Data/SEACARshapes/Oyster_Beds_in_Florida/archive/7-29-24/Oyster_Beds_in_Florida.shp") %>%
  st_transform(crs = analysis_crs) %>% st_make_valid()
# Bring in all_oysters historical shapefile(large number of columns, non-standardized, must be handled later)
fwc_hist <- readRDS("AllOysterShapes/all_oysters_2026-07-29.rds") %>%
  st_transform(crs = analysis_crs) %>% st_make_valid() %>%
  mutate(METADATA_hist = ifelse(!is.na(layer), layer, METADATA)) %>% 
  select(METADATA_hist, OBJECTID, Shape__Are, Shape__Len)

###############################################################################
# 2. VALIDATE THE ANALYSIS CRS
###############################################################################

if(is.na(analysis_crs)){
  stop("analysis_crs is undefined.")
}

if(isTRUE(st_is_longlat(analysis_crs))){
  stop(
    "analysis_crs must be projected. EPSG:4326 is longitude/latitude; ",
    "use EPSG:6439 for the meter-based spatial workflow."
  )
}

analysis_units <- analysis_crs$units_gdal

if(is.null(analysis_units) || 
   !tolower(analysis_units) %in% c("metre", "meter", "metres", "meters", "m")){
  stop("analysis_crs must use meters. Current units: ", analysis_units)
}

if(is.na(output_crs)){
  stop("oy_sample_locs does not have a defined CRS.")
}

###############################################################################
# 3. DEFINE FWC POLYGON LAYERS
###############################################################################

# The order must always be chronological:
#   historical, oldest annual layer, newer annual layers, current
#
# A future example might be:
#
# fwc_shapes <- list(
#   "historical" = fwc_hist,
#   "2024" = fwc_2024,
#   "2026" = fwc_2026,
#   "current" = fwc_2027
# )

fwc_shapes <- list(
  "historical" = fwc_hist,
  "2024" = fwc_2024,
  "2026" = fwc_new
)

if(anyDuplicated(names(fwc_shapes))){
  stop("fwc_shapes contains duplicate layer names.")
}

if(names(fwc_shapes)[[1]] != "historical"){
  stop("The first entry in fwc_shapes must be named 'historical'.")
}

if(tail(names(fwc_shapes), 1) == "historical"){
  stop(
    "fwc_shapes must contain at least one modern reef layer ",
    "after the historical layer."
  )
}

modern_layer_names <- names(fwc_shapes)[names(fwc_shapes) != "historical"]

if(any(!str_detect(modern_layer_names, "^\\d{4}$"))){
  stop(
    "All nonhistorical entries in fwc_shapes must be named ",
    "using their four-digit source year."
  )
}

###############################################################################
# 4. CREATE PROJECTED ANALYSIS COPIES
###############################################################################

oy_sample_locs_analysis <- oy_sample_locs %>%
  st_transform(analysis_crs)

fwc_shapes_analysis <- imap(
  fwc_shapes,
  function(fwc, layer_name){
    if(is.na(st_crs(fwc))){
      stop("FWC layer '", layer_name, "' does not have a defined CRS.")
    }
    fwc %>%
      st_transform(analysis_crs) %>%
      st_make_valid()
  }
)

stopifnot(!isTRUE(st_is_longlat(oy_sample_locs_analysis)))

walk(
  fwc_shapes_analysis,
  function(fwc){
    stopifnot(!isTRUE(st_is_longlat(fwc)))
  }
)

###############################################################################
# 5. DYNAMIC LAYER INFORMATION
###############################################################################

layer_info <- tibble(
  layer_name  = names(fwc_shapes_analysis),
  layer_order = seq_along(fwc_shapes_analysis)
) %>%
  mutate(
    is_historical = layer_name == "historical",
    is_latest = layer_order == max(layer_order),
    layer_suffix = if_else(is_historical, "hist", layer_name),
    # Historical OBJECTIDs are plural because multiple historical
    # polygons may overlap one sample location.
    objectid_col = if_else(is_historical, "OBJECTIDs_hist", paste0("OBJECTID_", layer_suffix)),
    metadata_col = paste0("METADATA_", layer_suffix),
    match_col = paste0("MATCH_", layer_suffix),
    n_direct_col = paste0("N_DIRECT_", layer_suffix),
    n_buffer_col = paste0("N_BUFFER_", layer_suffix)
  )

###############################################################################
# 6. GENERAL HELPER FUNCTIONS
###############################################################################

collapse_match_values <- function(x){
  values <- unique(as.character(x[!is.na(x)]))
  values <- values[values != ""]
  if(length(values) == 0){
    return(NA_character_)
  }
  numeric_values <- suppressWarnings(as.numeric(values))
  if(all(!is.na(numeric_values))){
    values <- values[order(numeric_values)]
  } else {
    values <- sort(values)
  }
  paste(values, collapse = "; ")
}

clean_polygon_geometry <- function(geometry, snap_tolerance_m = duplicate_vertex_tolerance_m){
  if(is.na(st_crs(geometry))){
    stop("clean_polygon_geometry() received geometry without a CRS.")
  }
  if(isTRUE(st_is_longlat(geometry))){
    stop("clean_polygon_geometry() requires a projected CRS.")
  }
  # Remove Z or M dimensions if they exist. The reef comparison
  # workflow only requires two-dimensional XY geometry.
  geometry <- st_zm(
    geometry,
    drop = TRUE,
    what = "ZM"
  )
  # Optional millimeter-scale coordinate normalization.
  # lwgeom::st_snap_to_grid() is available in current lwgeom releases.
  # If lwgeom is not installed, the workflow skips this step rather
  # than failing.
  if(snap_tolerance_m > 0 && requireNamespace("lwgeom", quietly = TRUE)){
    geometry <- suppressWarnings(
      lwgeom::st_snap_to_grid(
        geometry,
        size = snap_tolerance_m
      )
    )
  }
  # Repair invalid rings and prevent collapsed rings from being retained
  # as lines or points.
  geometry <- suppressWarnings(
    st_make_valid(
      geometry,
      geos_method = "valid_structure",
      geos_keep_collapsed = FALSE
    )
  )
  # st_make_valid() can return geometry collections.
  # Retain only polygonal components.
  geometry <- suppressWarnings(
    st_collection_extract(
      geometry,
      "POLYGON",
      warn = FALSE
    )
  )
  geometry <- suppressWarnings(
    st_cast(
      geometry,
      "MULTIPOLYGON",
      warn = FALSE
    )
  )
  # A zero-distance buffer forces GEOS to rebuild polygon rings and
  # can remove residual spikes or degenerate edges.
  geometry <- suppressWarnings(
    st_buffer(
      geometry,
      dist = 0
    )
  )
  # Validate the rebuilt result one more time.
  geometry <- suppressWarnings(
    st_make_valid(
      geometry,
      geos_method = "valid_structure",
      geos_keep_collapsed = FALSE
    )
  )
  geometry <- suppressWarnings(
    st_collection_extract(
      geometry,
      "POLYGON",
      warn = FALSE
    )
  )
  geometry <- suppressWarnings(
    st_cast(
      geometry,
      "MULTIPOLYGON",
      warn = FALSE
    )
  )
  geometry
}

prepare_fwc_layer <- function(fwc, layer_name, target_crs){
  if(is.na(st_crs(fwc))){
    stop("The ", layer_name, " reef layer does not have a defined CRS.")
  }
  
  if(st_crs(fwc) != target_crs){
    fwc <- st_transform(fwc, target_crs)
  }
  
  fwc <- st_make_valid(fwc)
  
  if(layer_name == "historical"){
    reef_metadata <- as.character(fwc$METADATA)
  } else {
    reef_metadata <- as.character(rename_metadata(fwc$SOURCEDATE, fwc$METADATA))
  }
  
  fwc %>%
    mutate(
      .reef_objectid = as.character(OBJECTID),
      .reef_metadata = reef_metadata
    ) %>%
    filter(!is.na(.reef_objectid)) %>%
    select(.reef_objectid, .reef_metadata)
}

summarize_layer_matches <- function(target_sf, all_location_ids, prepared_fwc, distance_reference_sf = target_sf){
  if(nrow(target_sf) != nrow(distance_reference_sf) || 
     !identical(as.character(target_sf$LocationID), as.character(distance_reference_sf$LocationID))){
    stop("target_sf and distance_reference_sf must contain the same LocationID values in the same order.")
  }
  intersection_index <- st_intersects(target_sf, prepared_fwc)
  match_pairs <- imap_dfr(
    intersection_index,
    function(reef_rows, point_row){
      if(length(reef_rows) == 0){
        return(
          tibble(
            point_row = integer(),
            reef_row = integer()
          )
        )
      }
      tibble(
        point_row = rep(as.integer(point_row), length(reef_rows)),
        reef_row = reef_rows
      )
    }
  )
  
  if(nrow(match_pairs) == 0){
    match_summary <- tibble(
      LocationID = all_location_ids$LocationID[0],
      n_matches = integer(),
      objectids = character(),
      metadata = character(),
      candidate_distances = character(),
      min_distance_m = double(),
      closest_objectid = character(),
      closest_metadata = character(),
      n_closest_candidates = integer(),
      single_match_distance_m = double()
    )
    
  } else {
    
    match_pairs <- match_pairs %>%
      mutate(
        LocationID = target_sf$LocationID[point_row],
        .reef_objectid = prepared_fwc$.reef_objectid[reef_row],
        .reef_metadata = prepared_fwc$.reef_metadata[reef_row],
        distance_m = map2_dbl(
          point_row,
          reef_row,
          function(point_index, reef_index){
            as.numeric(
              st_distance(
                st_geometry(distance_reference_sf)[point_index],
                st_geometry(prepared_fwc)[reef_index]
              )
            )
          }
        )
      )
    
    # One OBJECTID may consist of several polygon parts.
    object_match_summary <- match_pairs %>%
      group_by(LocationID, .reef_objectid) %>%
      summarise(
        .reef_metadata = collapse_match_values(.reef_metadata),
        distance_m = min(distance_m, na.rm = TRUE),
        .groups = "drop"
      )
    
    match_summary <- object_match_summary %>%
      group_by(LocationID) %>%
      summarise(
        n_matches = n(),
        objectids = collapse_match_values(.reef_objectid),
        metadata = collapse_match_values(.reef_metadata),
        candidate_distances = paste(
          paste0(
            .reef_objectid,
            "=",
            format(
              round(distance_m, 3),
              nsmall = 3
            ),
            " m"
          ),
          collapse = "; "
        ),
        min_distance_m = min(distance_m, na.rm = TRUE),
        
        # Count how many candidates share the minimum distance.
        # A tie remains spatially ambiguous.
        n_closest_candidates = sum(
          dplyr::near(
            distance_m,
            min(distance_m, na.rm = TRUE)
          )
        ),
        
        # Retain the closest OBJECTID only when there is one uniquely
        # closest candidate.
        closest_objectid = {
          minimum_distance <- min(
            distance_m,
            na.rm = TRUE
          )
          
          closest_rows <- which(
            dplyr::near(
              distance_m,
              minimum_distance
            )
          )
          
          if (length(closest_rows) == 1) {
            .reef_objectid[
              closest_rows[[1]]
            ]
          } else {
            NA_character_
          }
        },
        
        # Retain the corresponding metadata.
        closest_metadata = {
          minimum_distance <- min(
            distance_m,
            na.rm = TRUE
          )
          
          closest_rows <- which(
            dplyr::near(
              distance_m,
              minimum_distance
            )
          )
          
          if (length(closest_rows) == 1) {
            .reef_metadata[
              closest_rows[[1]]
            ]
          } else {
            NA_character_
          }
        },
        
        single_match_distance_m = if (
          n() == 1
        ) {
          distance_m[[1]]
        } else {
          NA_real_
        },
        
        .groups = "drop"
      )
  }
  
  all_location_ids %>%
    left_join(
      match_summary,
      by = "LocationID"
    ) %>%
    mutate(n_matches = coalesce(n_matches, 0L))
}

build_reef_registry <- function(fwc, layer_name, layer_order, is_historical, target_crs){
  prepared_fwc <- prepare_fwc_layer(
    fwc = fwc,
    layer_name = layer_name,
    target_crs = target_crs
  )
  # Clean individual source features before grouping them by OBJECTID.
  st_geometry(prepared_fwc) <- clean_polygon_geometry(st_geometry(prepared_fwc))
  prepared_fwc <- prepared_fwc[!st_is_empty(prepared_fwc),]
  if(nrow(prepared_fwc) == 0){
    stop("No usable polygon geometry remained for layer '", layer_name, "' after geometry cleaning.")
  }
  reef_registry_layer <- prepared_fwc %>%
    group_by(.reef_objectid) %>%
    summarise(
      .reef_metadata = collapse_match_values(.reef_metadata),
      do_union = TRUE,
      .groups = "drop"
    )
  # The union can introduce new ring artifacts, so clean the grouped
  # geometries a second time.
  st_geometry(reef_registry_layer) <- clean_polygon_geometry(st_geometry(reef_registry_layer))
  
  reef_registry_layer <- reef_registry_layer[!st_is_empty(reef_registry_layer), ]
  
  if(nrow(reef_registry_layer) == 0){
    stop("No usable grouped polygon geometry remained for layer '", layer_name, "'.")
  }
  
  reef_registry_layer %>%
    mutate(
      layer_name = .env$layer_name,
      layer_order = .env$layer_order,
      is_historical = .env$is_historical,
      reef_key = paste(
        layer_name,
        .reef_objectid,
        sep = "::"
      ),
      reef_area_m2 = as.numeric(st_area(geometry))
    ) %>%
    select(
      reef_key,
      layer_name,
      layer_order,
      is_historical,
      .reef_objectid,
      .reef_metadata,
      reef_area_m2,
      geometry
    )
}

safe_intersection_area <- function(geometry_1, geometry_2){
  geometry_1 <- clean_polygon_geometry(geometry_1)
  geometry_2 <- clean_polygon_geometry(geometry_2)
  
  intersection_geometry <- suppressWarnings(
    st_intersection(geometry_1, geometry_2)
  )
  
  if(length(intersection_geometry) == 0 || all(st_is_empty(intersection_geometry))){
    return(0)
  }
  
  nonempty_geometry <- intersection_geometry[!st_is_empty(intersection_geometry)]
  
  if(length(nonempty_geometry) == 0){
    return(0)
  }
  
  sum(as.numeric(st_area(nonempty_geometry)),na.rm = TRUE)
}

empty_equivalence_table <- tibble(
  older_reef_key = character(),
  newer_reef_key = character(),
  older_layer = character(),
  newer_layer = character(),
  older_layer_order = integer(),
  newer_layer_order = integer(),
  pair_involves_historical = logical(),
  polygon_distance_m = double(),
  intersection_area_m2 = double(),
  overlap_of_smaller_polygon = double(),
  buffered_intersection_area_m2 = double(),
  buffered_overlap_ratio = double(),
  exact_equivalent = logical(),
  shifted_equivalent = logical(),
  equivalent = logical(),
  equivalence_type = character()
)

build_adjacent_equivalence <- function(older_sf, newer_sf, tolerance_m, 
                                       minimum_intersection_area_m2,
                                       minimum_buffered_overlap_ratio,
                                       allow_shifted_historical){
  
  if(nrow(older_sf) == 0 || nrow(newer_sf) == 0){
    return(empty_equivalence_table)
  }
  candidate_index <- st_is_within_distance(
    older_sf,
    newer_sf,
    dist = tolerance_m
  )
  candidate_pairs <- imap_dfr(
    candidate_index,
    function(newer_rows, older_row){
      if(length(newer_rows) == 0){
        return(
          tibble(
            older_row = integer(),
            newer_row = integer()
          )
        )
      }
      
      tibble(
        older_row = rep(
          as.integer(older_row),
          length(newer_rows)
        ),
        newer_row = newer_rows
      )
    }
  )
  
  if(nrow(candidate_pairs) == 0){
    return(empty_equivalence_table)
  }
  
  pmap_dfr(
    candidate_pairs,
    function(older_row, newer_row){
      older_row <- as.integer(older_row)
      newer_row <- as.integer(newer_row)
      older_reef_key <- older_sf$reef_key[[older_row]]
      newer_reef_key <- newer_sf$reef_key[[newer_row]]
      tryCatch(
        {
          
          older_geometry <- clean_polygon_geometry(st_geometry(older_sf)[older_row])
          
          newer_geometry <- clean_polygon_geometry(st_geometry(newer_sf)[newer_row])
          
          older_area <- older_sf$reef_area_m2[[older_row]]
          
          newer_area <- newer_sf$reef_area_m2[[newer_row]]
          
          smaller_polygon_area <- min(older_area, newer_area)
          
          polygon_distance_m <- as.numeric(st_distance(older_geometry, newer_geometry))
          
          intersection_area_m2 <- safe_intersection_area(older_geometry, newer_geometry)
          
          overlap_of_smaller_polygon <- if(smaller_polygon_area > 0){
            intersection_area_m2 / smaller_polygon_area
          } else {
            0
          }
          
          older_buffer <- st_buffer(older_geometry, dist = tolerance_m)
          
          newer_buffer <- st_buffer(newer_geometry, dist = tolerance_m)
          
          older_buffer <- clean_polygon_geometry(older_buffer)
          
          newer_buffer <- clean_polygon_geometry(newer_buffer)
          
          older_buffer_area <- as.numeric(st_area(older_buffer))
          
          newer_buffer_area <- as.numeric(st_area(newer_buffer))
          
          smaller_buffer_area <- min(older_buffer_area, newer_buffer_area)
          
          buffered_intersection_area_m2 <-safe_intersection_area(older_buffer, newer_buffer)
          
          buffered_overlap_ratio <- if(smaller_buffer_area > 0){
            buffered_intersection_area_m2 / smaller_buffer_area
          } else {
            0
          }
          
          pair_involves_historical <- older_sf$is_historical[[older_row]] || newer_sf$is_historical[[newer_row]]
          
          exact_equivalent <- intersection_area_m2 >= minimum_intersection_area_m2
          
          shifted_equivalent <- !exact_equivalent &&
            polygon_distance_m <= tolerance_m &&
            buffered_overlap_ratio >=
            minimum_buffered_overlap_ratio &&
           (
              !pair_involves_historical ||
                allow_shifted_historical
            )
          
          equivalent <- exact_equivalent || shifted_equivalent
          
          equivalence_type <- case_when(
            exact_equivalent ~ "positive_area_overlap",
            shifted_equivalent ~ "shifted_buffer_overlap",
            TRUE ~ "not_equivalent"
          )
          
          tibble(
            older_reef_key = older_reef_key,
            newer_reef_key = newer_reef_key,
            older_layer = older_sf$layer_name[[older_row]],
            newer_layer = newer_sf$layer_name[[newer_row]],
            older_layer_order = older_sf$layer_order[[older_row]],
            newer_layer_order = newer_sf$layer_order[[newer_row]],
            pair_involves_historical = pair_involves_historical,
            polygon_distance_m = polygon_distance_m,
            intersection_area_m2 = intersection_area_m2,
            overlap_of_smaller_polygon = overlap_of_smaller_polygon,
            buffered_intersection_area_m2 = buffered_intersection_area_m2,
            buffered_overlap_ratio = buffered_overlap_ratio,
            exact_equivalent = exact_equivalent,
            shifted_equivalent = shifted_equivalent,
            equivalent = equivalent,
            equivalence_type = equivalence_type
          )
        },
        
        error = function(e){
          stop(
            paste0(
              "Cross-version geometry comparison failed for ",
              older_reef_key,
              " and ",
              newer_reef_key,
              ". Original error: ",
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      )
    }
  )
}

build_unmatched_clusters <- function(unmatched_points, distance_m){
  unmatched_points <- unmatched_points %>%
    arrange(LocationID)
  
  if(nrow(unmatched_points) == 0){
    return(
      tibble(
        LocationID =
          unmatched_points$LocationID,
        
        unmatched_cluster_id =
          integer()
      )
    )
  }
  
  neighbors <- st_is_within_distance(
    unmatched_points,
    unmatched_points,
    dist = distance_m
  )
  
  edges <- map_dfr(
    seq_along(neighbors),
    function(i){
      
      connected_rows <- neighbors[[i]]
      
      # Remove self-connections and keep one copy of each pair.
      connected_rows <- connected_rows[
        connected_rows > i
      ]
      
      if(length(connected_rows) == 0){
        
        return(
          tibble(
            from = integer(),
            to = integer()
          )
        )
      }
      
      tibble(
        from = i,
        to = connected_rows
      )
    }
  )
  
  point_graph <- make_empty_graph(
    n = nrow(unmatched_points),
    directed = FALSE
  )
  
  if(nrow(edges) > 0){
    
    point_graph <- add_edges(
      point_graph,
      as.vector(
        t(
          as.matrix(edges)
        )
      )
    )
  }
  
  component_membership <- components(point_graph)$membership
  
  tibble(
    LocationID = unmatched_points$LocationID,
    .component = component_membership
  ) %>%
    group_by(.component) %>%
    mutate(
      .cluster_sort = min(as.character(LocationID))
    ) %>%
    ungroup() %>%
    mutate(
      unmatched_cluster_id = dense_rank(.cluster_sort)
    ) %>%
    select(
      LocationID,
      unmatched_cluster_id
    )
}

###############################################################################
# 7. VALIDATE INPUT SAMPLE LOCATIONS
###############################################################################

duplicate_input_locations <- oy_sample_locs_analysis %>%
  st_drop_geometry() %>%
  count(
    LocationID,
    name = "n_rows"
  ) %>%
  filter(n_rows > 1)

if(nrow(duplicate_input_locations) > 0){
  stop(
    "oy_sample_locs contains duplicate LocationID values. ",
    "Review duplicate_input_locations before continuing."
  )
}

all_location_ids <- oy_sample_locs_analysis %>%
  st_drop_geometry() %>%
  distinct(LocationID)

###############################################################################
# 8. DIRECT POLYGON MATCHING
###############################################################################

direct_results <- imap(
  fwc_shapes_analysis,
  function(fwc, layer_name){
    
    layer_row <- layer_info[layer_info$layer_name == layer_name, ]
    
    prepared_fwc <- prepare_fwc_layer(
      fwc = fwc,
      layer_name = layer_name,
      target_crs = analysis_crs
    )
    
    layer_summary <- summarize_layer_matches(
      target_sf = oy_sample_locs_analysis,
      all_location_ids = all_location_ids,
      prepared_fwc = prepared_fwc,
      distance_reference_sf = oy_sample_locs_analysis
    )
    
    objectid_col <- layer_row$objectid_col[[1]]
    metadata_col <- layer_row$metadata_col[[1]]
    match_col <- layer_row$match_col[[1]]
    n_direct_col <- layer_row$n_direct_col[[1]]
    
    wide <- layer_summary %>%
      transmute(
        LocationID,
        "{objectid_col}" := objectids,
        "{metadata_col}" := metadata,
        "{match_col}" := n_matches > 0,
        "{n_direct_col}" := n_matches
      )
    
    long <- layer_summary %>%
      mutate(
        layer_name = layer_row$layer_name[[1]],
        layer_order = layer_row$layer_order[[1]],
        layer_suffix = layer_row$layer_suffix[[1]],
        is_historical = layer_row$is_historical[[1]]
      )
    
    list(
      wide = wide,
      long = long
    )
  }
)

direct_wide <- direct_results %>%
  map("wide") %>%
  reduce(
    left_join,
    by = "LocationID"
  )

direct_layer_summary <- direct_results %>%
  map("long") %>%
  bind_rows()

pts_fwc <- oy_sample_locs_analysis %>%
  left_join(direct_wide, by = "LocationID")

###############################################################################
# 9. DETERMINE THE HIGHEST-PRIORITY DIRECT MATCH
###############################################################################

direct_selected <- direct_layer_summary %>%
  filter(n_matches > 0) %>%
  group_by(LocationID) %>%
  slice_max(
    order_by = layer_order,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

direct_decisions <- direct_selected %>%
  mutate(
    requires_review =(!is_historical & n_matches > 1),
    # An ambiguous direct intersection remains a no-match for UID
    # assignment but retains MATCH_Type = direct for auditing.
    MATCH_Final = if_else(requires_review, "no match", paste0(layer_suffix,"_match")),
    MATCH_Type = "direct",
    MATCH_Review = if_else(requires_review, paste0("direct_multiple_", layer_suffix),NA_character_),
    MATCH_Layer = layer_name,
    MATCH_OBJECTID = if_else(requires_review, NA_character_, objectids),
    MATCH_METADATA = if_else(requires_review, NA_character_, metadata),
    MATCH_Distance_m = if_else(requires_review, NA_real_, single_match_distance_m),
    MATCH_N_Reefs = n_matches,
    MATCH_Candidate_OBJECTIDs = objectids,
    MATCH_Candidate_METADATA = metadata,
    MATCH_Candidate_Distances = candidate_distances
  ) %>%
  select(
    LocationID,
    MATCH_Final,
    MATCH_Type,
    MATCH_Review,
    MATCH_Layer,
    MATCH_OBJECTID,
    MATCH_METADATA,
    MATCH_Distance_m,
    MATCH_N_Reefs,
    MATCH_Candidate_OBJECTIDs,
    MATCH_Candidate_METADATA,
    MATCH_Candidate_Distances
  )

###############################################################################
# 10. IDENTIFY POINTS WITH NO DIRECT MATCH IN ANY LAYER
###############################################################################

locations_without_direct_matches <- direct_layer_summary %>%
  group_by(LocationID) %>%
  summarise(has_direct_match = any(n_matches > 0),
            .groups = "drop") %>%
  filter(!has_direct_match) %>%
  select(LocationID)

buffer_candidates <- oy_sample_locs_analysis %>%
  semi_join(
    locations_without_direct_matches,
    by = "LocationID"
  )

buffer_candidate_ids <- buffer_candidates %>%
  st_drop_geometry() %>%
  select(LocationID)

buffer_targets <- st_buffer(
  buffer_candidates,
  dist = buffer_distance_m
)

###############################################################################
# 11. BUFFERED POLYGON MATCHING
###############################################################################

buffer_results <- imap(
  fwc_shapes_analysis,
  function(fwc, layer_name){
    layer_row <- layer_info[layer_info$layer_name == layer_name, ]
    prepared_fwc <- prepare_fwc_layer(
      fwc = fwc,
      layer_name = layer_name,
      target_crs = analysis_crs
    )
    layer_summary <- summarize_layer_matches(
      target_sf = buffer_targets,
      all_location_ids = buffer_candidate_ids,
      prepared_fwc = prepared_fwc,
      # Distances are measured from the original point, not its buffer.
      distance_reference_sf = buffer_candidates
    )
    
    layer_summary %>%
      mutate(
        layer_name = layer_row$layer_name[[1]],
        layer_order = layer_row$layer_order[[1]],
        layer_suffix = layer_row$layer_suffix[[1]],
        is_historical = layer_row$is_historical[[1]]
      )
  }
)

buffer_layer_summary <- bind_rows(buffer_results)

# Use the highest-priority layer that has at least one candidate.
# Multiple candidates in that layer stop the hierarchy and are tagged
# for spatial review.
buffer_selected <- buffer_layer_summary %>%
  filter(n_matches > 0) %>%
  group_by(LocationID) %>%
  slice_max(
    order_by = layer_order,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

buffer_decisions <- buffer_selected %>%
  mutate(
    # Accept a multiple-reef buffered result when one reef is uniquely
    # closest and is no more than 1 m from the sample point.
    accept_closest_within_threshold =
      n_matches > 1 &
      n_closest_candidates == 1 &
      min_distance_m <=
      buffered_ambiguity_accept_distance_m,
    
    # A multiple match requires review only when the new closest-reef
    # exception does not apply.
    requires_review = n_matches > 1 & !accept_closest_within_threshold,
    
    MATCH_Final = if_else(
      requires_review,
      "no match",
      paste0(
        layer_suffix,
        "_match"
      )
    ),
    
    MATCH_Type =
      "buffered",
    
    MATCH_Review = if_else(
      requires_review,
      paste0(
        "buffered_multiple_",
        layer_suffix
      ),
      NA_character_
    ),
    
    MATCH_Layer =
      layer_name,
    
    # For an accepted multiple match, use the closest candidate.
    # For a normal single match, use the original OBJECTID value.
    MATCH_OBJECTID = case_when(
      accept_closest_within_threshold ~
        closest_objectid,
      
      requires_review ~
        NA_character_,
      
      TRUE ~
        objectids
    ),
    
    # Use metadata associated with the accepted closest candidate.
    MATCH_METADATA = case_when(
      accept_closest_within_threshold ~ closest_metadata,
      requires_review ~ NA_character_,
      TRUE ~ metadata
    ),
    
    # Record the closest distance for accepted multiple matches.
    MATCH_Distance_m = case_when(
      accept_closest_within_threshold ~ min_distance_m,
      requires_review ~ NA_real_,
      TRUE ~ single_match_distance_m
    ),
    
    # Retain the total number of candidate reefs, even when the
    # closest candidate is accepted.
    MATCH_N_Reefs = n_matches,
    MATCH_Candidate_OBJECTIDs = objectids,
    MATCH_Candidate_METADATA = metadata,
    MATCH_Candidate_Distances = candidate_distances
  ) %>%
  select(
    LocationID,
    MATCH_Final,
    MATCH_Type,
    MATCH_Review,
    MATCH_Layer,
    MATCH_OBJECTID,
    MATCH_METADATA,
    MATCH_Distance_m,
    MATCH_N_Reefs,
    MATCH_Candidate_OBJECTIDs,
    MATCH_Candidate_METADATA,
    MATCH_Candidate_Distances
  )

###############################################################################
# 12. ADD BUFFER COUNTS TO THE WIDE POINT TABLE
###############################################################################

buffer_count_tables <- map(
  seq_len(nrow(layer_info)),
  function(i){
    layer_name <- layer_info$layer_name[[i]]
    n_buffer_col <- layer_info$n_buffer_col[[i]]
    buffer_layer_summary %>%
      filter(.data$layer_name == .env$layer_name) %>%
      transmute(
        LocationID,
        "{n_buffer_col}" := n_matches
      )
  }
)

buffer_count_wide <- buffer_count_tables %>%
  reduce(left_join, by = "LocationID")

pts_fwc <- pts_fwc %>%
  left_join(buffer_count_wide, by = "LocationID")

# Locations that were not buffer candidates receive zero buffer matches.
for(n_buffer_col in layer_info$n_buffer_col){
  pts_fwc[[n_buffer_col]] <- coalesce(pts_fwc[[n_buffer_col]], 0L)
}

###############################################################################
# 13. COMBINE DIRECT AND BUFFERED MATCH DECISIONS
###############################################################################

match_decisions <- bind_rows(direct_decisions, buffer_decisions)

pts_fwc_final <- pts_fwc %>%
  left_join(match_decisions, by = "LocationID") %>%
  mutate(
    MATCH_Final = coalesce(MATCH_Final, "no match"),
    MATCH_Type = coalesce(MATCH_Type, "unmatched"),
    MATCH_N_Reefs = coalesce(MATCH_N_Reefs, 0L)
  )

###############################################################################
# 14. RECORD ACCEPTED BUFFERED MATCHES IN LAYER-SPECIFIC COLUMNS
###############################################################################

for(i in seq_len(nrow(layer_info))){
  layer_name <- layer_info$layer_name[[i]]
  objectid_col <- layer_info$objectid_col[[i]]
  metadata_col <- layer_info$metadata_col[[i]]
  match_col <- layer_info$match_col[[i]]
  
  # Use the completed buffered decisions so accepted multiple matches
  # are written into the appropriate layer-specific columns.
  buffered_updates <- buffer_decisions %>%
    filter(
      MATCH_Layer == .env$layer_name,
      MATCH_Final != "no match",
      !is.na(MATCH_OBJECTID)
    ) %>%
    transmute(
      LocationID,
      .buffer_objectid = MATCH_OBJECTID,
      .buffer_metadata = MATCH_METADATA
    )
  
  if(nrow(buffered_updates) == 0){
    next
  }
  
  pts_fwc_final <- pts_fwc_final %>%
    left_join(
      buffered_updates,
      by = "LocationID"
    ) %>%
    mutate(
      "{objectid_col}" := coalesce(.data[[objectid_col]], .buffer_objectid),
      "{metadata_col}" := coalesce(.data[[metadata_col]], .buffer_metadata),
      "{match_col}" := coalesce(.data[[match_col]], FALSE) | !is.na(.buffer_objectid)
    ) %>%
    select(
      -.buffer_objectid,
      -.buffer_metadata
    )
}

###############################################################################
# 15. SPATIAL-MATCH REVIEW LOG
###############################################################################

spatial_match_review <- pts_fwc_final %>%
  filter(!is.na(MATCH_Review)) %>%
  mutate(automatic_resolution = "treat_as_unmatched_point_cluster") %>%
  select(
    LocationID,
    ProgramID,
    ProgramLoc,
    MATCH_Type,
    MATCH_Review,
    MATCH_Layer,
    MATCH_N_Reefs,
    MATCH_Candidate_OBJECTIDs,
    MATCH_Candidate_METADATA,
    MATCH_Candidate_Distances,
    automatic_resolution,
    geometry
  )

###############################################################################
# 16. IMPORT HISTORICAL UID ASSIGNMENTS
###############################################################################

#### IMPORTANT ####
# `original_crosswalk_path` should point to the output file (of this script) from the previous run.
# This import and format may need adjusting in future run-through.
# The current import is "new_crosswalk2_2024-12-04.rds" which is the output 
# from the previous (now deprecated) Oyster Reef UID script (OysterCrosswalk.R).
# The following lines may need alterations as the .rds objects from this 
# script (UpdateOysterURID.R) will be different.
# The date is extracted from the previous run file name and used as a column 
# heading for the UID associations created on that date.
# These original UID associations and any previous date columns are imported
# and used to maintain a running log of UID associations through the years.
###################

original_crosswalk_path <- "output/new_crosswalk2_2024-12-04.rds"

# # Future path (date may need changing)
# original_crosswalk_path <- "output/oyster_crosswalk_2026-07-28.rds"

original_crosswalk <- readRDS(original_crosswalk_path)
# Extract date from previous run-through
original_crosswalk_date_iso <- str_extract(
  basename(original_crosswalk_path),
  "(?<!\\d)\\d{4}-\\d{2}-\\d{2}(?!\\d)"
)

if(is.na(original_crosswalk_date_iso)){
  stop(
    "A YYYY-MM-DD date could not be extracted from ",
    basename(original_crosswalk_path),
    "."
  )
}

original_uid_column <- format(as.Date(original_crosswalk_date_iso), "%m-%d-%Y")

# Date represented by the previous crosswalk file.
original_crosswalk_date <- as.Date(original_crosswalk_date_iso)

existing_uid_history_columns <- names(original_crosswalk)[str_detect(names(original_crosswalk),"^\\d{2}-\\d{2}-\\d{4}$")]

if(original_uid_column %in% existing_uid_history_columns){
  stop(
    "The original crosswalk already contains a UID-history column named ",
    original_uid_column,
    "."
  )
}

# Older crosswalks may predate the AssignmentDate field
# Initialize all existing assignments using date extracted from source filename
if(!"AssignmentDate" %in% names(original_crosswalk)){
  original_crosswalk <- original_crosswalk %>%
    mutate(AssignmentDate = original_crosswalk_date)
} else {
  # Ensure consistent Date storage in future iterations.
  original_crosswalk <- original_crosswalk %>%
    mutate(AssignmentDate = as.Date(AssignmentDate))
}

original_uid_lookup <- original_crosswalk

if(inherits(original_uid_lookup, "sf")){
  original_uid_lookup <- st_drop_geometry(original_uid_lookup)
}

original_uid_lookup <- original_uid_lookup %>%
  select(
    LocationID,
    all_of(existing_uid_history_columns),
    AssignmentDate,
    UniversalReefID
  ) %>%
  rename(
    "{original_uid_column}" := UniversalReefID,
    AssignmentDate_old = AssignmentDate
  )

duplicate_original_locations <- original_uid_lookup %>%
  count(LocationID, name = "n_records") %>%
  filter(n_records > 1)

if(nrow(duplicate_original_locations) > 0){
  stop(
    "original_crosswalk contains duplicate LocationID values. ",
    "Review duplicate_original_locations before continuing."
  )
}

point_uid_columns <- names(pts_fwc_final)[names(pts_fwc_final) == "UniversalReefID" | 
                                            str_detect(names(pts_fwc_final),"^\\d{2}-\\d{2}-\\d{4}$")]

pts_fwc_final <- pts_fwc_final %>%
  select(-any_of(point_uid_columns)) %>%
  left_join(
    original_uid_lookup,
    by = "LocationID"
  ) %>%
  mutate(
    UniversalReefID_old = suppressWarnings(as.numeric(as.character(.data[[original_uid_column]])))
  )

invalid_original_uid_values <- pts_fwc_final %>%
  st_drop_geometry() %>%
  filter(
    !is.na(.data[[original_uid_column]]),
    is.na(UniversalReefID_old)
  )

if(nrow(invalid_original_uid_values) > 0){
  stop(
    "Some previous UniversalReefID values could not be converted ",
    "to numeric values. Review invalid_original_uid_values."
  )
}

###############################################################################
# 17. BUILD A SAMPLED-REEF-ONLY CROSS-VERSION REGISTRY
###############################################################################

# The statewide registry is unnecessary because only reefs associated
# with sample locations can affect the UniversalReefID crosswalk.
# First, identify every unambiguous polygon match recorded for a sample
# location in every layer. These are not limited to MATCH_Final because
# an older-layer match can provide useful cross-version evidence even
# when a newer layer takes priority.

direct_sampled_reef_matches <- direct_layer_summary %>%
  filter(n_matches == 1) %>%
  transmute(
    LocationID,
    layer_name,
    layer_order,
    is_historical,
    reef_objectid = as.character(objectids),
    point_match_type = "direct",
    match_priority = 1L
  )

buffered_sampled_reef_matches <- buffer_layer_summary %>%
  filter(n_matches == 1) %>%
  transmute(
    LocationID,
    layer_name,
    layer_order,
    is_historical,
    reef_objectid = as.character(objectids),
    point_match_type = "buffered",
    match_priority = 2L
  )

# Combine direct and buffered evidence.
# Direct matches take priority if both types somehow exist for the same
# LocationID and layer.
sampled_reef_matches <- bind_rows(
  direct_sampled_reef_matches, 
  buffered_sampled_reef_matches
) %>%
  arrange(LocationID, layer_order, match_priority) %>%
  group_by(
    LocationID,
    layer_name,
    layer_order,
    is_historical
  ) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(
    reef_key = paste(layer_name, reef_objectid, sep = "::")
  )

# Create one row per sampled polygon.
sampled_reef_ids_by_layer <- sampled_reef_matches %>%
  distinct(
    layer_name,
    layer_order,
    is_historical,
    reef_objectid,
    reef_key
  )

# Optional diagnostic showing how many statewide polygons will be
# excluded from the equivalence process.
reef_registry_reduction_log <- map_dfr(
  seq_len(nrow(layer_info)),
  function(i){
    layer_name <- layer_info$layer_name[[i]]
    
    total_polygon_ids <- fwc_shapes_analysis[[layer_name]] %>%
      st_drop_geometry() %>%
      transmute(
        reef_objectid = as.character(OBJECTID)
      ) %>%
      distinct(reef_objectid) %>%
      nrow()
    
    sampled_polygon_ids <- sampled_reef_ids_by_layer %>%
      filter(.data$layer_name == .env$layer_name) %>%
      distinct(reef_objectid) %>%
      nrow()
    
    tibble(
      layer_name = layer_name,
      
      total_polygon_ids = total_polygon_ids,
      
      sampled_polygon_ids = sampled_polygon_ids,
      
      excluded_polygon_ids = total_polygon_ids - sampled_polygon_ids,
      
      percent_retained = if(total_polygon_ids > 0){
        100 * sampled_polygon_ids / total_polygon_ids
      } else {
        NA_real_
      }
    )
  }
)

# Build registry geometries only for polygons associated with at least
# one sample location.
reef_registry_list <- map(
  seq_len(nrow(layer_info)),
  function(i){
    layer_name <- layer_info$layer_name[[i]]
    layer_order <- layer_info$layer_order[[i]]
    is_historical <- layer_info$is_historical[[i]]
    sampled_objectids <- sampled_reef_ids_by_layer %>%
      filter(.data$layer_name == .env$layer_name) %>%
      pull(reef_objectid) %>%
      unique()
    # A layer with no sampled polygons does not need to appear in
    # the cross-version registry.
    if(length(sampled_objectids) == 0){
      return(NULL)
    }
    
    fwc_subset <- fwc_shapes_analysis[[layer_name]] %>%
      mutate(.objectid_character = as.character(OBJECTID)) %>%
      filter(.objectid_character %in% sampled_objectids) %>%
      select(-.objectid_character)
    
    if(nrow(fwc_subset) == 0){
      return(NULL)
    }
    
    build_reef_registry(
      fwc = fwc_subset,
      layer_name = layer_name,
      layer_order = layer_order,
      is_historical = is_historical,
      target_crs = analysis_crs
    )
  }
) %>% compact()

if(length(reef_registry_list) == 0){
  stop(
    "No sampled reef polygons were available for the ",
    "cross-version registry."
  )
}

reef_registry <- do.call(
  rbind,
  reef_registry_list
)

stopifnot(!isTRUE(st_is_longlat(reef_registry)))

# Confirm that the filtered registry contains only sampled reef keys.
unexpected_registry_keys <- reef_registry %>%
  st_drop_geometry() %>%
  filter(!reef_key %in% sampled_reef_ids_by_layer$reef_key)

if(nrow(unexpected_registry_keys) > 0){
  stop(
    "reef_registry contains polygon keys that were not identified ",
    "as sampled reefs. Review unexpected_registry_keys."
  )
}

reef_registry_validity <- reef_registry %>%
  mutate(
    is_valid = st_is_valid(geometry),
    validity_reason = st_is_valid(geometry, reason = TRUE),
    is_empty = st_is_empty(geometry)
  ) %>%
  st_drop_geometry()

invalid_reef_registry <- reef_registry_validity %>%
  filter(!is_valid | is_empty)

if(nrow(invalid_reef_registry) > 0){
  stop(
    "The sampled reef registry contains invalid or empty geometries. ",
    "Review invalid_reef_registry."
  )
}

###############################################################################
# 18. IDENTIFY EQUIVALENCE AMONG SAMPLED MODERN REEF POLYGONS
###############################################################################

# Historical is excluded from routine cross-version polygon equivalence.
# The historical layer contains many overlapping catalog polygons and can
# create unnecessarily large or misleading reef components. Historical-only
# matches still retain their raw historical reef keys and existing UIDs.
equivalence_layer_info <- layer_info %>%
  filter(!is_historical) %>%
  arrange(layer_order)

adjacent_equivalence_candidates <- if(nrow(equivalence_layer_info) < 2){
  empty_equivalence_table
} else {
  map_dfr(
    seq_len(nrow(equivalence_layer_info) - 1),
    function(i){
      older_order <- equivalence_layer_info$layer_order[[i]]
      newer_order <- equivalence_layer_info$layer_order[[i + 1]]
      older_layer_name <- equivalence_layer_info$layer_name[[i]]
      newer_layer_name <- equivalence_layer_info$layer_name[[i + 1]]
      older_sf <- reef_registry %>%
        filter(layer_order == older_order)
      newer_sf <- reef_registry %>%
        filter(layer_order == newer_order)
      # Return an empty result when one of the adjacent versions
      # contains no sampled reefs.
      if(nrow(older_sf) == 0 || nrow(newer_sf) == 0){
        return(empty_equivalence_table)
      }
      
      message(
        "Comparing sampled reef polygons: ",
        older_layer_name,
        " -> ",
        newer_layer_name,
        " (",
        nrow(older_sf),
        " older; ",
        nrow(newer_sf),
        " newer)"
      )
      
      build_adjacent_equivalence(
        older_sf = older_sf,
        newer_sf = newer_sf,
        tolerance_m = cross_version_tolerance_m,
        minimum_intersection_area_m2 = cross_version_min_intersection_area_m2,
        minimum_buffered_overlap_ratio = cross_version_min_buffered_overlap_ratio,
        allow_shifted_historical = FALSE
      )
    }
  )
}

###############################################################################
# 19. SELECT THE BEST NEWER POLYGON FOR EACH OLDER POLYGON
###############################################################################

ranked_equivalence_candidates <- adjacent_equivalence_candidates %>%
  filter(equivalent) %>%
  group_by(older_reef_key) %>%
  arrange(
    desc(exact_equivalent),
    desc(overlap_of_smaller_polygon),
    desc(buffered_overlap_ratio),
    polygon_distance_m,
    newer_reef_key,
    .by_group = TRUE
  ) %>%
  mutate(
    n_equivalent_candidates = n(),
    equivalence_rank = row_number(),
    selected_equivalence = equivalence_rank == 1,
    mapping_requires_review = n_equivalent_candidates > 1
  ) %>%
  ungroup()

cross_version_mapping_review <- ranked_equivalence_candidates %>%
  filter(mapping_requires_review) %>%
  mutate(automatic_resolution = "select_highest_ranked_newer_polygon")

selected_equivalence_edges <- ranked_equivalence_candidates %>%
  filter(selected_equivalence)

###############################################################################
# 20. CONSTRUCT CROSS-VERSION PHYSICAL REEF COMPONENTS
###############################################################################

reef_vertices <- reef_registry %>%
  st_drop_geometry() %>%
  transmute(
    name = reef_key,
    reef_key,
    layer_name,
    layer_order,
    is_historical,
    reef_objectid = .reef_objectid
  )

reef_edges <- selected_equivalence_edges %>%
  transmute(
    from = older_reef_key,
    to = newer_reef_key
  )

reef_equivalence_graph <- graph_from_data_frame(
  d = reef_edges,
  directed = FALSE,
  vertices = reef_vertices
)

component_membership <- components(reef_equivalence_graph)$membership

reef_component_lookup <- reef_vertices %>%
  mutate(component_id = unname(component_membership[name])) %>%
  group_by(component_id) %>%
  arrange(
    desc(layer_order),
    reef_key,
    .by_group = TRUE
  ) %>%
  mutate(
    # The newest polygon key represents the physical reef component.
    reef_component_key = first(reef_key),
    component_latest_layer = first(layer_name),
    component_latest_order = first(layer_order),
    component_n_polygons = n(),
    component_mapping_review = any(reef_key %in% cross_version_mapping_review$older_reef_key)
  ) %>%
  ungroup() %>%
  select(
    reef_key,
    reef_component_key,
    component_id,
    component_latest_layer,
    component_latest_order,
    component_n_polygons,
    component_mapping_review
  )

cross_version_component_log <- reef_component_lookup %>%
  group_by(
    component_id,
    reef_component_key,
    component_latest_layer,
    component_latest_order
  ) %>%
  summarise(
    n_polygon_versions = n(),
    reef_keys = collapse_match_values(reef_key),
    contains_mapping_review = any(component_mapping_review),
    .groups = "drop"
  )

###############################################################################
# 21. ASSIGN RAW REEF KEYS AND PHYSICAL REEF COMPONENT KEYS TO POINTS
###############################################################################

match_layer_order <- layer_info %>%
  select(
    MATCH_Layer = layer_name,
    MATCH_Layer_Order = layer_order
  )

pts_uid_work <- pts_fwc_final %>%
  left_join(
    match_layer_order,
    by = "MATCH_Layer"
  ) %>%
  mutate(
    MATCH_OBJECTID = as.character(MATCH_OBJECTID),
    # Accepted annual/current matches normally contain one OBJECTID.
    reef_key = case_when(
      MATCH_Final != "no match" & 
        !is.na(MATCH_Layer) & 
        !is.na(MATCH_OBJECTID) & 
        !str_detect(MATCH_OBJECTID,fixed(";")) ~ paste(MATCH_Layer, MATCH_OBJECTID, sep = "::"),
      # Historical matches may contain multiple OBJECTIDs.
      MATCH_Final != "no match" &
        !is.na(MATCH_Layer) &
        !is.na(MATCH_OBJECTID) ~ paste0(
          MATCH_Layer, 
          "::MULTI::",
          str_replace_all(MATCH_OBJECTID, "\\s*;\\s*", ";")
        ),
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(
    reef_component_lookup,
    by = "reef_key"
  ) %>%
  mutate(
    component_mapping_review = coalesce(component_mapping_review, FALSE),
    # Composite historical reef keys will not appear in the polygon
    # registry and therefore retain their raw key.
    reef_component_key = coalesce(reef_component_key, reef_key),
    reef_component_layer = coalesce(component_latest_layer, MATCH_Layer),
    reef_component_order = coalesce(component_latest_order, MATCH_Layer_Order),
    
    CROSS_VERSION_Review = case_when(
      component_mapping_review ~ "multiple_equivalent_newer_polygons",
      TRUE ~ NA_character_
    )
  )

###############################################################################
# 22. PROCESS UNMATCHED AND SPATIAL-REVIEW POINTS
###############################################################################

# All MATCH_Final == "no match" points are processed here.
# This includes:
#   - Points with no reef candidate.
#   - Direct searches with multiple polygons.
#   - Buffered searches with multiple polygons.
unmatched_processing_points <- pts_uid_work %>%
  filter(MATCH_Final == "no match") %>%
  mutate(
    Unmatched_Reason = case_when(
      !is.na(MATCH_Review) ~ "spatial_review_fallback",
      TRUE ~ "no_reef_match"
    )
  )

unmatched_clusters <- build_unmatched_clusters(
  unmatched_points = unmatched_processing_points,
  distance_m = buffer_distance_m
)

unmatched_processing_details <- unmatched_processing_points %>%
  st_drop_geometry() %>%
  select(
    LocationID,
    Unmatched_Reason
  )

###############################################################################
# 23. CREATE COMMON UID ASSIGNMENT GROUPS
###############################################################################

pts_uid_work <- pts_uid_work %>%
  left_join(
    unmatched_clusters,
    by = "LocationID"
  ) %>%
  left_join(
    unmatched_processing_details,
    by = "LocationID"
  ) %>%
  mutate(
    assignment_group_type = case_when(
      !is.na(reef_component_key) ~ "reef_component",
      MATCH_Final == "no match" ~ "unmatched_cluster",
      TRUE ~ NA_character_
    ),
    assignment_group_key = case_when(
      assignment_group_type == "reef_component" ~ reef_component_key,
      assignment_group_type == "unmatched_cluster" ~ paste0("unmatched_cluster::", unmatched_cluster_id),
      TRUE ~ NA_character_
    ),
    assignment_group_layer = case_when(
      assignment_group_type == "reef_component" ~ reef_component_layer,
      TRUE ~ NA_character_
    ),
    assignment_layer_order = case_when(
      assignment_group_type == "reef_component" ~ reef_component_order,
      assignment_group_type == "unmatched_cluster" ~ 0L,
      TRUE ~ NA_integer_
    )
  )

###############################################################################
# 24. SUMMARIZE EXISTING UIDS WITHIN EACH ASSIGNMENT GROUP
###############################################################################

assignment_group_summary_base <- pts_uid_work %>%
  st_drop_geometry() %>%
  filter(!is.na(assignment_group_key)) %>%
  group_by(
    assignment_group_key,
    assignment_group_type,
    assignment_group_layer,
    assignment_layer_order
  ) %>%
  summarise(
    n_locations = n(),
    LocationIDs = collapse_match_values(LocationID),
    ProgramIDs = collapse_match_values(ProgramID),
    ProgramLocs = collapse_match_values(ProgramLoc),
    raw_reef_keys = collapse_match_values(reef_key),
    match_types = collapse_match_values(MATCH_Type),
    any_direct_match = any(MATCH_Type == "direct", na.rm = TRUE),
    any_buffered_match = any(MATCH_Type == "buffered", na.rm = TRUE),
    any_spatial_review = any(!is.na(MATCH_Review)),
    spatial_review_types = collapse_match_values(MATCH_Review),
    n_existing_uids = n_distinct(UniversalReefID_old, na.rm = TRUE),
    existing_uids = collapse_match_values(UniversalReefID_old),
    single_existing_uid = {
      uid_values <- sort(unique(UniversalReefID_old[!is.na(UniversalReefID_old)]))
      if(length(uid_values) == 1){
        as.numeric(uid_values[[1]])
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  )

###############################################################################
# 25. DETECT ONE HISTORICAL UID ON DISCONNECTED REEF GROUPS
###############################################################################

group_uid_links <- pts_uid_work %>%
  st_drop_geometry() %>%
  filter(
    !is.na(assignment_group_key),
    !is.na(UniversalReefID_old)
  ) %>%
  distinct(
    assignment_group_key,
    assignment_group_type,
    assignment_group_layer,
    UniversalReefID_old
  )

# Ignore purely historical reef components for the automatic UID split rule.
modern_group_uid_links <- group_uid_links %>%
  filter(!(assignment_group_type == "reef_component" & assignment_group_layer == "historical"))

uid_disconnected_group_summary <- modern_group_uid_links %>%
  group_by(UniversalReefID_old) %>%
  summarise(
    n_disconnected_groups = n_distinct(assignment_group_key),
    disconnected_group_keys = collapse_match_values(assignment_group_key),
    .groups = "drop"
  )

assignment_group_split_flags <- modern_group_uid_links %>%
  inner_join(
    uid_disconnected_group_summary,
    by = "UniversalReefID_old"
  ) %>%
  filter(n_disconnected_groups > 1) %>%
  group_by(assignment_group_key) %>%
  summarise(
    contains_disconnected_uid = TRUE,
    split_source_uids = collapse_match_values(UniversalReefID_old),
    .groups = "drop"
  )

###############################################################################
# 26. DETERMINE UID ACTION FOR EACH GROUP
###############################################################################

assignment_group_summary <- assignment_group_summary_base %>%
  left_join(
    assignment_group_split_flags,
    by = "assignment_group_key"
  ) %>%
  mutate(
    contains_disconnected_uid = coalesce(contains_disconnected_uid, FALSE),
    group_action = case_when(
      # Ambiguous spatial matches become new unmatched clusters.
      any_spatial_review ~ "assign_new_uid_spatial_review_cluster",
      # Previously unsampled reef or unmatched cluster.
      n_existing_uids == 0 ~ "assign_new_uid",
      # Multiple previous UIDs on one physical reef component.
      n_existing_uids > 1 ~ "merge_multiple_existing_uids",
      # One previous UID on multiple geographically disconnected groups.
      n_existing_uids == 1 & contains_disconnected_uid ~ "split_disconnected_uid",
      TRUE ~ "use_existing_uid"
    ),
    
    requires_uid_review = case_when(
      n_existing_uids > 1 ~ TRUE,
      contains_disconnected_uid ~ TRUE,
      TRUE ~ FALSE
    )
  )


###############################################################################
# 27. CREATE REVIEW LOGS BEFORE STOP SETTINGS ARE EVALUATED
###############################################################################

assignment_run_timestamp <- Sys.time()

uid_merge_log <- assignment_group_summary %>%
  filter(group_action == "merge_multiple_existing_uids") %>%
  mutate(automatic_resolution = "assign_one_new_uid_to_component")

uid_merge_points <- pts_uid_work %>%
  filter(assignment_group_key %in% uid_merge_log$assignment_group_key)

buffered_uid_conflict_log <- assignment_group_summary %>%
  filter(assignment_group_type == "reef_component", 
         n_existing_uids > 1, 
         any_buffered_match) %>%
  mutate(automatic_resolution = "assign_one_new_uid_to_component")

buffered_uid_conflict_points <- pts_uid_work %>%
  filter(assignment_group_key %in% buffered_uid_conflict_log$assignment_group_key)

uid_split_log <- assignment_group_summary %>%
  filter(group_action == "split_disconnected_uid") %>%
  mutate(automatic_resolution = "assign_new_uid_to_each_disconnected_component")

uid_split_points <- pts_uid_work %>%
  filter(assignment_group_key %in% uid_split_log$assignment_group_key)

# ### (optional) View points and ensure they should all receive the same UID
# for(id in unique(buffered_uid_conflict_points$assignment_group_key)){
#   print(id)
#   subset <- buffered_uid_conflict_points %>% filter(assignment_group_key==id)
#   layer <- unique(subset$assignment_group_layer)
#   oids <- unique(subset$MATCH_Candidate_OBJECTIDs)
#   print(
#     mapView(subset, zcol = "MATCH_Type") +
#       mapView(fwc_shapes[[layer]] %>% filter(OBJECTID %in% oids))
#   )
# }

###############################################################################
# 28. APPLY OPTIONAL STOP SETTINGS
###############################################################################

if(stop_on_spatial_review && nrow(spatial_match_review) > 0){
  stop(
    nrow(spatial_match_review),
    " location(s) require spatial-match review. ",
    "Review spatial_match_review. To apply the automatic unmatched-point ",
    "fallback, set stop_on_spatial_review = FALSE."
  )
}

if(stop_on_buffered_uid_conflict && nrow(buffered_uid_conflict_log) > 0){
  stop(
    nrow(buffered_uid_conflict_log),
    " reef component(s) contain buffered matches and multiple existing ",
    "UniversalReefID values. Review buffered_uid_conflict_log. ",
    "To merge them automatically, set ",
    "stop_on_buffered_uid_conflict = FALSE."
  )
}


###############################################################################
# 29. DETERMINE THE MAXIMUM EXISTING UID
###############################################################################

existing_uid_values <- pts_uid_work$UniversalReefID_old[!is.na(pts_uid_work$UniversalReefID_old)]

maximum_existing_uid <- if(length(existing_uid_values) == 0){
  0
} else {
  max(existing_uid_values)
}

###############################################################################
# 30. ASSIGN NEW UIDS TO MERGES, SPLITS, NEW REEFS, AND UNMATCHED GROUPS
###############################################################################

new_uid_groups <- assignment_group_summary %>%
  filter(
    group_action %in% c(
      "assign_new_uid",
      "assign_new_uid_spatial_review_cluster",
      "merge_multiple_existing_uids",
      "split_disconnected_uid"
    )
  ) %>%
  mutate(
    group_type_order = case_when(
      assignment_group_type == "reef_component" ~ 1L,
      assignment_group_type == "unmatched_cluster" ~ 2L
    ),
    group_sort_numeric = suppressWarnings(
      as.numeric(str_extract(assignment_group_key, "\\d+$"))
    )
  ) %>%
  arrange(
    group_type_order,
    # Newest reef-layer components receive IDs first.
    desc(assignment_layer_order),
    is.na(group_sort_numeric),
    group_sort_numeric,
    assignment_group_key
  ) %>%
  mutate(
    new_uid = maximum_existing_uid + row_number()
  ) %>%
  select(
    assignment_group_key,
    new_uid
  )

group_uid_assignments <- assignment_group_summary %>%
  left_join(
    new_uid_groups,
    by = "assignment_group_key"
  ) %>%
  mutate(
    assigned_uid = case_when(
      group_action == "use_existing_uid" ~ single_existing_uid,
      group_action %in% c(
        "assign_new_uid",
        "assign_new_uid_spatial_review_cluster",
        "merge_multiple_existing_uids",
        "split_disconnected_uid"
      ) ~ new_uid,
      TRUE ~ NA_real_
    )
  )

###############################################################################
# 31. APPLY ASSIGNED UIDS TO EVERY POINT
###############################################################################

pts_uid_updated <- pts_uid_work %>%
  left_join(
    group_uid_assignments %>%
      select(
        assignment_group_key,
        n_locations,
        n_existing_uids,
        existing_uids,
        split_source_uids,
        contains_disconnected_uid,
        group_action,
        requires_uid_review,
        assigned_uid
      ),
    by = "assignment_group_key"
  ) %>%
  mutate(
    UniversalReefID_new = assigned_uid,
    uid_changed = case_when(
      is.na(UniversalReefID_old) & is.na(UniversalReefID_new) ~ FALSE,
      is.na(UniversalReefID_old) | is.na(UniversalReefID_new) ~ TRUE,
      TRUE ~ UniversalReefID_old != UniversalReefID_new
    ),
    AssignmentDate_new = case_when(
      # New location receiving UID for the first time
      is.na(UniversalReefID_old) & !is.na(UniversalReefID_new) ~ current_run_date,
      # Existing LocationID whose UID has changed
      uid_changed ~ current_run_date,
      # Existing LocationID retaining its current UID
      TRUE ~ AssignmentDate_old
    ),
    UID_Action = case_when(
      group_action == "assign_new_uid_spatial_review_cluster" ~ "assigned_new_uid_spatial_review_fallback",
      group_action == "merge_multiple_existing_uids" ~ "merged_multiple_existing_uids",
      group_action == "split_disconnected_uid" ~ "split_disconnected_existing_uid",
      group_action == "use_existing_uid" & is.na(UniversalReefID_old) ~ "inherited_existing_uid",
      group_action == "use_existing_uid" & !uid_changed ~ "retained_existing_uid",
      group_action == "assign_new_uid" & assignment_group_type == "reef_component" ~ "assigned_new_uid_unsampled_reef",
      group_action == "assign_new_uid" & assignment_group_type == "unmatched_cluster" ~ "assigned_new_uid_unmatched_cluster",
      TRUE ~ "review"
    )
  )

###############################################################################
# 32. CREATE UID AUDIT LOGS
###############################################################################

urid_group_log <- group_uid_assignments %>%
  mutate(
    run_timestamp = assignment_run_timestamp,
    source_uid_column = original_uid_column,
    maximum_uid_before_run = maximum_existing_uid
  ) %>%
  select(
    run_timestamp,
    source_uid_column,
    assignment_group_key,
    assignment_group_type,
    assignment_group_layer,
    assignment_layer_order,
    n_locations,
    LocationIDs,
    ProgramIDs,
    ProgramLocs,
    raw_reef_keys,
    match_types,
    any_direct_match,
    any_buffered_match,
    any_spatial_review,
    spatial_review_types,
    n_existing_uids,
    existing_uids,
    contains_disconnected_uid,
    split_source_uids,
    group_action,
    requires_uid_review,
    assigned_uid,
    maximum_uid_before_run
  )

urid_change_log <- pts_uid_updated %>%
  st_drop_geometry() %>%
  filter(uid_changed) %>%
  mutate(
    run_timestamp = assignment_run_timestamp,
    previous_uid_column = original_uid_column
  ) %>%
  select(
    run_timestamp,
    previous_uid_column,
    UID_Action,
    group_action,
    assignment_group_key,
    assignment_group_type,
    reef_key,
    reef_component_key,
    CROSS_VERSION_Review,
    UniversalReefID_old,
    AssignmentDate_old,
    UniversalReefID_new,
    AssignmentDate_new,
    LocationID,
    ProgramID,
    ProgramLoc,
    Latitude_D,
    Longitude_,
    MATCH_Final,
    MATCH_Type,
    MATCH_Review,
    MATCH_Layer,
    MATCH_OBJECTID,
    MATCH_METADATA,
    MATCH_Distance_m,
    MATCH_N_Reefs,
    MATCH_Candidate_OBJECTIDs,
    MATCH_Candidate_METADATA,
    MATCH_Candidate_Distances,
    Unmatched_Reason,
    everything()
  )

uid_lineage_log <- pts_uid_updated %>%
  st_drop_geometry() %>%
  filter(
    !is.na(UniversalReefID_old),
    uid_changed
  ) %>%
  distinct(
    UniversalReefID_old,
    UniversalReefID_new,
    assignment_group_key,
    assignment_group_type,
    group_action,
    UID_Action
  ) %>%
  arrange(
    UniversalReefID_old,
    UniversalReefID_new
  )

unmatched_cluster_log <- pts_uid_updated %>%
  st_drop_geometry() %>%
  filter(assignment_group_type == "unmatched_cluster") %>%
  group_by(
    unmatched_cluster_id,
    assignment_group_key
  ) %>%
  summarise(
    n_locations = n(),
    LocationIDs = collapse_match_values(LocationID),
    unmatched_reasons = collapse_match_values(Unmatched_Reason),
    spatial_review_types = collapse_match_values(MATCH_Review),
    previous_uids = collapse_match_values(UniversalReefID_old),
    assigned_uid = first(UniversalReefID_new),
    UID_Actions = collapse_match_values(UID_Action),
    .groups = "drop"
  )

###############################################################################
# 33. VALIDATE FINAL UID ASSIGNMENTS
###############################################################################

unresolved_uid_rows <- pts_uid_updated %>%
  filter(is.na(UniversalReefID_new))

if(nrow(unresolved_uid_rows) > 0){
  stop(
    nrow(unresolved_uid_rows),
    " LocationID value(s) remain without a UniversalReefID. ",
    "Inspect unresolved_uid_rows."
  )
}

assignment_group_validation <- pts_uid_updated %>%
  st_drop_geometry() %>%
  filter(!is.na(assignment_group_key)) %>%
  group_by(assignment_group_key) %>%
  summarise(
    n_final_uids = n_distinct(UniversalReefID_new, na.rm = TRUE),
    final_uids = collapse_match_values(UniversalReefID_new),
    .groups = "drop"
  ) %>%
  filter(n_final_uids != 1)

if(nrow(assignment_group_validation) > 0){
  stop(
    "One or more assignment groups contain multiple final UID values. ",
    "Review assignment_group_validation."
  )
}

###############################################################################
# 34. CHECK FOR ONE FINAL UID ON MULTIPLE MODERN REEF COMPONENTS
###############################################################################

uid_multiple_recent_reef_flags_after <- pts_uid_updated %>%
  st_drop_geometry() %>%
  filter(assignment_group_type == "reef_component",
         assignment_group_layer != "historical",
         !is.na(UniversalReefID_new)) %>%
  distinct(
    UniversalReefID = UniversalReefID_new,
    reef_component_key
  ) %>%
  group_by(UniversalReefID) %>%
  summarise(
    n_reef_components = n_distinct(reef_component_key),
    reef_component_keys = collapse_match_values(reef_component_key),
    .groups = "drop"
  ) %>%
  filter(n_reef_components > 1)

uid_multiple_recent_reef_points_after <- pts_uid_updated %>%
  filter(UniversalReefID_new %in% uid_multiple_recent_reef_flags_after$UniversalReefID)

# Broader diagnostic that includes unmatched clusters and historical groups.
uid_multiple_assignment_group_flags_after <- pts_uid_updated %>%
  st_drop_geometry() %>%
  filter(!is.na(UniversalReefID_new), !is.na(assignment_group_key)) %>%
  distinct(UniversalReefID = UniversalReefID_new,
           assignment_group_key) %>%
  group_by(UniversalReefID) %>%
  summarise(
    n_assignment_groups = n_distinct(assignment_group_key),
    assignment_group_keys = collapse_match_values(assignment_group_key),
    .groups = "drop") %>%
  filter(n_assignment_groups > 1)

###############################################################################
# 35. CREATE FINAL SPATIAL CROSSWALK
###############################################################################

pts_fwc_final_updated <- pts_uid_updated %>%
  mutate(UniversalReefID = UniversalReefID_new,
         AssignmentDate = AssignmentDate_new) %>%
  select(
    -any_of(
      c(
        "UniversalReefID_old",
        "UniversalReefID_new",
        "AssignmentDate_old",
        "AssignmentDate_new",
        "assigned_uid",
        "component_id",
        "component_latest_layer",
        "component_latest_order",
        "component_n_polygons",
        "component_mapping_review",
        "MATCH_Layer_Order",
        "reef_component_layer",
        "reef_component_order",
        "unmatched_cluster_id",
        "assignment_group_type",
        "assignment_group_key",
        "assignment_group_layer",
        "assignment_layer_order",
        "n_locations",
        "n_existing_uids",
        "existing_uids",
        "split_source_uids",
        "contains_disconnected_uid",
        "group_action",
        "requires_uid_review",
        "uid_changed"
      )
    )
  )


###############################################################################
# 36. ORDER UID-HISTORY AND AUDIT COLUMNS
###############################################################################

final_uid_history_columns <- names(pts_fwc_final_updated)[str_detect(names(pts_fwc_final_updated),"^\\d{2}-\\d{2}-\\d{4}$")]

if(length(final_uid_history_columns) > 1){
  history_dates <- as.Date(
    final_uid_history_columns,
    format = "%m-%d-%Y"
  )
  final_uid_history_columns <- final_uid_history_columns[order(history_dates)]
}

final_processing_columns <- intersect(
  c(
    "MATCH_Final",
    "MATCH_Type",
    "MATCH_Review",
    "MATCH_Layer",
    "MATCH_OBJECTID",
    "MATCH_METADATA",
    "MATCH_Distance_m",
    "MATCH_N_Reefs",
    "MATCH_Candidate_OBJECTIDs",
    "MATCH_Candidate_METADATA",
    "MATCH_Candidate_Distances",
    "reef_key",
    "reef_component_key",
    "CROSS_VERSION_Review",
    "Unmatched_Reason",
    "UID_Action"
  ),
  names(pts_fwc_final_updated)
)

pts_fwc_final_updated <- pts_fwc_final_updated %>%
  relocate(
    all_of(final_processing_columns),
    .before = UniversalReefID
  ) %>%
  relocate(
    all_of(final_uid_history_columns),
    .before = UniversalReefID
  ) %>%
  relocate(
    UniversalReefID,
    AssignmentDate,
    .before = geometry
  ) %>%
  # Rename columns for better display
  rename(
    OriginalLatitude = Latitude_D,
    OriginalLongitude = Longitude_,
    ProgramLocationID = ProgramLoc
  )


###############################################################################
# 37. RETURN FINAL POINT LAYER TO ITS ORIGINAL CRS
###############################################################################

pts_fwc_final_updated <- pts_fwc_final_updated %>%
  st_transform(output_crs)

# Add additional flags
pts_fwc_final_updated <- pts_fwc_final_updated %>%
  mutate(
    SEACAR_flag = case_when(
      MATCH_Final=="no match" ~ "S",
      MATCH_Final!="no match" & MATCH_Type=="buffered" ~ "B",
      MATCH_Final!="no match" & MATCH_Type=="direct" ~ "D",
      TRUE ~ NA_character_
    )
  )

# Relocate UniversalReefID and SEACAR_flag to be next to LocationID at begininng
pts_fwc_final_updated <- pts_fwc_final_updated %>%
  relocate(
    all_of(c("UniversalReefID", "SEACAR_flag", "AssignmentDate")),
    .after = LocationID
  ) %>% rename(
    UID_AssignmentDate = AssignmentDate
  )

finalMetadataFormat <- pts_fwc_final_updated %>%
  st_drop_geometry() %>%
  mutate(AmbiguousFlag = ifelse(!is.na(MATCH_Review), "a", NA)) %>%
  relocate(
    all_of(c("AmbiguousFlag")),
    .after = SEACAR_flag
  ) %>%
  select(-c(
    OBJECTIDs_hist,
    MATCH_hist,
    N_DIRECT_hist,
    MATCH_2024,
    N_DIRECT_2024,
    MATCH_2026,
    N_DIRECT_2026,
    N_BUFFER_hist,
    N_BUFFER_2024, 
    N_BUFFER_2026,
    MATCH_Layer, 
    MATCH_OBJECTID,
    MATCH_METADATA,
    MATCH_Distance_m,
    MATCH_N_Reefs,
    MATCH_Candidate_OBJECTIDs,
    MATCH_Candidate_METADATA,
    MATCH_Candidate_Distances,
    reef_key, 
    reef_component_key, 
    CROSS_VERSION_Review,
    Unmatched_Reason
  ))

finalMetadataFormat <- finalMetadataFormat %>%
  mutate(
    UniversalReefID = if_else(
      is.na(AmbiguousFlag), 
      paste0(UniversalReefID, "_", SEACAR_flag), 
      paste0(UniversalReefID, "_", SEACAR_flag, AmbiguousFlag)
    )
  ) %>%
  relocate(all_of(c("UniversalReefID")),
           .after = "LocationID") %>%
  select(-c(
    SEACAR_flag, 
    AmbiguousFlag
  )) %>%
  mutate(
    MATCH_Type = case_when(
      MATCH_Type == "unmatched" ~ "SEACAR-designated",
      MATCH_Type == "direct" ~ "Direct match",
      !is.na(MATCH_Review) ~ "Buffered match - ambiguous",
      MATCH_Type == "buffered" ~ "Buffered match",
      TRUE ~ MATCH_Type
    )
  )

## Write metadata output file to send to USF
openxlsx::write.xlsx(finalMetadataFormat, asTable=T, file = paste0("output/UID_Metadata_", Sys.Date(), ".xlsx"))

finalMap <- 
  mapView(pts_fwc_final_updated, zcol = "SEACAR_flag") +
  mapView(pts_fwc_final_updated, zcol = "UniversalReefID") +
  mapView(pts_fwc_final_updated, zcol = "12-04-2024") +
  mapView(pts_fwc_final_updated, zcol = "UID_Action") +
  mapView(fwc_shapes[[1]], col.regions = "firebrick", layer.name = names(fwc_shapes)[[1]]) +
  mapView(fwc_shapes[[2]], col.regions = "darkblue", layer.name = names(fwc_shapes)[[2]]) +
  mapView(fwc_shapes[[3]], col.regions = "darkgreen", layer.name = names(fwc_shapes)[[3]])

mapshot(finalMap, url = paste0("output/maps/finalMap_", Sys.Date(), ".html"))

# Display differences in reef splitting / grouping dynamic
temp <- pts_fwc_final_updated %>% filter(LocationID %in% c(
  939358, 939339, 939385, 939319, 939402, 939340, 939357, 939401, 919943, 939383,
  939399, 939323, 939322, 939341, 939342, 939356, 939384, 939325, 939355, 939324,
  939321, 939386, 939400, 939326
)) %>% mutate(UniversalReefID = as.factor(UniversalReefID),
              `12-04-2024` = as.factor(`12-04-2024`))

temp_map <-
  mapView(temp, zcol = "UniversalReefID") +
  mapView(temp, zcol = "12-04-2024")

# Function to collapse years into groups
collapse_years <- function(years) {
  years <- sort(unique(years))
  gaps <- c(TRUE, diff(years) > 1)
  starts <- years[gaps]
  ends <- years[c(gaps[-1], TRUE)]
  # Format the output with ranges or single years
  formatted <- ifelse(starts == ends, as.character(starts), paste(starts, ends, sep = "-"))
  paste(formatted, collapse = ", ")
}

# Grab relevant LocationID-level information to provide in metadata?
loc_id_stats <- oysterraw %>% 
  group_by(LocationID) %>%
  reframe(
    Parameters = paste0(unique(ParameterName), collapse = ", "),
    Years = collapse_years(Year),
    EearliestSampleDate = format(min(SampleDate)),
    LatestSampleDate = format(max(SampleDate)),
    SurveyMethods = paste0(unique(SurveyMethod), collapse = ", "),
    HabitatClassification = paste0(unique(HabitatClassification), collapse = ", "),
    QuadSizes = paste0(unique(QuadSize_m2), collapse = ", "),
    LiveDate_Qualifiers = paste0(unique(LiveDate_Qualifier), collapse = ", "),
  )

pts_fwc_final_updated_stats <- pts_fwc_final_updated %>%
  left_join(loc_id_stats, by = "LocationID")

###############################################################################
# 38. PRINCIPAL OUTPUTS
###############################################################################

# # Final crosswalk
# pts_fwc_final_updated
# 
# # Spatial ambiguity logs
# spatial_match_review
# 
# # Cross-version polygon equivalence
# adjacent_equivalence_candidates
# ranked_equivalence_candidates
# selected_equivalence_edges
# cross_version_mapping_review
# cross_version_component_log
# 
# # UID merge and split logs
# uid_merge_log
# uid_merge_points
# buffered_uid_conflict_log
# buffered_uid_conflict_points
# uid_split_log
# uid_split_points
# 
# # General UID audit logs
# urid_group_log
# urid_change_log
# uid_lineage_log
# unmatched_cluster_log
# 
# # Sampled-reef registry diagnostics
# sampled_reef_matches
# reef_registry_reduction_log
# 
# # Validation
# unresolved_uid_rows
# assignment_group_validation
# uid_multiple_recent_reef_flags_after
# uid_multiple_recent_reef_points_after
# uid_multiple_assignment_group_flags_after


###############################################################################
# 39. SAVE OUTPUT USING THE RUN DATE
###############################################################################

current_run_date <- Sys.Date()

output_crosswalk_path <- paste0(
  "output/oyster_crosswalk_",
  format(current_run_date, "%Y-%m-%d"),
  ".rds"
)

saveRDS(
  pts_fwc_final_updated,
  output_crosswalk_path
)

###############################################################################
# 40. CREATE OVERVIEW OF REEF_COMPONENT_KEY
###############################################################################

# Cross-ref LocID with reef_component_key
reef_components <- pts_fwc_final_updated %>% 
  select(LocationID, reef_component_key, UniversalReefID, SEACAR_flag) %>% 
  st_drop_geometry() %>%
  mutate(
    reef_component_key = if_else(is.na(reef_component_key), paste0("SEACAR::", row_number()), reef_component_key)
  )

reef_overview <- oysterraw %>%
  select(-UniversalReefID) %>%
  left_join(reef_components) %>%
  group_by(reef_component_key, UniversalReefID, SEACAR_flag) %>%
  reframe(
    ManagedAreaName = unique(ManagedAreaName),
    OIMMP = unique(OIMMP),
    N_Programs = length(unique(ProgramID)),
    Programs = paste0(unique(ProgramID), collapse = " | "),
    N_SamplingLocIDs = length(unique(LocationID)),
    SamplingLocIDs = paste0(unique(LocationID), collapse = " | "),
    SamplingLocs = paste0(unique(ProgramLocationID), collapse = " | "),
    SurveyMethods = paste0(unique(SurveyMethod), collapse = " | "),
    N_QuadSizes = length(unique(QuadSize_m2)),
    QuadSizes = paste0(unique(QuadSize_m2), collapse = " | "),
    N_ReefIdentifiers = length(unique(ReefIdentifier)),
    ReefIdentifiers = paste0(unique(ReefIdentifier), collapse = " | "),
    EarliestSampleDate = format(min(SampleDate)),
    LatestSampleDate = format(max(SampleDate)),
    
    ## Metrics by parameter
    # Shell Height
    meanShellHeight = round(mean(ResultValue[ParameterName=="Shell Height"], na.rm = T), 2),
    minShellHeight = round(min(ResultValue[ParameterName=="Shell Height"], na.rm = T), 2),
    maxShellHeight = round(max(ResultValue[ParameterName=="Shell Height"], na.rm = T), 2),
    # Density
    meanDensity = round(mean(ResultValue[ParameterName=="Density"], na.rm = T), 2),
    minDensity = round(min(ResultValue[ParameterName=="Density"], na.rm = T), 2),
    maxDensity = round(max(ResultValue[ParameterName=="Density"], na.rm = T), 2),
    # Percent Live
    meanPctLive = round(mean(ResultValue[ParameterName=="Percent Live"], na.rm = T), 2),
    minPctLive = round(min(ResultValue[ParameterName=="Percent Live"], na.rm = T), 2),
    maxPctLive = round(max(ResultValue[ParameterName=="Percent Live"], na.rm = T), 2),
    # Reef Height
    meanReefHeight = round(mean(ResultValue[ParameterName=="Reef Height"], na.rm = T), 2),
    minReefHeight = round(min(ResultValue[ParameterName=="Reef Height"], na.rm = T), 2),
    maxReefHeight = round(max(ResultValue[ParameterName=="Reef Height"], na.rm = T), 2),
  )

# Reefs sampled by more than 1 program
popular_reefs <- reef_overview %>% filter(N_Programs>1) %>% 
  rowwise() %>%
  mutate(
    OBJECTID = case_when(
      str_detect(reef_component_key, "MULTI") ~ tail(str_split_1(reef_component_key, "::"), 1),
      !str_detect(reef_component_key, "MULTI") ~ str_split_1(reef_component_key, "::")[[2]],
      TRUE ~ NA
    ),
    layer = case_when(
      str_detect(reef_component_key, "2026") ~ "current",
      str_detect(reef_component_key, "historical") ~ "historical",
    )
  )

# Most popular reefs
popular_reef_map <- 
  mapView(
    pts_fwc_final_updated %>% 
      filter(reef_component_key %in% unique(popular_reefs$reef_component_key)) %>%
      mutate(ProgramID = as.factor(ProgramID)), 
    zcol = "ProgramID",
    layer.name = "ProgramID"
) + mapView(
  fwc_shapes[["2026"]] %>% 
    filter(OBJECTID %in% popular_reefs$OBJECTID[popular_reefs$layer=="current"]),
  layer.name = "current FWC",
  col.regions = "darkblue"
) + mapView(
  fwc_shapes[["historical"]] %>% 
    filter(OBJECTID %in% str_split_1(popular_reefs$OBJECTID[popular_reefs$layer=="historical"], ";")),
  layer.name = "historical FWC",
  col.regions = "firebrick"
)

mapshot(popular_reef_map, url = paste0("output/maps/popular_reefs_", Sys.Date(), ".html"))

###############################################################################
# 41. SAVE COMBINED WORKBOOK
###############################################################################

# Export combined workbook of ALL outputs
library(openxlsx)
# Create named list of data objects (lst automatically creates named list from variable names)
wb <- lst(
  # Final crosswalk
  pts_fwc_final_updated,
  # Spatial ambiguity logs
  spatial_match_review,
  # Cross-version polygon equivalence
  adjacent_equivalence_candidates,
  ranked_equivalence_candidates,
  selected_equivalence_edges,
  cross_version_mapping_review,
  cross_version_component_log,
  # UID merge and split logs
  uid_merge_log,
  uid_merge_points,
  buffered_uid_conflict_log,
  buffered_uid_conflict_points,
  uid_split_log,
  uid_split_points,
  # General UID audit logs
  urid_group_log,
  urid_change_log,
  uid_lineage_log,
  unmatched_cluster_log,
  # Sampled-reef registry diagnostics
  sampled_reef_matches,
  reef_registry_reduction_log,
  # Additional information
  pts_fwc_final_updated_stats,
  reef_overview,
  popular_reefs,
  # Validation
  unresolved_uid_rows = if(nrow(unresolved_uid_rows)>0) unresolved_uid_rows else NULL,
  assignment_group_validation = if(nrow(assignment_group_validation)>0) assignment_group_validation else NULL,
  uid_multiple_recent_reef_flags_after = if(nrow(uid_multiple_recent_reef_flags_after)>0) uid_multiple_recent_reef_flags_after else NULL,
  uid_multiple_recent_reef_points_after = if(nrow(uid_multiple_recent_reef_points_after)>0) uid_multiple_recent_reef_points_after else NULL,
  uid_multiple_assignment_group_flags_after = if(nrow(uid_multiple_assignment_group_flags_after)>0) uid_multiple_assignment_group_flags_after else NULL,
)

# Remove geometry and convert to data.table for formatting (compact removes NA validation dataframes)
wb_adjusted <- lapply(compact(wb), function(x) x %>% st_drop_geometry() %>% as.data.table())
names(wb_adjusted) <- stringr::str_sub(names(wb_adjusted), 1, 31)

# Write to file
openxlsx::write.xlsx(
  wb_adjusted,
  file = paste0("output/FullWorkbooks/oyster_results_", 
                format(current_run_date, "%Y-%m-%d"), 
                ".xlsx"), 
  asTable = T)
