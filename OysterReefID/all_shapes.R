library(stringr)
library(sf)
library(tidyverse)
library(dplyr)

oyster_shapes <- list.files(oyster_shapefile_loc, pattern = ".shp", full.names = T)
oyster_shapes <- str_subset(oyster_shapes, ".xml", negate = T)

shape_directory <- list()
for(shape in oyster_shapes){
  name <- tail(str_split(shape, "/")[[1]],1)
  print(paste0("Processing shapefile: ", name))
  df <- st_read(shape)
  df$geometry <- st_make_valid(df$geometry)
  df <- st_transform(df, crs=4326)

  shape_directory[[name]] <- df %>% mutate(across(names(df)[names(df)!="geometry"], as.character))
}

all_oysters <- bind_rows(shape_directory, .id="id")
all_oysters <- st_transform(all_oysters, crs=4326)
all_oysters$geometry <- st_make_valid(all_oysters$geometry)

# Function to find matching polygons with error handling
find_matching_polygons <- function(shapefile_df, sample_geometry, buffer = 20) {
  tryCatch({
    buffered_sample_geometry <- st_buffer(sample_geometry, buffer)
    
    # Find rows in the shapefile dataframe where the buffered sample geometry intersects with the geometry
    matched_rows <- shapefile_df %>%
      filter(st_intersects(geometry, buffered_sample_geometry, sparse = FALSE))
    
    return(matched_rows %>% select(where(~sum(!is.na(.))>0)))
    
  }, error = function(e) {
    message("An error occurred during the intersection check: ", e$message)
    return(NULL)
  })
}

matched_polygons <- find_matching_polygons(
  shapefile_df = all_oysters, 
  sample_geometry = reefcrosswalk_rcp %>% 
    filter(ProgramLoc == "Pinellas Point Park") %>% 
    pull(geometry),
  buffer = 20
)


# matched_polygons <- find_matching_polygons(all_oysters, oysterraw[1, "geometry"], buffer=0)
# tb_shapes <- st_transform(shape_directory[["TB_oysters_2020_final_1.shp"]], crs = 4326)

new <- oysamplelocs_m[oysamplelocs_m$ProgramLoc=="69a", ]

tb <- st_transform(oysamplelocs_m[1002, ], crs = 4326)
tb$geometry <- st_make_valid(tb$geometry)

buffered_sample_geometry <- st_buffer(tb, dist = 100)

matched_polygons <- find_matching_polygons(all_oysters, buffered_sample_geometry)

################
no_match <- reefcrosswalk_rcp %>% filter(match_cat=="no match")

tb <- st_transform(no_match[501,], crs = 4326)
tb$geometry <- st_make_valid(tb$geometry)

buffered_sample_geometry <- st_buffer(tb, dist = 100)
matched_polygons <- find_matching_polygons(all_oysters, buffered_sample_geometry)

# fwcoymap_m_rcp <- st_transform(fwcoymap_m_rcp, crs=4326)
# fwcoymap_m_rcp$geometry <- st_make_valid(fwcoymap_m_rcp$geometry)
matched_polygons_ORF <- find_matching_polygons(fwcoymap_m_rcp, buffered_sample_geometry)
