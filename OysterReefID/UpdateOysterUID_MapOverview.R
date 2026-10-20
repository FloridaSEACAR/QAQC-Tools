###############################################################################
# STATIC SPATIAL-MATCH REVIEW PLOTS
#
# Required objects from the UniversalReefID workflow:
#
#   spatial_match_review
#   pts_fwc_final_updated
#   fwc_shapes_analysis
#   layer_info
#
# Optional existing objects:
#
#   analysis_crs
#   buffer_distance_m
###############################################################################


###############################################################################
# 1. PACKAGES
###############################################################################

library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(patchwork)

###############################################################################
# 2. PLOT SETTINGS
###############################################################################

plot_crs <- if(exists("analysis_crs")){
  analysis_crs
} else {
  st_crs(6439)
}

review_buffer_m <- if(exists("buffer_distance_m")){
  buffer_distance_m
} else {
  14.21
}

panels_per_page <- 9
page_ncol <- 3
page_nrow <- 3

plot_output_directory <- "output/spatial_match_review_plots"

dir.create(plot_output_directory, recursive = TRUE, showWarnings = FALSE)

###############################################################################
# 3. IDENTIFY THE MOST RECENT PREVIOUS-UID COLUMN
###############################################################################

uid_history_columns <- names(pts_fwc_final_updated)[str_detect(names(pts_fwc_final_updated),"^\\d{2}-\\d{2}-\\d{4}$")]

if(length(uid_history_columns) == 0){
  stop("No dated UID-history columns were found in pts_fwc_final_updated.")
}

uid_history_dates <- as.Date(
  uid_history_columns,
  format = "%m-%d-%Y"
)

if(any(is.na(uid_history_dates))){
  stop("One or more UID-history column names could not be converted to dates.")
}

previous_uid_column <-uid_history_columns[which.max(uid_history_dates)]

previous_uid_date <- max(uid_history_dates)

message("Using previous UID column: ", previous_uid_column)

###############################################################################
# 4. PREPARE REVIEW LOCATIONS
###############################################################################

if(nrow(spatial_match_review) == 0){
  stop("spatial_match_review contains no review locations.")
}

uid_lookup <- pts_fwc_final_updated %>%
  st_drop_geometry() %>%
  transmute(
    LocationID,
    previous_uid = .data[[previous_uid_column]],
    current_uid = UniversalReefID
  )

review_points <- spatial_match_review %>%
  st_transform(plot_crs) %>%
  left_join(uid_lookup, by = "LocationID") %>%
  arrange(MATCH_Review, LocationID) %>%
  mutate(
    review_id = row_number(),
    previous_uid_label = case_when(
      is.na(previous_uid) ~ "NA",
      TRUE ~ as.character(previous_uid)
    ),
    current_uid_label = case_when(
      is.na(current_uid) ~ "NA",
      TRUE ~ as.character(current_uid)
    ),
    uid_transition_label = paste0(
      "Location ",
      LocationID,
      "\n",
      previous_uid_label,
      " \u2192 ",
      current_uid_label
    )
  )

###############################################################################
# 5. PARSE CANDIDATE REEF OBJECTIDS
###############################################################################

review_candidates <- review_points %>%
  st_drop_geometry() %>%
  transmute(
    review_id,
    review_location_id = LocationID,
    layer_name = MATCH_Layer,
    review_type = MATCH_Review,
    point_match_type = MATCH_Type,
    candidate_objectid_list = MATCH_Candidate_OBJECTIDs
  ) %>%
  mutate(candidate_objectid = str_split(as.character(candidate_objectid_list),"\\s*;\\s*")) %>%
  unnest_longer(candidate_objectid) %>%
  mutate(candidate_objectid = str_trim(candidate_objectid)) %>%
  filter(!is.na(candidate_objectid), candidate_objectid != "") %>%
  distinct(
    review_id,
    review_location_id,
    layer_name,
    review_type,
    point_match_type,
    candidate_objectid
  )

if(nrow(review_candidates) == 0){
  stop("No candidate OBJECTID values could be extracted from spatial_match_review.")
}

###############################################################################
# 6. EXTRACT CANDIDATE REEF POLYGONS
###############################################################################

missing_review_layers <- setdiff(
  unique(review_candidates$layer_name),
  names(fwc_shapes_analysis)
)

if(length(missing_review_layers) > 0){
  stop("The following review layers are not available in fwc_shapes_analysis: ",
       paste(missing_review_layers, collapse = ", "))
}

candidate_polygon_list <- map(
  unique(review_candidates$layer_name),
  function(current_layer) {
    layer_candidates <- review_candidates %>%
      filter(layer_name == current_layer)
    
    candidate_ids <- layer_candidates %>%
      distinct(candidate_objectid)
    
    layer_polygons <- fwc_shapes_analysis[[current_layer]] %>%
      st_transform(plot_crs) %>%
      mutate(candidate_objectid = as.character(OBJECTID)) %>%
      semi_join(
        candidate_ids,
        by = "candidate_objectid"
      )
    
    if(nrow(layer_polygons) == 0){
      return(NULL)
    }
    
    # Combine multipart records belonging to the same OBJECTID.
    layer_polygons <- layer_polygons %>%
      group_by(candidate_objectid) %>%
      summarise(
        do_union = TRUE,
        .groups = "drop"
      )
    
    # Repeat the polygon when the same candidate reef is involved
    # in more than one separate review case.
    layer_polygons %>%
      left_join(
        layer_candidates,
        by = "candidate_objectid"
      )
  }
) %>%
  compact()

if(length(candidate_polygon_list) == 0){
  stop("No candidate reef polygons could be extracted.")
}

candidate_polygons <- do.call(
  rbind,
  candidate_polygon_list
)

found_candidate_polygons <- candidate_polygons %>%
  st_drop_geometry() %>%
  distinct(
    review_id,
    layer_name,
    candidate_objectid
  )

missing_candidate_polygons <- review_candidates %>%
  anti_join(
    found_candidate_polygons,
    by = c(
      "review_id",
      "layer_name",
      "candidate_objectid"
    )
  )

if(nrow(missing_candidate_polygons) > 0){
  warning(nrow(missing_candidate_polygons), 
          " candidate reef polygon(s) could not be found. ", 
          "Inspect missing_candidate_polygons.")
}

###############################################################################
# 7. CREATE POINT-TO-LAYER REEF INDEX
###############################################################################

final_points_projected <- pts_fwc_final_updated %>%
  st_transform(plot_crs)

final_point_attributes <- final_points_projected %>%
  st_drop_geometry()

point_geometry_lookup <- tibble(
  LocationID = final_points_projected$LocationID,
  geometry = st_geometry(final_points_projected)
)

point_layer_index_attributes <- map_dfr(
  seq_len(nrow(layer_info)),
  function(i) {
    current_layer <- layer_info$layer_name[[i]]
    objectid_column <- layer_info$objectid_col[[i]]
    if(!objectid_column %in% names(final_point_attributes)){
      return(NULL)
    }
    
    final_point_attributes %>%
      transmute(
        LocationID,
        previous_uid = .data[[previous_uid_column]],
        current_uid = UniversalReefID,
        layer_name = current_layer,
        layer_objectids = as.character(.data[[objectid_column]])
      ) %>%
      mutate(candidate_objectid = str_split(layer_objectids,"\\s*;\\s*")) %>%
      unnest_longer(candidate_objectid) %>%
      mutate(candidate_objectid = str_trim(candidate_objectid)) %>%
      filter(!is.na(candidate_objectid), candidate_objectid != "") %>%
      select(-layer_objectids)
  }
)

point_layer_index <- point_layer_index_attributes %>%
  left_join(point_geometry_lookup, by = "LocationID") %>%
  st_as_sf(sf_column_name = "geometry", crs = plot_crs)

###############################################################################
# 8. IDENTIFY POINTS ASSOCIATED WITH CANDIDATE REEFS
###############################################################################

review_candidate_links <- review_candidates %>%
  select(
    review_id,
    review_location_id,
    layer_name,
    candidate_objectid
  )

associated_points <- point_layer_index %>%
  inner_join(
    review_candidate_links,
    by = c(
      "layer_name",
      "candidate_objectid"
    )
  ) %>%
  filter(
    LocationID !=
      review_location_id
  ) %>%
  mutate(
    # Use the exact same category as the polygon-fill scale.
    candidate_reef_label = paste0(
      layer_name,
      "::",
      candidate_objectid
    )
  ) %>%
  arrange(
    review_id,
    LocationID,
    candidate_reef_label
  ) %>%
  distinct(
    review_id,
    LocationID,
    candidate_reef_label,
    .keep_all = TRUE
  )

associated_point_counts <- associated_points %>%
  st_drop_geometry() %>%
  count(
    review_id,
    LocationID,
    name = "n_candidate_reefs"
  )

associated_points <- associated_points %>%
  left_join(
    associated_point_counts,
    by = c(
      "review_id",
      "LocationID"
    )
  )

###############################################################################
# 9. CREATE POINT-TO-REEF DISTANCE LINES
###############################################################################

build_distance_objects <- function(review_points, candidate_polygons){
  candidate_pairs <- candidate_polygons %>%
    st_drop_geometry() %>%
    distinct(
      review_id,
      layer_name,
      candidate_objectid
    )
  
  pair_results <- map(
    seq_len(nrow(candidate_pairs)),
    function(i){
      current_review_id <- candidate_pairs$review_id[[i]]
      current_objectid <- candidate_pairs$candidate_objectid[[i]]
      current_layer <- candidate_pairs$layer_name[[i]]
      review_point <- review_points %>%
        filter(review_id == current_review_id)
      
      reef_polygon <- candidate_polygons %>%
        filter(
          review_id == current_review_id,
          candidate_objectid == current_objectid
        )
      
      if(nrow(review_point) == 0 || nrow(reef_polygon) == 0){
        return(NULL)
      }
      
      nearest_line <- st_nearest_points(
        st_geometry(review_point),
        st_geometry(reef_polygon)
      )
      
      distance_m <- as.numeric(st_length(nearest_line))
      
      line_sf <- st_sf(
        review_id = current_review_id,
        layer_name = current_layer,
        candidate_objectid = current_objectid,
        distance_m = distance_m,
        distance_label = paste0(
          format(
            round(distance_m, 2),
            nsmall = 2
          ),
          " m"
        ),
        geometry = nearest_line,
        crs = plot_crs
      )
      
      if(is.finite(distance_m) && distance_m > 0.001){
        label_geometry <- st_line_sample(nearest_line, sample = 0.5) %>% st_cast("POINT")
      } else {
        label_geometry <- st_geometry(review_point)
      }
      
      label_sf <- st_sf(
        review_id = current_review_id,
        layer_name = current_layer,
        candidate_objectid = current_objectid,
        distance_m = distance_m,
        distance_label = line_sf$distance_label,
        geometry = label_geometry,
        crs = plot_crs
      )
      
      list(
        line = line_sf,
        label = label_sf
      )
    }
  ) %>%
    compact()
  
  if(length(pair_results) == 0){
    return(
      list(
        lines = NULL,
        labels = NULL
      )
    )
  }
  
  list(
    lines = do.call(
      rbind,
      map(
        pair_results,
        "line"
      )
    ),
    
    labels = do.call(
      rbind,
      map(
        pair_results,
        "label"
      )
    )
  )
}

distance_objects <- build_distance_objects(
  review_points = review_points,
  candidate_polygons = candidate_polygons
)

distance_lines <- distance_objects$lines
distance_labels <- distance_objects$labels

###############################################################################
# 10. CONVERT SF LABEL LOCATIONS TO X/Y
###############################################################################

add_xy_coordinates <- function(x) {
  coordinates <- st_coordinates(st_geometry(x))
  
  x %>%
    st_drop_geometry() %>%
    mutate(
      plot_x = coordinates[, "X"],
      plot_y = coordinates[, "Y"]
    )
}

###############################################################################
# 11. CREATE ONE REVIEW PANEL
###############################################################################

make_spatial_review_panel <- function(
    current_review_id
) {
  
  panel_review_point <- review_points %>%
    filter(
      review_id ==
        current_review_id
    )
  
  panel_polygons <- candidate_polygons %>%
    filter(
      review_id ==
        current_review_id
    )
  
  # layer-qualified label used in the candidate-reef legend.
  panel_polygons <- panel_polygons %>%
    mutate(
      candidate_reef_label = paste0(
        layer_name,
        "::",
        candidate_objectid
      )
    )
  
  panel_reef_levels <- sort(unique(panel_polygons$candidate_reef_label))
  
  panel_reef_colors <- scales::hue_pal()(length(panel_reef_levels))
  
  names(panel_reef_colors) <- panel_reef_levels
  
  panel_associated_points <- associated_points %>%
    filter(review_id == current_review_id)
  
  panel_distance_lines <- if (
    is.null(distance_lines)
  ) {
    NULL
  } else {
    distance_lines %>%
      filter(
        review_id ==
          current_review_id
      )
  }
  
  panel_distance_labels <- if (
    is.null(distance_labels)
  ) {
    NULL
  } else {
    distance_labels %>%
      filter(
        review_id ==
          current_review_id
      )
  }
  
  panel_buffer <- st_buffer(
    panel_review_point,
    dist = review_buffer_m
  )
  
  # # Place polygon labels inside each candidate reef.
  # polygon_label_points <- suppressWarnings(
  #   st_point_on_surface(
  #     panel_polygons
  #   )
  # )
  # 
  # polygon_label_data <- add_xy_coordinates(
  #   polygon_label_points
  # ) %>%
  #   mutate(
  #     polygon_label = paste0(
  #       layer_name,
  #       "::",
  #       candidate_objectid
  #     )
  #   )
  
  # review_label_data <- add_xy_coordinates(
  #   panel_review_point
  # )
  
  # associated_label_data <- if (
  #   nrow(panel_associated_points) == 0
  # ) {
  #   
  #   NULL
  #   
  # } else {
  #   
  #   add_xy_coordinates(
  #     panel_associated_points
  #   )
  # }
  
  distance_label_data <- if (
    is.null(panel_distance_labels) ||
    nrow(panel_distance_labels) == 0
  ) {
    
    NULL
    
  } else {
    
    add_xy_coordinates(
      panel_distance_labels
    )
  }
  
  # Calculate a panel-specific extent.
  extent_geometries <- c(
    st_geometry(
      panel_polygons
    ),
    st_geometry(
      panel_buffer
    ),
    st_geometry(
      panel_review_point
    )
  )
  
  if (nrow(panel_associated_points) > 0) {
    
    extent_geometries <- c(
      extent_geometries,
      st_geometry(
        panel_associated_points
      )
    )
  }
  
  panel_bbox <- st_bbox(
    extent_geometries
  )
  
  x_range <- as.numeric(
    panel_bbox["xmax"] -
      panel_bbox["xmin"]
  )
  
  y_range <- as.numeric(
    panel_bbox["ymax"] -
      panel_bbox["ymin"]
  )
  
  maximum_range <- max(
    x_range,
    y_range,
    review_buffer_m * 2,
    na.rm = TRUE
  )
  
  plot_padding <- max(
    review_buffer_m * 0.35,
    maximum_range * 0.08
  )
  
  panel_title <- paste0(
    "LocationID ",
    panel_review_point$LocationID[[1]]
  )
  
  panel_subtitle <- paste0(
    "ProgramID: ",
    panel_review_point$ProgramID[[1]],
    "\nPrevious UID: ",
    panel_review_point$previous_uid_label[[1]],
    " | New UID: ",
    panel_review_point$current_uid_label[[1]]
  )
  
  panel_plot <- ggplot() +
    
    # Candidate reefs
    geom_sf(
      data = panel_polygons,
      aes(
        fill = candidate_reef_label
      ),
      alpha = 0.32,
      linewidth = 0.55,
      show.legend = TRUE
    ) +
    
    # Search buffer around the reviewed point
    geom_sf(
      data = panel_buffer,
      fill = NA,
      linetype = "dashed",
      linewidth = 0.65
    ) +
    
    # Point-to-reef distance lines
    {
      if (
        !is.null(panel_distance_lines) &&
        nrow(panel_distance_lines) > 0
      ) {
        
        geom_sf(
          data = panel_distance_lines,
          linetype = "dotted",
          linewidth = 0.8
        )
        
      } else {
        
        geom_blank()
      }
    } +
    
    # Other locations associated with the candidate reefs
    {
      if (
        nrow(panel_associated_points) > 0
      ) {
        
        geom_sf(
          data = panel_associated_points %>%
            filter(
              n_candidate_reefs == 1
            ),
          aes(
            fill = candidate_reef_label
          ),
          shape = 21,
          color = "black",
          stroke = 0.45,
          size = 3,
          alpha = 0.95,
          show.legend = FALSE
        )
        
      } else {
        
        geom_blank()
      }
    } +
    
    {
      panel_multiple_points <- panel_associated_points %>%
        filter(
          n_candidate_reefs > 1
        ) %>%
        distinct(
          review_id,
          LocationID,
          .keep_all = TRUE
        )
      
      if (nrow(panel_multiple_points) > 0) {
        
        geom_sf(
          data = panel_multiple_points,
          shape = 23,
          fill = "white",
          color = "black",
          stroke = 0.8,
          size = 3.4,
          show.legend = FALSE
        )
        
      } else {
        
        geom_blank()
      }
    } +
    
    # Reviewed location
    geom_sf(
      data = panel_review_point,
      shape = 8,
      size = 4.5,
      stroke = 1.2
    ) +
    
    # # Candidate reef labels
    # geom_label_repel(
    #   data = polygon_label_data,
    #   aes(
    #     x = plot_x,
    #     y = plot_y,
    #     label = polygon_label
    #   ),
    #   size = 2.6,
    #   min.segment.length = 0,
    #   seed = 1,
    #   label.size = 0.15,
    #   max.overlaps = Inf
    # ) +
    
    # # Reviewed point label
    # geom_label_repel(
    #   data = review_label_data,
    #   aes(
    #     x = plot_x,
    #     y = plot_y,
    #     label = uid_transition_label
    #   ),
    #   size = 2.6,
    #   fontface = "bold",
    #   min.segment.length = 0,
    #   seed = 2,
    #   label.size = 0.2,
    #   max.overlaps = Inf
    # ) +
    
    # # Labels for other points on the candidate reefs
    # {
    #   if (
    #     !is.null(associated_label_data) &&
    #     nrow(associated_label_data) > 0
    #   ) {
    #     
    #     geom_text_repel(
    #       data = associated_label_data,
    #       aes(
    #         x = plot_x,
    #         y = plot_y,
    #         label = uid_transition_label
    #       ),
    #       size = 2.25,
    #       min.segment.length = 0,
    #       seed = 3,
    #       max.overlaps = Inf
    #     )
    #     
    #   } else {
    #     
    #     geom_blank()
    #   }
    # } +
    
    # Distance labels
    {
      if (
        !is.null(distance_label_data) &&
        nrow(distance_label_data) > 0
      ) {
        
        geom_label_repel(
          data = distance_label_data,
          aes(
            x = plot_x,
            y = plot_y,
            label = distance_label,
            
            fill = paste0(
              layer_name,
              "::",
              candidate_objectid
            )
          ),
          size = 2.3,
          color = "black",
          min.segment.length = 0,
          seed = 4,
          label.size = 0.15,
          max.overlaps = Inf,
          
          # The polygon layer already provides the reef legend.
          show.legend = FALSE
        )
        
      } else {
        
        geom_blank()
      }
    } +
    
    coord_sf(
      xlim = c(
        as.numeric(
          panel_bbox["xmin"]
        ) - plot_padding,
        
        as.numeric(
          panel_bbox["xmax"]
        ) + plot_padding
      ),
      
      ylim = c(
        as.numeric(
          panel_bbox["ymin"]
        ) - plot_padding,
        
        as.numeric(
          panel_bbox["ymax"]
        ) + plot_padding
      ),
      
      expand = FALSE,
      datum = NA
    ) +
    
    scale_fill_manual(
      values = panel_reef_colors,
      breaks = panel_reef_levels,
      drop = FALSE
    ) +
    
    labs(
      title = panel_title,
      subtitle = panel_subtitle,
      fill = "Candidate reef",
      caption = paste0(
        "Dashed outline = ",
        review_buffer_m,
        " m search buffer; dotted lines show point-to-reef distance. ",
        "\nColored circles = locations associated with one candidate reef. "
        # ,"\nWhite diamonds = locations associated with multiple candidates."
      )
    ) +
    
    guides(
      fill = guide_legend(
        title = "Candidate reef",
        override.aes = list(
          alpha = 0.65
        )
      )
    ) +
    
    theme_minimal(
      base_size = 9
    ) +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 11
      ),
      
      plot.subtitle = element_text(
        size = 8.5
      ),
      
      plot.caption = element_text(
        size = 6.5,
        hjust = 0
      ),
      
      axis.title = element_blank(),
      
      axis.text = element_text(
        size = 6
      ),
      
      panel.grid.minor = element_blank(),
      
      legend.position = c(0.98, 0.98),
      
      legend.justification = c(1, 1),
      
      legend.background = element_rect(
        fill = scales::alpha("white",0.88),
        color = "grey70",
        linewidth = 0.25
      ),
      
      legend.title = element_text(
        face = "bold",
        size = 7.5
      ),
      
      legend.text = element_text(size = 7),
      
      legend.key.size = grid::unit(0.35, "cm"),
      
      plot.margin = margin(7, 7, 7, 7)
    )
  
  panel_plot
}

###############################################################################
# 12. CREATE ALL REVIEW PANELS
###############################################################################

review_panel_list <- map(
  review_points$review_id,
  make_spatial_review_panel
)

names(review_panel_list) <- paste0(
  "LocationID_",
  review_points$LocationID
)

###############################################################################
# 13. CREATE 3 X 3 REVIEW PAGES
###############################################################################

page_membership <- ceiling(
  seq_along(review_panel_list) /
    panels_per_page
)

review_panel_groups <- split(
  review_panel_list,
  page_membership
)

spatial_match_review_pages <- imap(
  review_panel_groups,
  function(panel_group, page_number) {
    
    wrap_plots(
      panel_group,
      ncol = page_ncol,
      nrow = page_nrow
    ) +
      plot_annotation(
        title = paste0(
          "Spatial Match Review — Page ",
          page_number
        ),
        
        subtitle = paste0(
          "Previous UID values are from ",
          format(
            previous_uid_date,
            "%B %d, %Y"
          ),
          "."
        ),
        
        theme = theme(
          plot.title = element_text(
            face = "bold",
            size = 17
          ),
          
          plot.subtitle = element_text(
            size = 11
          )
        )
      )
  }
)

###############################################################################
# 14. SAVE ONE MULTI-PAGE PDF
###############################################################################

review_pdf_path <- file.path(
  plot_output_directory,
  paste0("OysterReefID_spatial_match_review_", Sys.Date(), ".pdf")
)

pdf(
  review_pdf_path,
  width = 18,
  height = 18,
  onefile = TRUE
)

walk(
  spatial_match_review_pages,
  print
)

dev.off()
