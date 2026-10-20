###############################################################################
# CROSS-VERSION POLYGON EQUIVALENCE REVIEW PLOTS
#
# Required objects from UniversalReefID workflow:
#
#   cross_version_mapping_review
#   reef_registry
#   sampled_reef_matches
#   pts_fwc_final_updated
#
# Optional:
#   analysis_crs
#   cross_version_tolerance_m
###############################################################################


###############################################################################
# 1. PACKAGES
###############################################################################

library(sf)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(patchwork)

###############################################################################
# 2. PLOT SETTINGS
###############################################################################

plot_crs <- if (
  exists("analysis_crs")
) {
  analysis_crs
} else {
  st_crs(6439)
}


equivalence_buffer_m <- if (
  exists("cross_version_tolerance_m")
) {
  cross_version_tolerance_m
} else {
  14.21
}


panels_per_page <- 9
page_ncol <- 3
page_nrow <- 3


plot_output_directory <-
  "output/cross_version_equivalence_review"

dir.create(
  plot_output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)


###############################################################################
# 3. VALIDATE REVIEW DATA
###############################################################################

if (nrow(cross_version_mapping_review) == 0) {
  
  stop(
    "cross_version_mapping_review contains no cross-version ",
    "polygon-equivalence cases requiring review."
  )
}


###############################################################################
# 4. PREPARE REVIEW TABLE
###############################################################################

equivalence_review <- cross_version_mapping_review %>%
  arrange(
    older_layer_order,
    older_reef_key,
    equivalence_rank
  ) %>%
  mutate(
    candidate_label = paste0(
      newer_reef_key,
      if_else(
        selected_equivalence,
        " [SELECTED]",
        ""
      )
    ),
    
    overlap_percent =
      100 * overlap_of_smaller_polygon,
    
    buffered_overlap_percent =
      100 * buffered_overlap_ratio
  )


# Create one panel per older reef requiring review.
equivalence_review_cases <- equivalence_review %>%
  distinct(
    older_reef_key,
    older_layer,
    older_layer_order
  ) %>%
  arrange(
    older_layer_order,
    older_reef_key
  ) %>%
  mutate(
    review_id = row_number()
  )


equivalence_review <- equivalence_review %>%
  left_join(
    equivalence_review_cases %>%
      select(
        older_reef_key,
        review_id
      ),
    by = "older_reef_key"
  )


###############################################################################
# 5. EXTRACT OLDER POLYGONS
###############################################################################

older_polygons <- reef_registry %>%
  st_transform(plot_crs) %>%
  inner_join(
    equivalence_review_cases %>%
      select(
        older_reef_key,
        review_id
      ),
    by = c(
      "reef_key" =
        "older_reef_key"
    )
  ) %>%
  rename(
    older_reef_key =
      reef_key
  )


###############################################################################
# 6. EXTRACT NEWER CANDIDATE POLYGONS
###############################################################################

candidate_polygon_attributes <- equivalence_review %>%
  select(
    review_id,
    older_reef_key,
    newer_reef_key,
    candidate_label,
    equivalence_rank,
    selected_equivalence,
    equivalence_type,
    polygon_distance_m,
    intersection_area_m2,
    overlap_of_smaller_polygon,
    overlap_percent,
    buffered_overlap_ratio,
    buffered_overlap_percent
  )


newer_polygons <- reef_registry %>%
  st_transform(plot_crs) %>%
  rename(
    newer_reef_key =
      reef_key
  ) %>%
  inner_join(
    candidate_polygon_attributes,
    by = "newer_reef_key"
  )


###############################################################################
# 7. CREATE SAMPLE-POINT LOOKUP
###############################################################################

point_geometry_lookup <- pts_fwc_final_updated %>%
  st_transform(plot_crs) %>%
  select(
    LocationID,
    UniversalReefID,
    geometry
  )


sampled_point_reef_links <- sampled_reef_matches %>%
  select(
    LocationID,
    reef_key,
    point_match_type
  ) %>%
  distinct()


###############################################################################
# 8. IDENTIFY SAMPLE POINTS ASSOCIATED WITH REVIEW REEFS
###############################################################################

review_reef_keys <- bind_rows(
  
  equivalence_review_cases %>%
    transmute(
      review_id,
      reef_key =
        older_reef_key,
      reef_role =
        "older"
    ),
  
  equivalence_review %>%
    transmute(
      review_id,
      reef_key =
        newer_reef_key,
      reef_role =
        "candidate"
    )
  
) %>%
  distinct()


review_sample_points <- sampled_point_reef_links %>%
  inner_join(
    review_reef_keys,
    by = "reef_key"
  ) %>%
  left_join(
    point_geometry_lookup,
    by = "LocationID"
  ) %>%
  filter(
    !is.na(geometry)
  ) %>%
  st_as_sf(
    sf_column_name = "geometry",
    crs = plot_crs
  )


# Add candidate labels to points matched to newer reefs.
review_sample_points <- review_sample_points %>%
  left_join(
    equivalence_review %>%
      select(
        review_id,
        newer_reef_key,
        candidate_label
      ) %>%
      distinct(),
    by = c(
      "review_id",
      "reef_key" =
        "newer_reef_key"
    )
  )


###############################################################################
# 9. CREATE PAIRWISE OVERLAP GEOMETRIES
###############################################################################

build_overlap_geometries <- function(
    older_polygons,
    newer_polygons
) {
  
  review_ids <- sort(
    unique(
      newer_polygons$review_id
    )
  )
  
  overlap_list <- map(
    review_ids,
    function(current_review_id) {
      
      current_older <- older_polygons %>%
        filter(
          review_id ==
            current_review_id
        )
      
      current_candidates <- newer_polygons %>%
        filter(
          review_id ==
            current_review_id
        )
      
      if (
        nrow(current_older) == 0 ||
        nrow(current_candidates) == 0
      ) {
        return(NULL)
      }
      
      candidate_overlaps <- map(
        seq_len(
          nrow(current_candidates)
        ),
        function(i) {
          
          candidate <- current_candidates[i, ]
          
          overlap <- suppressWarnings(
            st_intersection(
              st_geometry(
                current_older
              ),
              st_geometry(
                candidate
              )
            )
          )
          
          if (
            length(overlap) == 0 ||
            all(
              st_is_empty(overlap)
            )
          ) {
            return(NULL)
          }
          
          overlap <- overlap[
            !st_is_empty(overlap)
          ]
          
          if (length(overlap) == 0) {
            return(NULL)
          }
          
          st_sf(
            review_id =
              current_review_id,
            
            newer_reef_key =
              candidate$newer_reef_key[[1]],
            
            candidate_label =
              candidate$candidate_label[[1]],
            
            geometry =
              overlap,
            
            crs =
              plot_crs
          )
        }
      ) %>%
        compact()
      
      if (length(candidate_overlaps) == 0) {
        return(NULL)
      }
      
      do.call(
        rbind,
        candidate_overlaps
      )
    }
  ) %>%
    compact()
  
  
  if (length(overlap_list) == 0) {
    return(NULL)
  }
  
  
  do.call(
    rbind,
    overlap_list
  )
}


equivalence_overlap_polygons <-
  build_overlap_geometries(
    older_polygons =
      older_polygons,
    
    newer_polygons =
      newer_polygons
  )


###############################################################################
# 10. CREATE POLYGON-TO-POLYGON DISTANCE LINES
###############################################################################

build_polygon_distance_lines <- function(
    older_polygons,
    newer_polygons
) {
  
  output <- map(
    seq_len(
      nrow(newer_polygons)
    ),
    function(i) {
      
      candidate <-
        newer_polygons[i, ]
      
      current_review_id <-
        candidate$review_id[[1]]
      
      older <- older_polygons %>%
        filter(
          review_id ==
            current_review_id
        )
      
      if (nrow(older) == 0) {
        return(NULL)
      }
      
      distance_m <- as.numeric(
        st_distance(
          st_geometry(older),
          st_geometry(candidate)
        )
      )
      
      # No useful distance line is needed when polygons already overlap.
      if (
        !is.finite(distance_m) ||
        distance_m <= 0.01
      ) {
        return(NULL)
      }
      
      nearest_line <- st_nearest_points(
        st_geometry(older),
        st_geometry(candidate)
      )
      
      st_sf(
        review_id =
          current_review_id,
        
        newer_reef_key =
          candidate$newer_reef_key[[1]],
        
        candidate_label =
          candidate$candidate_label[[1]],
        
        distance_m =
          distance_m,
        
        geometry =
          nearest_line,
        
        crs =
          plot_crs
      )
    }
  ) %>%
    compact()
  
  
  if (length(output) == 0) {
    return(NULL)
  }
  
  
  do.call(
    rbind,
    output
  )
}


equivalence_distance_lines <-
  build_polygon_distance_lines(
    older_polygons =
      older_polygons,
    
    newer_polygons =
      newer_polygons
  )


###############################################################################
# 11. BUILD COMPACT METRIC TEXT FOR EACH REVIEW CASE
###############################################################################

equivalence_metric_text <- equivalence_review %>%
  arrange(
    review_id,
    equivalence_rank
  ) %>%
  mutate(
    metric_line = paste0(
      "Rank ",
      equivalence_rank,
      ": ",
      newer_reef_key,
      if_else(
        selected_equivalence,
        " [SELECTED]",
        ""
      ),
      " | ",
      equivalence_type,
      " | distance=",
      format(
        round(
          polygon_distance_m,
          2
        ),
        nsmall = 2
      ),
      " m",
      " | overlap=",
      format(
        round(
          overlap_percent,
          1
        ),
        nsmall = 1
      ),
      "%",
      " | buffered overlap=",
      format(
        round(
          buffered_overlap_percent,
          1
        ),
        nsmall = 1
      ),
      "%"
    )
  ) %>%
  group_by(
    review_id
  ) %>%
  summarise(
    metric_text = paste(
      metric_line,
      collapse = "\n"
    ),
    
    .groups = "drop"
  )


###############################################################################
# 12. CREATE ONE CROSS-VERSION REVIEW PANEL
###############################################################################

make_equivalence_review_panel <- function(
    current_review_id
) {
  
  panel_older <- older_polygons %>%
    filter(
      review_id ==
        current_review_id
    )
  
  
  panel_candidates <- newer_polygons %>%
    filter(
      review_id ==
        current_review_id
    )
  
  
  panel_points <- review_sample_points %>%
    filter(
      review_id ==
        current_review_id
    )
  
  
  panel_overlap <- if (
    is.null(equivalence_overlap_polygons)
  ) {
    
    NULL
    
  } else {
    
    equivalence_overlap_polygons %>%
      filter(
        review_id ==
          current_review_id
      )
  }
  
  
  panel_distance_lines <- if (
    is.null(equivalence_distance_lines)
  ) {
    
    NULL
    
  } else {
    
    equivalence_distance_lines %>%
      filter(
        review_id ==
          current_review_id
      )
  }
  
  
  panel_metrics <- equivalence_metric_text %>%
    filter(
      review_id ==
        current_review_id
    )
  
  
  panel_review <- equivalence_review %>%
    filter(
      review_id ==
        current_review_id
    )
  
  
  selected_candidate <- panel_review %>%
    filter(
      selected_equivalence
    ) %>%
    pull(
      newer_reef_key
    )
  
  
  if (length(selected_candidate) == 0) {
    
    selected_candidate <-
      "None"
    
  } else {
    
    selected_candidate <-
      selected_candidate[[1]]
  }
  
  
  #############################################################################
  # TOLERANCE BUFFER AROUND OLDER POLYGON
  #############################################################################
  
  panel_tolerance_buffer <- st_buffer(
    panel_older,
    dist = equivalence_buffer_m
  )
  
  
  #############################################################################
  # CANDIDATE COLOR SCALE
  #############################################################################
  
  candidate_levels <- panel_candidates %>%
    arrange(
      equivalence_rank
    ) %>%
    pull(
      candidate_label
    ) %>%
    unique()
  
  
  candidate_colors <- scales::hue_pal()(
    length(
      candidate_levels
    )
  )
  
  names(candidate_colors) <-
    candidate_levels
  
  
  #############################################################################
  # PANEL EXTENT
  #############################################################################
  
  extent_geometry <- c(
    st_geometry(
      panel_tolerance_buffer
    ),
    st_geometry(
      panel_candidates
    )
  )
  
  
  if (nrow(panel_points) > 0) {
    
    extent_geometry <- c(
      extent_geometry,
      st_geometry(
        panel_points
      )
    )
  }
  
  
  panel_bbox <- st_bbox(
    extent_geometry
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
    equivalence_buffer_m * 2,
    na.rm = TRUE
  )
  
  
  plot_padding <- max(
    equivalence_buffer_m * 0.30,
    maximum_range * 0.08
  )
  
  
  #############################################################################
  # TITLE AND SUBTITLE
  #############################################################################
  
  panel_title <- paste0(
    "Older reef: ",
    panel_older$older_reef_key[[1]]
  )
  
  
  panel_subtitle <- paste0(
    nrow(panel_candidates),
    " candidate newer reef",
    if_else(
      nrow(panel_candidates) == 1,
      "",
      "s"
    ),
    " | Auto-selected: ",
    selected_candidate
  )
  
  
  #############################################################################
  # PLOT
  #############################################################################
  
  panel_plot <- ggplot() +
    
    # 14.21 m cross-version tolerance around the older polygon
    geom_sf(
      data =
        panel_tolerance_buffer,
      fill = NA,
      linetype = "dashed",
      linewidth = 0.6
    ) +
    
    # Older polygon
    geom_sf(
      data =
        panel_older,
      fill = "grey80",
      color = "black",
      alpha = 0.5,
      linewidth = 0.8
    ) +
    
    # All candidate newer polygons
    geom_sf(
      data =
        panel_candidates,
      aes(
        fill =
          candidate_label
      ),
      alpha = 0.28,
      linewidth = 0.5,
      show.legend = TRUE
    ) +
    
    # Darker area indicates actual polygon overlap
    {
      if (
        !is.null(panel_overlap) &&
        nrow(panel_overlap) > 0
      ) {
        
        geom_sf(
          data =
            panel_overlap,
          aes(
            fill =
              candidate_label
          ),
          alpha = 0.65,
          color = NA,
          show.legend = FALSE
        )
        
      } else {
        
        geom_blank()
      }
    } +
    
    # Emphasize the automatically selected candidate
    {
      selected_polygon <- panel_candidates %>%
        filter(
          selected_equivalence
        )
      
      if (nrow(selected_polygon) > 0) {
        
        geom_sf(
          data =
            selected_polygon,
          fill = NA,
          color = "black",
          linewidth = 1.25,
          show.legend = FALSE
        )
        
      } else {
        
        geom_blank()
      }
    } +
    
    # Polygon-to-polygon distance for separated candidates
    {
      if (
        !is.null(panel_distance_lines) &&
        nrow(panel_distance_lines) > 0
      ) {
        
        geom_sf(
          data =
            panel_distance_lines,
          aes(
            color =
              candidate_label
          ),
          linetype = "dotted",
          linewidth = 0.8,
          show.legend = FALSE
        )
        
      } else {
        
        geom_blank()
      }
    } +
    
    # Sample points associated with older reef
    {
      older_points <- panel_points %>%
        filter(
          reef_role ==
            "older"
        )
      
      if (nrow(older_points) > 0) {
        
        geom_sf(
          data =
            older_points,
          shape = 24,
          fill = "white",
          color = "black",
          size = 2.8,
          stroke = 0.7,
          show.legend = FALSE
        )
        
      } else {
        
        geom_blank()
      }
    } +
    
    # Sample points associated with candidate newer reefs
    {
      candidate_points <- panel_points %>%
        filter(
          reef_role ==
            "candidate",
          !is.na(candidate_label)
        )
      
      if (nrow(candidate_points) > 0) {
        
        geom_sf(
          data =
            candidate_points,
          aes(
            fill =
              candidate_label
          ),
          shape = 21,
          color = "black",
          stroke = 0.35,
          size = 2.6,
          show.legend = FALSE
        )
        
      } else {
        
        geom_blank()
      }
    } +
    
    scale_fill_manual(
      values =
        candidate_colors,
      breaks =
        candidate_levels,
      drop = FALSE
    ) +
    
    scale_color_manual(
      values =
        candidate_colors,
      guide = "none"
    ) +
    
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
    
    labs(
      title =
        panel_title,
      
      subtitle =
        panel_subtitle,
      
      fill =
        "Newer reef candidates",
      
      caption =
        panel_metrics$metric_text[[1]]
    ) +
    
    guides(
      fill = guide_legend(
        title =
          "Newer reef candidates",
        
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
        size = 10.5
      ),
      
      plot.subtitle = element_text(
        size = 8
      ),
      
      # Metrics appear below the map instead of covering spatial features.
      plot.caption = element_text(
        size = 6.2,
        hjust = 0,
        lineheight = 1.05
      ),
      
      axis.title =
        element_blank(),
      
      axis.text =
        element_text(
          size = 5.5
        ),
      
      panel.grid.minor =
        element_blank(),
      
      legend.position = c(
        0.98,
        0.98
      ),
      
      legend.justification = c(
        1,
        1
      ),
      
      legend.background = element_rect(
        fill = scales::alpha(
          "white",
          0.90
        ),
        color = "grey65",
        linewidth = 0.25
      ),
      
      legend.title = element_text(
        face = "bold",
        size = 7
      ),
      
      legend.text = element_text(
        size = 6.5
      ),
      
      legend.key.size = grid::unit(
        0.32,
        "cm"
      ),
      
      plot.margin = margin(
        7,
        7,
        7,
        7
      )
    )
  
  
  panel_plot
}

###############################################################################
# 13. CREATE ALL CROSS-VERSION REVIEW PANELS
###############################################################################

equivalence_review_panel_list <- map(
  equivalence_review_cases$review_id,
  make_equivalence_review_panel
)


names(
  equivalence_review_panel_list
) <- equivalence_review_cases$
  older_reef_key

###############################################################################
# 14. CREATE 3 X 3 REVIEW PAGES
###############################################################################

page_membership <- ceiling(
  seq_along(
    equivalence_review_panel_list
  ) /
    panels_per_page
)


equivalence_panel_groups <- split(
  equivalence_review_panel_list,
  page_membership
)


cross_version_review_pages <- imap(
  equivalence_panel_groups,
  function(
    panel_group,
    page_number
  ) {
    
    wrap_plots(
      panel_group,
      ncol = page_ncol,
      nrow = page_nrow
    ) +
      
      plot_annotation(
        title = paste0(
          "Cross-Version Polygon Equivalence Review — Page ",
          page_number
        ),
        
        subtitle = paste0(
          "Grey = older reef | Colored = candidate newer reefs | ",
          "Darker fill = direct polygon overlap | ",
          "Dashed outline = ",
          equivalence_buffer_m,
          " m equivalence tolerance"
        ),
        
        theme = theme(
          plot.title = element_text(
            face = "bold",
            size = 17
          ),
          
          plot.subtitle = element_text(
            size = 10
          )
        )
      )
  }
)

###############################################################################
# 15. EXPORT MULTI-PAGE PDF
###############################################################################

cross_version_review_pdf <- file.path(
  plot_output_directory,
  "cross_version_polygon_equivalence_review.pdf"
)


pdf(
  cross_version_review_pdf,
  width = 18,
  height = 18,
  onefile = TRUE
)


walk(
  cross_version_review_pages,
  print
)


dev.off()
