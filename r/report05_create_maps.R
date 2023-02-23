### Multiregional Multispecies Mesocarnivore Monitoring (MMMM)
### Jordan Heiman, Jessie Golding, Jody Tucker
## Date: 02/02/2022

## Function purpose: Create maps for report

#################################### Intro #####################################

# Name: report05_create_maps
# Description:  Function to create maps for embedding in report

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2/2/2022

################################# Arguments ####################################

# sf_obj:
#       An sf object of the cleaned data, use the object returned from the 
#       format spatial data function
# tl_sf:
#       An sf object of all the track logs that were loaded, use the object
#       returned from the format track logs function
# targets: 
#       List of the target species, this can be pulled from the output of the 
#       format spatial data function
# rd_shp:
#       File name of the ranger district map shape file as a character string, 
#       include full file path

################################# Output #######################################

# map_lst
#       A named list of map plots, one for each ranger district 
# JPEG maps
#       A jpeg of each map is saved to data/temp_data as a temporary holding

################################################################################
## Function

report05_create_maps <- function(sf_obj,
                                 tl_sf,
                                 targets,
                                 rd_shp = here("data", "raw_data", "usfs_ranger_district_map.shp")){
  
  # Ask user if they want to limit the date range of the data
  limit_date_tf <- showQuestion(title = "Limit Date Range",
                                message = "Would you like to limit the date range of the data shown in the reports?",
                                ok = "Yes", cancel = "No")
  
  # If the user does want to limit dates, get the range in a series of prompts
  if (limit_date_tf == TRUE){
    
    # Ask for the starting date of the range desired
    lower_date <- showPrompt(title = "Start Date",
                             message = "Please enter the starting date to limit the report data in the format MM/DD/YYYY")
    
    # Error handling if date is not entered properly
    if (is.null(lower_date) | is.na(mdy(lower_date, quiet = TRUE))){
      lower_date <- showPrompt(title = "Date Error",
                               message = "You must provide a starting date in the format MM/DD/YYYY")
    }
    
    # Format the date provided as a date
    lower_date <- mdy(lower_date)
    
    # Ask for the ending date of the range desired
    upper_date <- showPrompt(title = "End Date",
                             message = "Please enter the ending date to limit the report data in the format MM/DD/YYYY")
    
    # Error handling if date is not entered properly
    if (is.null(upper_date) | is.na(mdy(upper_date, quiet = TRUE))){
      upper_date <- showPrompt(title = "Date Error",
                               message = "You must provide a starting date in the format MM/DD/YYYY")
    }
    
    # Format the date provided as a date
    upper_date <- mdy(upper_date)
    
    # Error handling if dates were entered in wrong order
    if (lower_date > upper_date){
      showDialog(title = "Date Error",
                 message = "Your starting date was after your ending date, data will not be limited to a specific date range")
    }
  }
  
  # If a date range is not selected, set the range to the minimum and maximum 
  # date in the data
  if (limit_date_tf == FALSE | get0("lower_date", ifnotfound = 1) > get0("upper_date", ifnotfound = 0)){
    lower_date <- min(sf_obj$survey_start_date, tl_sf$survey_start_date, na.rm = TRUE)
    upper_date <- max(sf_obj$survey_start_date, tl_sf$survey_start_date, na.rm = TRUE)
  }
  
  # Limit the data to be within date range, track log sf object must be handled 
  # in an if/else statement in case it is an empty object that does not have all 
  # the columns that will be required later
  sf_obj_dates <- sf_obj[between(survey_start_date, lower_date, upper_date), ]
  if (nrow(tl_sf) > 0){
    tl_sf_dates <- filter(tl_sf, between(survey_start_date, lower_date, upper_date))
  }else {
    tl_sf_dates <- as.data.table(tl_sf)[, ":=" (forest_name = "",
                                                ranger_district = "")]
  }
  
  # Read in the ranger district map for outlines of each ranger district 
  rd_sf <- read_sf(rd_shp) %>% 
    mutate(DISTRICTNA = gsub("/", "-", DISTRICTNA))
  
  # The targets detected variable needs to be a factor for mapping
  sf_obj_dates$targets_detect <- as.factor(sf_obj_dates$targets_detect)
  
  # Create a table of all the ranger districts and forests from the detection 
  # data and the track logs
  rd_forest_tbl <- rbind(sf_obj_dates[, c("forest_name", "ranger_district")], 
                         tl_sf_dates[, c("forest_name", "ranger_district")], 
                         fill = TRUE)
  rd_forest_tbl1.1 <- unique(rd_forest_tbl[, geometry := NULL
                                           ][!is.na(forest_name) & !is.na(ranger_district), ])
  setorder(rd_forest_tbl1.1, forest_name, ranger_district)
  
  # Set up map caption
  map_cap <- str_wrap(paste("Target species include:", paste(targets, collapse = ", ")))

  # Create an empty ranger district map list to fill
  rd_map_lst <- list()
  
  # For each ranger district detection table, create a map and add it to a list of maps
  for (i in 1:nrow(rd_forest_tbl1.1)){
    
    # Set up objects to hold the ranger district and forest name and create the map title
    rd_name <- rd_forest_tbl1.1[i, ranger_district]
    forest_name <- rd_forest_tbl1.1[i, forest_name]
    map_title <- paste(rd_name, "Survey Locations")
    
    # Extract the ranger districts detections and return data.table to an sf object, 
    # transform to match base map (WGS84 lat/long)
    rd_detect_sf <- st_as_sf(sf_obj_dates[forest_name == forest_name & ranger_district == rd_name, ]) %>%
      st_transform(crs = 4326) 
    
    # Extract the track logs for track surveys in the ranger district
    if (nrow(tl_sf_dates) > 0){
      rd_tl_sf <- filter(tl_sf_dates, forest_name == forest_name & ranger_district == rd_name) %>% 
        st_transform(crs = 4326)
    }else{
      rd_tl_sf <- st_sf(st_sfc())
    }
    
    # Set up a layer that is just the ranger district boundary and transform it
    # to match base map(WGS84 lat/long)
    rd_bound <- filter(rd_sf, DISTRICTNA == rd_name & FORESTNAME == forest_name) %>% 
      st_transform(crs = 4326)
    
    # Set up a bounding box for the ranger district boundary
    rd_extent <- st_bbox(rd_bound)
    
    # Add buffer to each edge of the ranger district
    rd_extent[[1]] <- (rd_extent[[1]] - .025)
    rd_extent[[2]] <- (rd_extent[[2]] - .01)
    rd_extent[[3]] <- (rd_extent[[3]] + .025)
    rd_extent[[4]] <- (rd_extent[[4]] + .01)
    
    # Set up a bounding box for the track logs, if there are not track logs for 
    # this ranger district, the bounding box will be all NA values
    tl_extent <- st_bbox(rd_tl_sf)
    
    # Add a buffer to the bounding box
    tl_extent[[1]] <- (tl_extent[[1]] - .025)
    tl_extent[[2]] <- (tl_extent[[2]] - .01)
    tl_extent[[3]] <- (tl_extent[[3]] + .025)
    tl_extent[[4]] <- (tl_extent[[4]] + .01)
    
    # Compare the values of the 2 bounding boxes and use the bigger value for each
    map_extent <- c(
      min(rd_extent[[1]], tl_extent[[1]], na.rm = TRUE),
      min(rd_extent[[2]], tl_extent[[2]], na.rm = TRUE),
      max(rd_extent[[3]], tl_extent[[3]], na.rm = TRUE),
      max(rd_extent[[4]], tl_extent[[4]], na.rm = TRUE)
    ) 
    
    names(map_extent) <- names(rd_extent)
    
    # Get a base map for the ranger district
    basemap <- get_map(as.numeric(map_extent), maptype = "terain-background")
    
    # Create a 'mask' for areas outside of the ranger district by creating a spatial 
    # polygon the size of the whole base map then using erase to remove the ranger 
    # district polygon from it
    poly_mask <- as_Spatial(st_make_valid(rd_bound))
    bb <- unlist(map_extent)
    mypoly <- Polygon(cbind(bb[c(1,3,3,1)], bb[c(2,2,4,4)]))
    mypolys <- Polygons(list(mypoly), ID = "A")
    sp <- SpatialPolygons(list(mypolys),
                          proj4string = CRS(proj4string(poly_mask)))
    sp_diff <- erase(sp, poly_mask)
    
    # For labeling survey unit centroids, create a table with the unit centroid 
    # based on survey points
    units_sf <- st_as_sf(rd_detect_sf[, c("project_name", "unit_name", "geometry")]) %>% 
      cbind(st_coordinates(rd_detect_sf))
    
    # Do the same but for the linear surveys, and turn it into a table that can 
    # be added to the other unit centroids
    if (nrow(rd_tl_sf) > 0){
    
      linear_cent <- st_centroid(rd_tl_sf)
      
      linear_cent2.0 <- cbind(linear_cent[, c("project_name", "unit_name", "geometry")], 
                              st_coordinates(linear_cent))
          
      # Combine the linear and point unit labels
      units_tbl <- rbind(linear_cent2.0, units_sf) %>% 
        as.data.table()
    
    }else {
      
      units_tbl <- as.data.table(units_sf)
      
    }
    
    # Turn the centroids sf into a data table for map labeling
    units_tbl2 <- unique(units_tbl[, ":=" (x_cent = mean(X), y_cent = mean(Y)), by = c("project_name", "unit_name")
                                   ][, c("X", "Y", "geometry") := NULL])
    
    # Create the map for if there are target species
    rd_map_lst[[i]] <- ggmap(basemap, extent  = "normal") +
      geom_polypath(sp_diff, 
                    inherit.aes = FALSE, 
                    mapping = aes(x = long, y = lat, group = group),
                    fill = "cornsilk1",
                    alpha = 0.4) +
      {if (nrow(rd_tl_sf) > 0) geom_sf(rd_tl_sf, 
                                       inherit.aes = FALSE,
                                       mapping = aes(geometry = geometry, color = targets_detect))} +
      geom_sf(rd_detect_sf, 
              inherit.aes = FALSE,
              mapping = aes(geometry = geometry, color = targets_detect)) +
      geom_label_repel(units_tbl2,
                       mapping = aes(x_cent, y_cent, label = unit_name, fontface = "bold"),
                       min.segment.length = 0,
                       fill = alpha(c("white"), 0.75),
                       size = 3,
                       nudge_y = .02) +
      scale_color_manual(values = c("#1F78B4", "#E31A1C"),
                         name = "Target Species Detected",
                         limits = c("Yes", "No")) +
      theme_void() +
      labs(title = map_title,
           caption = map_cap) +
      theme(legend.background = element_rect(fill = NA, linetype = "blank"),
            legend.key = element_rect(fill = NA, linetype = "blank"),
            legend.margin = margin(1, 4, 1, 4),
            legend.title = element_text(hjust = .5, face = "bold"), 
            legend.position = "bottom",
            plot.title = element_text(hjust = .5, vjust = .5, face = "bold", size = 16),
            plot.caption = element_text(hjust = .5)) 
    
    # Name the map in the list with the forest and ranger district
    names(rd_map_lst)[[i]] <- paste(forest_name, rd_name, sep = "; ")
    
  }
  
  # Create a list of the file names based on the list of maps 
  filenames <- paste0(gsub(" |; ", "_", names(rd_map_lst)), ".jpeg")
  
  # Save a jpeg version of each map
  lapply(seq_along(rd_map_lst), 
         function(x) ggsave(here("data", "temp_data", filenames[[x]]), rd_map_lst[[x]], height = 8, width = 8, units = "in"))
  
  # Add the date range to the end of the map list variable so it can be used by other functions
  rd_map_lst$date_range <- c(lower_date, upper_date)
  
  return(rd_map_lst)
  
}
