### Multiregional Multispecies Mesocarnivore Monitoring (MMMM)
### Jordan Heiman, Jessie Golding, Jody Tucker
## Date: 02/02/2022

## Function purpose: Format track logs

#################################### Intro #####################################

# Name: report04a_format_track_logs
# Description:  Function to set up track data for maps and mileage calculations

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2/2/2022

################################# Arguments ####################################

# tbl:
#       Data table as cleaned by previous clean data function 
# rd_shp:
#       File name of the ranger district map shape file as a character string, 
#       include full file path
# final_crs:
#       Character string of the final coordinate reference system (CRS) desired; 
#       options include = "NAD83" (default), "WGS84", "NAD27", "Mercator", 
#                       "UTMZone##" (replace ## with the  two digit zone number)
#       *Warning to only us UTM Zone if all the data is within one zone!*

################################# Output #######################################

# sf obj:
#       An sf object of all the track logs that were loaded and have matching 
#       data in the cleaned table; this will be a blank sf object if no track 
#       logs are selected to load

################################################################################
## Function

report04a_format_track_logs <- function(tbl,  
                                        targets,
                                        rd_shp = here("data", "raw_data", "usfs_ranger_district_map.shp"),
                                        final_crs = "NAD83"){
  
  # First, if there are track surveys, prompt the user to see if there are any 
  # track logs to add to the report data
  if ("track survey" %in% tbl$survey_type | "back track" %in% tbl$survey_type){
    
    track_log_tf <- showQuestion(title = "Optional Track Logs",
                                 message = "Would you like to add track logs to the report? 
                                 \nIf yes, the next prompt will ask for the location of all track logs, they must be all in one folder with no other shapefiles.
                                 \n If track log corresponds to data in loaded CSV the file names must match site names in data CSV.",
                                 ok = "Yes",
                                 cancel = "No")
    
    # If the user would like to add track logs, get the location of them
    if(track_log_tf == TRUE){
      
      tl_loc <- selectDirectory(caption = "Select folder containing tracklogs")
      
    }else{
      
      # Returns an empty sf object if the user does not want to add track logs
      return(st_sf(st_sfc()))
      
    }
    
  }else{
    
    # Returns an empty sf object  if there are not track surveys in the data
    return(st_sf(st_sfc()))
    
  }
  
  # Read in all track logs as a list of sf objects, each named as the name of 
  # the shape file it came from
  tl_files <- list.files(tl_loc, pattern = "\\.shp$", full.names = TRUE)
  tl_sf_lst <- lapply(tl_files, read_sf, dim = "XY", type = 5)
  names(tl_sf_lst) <- substr(basename(tl_files), 1, nchar(basename(tl_files))-4)
  
  # This removes the 'z' part of the geometry which will throw an error in the
  # next step if left in and is unnecessary in the data set anyway
  tl_sf_lst <- st_zm(tl_sf_lst)
  
  # Merge the list of track SF objects into one data.table and remove all columns 
  # except the name and geometry
  tl_dt <- rbindlist(tl_sf_lst, idcol = "survey_name", fill = TRUE
                     )[, c("survey_name", "geometry")]

  # Remove any flagged data from the table and limit data to just linear surveys
  if ("flag_reason" %in% colnames(tbl)){
    
    tbl1.0 <- tbl[survey_cat == "linear" & flag_reason == "",
                  ][, flag_reason := NULL]
    
  }else{
    
    tbl1.0 <- tbl[survey_cat == "linear",]
    
  }
  
  # Dates and times will not be necessary in the maps so remove those columns 
  # and any duplicates once those columns are removed
  tbl1.1 <- unique(tbl1.0[, c("project_name", "survey_name", "survey_id", "unit_name", "survey_type", "survey_start_date", 
                              "survey_end_date", "detection_type", "genus", "species")])
  
  # Create a column for the full Latin species name
  tbl1.2 <- tbl1.1[!is.na(detection_type), genus_species := str_to_sentence(paste(genus, species, sep = " "))
                   ][is.na(detection_type), genus_species := "None"]
  
  # Set up the target species list for use as a regular expression
  target_regex <- paste(targets, collapse = "|")
  
  # Merge detections for each survey site into a list in a single column and 
  # add a column for if a target was detected
  tbl1.3 <- unique(tbl1.2[, detections := toString(genus_species), by = c("project_name", "survey_name", "survey_id", 
                                                                          "unit_name", "survey_type",
                                                                          "survey_start_date",
                                                                          "survey_end_date",
                                                                          "detection_type")
                          ][, c("genus", "species", "genus_species") := NULL
                            ][grepl(target_regex, detections, ignore.case = TRUE), targets_detect := "Yes"
                              ][!grepl(target_regex, detections, ignore.case = TRUE), targets_detect := "No"])
  
  # Join this table with the data from the cleaned survey track log table
  tl_dt2 <- merge(tl_dt, tbl1.3, all.x = TRUE)
  
  # Set up table of surveys that do not have data in the cleaned data table
  na_tbl <- tl_dt2[!complete.cases(tl_dt2[, c("project_name", "survey_name", "survey_id")]), ]

  if (nrow(na_tbl) > 0){
    
    # Warn the user that some data track logs do not have corresponding data in 
    # the loaded data CSV
    showDialog(title = "Track Logs Flagged", 
               message = "Some track logs do not have matching data in the cleaned data table. A CSV will be saved to the main folder with a list of these")
    
    # Save a CSV listing the shape files that did not have matching data
    err_csvs <- na_tbl[, c("project_name", "survey_name")
                       ][, shape_file := paste0(survey_name, ".shp")
                         ][, survey_name := NULL]
    
    write.csv(err_csvs, here("flagged_tracklogs.csv"), row.names = FALSE)
    
  }
  
  # Get a list of surveys that have no track log provided and save it for the user
  missing_tl <- tbl1.3[!(survey_id %in% tl_dt2$survey_id), ]
  
  if (nrow(missing_tl) > 0){
    
    # Warn the user that some data do not have corresponding track logs in the 
    # provided folder
    showDialog(title = "Track Logs Missing", 
               message = "Some track surveys do not have matching track logs in the provided folder. A CSV will be saved to the main folder with a list of these")
    
    # Save a CSV listing the shape files that did not have matching data
    write.csv(missing_tl, here("missing_track_logs.csv"), row.names = FALSE)
    
  }
  
  # Remove the track logs that do not relate to data in the cleaned csv
  tl_dt3 <- tl_dt2[complete.cases(tl_dt2[, c("survey_name", "survey_id")]), ]
  
  # Turn this back into an sf object and add a length column
  tl_sf <- st_as_sf(tl_dt3) %>% 
    mutate(survey_length_km = as.numeric(st_length(.))/1000) %>% 
    group_by(survey_id) %>% 
    mutate(total_survey_length_km = sum(survey_length_km), .keep = "unused") %>% 
    distinct() %>% 
    ungroup()
  
  # Handle projection consistency with other data
  # Set up a reference table for CRS and EPSG code options
  epsg_tbl <- data.table(crs_text = c("NAD83", "WGS84", "NAD27", "Mercator"),
                         crs_code = c(4269, 4326, 4267, 3857))
  
  # Get the correct EPSG code for the selected CRS
  if (grepl("UTMZone", final_crs, ignore.case = TRUE)){
    
    # If a UTM zone is provided, warn the user
    warning("UTM Zone selected as final Coordinate Reference System, this can only be used if all data falls into one zone!")
    
    # Set up the correct zone EPSG
    utm_zone <- substr(final_crs, 8, 9)
    crs_code <- 32600 + utm_zone
    
  }else if(final_crs %in% epsg_tbl$crs_text){ 
    
    # If final_crs is not UTM then use a table to retrieve the correct EPSG code 
    
    crs_code <- epsg_tbl[grepl(final_crs, crs_text), crs_code]
    
  }else {
    
    # Error if selected final_crs is not an option
    stop("final_crs not in list of options, see function script for options")
    
  }
  
  # Read in the ranger district map (for overlapping onto data) and transform it
  # to the final CRS projection
  rd_sf <- st_transform(read_sf(rd_shp), crs = crs_code) %>% 
    mutate(DISTRICTNA = gsub("/", "-", DISTRICTNA))
  
  # Fix some issues with intersecting polygons
  rd_sf1.1 <- st_make_valid(rd_sf)
  
  # Transform the track logs to the final projection
  tl_sf2 <- st_transform(tl_sf, crs = crs_code)
  
  # Join each site to the ranger district it is within
  tl_sf3 <- st_join(tl_sf2, rd_sf1.1)
  
  # Format the attribute table to only include necessary data
  # For faster processing, convert to a data.table first
  tl_sftbl <- as.data.table(tl_sf3)
  
  # Remove unnecessary columns
  tl_sftbl[, c("SHAPE_AREA", "SHAPE_LEN", "RANGERDIST", "FORESTNUMB", "DISTRICTNU", 
             "DISTRICTOR", "GIS_ACRES") := NULL]
  
  # Rename some columns to make more sense and make all column names lowercase
  setnames(tl_sftbl, colnames(tl_sftbl), tolower(colnames(tl_sftbl)))
  setnames(tl_sftbl, c("forestname", "districtna"), c("forest_name", "ranger_district"))
  
  # Reorder the columns
  setcolorder(tl_sftbl, c("project_name", "survey_id", "survey_name", "unit_name", 
                          "region", "forest_name", "ranger_district", "survey_type",
                          "survey_start_date", "survey_end_date", "total_survey_length_km", "geometry"))
  
  tl_final <- distinct(st_as_sf(tl_sftbl))
  
  return(tl_final)
  
}