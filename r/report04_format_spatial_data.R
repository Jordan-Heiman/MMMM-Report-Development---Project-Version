### Multiregional Multispecies Mesocarnivore Monitoring (MMMM)
### Jordan Heiman, Jessie Golding, Jody Tucker
## Date: 02/02/2022

## Function purpose: Format spatial data

#################################### Intro #####################################

# Name: report05_format_spatial_data
# Description:  Function to set formatting parameters for spatial data

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

# list
#       A list object containing 2 things:
#     sf_tbl
#           A data.table of the sf data for mapping
#     targets
#           A vector of the target species selected

################################################################################
## Function

report04_format_spatial_data <- function(tbl,   
                                         rd_shp = here("data", "raw_data", "usfs_ranger_district_map.shp"),
                                         final_datum = "WGS84"){
  
  # Set up a reference table for CRS and EPSG code options
  epsg_tbl <- data.table(datum = c("NAD83", "WGS84", "NAD27", "Mercator"),
                         crs_code = c(4269, 4326, 4267, 3857))
  
  # Get the correct EPSG code for the selected CRS
  if (grepl("UTMZone", final_datum, ignore.case = TRUE)){
    
    # If a UTM zone is provided, warn the user
    warning("UTM Zone selected as final Coordinate Reference System, this can only be used if all data falls into one zone!")
    
    # Set up the correct zone EPSG
    utm_zone <- substr(final_datum, 8, 9)
    final_crs <- 32600 + utm_zone
  
  }else if(final_datum %in% epsg_tbl$datum){ 
  
    # If final_datum is not UTM then use a table to retrieve the correct EPSG code 
    final_crs <- epsg_tbl[grepl(final_datum, datum), crs_code]
    
  }else {
    
    # Error if selected final_datum is not an option
    stop("final_datum not in list of options, see function script for options")
    
  }
  
  # Read in the ranger district map (for overlapping onto data) and transform it 
  # to the final CRS projection
  rd_sf <- st_transform(read_sf(rd_shp), crs = final_crs) %>% 
    mutate(DISTRICTNA = gsub("/", "-", DISTRICTNA))
  
  # Fix some issues with intersecting polygons
  rd_sf1.1 <- st_make_valid(rd_sf)
  
  # Working with only point data, remove any flagged data from the table
  if ("flag_reason" %in% colnames(tbl)){
    
    tbl1.0 <- copy(tbl)[survey_cat == "point" & flag_reason == "",
                        ][, flag_reason := NULL]
  
  }else{
    
    tbl1.0 <- tbl[survey_cat == "point",]
    
  }
  
  # Dates and times will not be necessary in the maps so remove those columns 
  # and any duplicates once those columns are removed
  tbl1.1 <- unique(tbl1.0[, c("detection_date", "detection_time", "detect_id") := NULL])
  
  # Create a column for the full Latin species name
  tbl1.2 <- tbl1.1[!is.na(detection_type), genus_species := str_to_sentence(paste(genus, species, sep = " "))
                   ][is.na(detection_type), genus_species := "None"]
                   
  # Create a list of possible species
  species_lst <- sort(unique(tbl1.2[genus_species != "None", genus_species])) 
  
  # Prompt the user for a list of target species based on the list of possible species  
  showDialog(title = "Target Species", message = "Please use the next dialog box to select any target species. Multiple species can be selected by holding 'ctrl'")
  target_lst <- select.list(species_lst, multiple = TRUE, 
                            title = "Please select target species", graphics = TRUE)
  
  # Set up the target species list for use as a regular expression
  target_regex <- paste(target_lst, collapse = "|")
  
  # Merge detections for each survey site into a list in a single column and 
  # add a column for if a target was detected
  tbl1.3 <- unique(tbl1.2[, detections := toString(genus_species), by = c("project_name", "easting_longitude", 
                                                                          "northing_latitude", "utm_zone", 
                                                                          "datum", "survey_name", "unit_name",
                                                                          "survey_type", "survey_start_date", 
                                                                          "survey_end_date", "detection_type")
                          ][, c("genus", "species", "genus_species") := NULL
                            ][grepl(target_regex, detections, ignore.case = TRUE), targets_detect := "Yes"
                              ][!grepl(target_regex, detections, ignore.case = TRUE), targets_detect := "No"])
  
  # Assign the correct crs code to each set of coordinates
  tbl1.3[!isInteger(easting_longitude) & !isInteger(northing_latitude) & datum == "wgs84", crs_code := 4326]
  tbl1.3[isInteger(easting_longitude) & isInteger(northing_latitude) & datum == "nad27", crs_code := 26700 + utm_zone]
  tbl1.3[isInteger(easting_longitude) & isInteger(northing_latitude) & datum == "nad83", crs_code := 32600 + utm_zone]
  
  # Split the data based on crs_code so that each can be transformed into the final crs code
  datum_data_lst <- split(tbl1.3, f = tbl1.3$crs_code)
  
  # Turn each table into an sf object with the correct coordinates
  datum_sf_lst <- lapply(seq_along(datum_data_lst), function(x) st_as_sf(datum_data_lst[[x]], 
                                                                        coords = c("easting_longitude", "northing_latitude"), 
                                                                        crs = unique(datum_data_lst[[x]]$crs_code)))
  
  # Transfer each table to the final datum/crs
  datum_sf_lst_trans <- lapply(datum_sf_lst, st_transform, crs = final_crs)
  
  # Merge all the sf object together, rbind() returns a data.table, remove the crs_code column and the datum column as all are now
  # in lat/long/NAD83 
  data_sf <- st_as_sf(rbindlist(datum_sf_lst_trans)[, c("crs_code", "datum") := NULL])
  
  # Join each site to the ranger district it is within
  data_sf2.0 <- st_join(data_sf, rd_sf1.1)
  
  if (nrow(filter(data_sf2.0, is.na(DISTRICTNA))) > 0){
    
    # For points that fell outside a ranger district, use a 500m buffer on the ranger 
    # district maps to give a ranger district and add them back to the table
    # First filter out those points that did not end up with a ranger district
    nonusfs_sf <- filter(data_sf2.0, is.na(DISTRICTNA))
    
    # Get rid of the empty columns (these will get replaced on the next line)
    nonusfs_sf2.0 <- nonusfs_sf[, c("project_name", "survey_id", "survey_name", "unit_name", 
                                    "utm_zone", "survey_type", "survey_start_date",
                                    "survey_end_date", "detection_type", "survey_cat", 
                                    "detections", "targets_detect", "geometry")]
    
    # Using a buffer of 500m find the ranger district to assign to the points
    nonusfs_sf3.0 <- st_join(nonusfs_sf2.0, rd_sf1.1, st_is_within_distance, dist = 1000)
    
    # Rename the columns to match the original table
    colnames(nonusfs_sf3.0) <- colnames(data_sf2.0)
    
    # Add the points back to the original table, avoiding repeats
    data_sf3.0 <- rbind(filter(data_sf2.0, !is.na(DISTRICTNA)), nonusfs_sf3.0)
  
  }else{
    
    data_sf3.0 <- copy(data_sf2.0)
    
  }
  
  # Format the attribute table to only include necessary data
  # For faster processing, convert to a data.table first
  sf_tbl <- as.data.table(data_sf3.0)
  
  # Remove unnecessary columns
  sf_tbl[, c("SHAPE_AREA", "SHAPE_LEN", "RANGERDIST", "FORESTNUMB", "DISTRICTNU", 
             "DISTRICTOR", "GIS_ACRES") := NULL]
  
  # Rename some columns to make more sense and make all column names lowercase
  setnames(sf_tbl, colnames(sf_tbl), tolower(colnames(sf_tbl)))
  setnames(sf_tbl, c("forestname", "districtna"), c("forest_name", "ranger_district"))
  
  # Reorder the columns
  setcolorder(sf_tbl, c("project_name", "survey_id", "unit_name", "survey_name", 
                        "region", "forest_name", "ranger_district", "utm_zone", 
                        "survey_type", "survey_start_date", "survey_end_date", 
                        "targets_detect", "detection_type", "detections", "geometry"))
  
  return(list("data_sf" = sf_tbl,
              "target_lst" = target_lst))
  
}