### Multiregional Multispecies Mesocarnivore Monitoring (MMMM)
### Jordan Heiman, Jessie Golding, Jody Tucker
## Date: 02/02/2022

## Function purpose: Clean data

#################################### Intro #####################################

# Name: report03_clean_data
# Description:  Function to clean data 

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2/2/2022

################################# Arguments ####################################

# tbl:
#       Data.table of the data to be used in the report

################################# Output #######################################

# clean_tbl
#       A cleaned and formatted table of the data to be used in the report

################################################################################
## Function

report03_clean_data <- function(tbl){
  
  # Check that the data table has the necessary columns
  # First format the current column names in case the formatting changed 
  tbl_col <- colnames(tbl) %>% 
    tolower() %>% # Change to lower case
    gsub(" \\(.*\\)", "", .) %>%  # Remove any phrases in parenthesis
    gsub(" |\\/", "_", .) %>% # Replace any spaces or slashes with underscores
    gsub("_$", "", .) # Remove any underscores that are at the end of a column name
  
  # Set these reformatted column names as the data table column names
  colnames(tbl) <- tbl_col
  
  # Establish the required column names
  req_col <- c("project_name", "easting_longitude", "northing_latitude", 
               "utm_zone", "datum", "survey_name", "unit_name", "survey_type", 
               "survey_start_date", "survey_end_date", "detection_date", 
               "detection_time", "detection_type", "genus", "species")
  
  # Give an error and stop code if required columns are missing
  stopifnot("Missing required columns in data table" = req_col %in% colnames(tbl))
  
  # Drop any extra columns from the data table and order the columns
  tbl1.1 <- tbl[, ..req_col]
  
  # Format the columns to the correct types
  tbl1.1[, ':=' (easting_longitude = as.numeric(easting_longitude),
                 northing_latitude = as.numeric(northing_latitude),
                 utm_zone = as.integer(utm_zone),
                 datum = tolower(datum),
                 survey_type = tolower(survey_type),
                 detection_type = tolower(detection_type), 
                 genus = tolower(genus),
                 species = tolower(species))]
  
  # Convert the date columns, in case there are some rows that do not have a proper date in them
  if (!is.Date(tbl1.1$survey_start_date)){tbl1.1[, survey_start_date := mdy(survey_start_date)]}
  if (!is.Date(tbl1.1$survey_end_date)){tbl1.1[, survey_end_date := mdy(survey_end_date)]}
  if (!is.Date(tbl1.1$detection_date)){tbl1.1[, detection_date := mdy(detection_date)]}
  
  # Change coordinates that are 0 to NA
  tbl1.1[tbl1.1 == 0] <- NA
  
  # Give each detection a unique ID
  tbl1.1[, detect_id := .I]
  
  ####### Error check columns that can be error checked  ####### 
  # survey_type checks:
  accp_pt_surveys <- c("camera", "track plate", "bait station")
  accp_ln_surveys <- c("back track", "track survey")
  
  # Assign a survey category to each row of data that has an acceptable survey type
  tbl1.1[survey_type %in% accp_pt_surveys, survey_cat := "point"
         ][survey_type %in% accp_ln_surveys, survey_cat := "linear"]
  
  # Give each site a unique ID, for grouping, only include coordinates for point surveys
  tbl1.1[survey_cat == "point", 
         survey_id := paste0("p_", as.character(.GRP)), 
         by = c("easting_longitude", "northing_latitude", "survey_name", "unit_name", 
                "survey_type", "survey_start_date", "survey_end_date")]
  tbl1.1[survey_cat == "linear", 
         survey_id := paste0("l_", as.character(.GRP)), 
         by = c("survey_name", "unit_name", "survey_type", "survey_start_date", "survey_end_date")]
  
  # Check that the survey type is one from our list of accepted types of surveys
  err_surtype <- tbl1.1[is.na(survey_cat),
                        ][, flag_reason := "Survey type not from list, see Metadata worksheet of Data Template excel workbook"]
  
  # Remove detections that are missing required data
  complete_tbl <- tbl1.1[complete.cases(tbl1.1[, c("project_name", "easting_longitude", 
                                                   "northing_latitude", "datum", 
                                                   "survey_name", "unit_name", 
                                                   "survey_type", "survey_start_date", 
                                                   "survey_end_date", "survey_id")]), ]
  
  # Add the removed detections to an error table
  na_tbl <- tbl1.1[!(detect_id %in% complete_tbl$detect_id), ]
  
  # Remove track data that have no coordinates
  err_na <- na_tbl[!(survey_cat == "linear" 
                     & (is.na(easting_longitude) | is.na(northing_latitude))), ]
  
  # Creates a column that lists what columns are NA
  err_na$col_na <- suppressWarnings(melt(err_na, "detect_id"
                                         )[, toString(variable[is.na(value)]), detect_id]$V1)
  
  # Take any optional columns out of the lists of columns that are missing data
  err_na[, col_na := gsub("survey_id|survey_cat|utm_zone|detection_date|detection_time|detection_type|genus|species", "", col_na)
         ][, col_na := gsub("^(, )*|(?<=(, )), |(, )*$", "", col_na, perl=T)]
  
  # Set up the flag for these detections
  err_na[, flag_reason := paste0("Required data missing: ", col_na)
         ][, col_na := NULL]
   
  # Check the coordinates for errors (UTMs are assumed to be integers and lat/long is assumed to not be integers, limited to coordinates
  # in the continental US)
  err_coor <- complete_tbl[(isInteger(easting_longitude) & !isInteger(northing_latitude)) # If coordinates in easting_longitude are UTM but the northing_latitude is lat/long
                           | (isInteger(northing_latitude) & !isInteger(easting_longitude)) # If coordinates in northing_latitude are UTM but the easting_longitude is lat/long
                           | (isInteger(easting_longitude) & !between(easting_longitude, 166000, 834000)) # If the easting_longitude is UTM but not in a range that makes sense
                           | (isInteger(northing_latitude) & !between(northing_latitude, 1100000, 9300000)) # If the northing_latitude is UTM but not in a range that makes sense
                           | (!isInteger(easting_longitude) & !between(easting_longitude, -125, -66.5)) # If the easting_longitude is lat/long but not in a range that makes sense
                           | (!isInteger(northing_latitude) & !between(northing_latitude, 24, 49.5)), # If the northing_latitude is lat/long but not in a range that makes sense
                           ][, flag_reason := "Coordinates not within US or coordinates do not match type"] 
  
  # Check that a UTM zone is provided if the easting_longitude and northing_latitude are both in UTMs
  err_zone1 <- complete_tbl[isInteger(easting_longitude)
                            & isInteger(northing_latitude) 
                            & (is.na(utm_zone) | utm_zone == 0),
                            ][, flag_reason := "UTM provided but no zone provided"]
  
  # Check that the UTM zone is one that is in the US
  err_zone2 <- complete_tbl[!is.na(utm_zone) 
                            & (utm_zone < 10 | utm_zone > 19),
                            ][, flag_reason := "UTM zone not within the US"]
  
  # Check that survey start is a reasonable date
  err_start <- complete_tbl[survey_start_date < as_date("1900-01-01"),
                            ][, flag_reason := "Start date unlikely"]
  
  # Check that survey end date is after the start date
  err_startend <- complete_tbl[survey_start_date > survey_end_date,
                               ][, flag_reason := "Survey start date is after end date"]
  
  # Check that the survey has ended
  err_end <- complete_tbl[survey_end_date > today(),
                          ][, flag_reason := "Survey end date has not happened yet"]
  
  # Check that the detection date falls between the survey start and end
  err_detectdate <- complete_tbl[detection_date > today() 
                                 | detection_date < survey_start_date 
                                 | detection_date > survey_end_date,
                                 ][, flag_reason := "Detection date is not during the survey dates"]
  
  # Combine all the flags into one table and condense that to just the detect_id 
  # and flag_reasons
  error_tbl <- rbindlist(mget(ls(pattern = "err_")))[, c("detect_id", "flag_reason")]
  
  # Merge the flag reasons for each unique detect_id
  error_tbl1.1 <- error_tbl[  , .(flag_reason = paste(flag_reason, collapse = "; ")), by = detect_id]
  
  # Check for flagged data
  if(nrow(error_tbl1.1) > 0){
    
    # Merge the flag reasons to the original table
    tbl2.0 <- merge.data.table(tbl1.1, error_tbl1.1, all = TRUE)
  
    # Warn the user that some data is flagged and return the table with all data
    showDialog(title = "Data flagged", 
               message = "Some data was flagged. A CSV file has been saved to the main project folder with the data and the reason for the flag. If left in the data table, it will not be processed or included in the report")
    
    # Order the columns of the table
    setcolorder(tbl2.0, c("flag_reason", "project_name", "survey_id", "survey_name", "unit_name", 
                          "easting_longitude", "northing_latitude", "utm_zone",
                          "datum", "survey_type", "survey_start_date", 
                          "survey_end_date", "detect_id", "detection_date", 
                          "detection_time", "detection_type", "genus", "species"))
    
    # Sort the table
    setorder(tbl2.0, "flag_reason", "unit_name", "survey_id", "detect_id")
    
    # Change NA values to just blank cells
    tbl2.0[is.na(flag_reason), flag_reason := ""]
    
    # Save a copy of the CSV
    write.csv(tbl2.0[flag_reason != "",], here("flagged_data.csv"), row.names = FALSE)
    
    return(tbl2.0)
    
  }else{
    
    # Order the columns of the table and save a copy
    setcolorder(tbl1.1, c("project_name", "survey_id", "survey_name", "unit_name", 
                          "easting_longitude", "northing_latitude", "utm_zone",
                          "datum", "survey_type", "survey_start_date", 
                          "survey_end_date", "detect_id", "detection_date", 
                          "detection_time", "detection_type", "genus", "species"))
    
    # Sort the table
    setorder(tbl1.1, "survey_id", "detect_id")
    
    # If no flagged data just return the formatted table
    return(tbl1.1)
    
  }
  
}
