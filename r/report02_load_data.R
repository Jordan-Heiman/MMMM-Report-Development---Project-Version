### Multiregional Multispecies Mesocarnivore Monitoring (MMMM)
### Jordan Heiman, Jessie Golding, Jody Tucker
## Date: 02/02/2022

## Function purpose: Load data

#################################### Intro #####################################

# Name: report02_load_data
# Description:  Function to load data 

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2/2/2022

################################# Arguments ####################################

# No arguments need to be provided to the function. The function itself will
# prompt the user for the location of the data csv

################################# Output #######################################

# data_tbl
#       A data.table of the data that is read in

################################################################################
## Function

report02_load_data <- function(){
  
  # Request the user selects a CSV file that contains the data in the proper
  # format; this is done in two prompts as the selectFile prompt does not allow
  # for much of a message to the user
  showDialog(title = "File selection", 
             message = "Please select the CSV file that contains data in the same format as the data template provided.")
  
  data_file <- selectFile(caption = "Select formatted data file", 
                          filter = "CSV Files (*.csv)")
            
  # Read the data file, returns them as a data.table   
  data_tbl <- fread(data_file)
  
  # Replace any blanks with NA
  data_tbl[data_tbl == ""] <- NA

  # Check that the data table has the necessary columns
  # First format the current column names in case the formatting changed 
  tbl_col <- colnames(data_tbl) %>% 
    tolower() %>% # Change to lower case
    gsub(" \\(.*\\)", "", .) %>%  # Remove any phrases in parenthesis
    gsub(" |\\/", "_", .) %>% # Replace any spaces or slashes with underscores
    gsub("_$", "", .) # Remove any underscores that are at the end of a column name
  
  # Set these reformatted column names as the data table column names
  colnames(data_tbl) <- tbl_col
  
  # Establish the required column names
  req_col <- c("project_name", "easting_longitude", "northing_latitude", 
               "utm_zone", "datum", "survey_name", "unit_name", "survey_type", 
               "survey_start_date", "survey_end_date", "detection_date", 
               "detection_time", "detection_type", "genus", "species")
  
  # Give an error and stop code if required columns are missing
  stopifnot("Missing required columns in data table" = req_col %in% colnames(data_tbl))
  
  # Drop any extra columns from the data table and order the columns
  data_tbl_2 <- data_tbl[, ..req_col]
  
  # Return the full data table
  return(data_tbl_2)
  
}