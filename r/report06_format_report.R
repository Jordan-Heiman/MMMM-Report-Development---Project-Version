### Multiregional Multispecies Mesocarnivore Monitoring (MMMM)
### Jordan Heiman, Jessie Golding, Jody Tucker
## Date: 02/02/2022

## Function purpose: Format report

#################################### Intro #####################################

# Name: report06_format_report
# Description:  Function to set up and run the rMarkdown file which creates a 
# PDF version of a report for a specified forest and ranger district

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
# tbl:
#       Data table as cleaned by previous clean data function
# date_range:
#       Date range for the desired report, this is pulled from the map_lst 
#       created by the create maps function
# targets: 
#       List of the target species, this can be pulled from the output of the 
#       format spatial data function
# rmd_file:
#       The file path for the rMarkdown file to render into a PDF


################################# Output #######################################

# Folder with name reflecting National Forest and Ranger District
#       This folder contains 3 items:
#     PDF document
#           A PDF file is saved into the folder selected through file dialog with 
#           the file name corresponding to the forest and ranger district selected
#     JPEG Map
#           A map of the survey locations within the ranger district
#     CSV File
#           A CSV file of the data from the survey locations in the ranger district

################################################################################
## Function

report06_format_report <- function(sf_obj,
                                   tl_sf,
                                   tbl,
                                   date_range,
                                   targets,
                                   rmd_file = here("report06_format_report.Rmd")){
  
  # Copy tbl variable so that changes are not made to original data table
  tbl <- copy(tbl)
  
  # Add a flag_reason column if there isn't one 
  if (!("flag_reason" %in% colnames(tbl))){
    tbl[, flag_reason := ""]
  }
  
  # Create a table of the possible forest and ranger district combinations that
  # have data within the choosen date range by first limiting the data to the 
  # correct range (again the track log table may need to be modified if it is an 
  # empty table); then pull out just the region, forest and ranger district names
  sf_obj_dates <- sf_obj[between(survey_start_date, date_range[[1]], date_range[[2]]), ]
  if(nrow(tl_sf) > 0){
    tl_sf_dates <- filter(tl_sf, between(survey_start_date, date_range[[1]], date_range[[2]]))
  }else{
    tl_sf_dates <- as.data.table(tl_sf)[, ":=" (region_name = "",
                                                region = "",
                                                forest_name = "",
                                                ranger_district = "")]
  }
  rd_forest_tbl <- rbind(sf_obj_dates[, c("region", "forest_name", "ranger_district")], 
                         tl_sf_dates[, c("region", "forest_name", "ranger_district")], 
                         fill = TRUE)
  if("geometry" %in% colnames(rd_forest_tbl)){
    rd_forest_tbl <- rd_forest_tbl[, geometry := NULL]
  }
  
  # Create a reference table for region names
  reg_tbl <- data.table(region_num = c("01", "02", "03", "04", "05", "06", "08", "09", "10"),
                        region_name = c("Northern", "Rocky Mountain", "Southwestern", "Intermountain", 
                                        "Pacific Southwest", "Pacific Northwest", "Southern", "Eastern", "Alaska")
                        )[, region_full := paste("Region", gsub("^0", "", region_num), "-", region_name)]
  
  # Merge this reference table with the created table, remove any duplicates, 
  # and reorder the table a little
  rfrd_tbl <- unique(merge.data.table(rd_forest_tbl, reg_tbl, by.x = "region", by.y = "region_num", all.x = TRUE
                                        )[!is.na(forest_name) & !is.na(ranger_district),])
  setcolorder(rfrd_tbl, "region_full")
  setorder(rfrd_tbl, region, forest_name, ranger_district)
  
  # Make a list of possible regions
  reg_lst <- sort(unique(rfrd_tbl$region_full))
  
  # Ask for a region from the user
  region_prop <- select.list(reg_lst, title = "Please select a region", graphics = TRUE)
  
  # Make a list of possible forests
  forest_lst <- sort(unique(rfrd_tbl[region_full == region_prop, forest_name]))
  
  # Ask for the forest from the user
  forest_prop <- select.list(forest_lst, title = "Please select a forest", graphics = TRUE, multiple = TRUE)
  
  # If the user selects all the forests available, add an All to the end of the 
  # list to denote that all forests are being reported
  if (identical(forest_prop, forest_lst)){
    forest_prop <- c(forest_prop, "All")
  }
      
  # Use the forest selected to make a list of possible ranger
  rd_lst <- sort(unique(rfrd_tbl[forest_name %in% forest_prop, ranger_district]))
    
  # Only ask for ranger districts if only one forest was selected, otherwise 
  # just select all available ranger districts from the selected forests
  if (length(forest_prop) == 1){
    rd_prop <- select.list(rd_lst, title = "Please select a ranger district", graphics = TRUE, multiple = TRUE)
  }else{
    rd_prop <- sort(unique(rfrd_tbl[forest_name %in% forest_prop, ranger_district]))
  }
  
  # If the user selects all the ranger districts available, add an All to the 
  # end of the list to denote that all ranger districts are being reported
  if (identical(rd_prop, rd_lst)){
    rd_prop <- c(rd_prop, "All")
  }
  
  # Ask if the user would like to add anything extra to the report
  add_on_tf <- showQuestion(title = "Optional Report Add-ons",
                            message = "Would you like to add an extra section to the report? 
                            \n(If yes, the next two prompts will ask for the section header and a .txt file that contains the text of the section)",
                            ok = "Yes",
                            cancel = "No")
  
  # If extra text is selected, prompt the user for a heading for the section as 
  # well as for the text document that holds the extra text. 
  if (add_on_tf) {
    add_on_h <- str_to_title(showPrompt(title = "Report Add-On Header",
                                        message = "What would you like the header of the additional section to be?"))
    add_on_file <- selectFile(caption = "Select the add-on text file",
                              filter = "Plain Text (*.txt)")
    
    # This if/else clause handles making sure the text is in the right encoding 
    # for Rmd to read (UTF-8)
    if (detect_file_enc(add_on_file) != "UTF-8" 
        & detect_file_enc(add_on_file) != "WINDOWS-1252"){
      add_on_txt_org <- readChar(add_on_file, file.size(add_on_file))
      add_on_txt <- iconv(add_on_txt_org, 
                          from = detect_file_enc(add_on_file), 
                          to = "UTF-8")
    }else{
      add_on_txt <- readChar(add_on_file, file.size(add_on_file))
    }
  }
  
  # Prompt user for where to save PDF, code will copy map JPEG(s) there as well 
  showDialog(title = "Report Output",
             message = "Please select the location to save the final report folder to. (Folder contains a PDF report, JPEG maps, and a CSV of report data)")
  output_loc <- selectDirectory(caption = "Select where to save PDF report and JPEG map")
  
  # TinyTeX is needed for creating PDF files, this will install it and the 
  # needed LaTex packages
  #install_tinytex(force = TRUE)
  
  # Create a function for relocating map files
  map_relocate <- function(map, folder){
    file.copy(here("data", "temp_data", map), here(output_loc, folder, map), overwrite = TRUE)
  }
  
  # Based on how many forests and ranger districts were selected, set up the 
  # output folders, copy the map JPEG(s), and set up some text for the report
  if (length(rd_prop) == 1){
    
    # Use the forest and ranger district to create a name for the folder and 
    # the name of the map that will go with the report so that it can be located
    foldername <- gsub(" ", "_", paste(forest_prop, rd_prop))
    mapname <- paste0(foldername, ".jpeg")
    
    # Create a folder in the selected location using the name of the forest and 
    # ranger district
    dir.create(here(output_loc, foldername), showWarnings = FALSE)
    
    # Copy map file to the created folder
    map_relocate(mapname, folder = foldername)
    
    # Set up a subtitle for the report
    subtitle_txt <- paste(forest_prop, "-", rd_prop)
  
  }else if (length(rd_prop) > 1 & length(forest_prop) == 1){
    
    # Use the forest to create a name for the folder and combine the forests and 
    # ranger districts to create the names of the map JPEGs that will go with the report
    foldername <- gsub(" ", "_", forest_prop)    
    mapname <- lapply(seq_along(rd_prop), 
                      function(x) gsub(" ", "_", paste0(forest_prop, "_", rd_prop[[x]], ".jpeg"))) %>% 
      unlist
    
    # Create a folder in the selected location using the name of the forest 
    dir.create(here(output_loc, foldername), showWarnings = FALSE)
    
    # Within this directory create a folder for all the maps
    dir.create(here(output_loc, foldername, "Maps"), showWarnings = FALSE)
    
    # Copy map files to the map folder
    lapply(mapname, map_relocate, folder = paste0(foldername, "/Maps"))
    
    # Create subtitle text of the forest name
    subtitle_txt <- forest_prop
    
  }else if (length(rd_prop) > 1 & length(forest_prop) > 1){
    
    # Use the region to create a name for the folder and combine the forests and 
    # ranger districts to create the names of the map JPEGs that will go with the report
    foldername <- gsub(" - | ", "_", region_prop)
    mapname <- unique(rfrd_tbl[ranger_district %in% rd_prop, 
                               ][, mapname_col := gsub(" ", "_", paste0(forest_name, "_", ranger_district, ".jpeg"))
                                 ][, mapname_col])
    
    # Create a folder in the selected location using the name of the region
    dir.create(here(output_loc, foldername), showWarnings = FALSE)
    
    # Within this directory create a folder for all the maps
    dir.create(here(output_loc, foldername, "Maps"), showWarnings = FALSE)
    
    # Copy map files to the map folder
    lapply(mapname, map_relocate, folder = paste0(foldername, "/Maps"))
    
    # Create subtitle text of the region
    subtitle_txt <- region_prop

  }
  
  # Set up a PDF and CSV file name that matches the created folder name
  pdfname <- paste0(foldername, ".pdf")
  csvname <- paste0(foldername, ".csv")
  
  # Render the PDF report document using the variables that have been created
  render(input = rmd_file, 
         output_format = "pdf_document",
         output_dir = here(output_loc, foldername),
         output_file = pdfname,
         params = list(
           region = region_prop,
           forest = forest_prop,
           ranger_district = rd_prop,
           sf_obj = sf_obj,
           tl_sf = tl_sf,
           data_tbl = tbl,
           dates = date_range,
           target_lst = targets,
           sub_txt = subtitle_txt,
           map = mapname,
           add_on_head = get0("add_on_h", ifnotfound = ""),
           add_on = get0("add_on_txt", ifnotfound = "")))
  
  # Merge the track log and SF table to create a table of all the data (this 
  # step may be better to have much earlier in all these scripts, possibly even 
  # in the format track logs script)
  if (nrow(tl_sf) > 0) {
    tl_dt <- as.data.table(tl_sf)[, geometry := NULL]
    sf_merge <- merge.data.table(tl_dt, sf_obj, all = TRUE)
  }else {
    sf_merge <- sf_obj[, total_survey_length_km := NA]
  }
  
  # Retrieve the region number from the region reference table that was created
  region_str <- unique(rfrd_tbl[region_full == region_prop, region])
  
  # Format the data table to only include data from whichever region, forests 
  # and ranger districts that were selected; remove unnecessary and sensitive 
  # data columns
  full_data <- unique(merge.data.table(sf_merge, tbl, 
                                       by = intersect(colnames(sf_merge), colnames(tbl)),
                                       all = TRUE)[region == region_str
                                                   &forest_name %in% forest_prop 
                                                   & ranger_district %in% rd_prop
                                                   & between(survey_start_date, date_range[[1]], date_range[[2]]), 
                                       ][, c("detections", "geometry", "easting_longitude", "northing_latitude", "utm_zone") := NULL])
  
  # Rearrange the data table columns to make a little more readable
  setcolorder(full_data, c("project_name", "survey_id", "survey_name", "unit_name", 
                           "region", "forest_name", "ranger_district", "survey_cat", 
                           "survey_type", "survey_start_date", "survey_end_date", 
                           "total_survey_length_km", "targets_detect", "detect_id", 
                           "detection_type", "detection_date", "detection_time", 
                           "genus", "species"))   
  
  # If any stations did not have detections in the table, but were within the 
  # selected areas, update the detection type column to reflect that
  full_data[is.na(detection_type), detection_type := "No Species Detected in the selected Ranger District(s)"]
  
  # Write a CSV of the data for the ranger district
  write.csv(full_data, here(output_loc, foldername, csvname), row.names = FALSE)
  
}

