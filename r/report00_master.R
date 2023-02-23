### Multiregional Multispecies Mesocarnivore Monitoring (MMMM)
### Jordan Heiman, Jessie Golding, Jody Tucker
## Primary Code Author: Jordan Heiman 
## Date: 02/02/2022

## Code purpose: master file code and work flow for generating reports for MMMM 

################################################################################
# Line to set working directory
if (!require("here", character.only = TRUE)) {
  install.packages("here", dependencies = TRUE)
  library("here", character.only = TRUE)
}

################################################################################

### Load functions 
source(here("r", "report01_load_packages.R"))
source(here("r", "report02_load_data.R"))
source(here("r", "report03_clean_data.R"))
source(here("r", "report04_format_spatial_data.R"))
source(here("r", "report04a_format_track_logs.R"))
source(here("r", "report05_create_maps.R"))
source(here("r", "report06_format_report.R"))

################################################################################
## 01. Load packages (There is not an actual function to run here, the line 
# above loads the packages when it loads that script)

## 02. Load data
data_tbl <- report02_load_data()
  
## 03. Clean data
clean_tbl <- report03_clean_data(tbl = data_tbl)
 
## 04. Format spatial data
data_lst <- report04_format_spatial_data(tbl = clean_tbl,
                                         rd_shp = here("data", "raw_data", "usfs_ranger_district_map.shp"), # Default selection
                                         final_datum = "NAD83") # Default selection

## 04a. Format track logs
track_log_sf <- report04a_format_track_logs(tbl = clean_tbl, 
                                            targets = data_lst$target_lst,
                                            rd_shp = here("data", "raw_data", "usfs_ranger_district_map.shp"), # Default selection
                                            final_crs = "WGS84") # Default selection

## 05. Create maps (this will also save a jpeg version of the map)
map_lst <- report05_create_maps(sf_obj = data_lst$data_sf,
                                tl_sf = track_log_sf,
                                targets = data_lst$target_lst,
                                rd_shp = here("data", "raw_data", "usfs_ranger_district_map.shp"))

# Maps are saved to data/temp_data folder at this point if they are needed on their own

## 06. Format and save report
report06_format_report(sf_obj = data_lst$data_sf,
                       tl_sf = track_log_sf,
                       tbl = clean_tbl,
                       date_range = map_lst$date_range,
                       targets = data_lst$target_lst,
                       rmd_file = here("r", "report06_format_report.Rmd")) # Default selection
