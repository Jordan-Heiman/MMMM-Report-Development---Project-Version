# Filter the sf_obj and track log sf to just the ranger district(s) and forest(s) 
# selected, also limit the table to the data to the date range and columns needed 
# for the report; create a survey length column that will serve as both days 
# deployed and kilometers surveyed (This is something that could possibly be done 
# before bringing into the markdown file and may speed up the PDF creation)
rd_sf <- unique(sf_obj[forest_name %in% forest_prop 
                       & ranger_district %in% rd_prop 
                       & between(survey_start_date, date_range[[1]], date_range[[2]]),
                       c("unit_name","survey_id", "survey_name", "survey_type", "survey_start_date",
                         "survey_end_date", "forest_name", "ranger_district")
                       ][, survey_length := as.numeric(survey_end_date - survey_start_date)
                         ][grepl("track survey|back track", survey_type), survey_length := NA])

if (nrow(tl_sf) > 0) {
  rd_tl <- as.data.table(filter(tl_sf, forest_name %in% forest_prop 
                                & ranger_district %in% rd_prop
                                & between(survey_start_date, date_range[[1]], date_range[[2]])
                                )[, c("unit_name", "survey_id", "survey_name", "survey_type", "survey_start_date", "survey_end_date",
                                      "total_survey_length_km", "forest_name", "ranger_district")])
  
    rd_tl[, geometry := NULL
          ][, survey_length := total_survey_length_km
            ][, total_survey_length_km := NULL]
}

# Start creating an effort table by first bringing together the two sf object 
# tables (one is track logs the other is all the point survey and detection data)
rd_surveys <- rbind(rd_sf, get0("rd_tl"), fill = TRUE)

# Use the site_ids from this merged table to filter the data table, then filter 
# the data table to only detections of target species
rd_data <- unique(tbl[survey_id %in% rd_surveys$survey_id
                      & flag_reason == "", c("survey_id", "genus", "species")
                      ][, genus_species := str_to_sentence(paste(genus, species, sep = " "))
                        ][, c("genus", "species") := NULL
                          ][(genus_species %in% targets), ])
##########################

# Filter the sf_obj and track log sf to just the ranger district(s) and forest(s) 
# selected, also limit the table to the data to the date range and columns needed 
# for the report; create a survey length column that will serve as both days 
# deployed and kilometers surveyed (This is something that could possibly be done 
# before bringing into the markdown file and may speed up the PDF creation)
rd_sf <- unique(params$sf_obj[forest_name %in% params$forest 
                              & ranger_district %in% params$ranger_district 
                              & between(survey_start_date, params$dates[[1]], params$dates[[2]]),
                              c("unit_name","survey_id", "survey_name", "survey_type", "survey_start_date",
                                "survey_end_date", "forest_name", "ranger_district")
                              ][, survey_length := as.numeric(survey_end_date - survey_start_date)
                                ][grepl("track survey|back track", survey_type), survey_length := NA])

if (nrow(params$tl_sf) > 0) {
  rd_tl <- as.data.table(filter(params$tl_sf, forest_name %in% params$forest 
                                & ranger_district %in% params$ranger_district 
                                & between(survey_start_date, params$dates[[1]], params$dates[[2]])
                                )[, c("unit_name", "survey_id", "survey_name", "survey_type", "survey_start_date", "survey_end_date",
                                      "total_survey_length_km", "forest_name", "ranger_district")])
  
  rd_tl[, geometry := NULL
        ][, survey_length := total_survey_length_km
        ][, total_survey_length_km := NULL]
}

# Start creating an effort table by first bringing together the two sf object 
# tables (one is track logs the other is all the point survey and detection data)
rd_surveys <- rbind(rd_sf, get0("rd_tl"), fill = TRUE)

# Use the site_ids from this merged table to filter the data table, then filter 
# the data table to only detections of target species
rd_data <- unique(params$data_tbl[survey_id %in% rd_surveys$survey_id 
                                  & flag_reason == "", c("survey_id", "genus", "species")
                                  ][, genus_species := str_to_sentence(paste(genus, species, sep = " "))
                                    ][, c("genus", "species") := NULL
                                      ][(genus_species %in% params$target_lst), ])