### Multiregional Multispecies Mesocarnivore Monitoring (MMMM)
### Jordan Heiman, Jessie Golding, Jody Tucker
## Date: 02/02/2022

## Code purpose: Load packages (check if installed and if not install them)

################################################################################
## Load required packages
# Function from https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/

# This prevents some warnings when mapping plots
options("rgdal_show_exportToProj4_warnings" = "none")

## First specify the packages of interest
packages = c("data.table", "dplyr", "flextable", "ggmap", "ggplot2", "ggrepel", 
             "ggspatial", "knitr", "lubridate", "officer", "scales", "stats", 
             "ttutils", "utils", "raster", "rgeos", "rmarkdown", "rstudioapi", "sf", "sp", 
             "stringr", "tinytex", "uchardet")

## Now check each packages, if it needs to be installed, install it, then load it
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)