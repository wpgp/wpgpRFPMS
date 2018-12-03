# wpRFPMS : Random Forests population modelling scripts 

[![N|Solid](http://maps.worldpop.org.uk/img/worldpop-logo.png)](http://maps.worldpop.org.uk)


* [Introduction ](../README.md)
* [Dependencies and installation ](Dependencies.md)
* [Outputs ](Outputs.md)
* [Using Shape file ](Shapefile.md)
* [Using AdminID ](AdminID.md)

 

# Getting started
The wpgpRFPMS script calls upon multiple R scripts as "modules" and all of the functions are stored in separate R files. In order to start using the wpRFPMS script you have to download all files from github to your local directory. You will be expected to have the following file structure:

  * **src**
    * calculate_zonal_stats.R
    * check_config_input.R
    * check_config.R
    * cluster_predict.R
    * create_dirs_for_prj.R
    * download_covariates.R
    * internal_functions.R
    * load_Packages.R 
    * prep_rst_cvr_adminid.R
    * report.R
    * rf_functions.R
    * variable_names.R
  * wpRFPMS.R
  * input.R
  * config.R


`input.R` is an input file for the main program containing the following user defined input parameters: 

| Name | Description |
| ------ | ---------------------------------------------------------------------------------------------------- |
| `rfg.input.countries` |  Declare the 3-letter ISO code(s) of the country(ies) you are interested in modelling.  NOTE:  You must declare the ISO codes of the countries you are modelling even if you plan on only modeling portions of them, i.e. declaring specific admin IDs below or using a shapefile to subset them |
| `rfg.input.poptables` | If you are using specific Population tables, i.e. non-standard, stored locally, declare their paths here. Otherwise, the script will source the ones from the WorldPop FTP. |
| `rfg.input.adminids` | Declare specific admin IDS by which to subset the above declared countries.  WARNING:  You can NOT use this option in conjunction with the shapefile subsetting option. At least one of the two subsetting options MUST be set to NULL |
| `rfg.input.shp` | Declare the paths to the shapefiles subsetting the countries of interest which were declared above. WARNING:  You can NOT use this option in conjunction with the adminID subsetting option. At least one of the two subsetting options MUST be set to NULL. |
| `rfg.input.year` | Declare the input year for which we are modeling. This must be declared as a numeric character string, e.g. `"2000"` |
| `rfg.input.cvr` | Declare a list of the character representations of the covaraites with which we intend to do modelling with. NOTE:  You can use the function `wpgpListCountryCovariates()` from the `wpgpCovariates` library to see what all covariates are available, but most covariates declared by the user will remain the same between covariate runs excluding the year specific part of their name. EXAMPLE: wpgpListCountryCovariates(ISO3="NPL", username = "", password = "") |
| `rfg.fixed.set` |  If `TRUE` we are using a fixed set in this modeling, i.e. are we  parameterizing, in part or in full, this RF model run upon another  country's(ies') RF model object.  If `rfg.fixed.set.incl.input.countries=TRUE` then contries from `rfg.input.countries` will be combined with the RF object from files available in the following directory /data/old_popfits/popfits_final and popfits_quant|

Example of `input.R` file is below
```
rfg.input.countries <- c("BTN")
rfg.input.poptables <- NULL
rfg.input.adminids <- NULL
rfg.input.shp <- NULL
rfg.input.year <- "2000"
rfg.input.cvr <- list("esaccilc_dst011_100m_2000",
                      "esaccilc_dst040_100m_2000",
                      "esaccilc_dst130_100m_2000",
                      "esaccilc_dst140_100m_2000",
                      "esaccilc_dst150_100m_2000",
                      "esaccilc_dst160_100m_2000",
                      "esaccilc_dst190_100m_2000",
                      "esaccilc_dst200_100m_2000",
                      "esaccilc_dst_water_100m_2000_2012",
                      "osm_dst_roadintersec_100m_2016",
                      "osm_dst_waterway_100m_2016",
                      "osm_dst_road_100m_2016",
                      "srtm_slope_100m",
                      "srtm_topo_100m",
                      "dst_coastline_100m_2000_2020",
                      "dmsp_100m_2000"
) 
rfg.fixed.set <- FALSE
rfg.fixed.set.incl.input.countries <- FALSE
rfg.fixed.set.description <- ""                       
```

`config.R` is a configuration file for the program containing the information for WorldPop FTP and the paths for python and gdal:

**Example for Unix:**
```sh
rfg.gdal_path <- paste0("/usr/bin/")
rfg.gdal_gdalwarp_path <- paste0(rfg.gdal_path,"gdalwarp")
rfg.gdal_merge_path <- paste0(rfg.gdal_path,"gdal_merge.py")
rfg.gdal_calc_path <- paste0(rfg.gdal_path,"gdal_calc.py")
rfg.gdal_polygonize_path <- paste0(rfg.gdal_path,"gdal_polygonize.py")
```
**Example for Windows:**
```sh
rfg.gdal_path <- paste("\"C:\\Program Files (x86)\\GDAL\\")
rfg.gdal_gdalwarp_path <- paste0(rfg.gdal_path,"gdalwarp.exe\"")
rfg.gdal_merge_path <- paste0(rfg.gdal_path,"gdal_merge.py\"")
rfg.gdal_calc_path <- paste0(rfg.gdal_path,"gdal_calc.py\"")
rfg.gdal_polygonize_path <- paste0(rfg.gdal_path,"gdal_polygonize.py\"")
```

`config.R` file has other important paramenters which need to be changed before using script. 

| Name | Description |
| ------ | ------ |
|`rfg.cluster_workers` | How many cores to use during colculation. You can limit the number of cores used in the program by changing `rfg.cluster_workers <- 5` otherwise max cores on PC minus one will be used.|
|`rfg.minblocks ` | How many blocks to use during prediction and other functions. If `NULL` the script will find the oprimal number of blocks based on memory and cores avalible on your PC|


In order to run the wpgpRFPMS script `root_path` needs to be changed to the directory where the script was copied. 


