##	The version of the scripts used to produce the mapping products, and
##		which will match the "_v" portion of the filename outputs:
rfg.version = "0.18"

rfg.DologDEBUG <- TRUE

## Calculate Zonal stats using R default if FALSE or Python if kPythonZonal is TRUE
rfg.kPythonZonal <- FALSE

## Calculate Zonal stats using R default if FALSE or Python if kPythonZonal is TRUE
rfg.saveZonalStats <- TRUE

##	Set the path to the Python location which has access to ArcGIS 
##		Geoprocessing facilities.  As long as you're running RStudio using
##		the batch file to start it the ARCPY environment variable should
##		contain the appropriate path:
rfg.python_path <- "python"

#if zonal stats was calculated before then overwrite it if TRUE
rfg.overwrite.compiled.covariates <- FALSE

## var name of the water mask
rfg.water.mask <- "esaccilc_water_100m_2000_2012"

## var name of the water mask
rfg.ccidadminl1 <- "subnational_admin_2000_2020"


rfg.input.quant.output <- FALSE


## Spesify a path to gdal_merge.py
## as an example on some system you should do gdal_merge_path <- "python  /local/software/gdal/1.10.1/gcc/bin/gdal_merge.py "
#
# EXAMPLE for Unix:
#
#   rfg.gdal_path <- paste0("/usr/bin/")
#   rfg.gdal_gdalwarp_path <- paste0(rfg.gdal_path,"gdalwarp")
#   rfg.gdal_merge_path <- paste0(rfg.gdal_path,"gdal_merge.py")
#   rfg.gdal_calc_path <- paste0(rfg.gdal_path,"gdal_calc.py")
#   rfg.gdal_polygonize_path <- paste0(rfg.gdal_path,"gdal_polygonize.py")

rfg.gdal_path <- paste0("\"C:\\Program Files (x86)\\GDAL\\")

rfg.gdal_gdalwarp_path <- paste0(rfg.gdal_path,"gdalwarp.exe\"")
rfg.gdal_merge_path <- paste0(rfg.gdal_path,"gdal_merge.py\"")
rfg.gdal_calc_path <- paste0(rfg.gdal_path,"gdal_calc.py\"")
rfg.gdal_polygonize_path <- paste0(rfg.gdal_path,"gdal_polygonize.py\"")



rfg.pkgs <- c("rgdal", 
              "raster", 
              "randomForest", 
              "quantregForest", 
              "foreign", 
              "snow",
              "doParallel",
              "gdalUtils",
              "jsonlite",
              "logging", 
              "doSNOW", 
              "RCurl", 
              "plyr")



#####
##	BEGIN:	RandomForest configuration

##	Configuration options for RandomForest modeling and prediction:

##	NOTE:  The following were moved to the Metadata.r file for per-country
##		configuration and reporting purposes:

##  If we are using a set of covariates from another country set the 
##		fixed_set variable to specify which will then be used to fix the 
##		covariate list to an existing randomForest object, otherwise, 
##		use the full set from the covariate metadata if it is NULL.
##
##    Note that the fixed_set flag also changes the way that the 
##		randomForest object is created below by eliminating the variable 
##		elimination routine:


##	You can control whether you want covariates re-aggregated/summarized
##		by setting this flag to TRUE or FALSE:
rfg.overwrite_compiled_covariates <- FALSE


##	You can control whether or not we should estimate or combine Random
##		Forest models (init_popfit, popfit, popfit_combined, popfit_final, and
##		the quantile output) by setting this flag to TRUE or FALSE.  This
##		is useful when we don't want to run the RF code at all for new
##		countries, but instead just want to use an existing popfit_final.RData
##		and popfit_quant.RData that we copied into the /output/XXX/tmp folder
##		for the current country:
rfg.estimate_RF <- TRUE

rfg.data.old.popfits.final <- paste0(root_path, "/data/old_popfits/popfits_final/")
rfg.data.old.popfits.quant <- paste0(root_path, "/data/old_popfits/popfits_quant/")
##	END:	RandomForest configuration
#####


##	Fixed parameters and defaults:
rfg.proj4str_gcs_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


rfg.cores = parallel:::detectCores()
rfg.cluster_workers <- rfg.cores[1]-1
#rfg.cluster_workers <- 4

# if rfg.minblocks  <- NULL then minblocks for cluster prediction parallesation 
# will be calculated based on aval memory 
# see function get_minblocks_rf_prd in internal_function.R file
#
rfg.minblocks  <- NULL

###################################################################################
###################################################################################
###################################################################################
###################################################################################
#### DO NOT change the below lines 
###################################################################################
###################################################################################
###################################################################################

rfg.gdal_merge <- paste(rfg.python_path, rfg.gdal_merge_path,spr=" ")
rfg.gdal_calc <- paste(rfg.python_path, rfg.gdal_calc_path,spr=" ")
rfg.gdal_polygonize <- paste(rfg.python_path, rfg.gdal_polygonize_path,spr=" ")

###################################################################################
