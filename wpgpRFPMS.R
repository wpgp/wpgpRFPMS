#########################################################################
##  wpgpRFPMS : Random Forests population modelling scripts                          
#########################################################################
##    Southampton University
##    WordlPop http://www.worldpop.org.uk
##   
##    Authors & Maintainer of the script  
## -- Maksym Bondarenko <mb4@soton.ac.uk> 
## -- Jeremiah J. Nieves <jeremiah.j.nieves@outlook.com>
##
##    Random Forest (RF)-based dasymetric mapping approach developed 
##    by Stevens et al. (2015)*
## 
##  * Stevens, F. R., Gaughan, A. E., Linard, C. & Tatem, A. J. 
##    Disaggregating Census Data for Population Mapping Using Random Forests 
##    with Remotely-Sensed and Ancillary Data. PLoS ONE 10, e0107042 (2015). 
##    http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0107042
#########################################################################
##
##    Depends 
##    R (>= 3.0.0), rgdal, raster, randomForest, quantregForest, foreign,
##    snow, doParallel, gdalUtils, jsonlite, logging, doSNOW, RCurl, 
##    plyr, wpgpCovariates, wpUtilities
##
#########################################################################
## -- Date:  Nov 2018
##
## -- More info on  https://github.com/wpgp/wpgpRFPMS
#########################################################################
##
##  Removes all object from memory:
rm(list=ls(all=TRUE)) 
#options(error=recover)



##  Set the root path (directory this script should be located in):
root_path = "E:\\WorldPop\\RF_NEW"


#####
##	BEGIN:	MODULE LOADING AND CONFIGURATION
#####

##  Load the model input script and get an idea about what countries we are 
##  working with:
source(paste0(root_path,"/input.R"))
rfg.input.countries <- as.list(rfg.input.countries)
rfg.nb.countries <- as.integer(length(rfg.input.countries))

##  Load all our other "modules" which do the heavy lifting for this script:
source(paste0(root_path,"/config.R"))
source(paste0(root_path,"/src/load_Packages.R"))
source(paste0(root_path,"/src/internal_functions.R"))  
source(paste0(root_path,"/src/check_config_input.R"))
source(paste0(root_path,"/src/create_dirs_for_prj.R"))
source(paste0(root_path,"/src/download_covariates.R"))

source(paste0(root_path,"/src/prep_rst_cvr_adminid.R"))
source(paste0(root_path,"/src/calculate_zonal_stats.R")) 
source(paste0(root_path,"/src/load_pop_table_and_farea.R"))
source(paste0(root_path,"/src/mrg_rst_cvr_countries.R"))  
source(paste0(root_path,"/src/cluster_predict.R"))
source(paste0(root_path,"/src/rf_functions.R")) 
source(paste0(root_path,"/src/check_result.R")) 

if (!load.Packages())
  stop("There was an error when loading R packages")

##  Check the configuration options:
##    Function is sourced from check_config_input.R
if (!check_config_input())
  stop("There was an error in your input or configuration!")


##  Create all the necessary directories for the model based upon the user 
##  input:
##    Function is sourced from create_dirs_for_prj.R
glPaths <- create_dirs_for_prj()  

##  Get the paths to the countries' data:
rfg.data.path.countries <- glPaths$data
##  Declare where we are outputting things:
rfg.output.path.countries <- glPaths$output
##  Declare where we are outputting things:
rfg.output.path.countries.cvr <- glPaths$data_cvr
##  Declare where our temporary path is:
rfg.output.path.countries.tmp <- paste0(rfg.output.path.countries, "tmp/")
##  Retrieve the country tag:
rfg.countries.tag <- glPaths$countries_tag


##  Get paht to a popfits.final and popfits.quant if we use fixed.set
if (rfg.fixed.set) {
  rfg.data.old.popfits.final <- glPaths$data_old_popfits_final
  rfg.data.old.popfits.quant <- glPaths$data_old_popfits_quant
}

##  Remove unnecessary items:
rm(glPaths)


#####
##	END:	MODULE LOADING AND CONFIGURATION
#####




#####
##  BEGIN:  COVARIATE LOADING
#####
##  Load module for dealing with variable names:
source(paste0(root_path,"/src/variable_names.R")) 

##  Pre allocate a list to hold all possible covariate names we will be dealing 
##  with:
covariates <- list()
covariates.var.names <- list()

##  If the files already exist locally:
if (file.exists(paste0(rfg.output.path.countries.tmp, rfg.countries.fln.Rdata))) {
  ##  Load them
  load(file=paste0(rfg.output.path.countries.tmp, rfg.countries.fln.Rdata))
}else{
  
  # if we are working with a shp file then we have to recalculate zonal stats for px_area
  # therefore we need to add it to a list of input covariates to download and then 
  # crop it by shape file 
  #
  #if (!is.null( rfg.input.shp )){
  #  rfg.input.cvr[[ length(rfg.input.cvr) + 1 ]] <- "px_area"
  #}     
  
  ##  Download the covariates from database to /Data/ 
  covariates <- Download_Covariates()
  
  ## Add custom covariates if specified in input file
  if (!is.null(rfg.input.custom.cvr)){
    covariates <- add.custom.covariates()
  }  

  ## If rfg.input.adminids is not NULL in input file then we 
  ## will crop all downloaded rasters to ID(s) mentioned in input parameters:
  if (!is.null(rfg.input.adminids)){
    ##  If the input admin IDs are not null, subset the covariate rasters:
    covariates <- prep_rst_cvr_adminid()
  }
  
  if (!is.null(rfg.input.shp)){
    ##  If the input shapefile is not null, subset the covariate rasters by it:
    #rfg.input.adminids <- prep_rst_cvr_shp()
    covariates <- prep_rst_cvr_shp()
  }  

  ##  Save the covariates object as a RData and a JSON file:
  Saving_Covariates_json_Rdata()
}

# updated rfg.input.cvr variable with a custom covariates
# if specified in input file
if (!is.null(rfg.input.custom.cvr)){
    rfg.input.cvr <- update.cvr.list.custom()
}  

#####
##  END:  COVARIATE LOADING
#####




#####
##  BEGIN:  COVARIATE SUMMARY OPERATIONS
#####
tmStart <- Sys.time()

##  If the zonal stats files already exist and we have not declared that we want
##  to NOT overwrite the compiled covariates:
if (file.exists(paste(rfg.output.path.countries.tmp, 
                      rfg.census.data.fln.Rdata, 
                      sep="")) & rfg.overwrite.compiled.covariates==FALSE) {
  
  loginfo("Zonal stats has been calculated before so we can load it. Loading census_covariates.RData")
  
  ##  Load the covariate summary tables:
  load(file=paste(rfg.output.path.countries.tmp, 
                  rfg.census.data.fln.Rdata, 
                  sep="")) 
} else {
  
  loginfo("Zonal stats has NOT been calculated before so we have to do it.")
  
  ##  Calculate the zonal stats for the area of interest and all pertinent 
  ##  covariates:
  ##  NOTE: Variable will have census_data for all countries seleceted.
  
  ##  Pre allocate the matrix frameworks we'll use:
  census_data <- matrix(nrow=0, ncol = 2) 
  POP_TABLE <- matrix(nrow=0, ncol = 2)
  colnames(POP_TABLE) <- c("ADMINID", "ADMINPOP") 
  
  ## Before calculating a zonal stat for country we  
  ## should check which zonal stats aval on FTP
  df.aval.zonal.stats.ftp  <- checkAvalZoanlStatFTP()  
  
  ## loop over each country to calculate zonal stats
  for ( icountry in rfg.input.countries ) {
    ##  Declare the temporary output path for the individual country:
    output.country.tmp <- paste0(root_path,"/","output/", icountry , "/tmp/")
    ##  Declare the path to the raster containing the zonal information:
    zonal_raster_path <- covariates[[icountry]][[rfg.ccidadminl1]][["path"]]
    ##  Bring in the zonal raster:
    zonal_raster <- raster(zonal_raster_path)
    
    ##  Set up the matrix to hold the census data for that country:
    census_data.country <- matrix(nrow=0, ncol = 2)
   
    ##	For each dataset in our covariate data structure, we need to 
    ##  run our custom Geoprocessing zonal statistics over the datasets:
    #
    loginfo(paste("Start summarizing covariates by zone for ",icountry))
    
    ##  Set up the text based progress bar (ignoring the L1 and water masks):
    pbCalZonalCovariates <- txtProgressBar(min = 0, 
                                           max = (length(covariates[[icountry]])-2), 
                                           style = 3, 
                                           width = 30)
    
    ## Loop over each covariate for a country to calculate zonal stats:
    for ( icvr in 1:length(covariates[[icountry]]) ) {
      
      ## Skip water mask and L1 in covariates based upon names of covariates:
      if( names(covariates[[icountry]][icvr]) %in%
          c(rfg.water.mask,rfg.ccidadminl1) ){next}
      
      ## Calculating a zonal stat for country. 
      ## Function called from zonal_functions.R      
      output_stats.sorted <- calculate.zonal.stats.country(icvr,
                                                           icountry,
                                                           zonal_raster)
      ##  If this is the first iteration:
      if (icvr == 1 ) { 
        census_data.country <- output_stats.sorted
      } else {
        ##  Merge with the previous iterations:
        census_data.country <- merge( as.data.frame(census_data.country), 
                                      as.data.frame(output_stats.sorted), 
                                      by="ADMINID", 
                                      sort=FALSE)
      }
      ##  Update the progress bar and ensure the output is sent to the console:
      setTxtProgressBar(pbCalZonalCovariates, icvr)  
      flush.console() 
      cat("\n")
      ## END loop for each covariate per country
    }   
    
    ##  Sort columns of a dataframe by column name before merging with another 
    ##  countries output:
    ##    Sort:
    census_data.country <- census_data.country[ , order(names(census_data.country))]  
    
    ##    Merge by ADMIN ID with the corresponding population table:
    # if shape file is used then we need to get a new pop table based on a new calculated 
    # zonal stats sum for px_area 
    # if not we will download a zoanl stats sum for this country from FTP
    #    
    census_data.country <- get.pop.table.and.farea( icountry, census_data.country )      
    

    #census_data.country <- merge( as.data.frame(census_data.country), 
    #                              as.data.frame(load.pop.table.and.farea(icountry)), 
    #                              by="ADMINID", 
    #                              sort=FALSE)       
    
    #   Merging census_data all countries:
    census_data <- rbind(census_data, census_data.country)   

    
    ##  END loop for each country
  }  

  
  ##	Convert our calculated admin unit areas into hectares and add them to the 
  ##  census data:
  census_data$AREA_HA <- census_data$F_AREA / 10000
  
  ##	Finally calculate our population density in people per hectare for use as 
  ##  our model's outcome of interest:
  census_data$POPD_PPHA <- census_data$ADMINPOP / census_data$AREA_HA
  
  ##  Save the compiled census data as a new file in the temporary output 
  ##  folder:
  saving_census_data()  
  
  ##  Convert that data to a dataframe for continuted use:
  census_data <- as.data.frame(census_data)
  
  ##  Close the progress bar:
  close(pbCalZonalCovariates)
 
  ##  END if census_covariates does not exist 
}

tmEnd <-  Sys.time()
loginfo(paste("Elapsed Processing Time:", tmDiff(tmStart,tmEnd)))


#####
##  END:  COVARIATE SUMMARY OPERATIONS
#####




#####
##  BEGIN: COVARIATE AREA FUSION OPERATIONS
#####
##  If we are working with more that one country:
if ( length(rfg.input.countries) > 1 ) {
  ##  Merge the many covariate rasters using either parallel computing 
  ##  (mode="cluster") or single core (mode="single")
  merging_rst_cvr_multy_country(mode="cluster")
} 

##  Create a list of covariates for main RF subrouting function called 
##  from mrg_rst_cvr_countries.R file:
covariates <- create_covariates_list_for_RF()

## Save the covariates as an RData file used in RF:
Saving_Covariates_RF_json_Rdata()

##  Retrieve the paths for the watermask and the census mask:
watermaskPathFileName <- covariates[[rfg.water.mask]]$path
censusmaskPathFileName <- covariates[[rfg.ccidadminl1]]$path

##  Remove AdminId, Watermask and px_area info from prepared covariates list:
if (rfg.water.mask %in% names(covariates)){
  covariates <- covariates[ - which(names(covariates) == rfg.water.mask)]
  }
if (rfg.ccidadminl1 %in% names(covariates)){
  covariates <- covariates[ - which(names(covariates) == rfg.ccidadminl1)]
}
if ('px_area_100m' %in% names(covariates)) {
  covariates <- covariates[ - which(names(covariates) == "px_area_100m")]
} 

# remove unnecessary variables
rm_unnecessary_var_fln()

# removing year from a covariates names for furture if we use fix_set
#
covariates <- rm_year_from_covariates_name(covariates)  
census_data <- rm_year_from_census_data_name(census_data) 
  


#####
##  END: COVARIATE AREA FUSION OPERATIONS
#####

##  ------------------------------------------------------------------------  ##
##                       RANDOM FOREST MODELING SECTION                       ##
##  ------------------------------------------------------------------------  ##

setwd(rfg.output.path.countries)

#####
##  BEGIN:  DATA SUBSETTING FOR RF
#####
##	Set up response and covariate dataframes for the random forest modeling.
##    Retrieve the population density data:
y_data <- census_data$POPD_PPHA

## Get a list of all covariates. Function called from a rf_functions.R file
fixed_predictors <- get_fixed_predictors() 

loginfo("Remove unnecessary covariates from x_data for RF... ") 	
##  Full covariate set:
x_data <- census_data[,fixed_predictors]

##	Subset x_data to remove NAs:
indexX <- complete.cases(x_data)

##	Subset to remove zero population densities or lower:
indexY <- y_data > 0

##	Subset data according to indices to make sure we maintain alignment:
y_data <- y_data[indexX & indexY]
x_data <- x_data[indexX & indexY,]

##	Transform (i.e. log) y_data as defined in the transY() function:
y_data <- transY(y_data)


#####
##  END:  DATA SUBSETTING FOR RF
#####


#####
##  BEGIN:  FITTING RANDOM FOREST
#####
## Fit the RF, removing any covariates which are not important to the model:
popfit <- get_popfit()

##  Fit the final RF and the quant RF:

if (!rfg.fixed.set) {
  
  popfit_final <- get_popfit_final()
  popfit_quant <- get_popfit_quant()
  
}else{
  
  if (rfg.fixed.set.incl.input.countries==TRUE & 
      (nrow(census_data) > rfg.fixed.set.idmin.id.threshold)) {
    
    popfit_final <- get_popfit_final()
    popfit_quant <- get_popfit_quant()
    
  }else{
    if (rfg.fixed.set.incl.input.countries){
      logwarn(paste0("Number of ADMIN units is less then threshold ",rfg.fixed.set.idmin.id.threshold))
      logwarn(paste0("popfit_final and popfit_quant will not be calculated"))
    }  
  }
}


##  Remove objects unnecessary for future operations:
rm(popfit)

## Set the fixed_set to existing countries if you are using an existing
##    set of randomForest objects to predict from:
set_fixed_set_to_existing_countries()

##	Last, to save on memory we don't have any need for the proximity 
##		matrix for prediction purposes, and for census data with many, many
##		units this proximity matrix can be extremely large. We remove it
##		here from the popfit_final object since this object will be 
##		duplicated across nodes of the cluster. If you need it, it is saved
##		with the object and can be load() from disk:
popfit_final$proximity <- NULL

#####
##	END:	FITTING RANDOM FOREST
#####

#####
##  BEGIN:  RF PREDICTION PREP
#####
##	Create a raster stack of our cropped covariates and the zonal_raster 
##  file which will allow us to restrict processing to just the areas within the 
##  boundaries of our census data area (NOTE: This should be changed here to 
##  match the covariates used in the estimation of the model, as well as the 
##  renaming applied in the cluster predict function. This will speed processing
##  up slightly, especially if used on a subset of the predictors:
census_mask <- raster(censusmaskPathFileName)
water_raster <- raster(watermaskPathFileName)
prediction_raster <- census_mask


##  Stack all of our covariates and masks together:
covariate_stack <- creat_raster_stack()

gcQuiet(quiet = F)

#####
##  END:  RF PREDICTION PREP
#####



#####
##  BEGIN:  RF PREDICTION
#####
loginfo("Starting cluster prediction")
 
##  Start up the cluster:
beginCluster(n=rfg.cluster_workers)

##  Create the population density weighting layer:
prediction_raster <- cluster_predict(prediction_raster, 
                                     quant_output=rfg.input.quant.output)

##  Terminate the cluster:
endCluster()

#####
##  END:  RF PREDICTION
#####



#####
##  BEGIN:  DASYMETRIC POPULATION MAP PRODUCTION
#####
apply_population_density()


#####
##  END:  DASYMETRIC POPULATION MAP PRODUCTION
#####



#####
##  BEGIN:  REPORT GENERATION
#####

source(paste0(root_path,"/src/report.R"))


#####
##  END:  REPORT GENERATION
#####


#####
##  BEGIN:  checking final RF result
#####

check_final_RF_result()

#####
##  END:  checking final RF result
#####


######################################################################################
tmEnd <-  Sys.time()
loginfo(paste("Elapsed Processing Time:", tmDiff(tmStart,tmEnd)))
