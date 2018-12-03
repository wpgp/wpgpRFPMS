####################################################################
# Function to crop a raster by shape file using gdal. This function 
# can be replaced by crop_raster_R_v2 which is using R functions
#
crop_raster_R <- function(srcfile, shapefile, distDir, NoDataValue) {

  rOrgFileName.ext <- basename(srcfile)
#  rOrgDirName <- dirname(srcfile)
#  rOrgFileName <- sub('\\.tif$', '', rOrgFileName.ext)
  
#  rTmpFileName <- paste0(rOrgFileName,"_tmp.tif") 
#  dstfile <- paste0(rOrgDirName,"/",rTmpFileName)
  
  dstfile <- paste0(distDir, rOrgFileName.ext)

  gdal_str <- paste0(rfg.gdal_gdalwarp_path,"  --config GDALWARP_IGNORE_BAD_CUTLINE YES -co COMPRESS=LZW -of GTiff  -overwrite -cutline ", shapefile, " -dstnodata ",NoDataValue," ",srcfile," ",dstfile)
  system(gdal_str, ignore.stdout = FALSE, ignore.stderr = FALSE, show.output.on.console = FALSE)
  
  
#  if(file.exists(dstfile)){
#    
#    unlink(srcfile , recursive = TRUE, force = FALSE)
#    file.rename(dstfile, srcfile)
#    
#  } 
  
}
######################################################################


##  Written by Maksym Bondarenko
##  Adapted and commented by Jeremiah J. Nieves
crop_raster_R_v2 <- function(srcfile, shapefile, distDir, NoDataValue) {
  ##  Function which crops a raster based upon an input shapefile.
  ##    In this case srcfile is a raster (i.e. .tif).
  
  ##  Retrieve the basename of the src file:
  rOrgFileName.ext <- basename(srcfile)
  ##  Retrieve the directory of the src file:
#  rOrgDirName <- dirname(srcfile)
  ##  Get the name of the file minus the extension:
#  rOrgFileName <- sub('\\.tif$', '', rOrgFileName.ext)
  
  ##  Declare the name of a temporary file:
#  rTmpFileName <- paste0(rOrgFileName,"_tmp.tif")
  
  ##  Declare the output file name:
#  dstfile <- paste0(rOrgDirName,"/",rTmpFileName)
  
  dstfile <- paste0(distDir, rOrgFileName.ext)
  
  ##  If that output file already exists, remove it:
  if(file.exists(dstfile)){ unlink(dstfile , recursive = TRUE, force = FALSE)}
  
  print(paste0("shapefile: ", shapefile))
  
  ##  Load the raster:
  rTmp <- raster(srcfile)
  ##  Load the shapefile:
  state.sub <- readOGR(shapefile, layer="out")
  ##  Crop the raster based upon the shapefile:
  rTmp.sub <- crop(rTmp, extent(state.sub))
  ##  Mask out any part of the raster which is not in the shapefile:
  rTmp.sub <- mask(rTmp.sub, state.sub)
  
  ##  Write this processed raster to disk:
  writeRaster(rTmp.sub, 
              filename=dstfile,
              format="GTiff", 
              overwrite=TRUE, 
              NAflag=NoDataValue, 
              options=c("COMPRESS=LZW")
  )  
  
  ##  Clean up the environment to save memory:
  rm(rTmp)
  rm(rTmp.sub)
  gc()
  
  ##  If the destination file exists:
#  if(file.exists(dstfile)){
#    ##  Remove the source file:
#    unlink(srcfile , recursive = TRUE, force = FALSE)
#    ##  Rename the the destination file to what the src file was:
#    file.rename(dstfile, srcfile)
#  }
  
}

##################################################################################



create_shape <- function(rasterFile,filePathOutputSHP) {
  ##  Passes a system call to gdal to create a polygon shapefile based upon a 
  ##  raster input.  
  system(
    paste(rfg.python_path,' ',rfg.gdal_polygonize_path,
          ' ',rasterFile,
          paste(' -f ','\"','ESRI Shapefile','\"',sep=''),
          paste0(" ",filePathOutputSHP,"")
    ), ignore.stdout = TRUE, ignore.stderr = FALSE) 
}

###############################################################################



create.shp.by.adminid <- function(iso3c){
  ##  Creates a shapefile based upon specified admin IDs.
  logdebug(paste0("Creating a shape file for  ",iso3c, " based on admin ID from input file"))
  
  ##  Declare the subdirectory for the country specific output shapefile:
  subDir.country.shp <- paste0(root_path,"/","output/", iso3c,"/shp/")
  ##  If that directory does not exist, create it:
  if(!file.exists(subDir.country.shp)){ 
    dir.create(subDir.country.shp, recursive = TRUE, showWarnings = FALSE)
    } 
  
  ##  Declare the path of the level 1 admin id raster:
  census.zones <- paste0(root_path,"/","data/",
                         iso3c,"/",
                         paste0(tolower(iso3c),"_grid_100m_","ccidadminl1.tif"))
  
  ##  Declare the path of the output subset level 1 admin id raster:
  census.zones.only.adms <- paste0(root_path,"/","output/",
                                   iso3c,"/shp/",
                                   paste0(tolower(iso3c),"_grid_100m_",
                                          "ccidadminl1_adms.tif"))
  
  ##  Retrieve the specified admin IDs for subsetting:
  adminidlist <- rfg.input.adminids[[iso3c]]

  
  #  gdal_calc.py 
  #  -A /scratch/mb4/split_cont/Data/ccid100m.tif 
  #  --outfile=/scratch/mb4/split_cont/Data/ccid100m_FJI.tif 
  #  --calc="(A==242)*1 + (A<>242)*9999" 
  #  --NoDataValue=9999 --overwrite --type=Uint16 --co="COMPRESS=LZW" 
  
  #python "C:\Program Files (x86)\GDAL\gdal_calc.py" 
  #  -A G:\WorldPop_Data\RandomeForest\data\BTN\Census\Derived\census_zones.tif 
  #  --outfile="G:\WorldPop_Data\RandomeForest\data\BTN\Census\Derived\census_zones_ID.tif" 
  #  --calc="1*any([A==645480,A==645478,A==645485],axis=0)" 
  #  --NoDataValue="0" 
  #  --co="COMPRESS=LZW"  
  
  #NoDValue <- .rasterInfo(cvr)
  #NoDValue <- 255
  
  #adminID <- strsplit(adminidlist, ",")
  
  ##  Construct the gdal_calc command as a string:
  str.crop <- paste(rfg.python_path,' ',rfg.gdal_calc_path ,' -A ',census.zones,
                    ' --outfile=\"',census.zones.only.adms,
                    '\" --calc=\"1*any([', sep='')
  ##  For item the admin ID list:
  for (fg in 1:length(adminidlist)) {
    ##  If this is the first iteration:
    if(fg == 1) {
      ##  Set the subsetting parameter to:
      str.crop <- paste0(str.crop, 'A==', adminidlist[fg])  
    }else{
      ##  Set the subsetting parameter to:
      str.crop <- paste0(str.crop, ',A==', adminidlist[fg])
    }
  }  
  
  # for (fg in 1:length(adminID[[1]])) {
  #   
  #   if(fg == 1) {
  #     str.crop <- paste0(str.crop, 'A==', adminID[[1]][fg])  
  #   }else{
  #     str.crop <- paste0(str.crop, ',A==', adminID[[1]][fg])
  #   }
  # }    
  
  ##  Add the output parameters to the string command:
  str.crop <- paste0(str.crop, 
                     '],axis=0)\" --NoDataValue=\"0\" --co=\"COMPRESS=LZW\"')
  
  ##  Run the gdal_calc command and print any output messages to console:
  print(system(str.crop, ignore.stdout = FALSE, ignore.stderr = FALSE))  
  
  ##  Create a polygon shapefile of the resultant raster subset which was 
  ##  carried out by gdal_calc:
  create_shape(census.zones.only.adms,paste0(root_path,"/","output/", iso3c,"/shp"))
  
  if (!rfg.DologDEBUG){
    if(file.exists(census.zones.only.adms)){ 
      unlink(census.zones.only.adms , recursive = TRUE, force = FALSE)
      }    
  }
  
  
  # 
  # 
  #   str.crop <- paste(gdal_calc_path,' -A ',cvr,' --outfile=\"',cvr.out,'\" --calc=\"logical_or(', sep='')
  # 
  # for (fg in 1:length(adminID[[1]])) {
  #   print(adminID[[1]][fg])
  #   
  #   if(fg ==1) {
  #     str.crop <- paste0(str.crop, 'A==', adminID[[1]][fg])  
  #   }else{
  #     str.crop <- paste0(str.crop, ',A==', adminID[[1]][fg])
  #   }
  # }  
  # 
  # str.crop <- paste0(str.crop,')*1+logical_or(')
  # 
  # for (fg in 1:length(adminID[[1]])) {
  #   print(adminID[[1]][fg])
  #   
  #   if(fg ==1) {
  #     str.crop <- paste0(str.crop, 'A<>', adminID[[1]][fg])  
  #   }else{
  #     str.crop <- paste0(str.crop, ',A<>', adminID[[1]][fg])
  #   }
  # }    
  # 
  # str.crop <- paste0(str.crop, ')*255\" --NoDataValue=\"',NoDValue, '\" --co=\"COMPRESS=LZW\"')
  # print(str.crop) 
  # 
  # print(system(str.crop, ignore.stdout = FALSE, ignore.stderr = FALSE))  
}

###############################################################################



prep_rst_cvr_adminid <- function(){
  ##  Prepares raster covariates for subsetting by admin ID.
  
  ##  Create a copy of covariates list for the outputs:
  out <- covariates   
  
  ##  For every country in the input countries:
  for ( icountry in rfg.input.countries ) { 
    ##  Create a shapefile of the admin IDs which we declared as being of 
    ##  interest in the input.R file:
    create.shp.by.adminid(icountry)
    
    ##  For every covariate we are using:
    for ( icvr in 1:length(covariates[[icountry]]) ) {
      ##  Retrieve the variable name
      var_name <- names(covariates[[icountry]][icvr])
      ##  Retrieve the corresponding object from the covariates object:
      covariate <- covariates[[icountry]][[icvr]]
      ##  Retrieve the corresponding summary information:
      dataset_summary <- covariate$dataset_summary
      ##  Retrieve the corresponding dataset class information:
      var_name_class <- covariate$dataset_class
      ##  Retrieve the corresponding path to the covariate data itself:
      raster_path <- covariate$path
      ##  Retrieve the raster file name of the covariate data:
      dataset_filename <- covariate$dataset_filename 
      
      ##  Load the covariate data:
      dataset_raster <- raster(raster_path)    
      # print(paste0("var_name", var_name))
      # print(paste0("covariate", covariate))
      # print(paste0("dataset_summary", dataset_summary))
      # print(paste0("raster_path", raster_path))
      # print(paste0("dataset_raster", dataset_raster))
      
      logdebug(paste0("Croping  ",
                      raster_path, 
                      " by adminID selected from input file"))
      
      ##  Retrieve the No Data value for that specific raster:
      NoDValue <- .rasterInfo(raster_path)
      #print(paste0("dataset_raster: ", NoDValue$NoDataValue))
      
      # Path to a folder where croped covariate will be stored
      distDir <- paste0(root_path,"/","output/", icountry , "/croped/")
      
      ##  Declare the country specific output shapefile location:
      subDir.country.shp <- paste0(root_path,"/","output/", icountry,"/shp/out.shp")
      ##  Crop the full covariate raster by that country specific area of 
      ##  interest shapefile, maintaining the same No Data value as the input 
      ##  raster:
      crop_raster_R(raster_path,
                    subDir.country.shp, 
                    distDir,
                    NoDValue$NoDataValue)
      
      
      out[[icountry]][[var_name]]$dataset_folder <- distDir 
      out[[icountry]][[var_name]]$path <- paste0(distDir, dataset_filename) 
      
    }
    
    # Crop Census raster for each country
    # zonal_raster_path <- paste0(root_path,"/","data","/",icountry,"/",paste0(tolower(icountry),"_grid_100m_","ccidadminl1.tif"))
    # NoDValue <- .rasterInfo(zonal_raster_path)
    # crop_raster_R(zonal_raster_path,subDir.country.shp,NoDValue$NoDataValue)    
    # 
    # # Crop water_mask raster for each country
    # water_mask_raster_flname <- paste0(tolower(icountry),"_grid_100m_","cciwat_cls.tif")
    # water_mask_raster_path <-paste(water_mask_raster, paste0(root_path,"/","data","/",icountry,"/",water_mask_raster_flname), spr=' ') 
    # NoDValue <- .rasterInfo(water_mask_raster_path)
    # crop_raster_R(water_mask_raster_path,subDir.country.shp,NoDValue$NoDataValue)     
  }
  

  ##  Return out:
  return(out)  
}  

###############################################################################



prep_rst_cvr_shp <- function(){
  ##  Prepare raster covariates for subsetting by a user supplied shapefile:
  
  ##  Create a copy of covariates list for the outputs:
  out <- covariates 
  ##  For every country in the input countries:
  for ( icountry in rfg.input.countries ) { 
    ##  For every covariate:
    for ( icvr in 1:length(covariates[[icountry]]) ) {
      
      ##  Retrieve the variable name:
      var_name <- names(covariates[[icountry]][icvr])
      ##  Retrieve the covariate attributes from the main list:
      covariate <- covariates[[icountry]][[icvr]]
      ##  Retrieve the corresponding dataset summary:
      dataset_summary <- covariate$dataset_summary
      ##  Retrieve the corresponding dataset class:
      var_name_class <- covariate$dataset_class
      ##  Retrieve the raster path of the covariate data:
      raster_path <- covariate$path
      ##  Retrieve the raster file name of the covariate data:
      dataset_filename <- covariate$dataset_filename      
      
      ##  Load the actual covariate data raster:
      dataset_raster <- raster(raster_path)    
      
      logdebug(paste0("Croping  ",raster_path, " by SHP file from input file"))
      
      ##  Retrieve the No Data value for that raster:
      NoDValue <- .rasterInfo(raster_path)
      
      ##  Retrieve the path of the shapefile which was suppleid for subsetting:
      subDir.country.shp <- as.character(rfg.input.shp[icountry])
      
      # Path to a folder where croped covariate will be stored
      distDir <- paste0(root_path,"/","output/", icountry , "/croped/") 
      
      ##  Crop the covariate raster using that shapefile while maintaining the 
      ##  No Data value from the input raster:
      crop_raster_R(raster_path,subDir.country.shp, distDir ,NoDValue$NoDataValue)
      
      out[[icountry]][[var_name]]$dataset_folder <- distDir 
      out[[icountry]][[var_name]]$path <- paste0(distDir,dataset_filename) 
      
    }
    
    ##  Retrieve the values of the raster cells as a dataframe:
#    pix.values.df <- as.data.frame(
#      rasterToPoints(
#        raster(covariates[[icountry]][[bsgm.ccidadminl1]]$path)
#        ))
    ##  Retrieve the unique values of the cells:
#    pix.values.df.uq <- (unique(pix.values.df[3]))
    
    ##  Remove the complete set of values from memory:
#    rm(pix.values.df)
    
#    gc()
    
    ##  Subset the unique values to those which are not zero:
#    pix.values.df.uq.wn <- pix.values.df.uq[pix.values.df.uq != 0, ]
    
    ##  Remove the complete set of unique values from memory:
#    rm(pix.values.df.uq)
#    gc()  
    
    ##  PRint the unique values:
#    print(pix.values.df.uq.wn)
    
    ##  Sort those unique values and store them in a country specific manner 
    ##  within out:
#    out[[icountry]] <- (pix.values.df.uq.wn[order(pix.values.df.uq.wn)])
  } 
  
  ##  Return out:
  return(out)
}  
