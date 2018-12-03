###############################################################################
###############################################################################


get_df_for_mergine_rst_cvr <- function( ){
  ##  If we have multiple countries then this subroutine will return a data frame
  ##  with covariates and raster paths for each covariate for all countries. It 
  ##  will help to merge rster for each covariate.
  
  emptyDfclassPath <- data.frame(class = character(), path = character()) 
  # adminIdPath <- ""
  # water_mask_raster <- ""
  
  icount = 0
  for ( icountry in rfg.input.countries )  {
    icount = (icount+1)  
    

    emptyDfclassPathTmp <- data.frame(class = character(), path = character()) 
    emptyDfclassFolder <- data.frame(class = character(), 
                                     folder = character(), 
                                     summary= character(),
                                     NoDataValue = character(),
                                     GDType = character() 
                                     ) 
    ##  For every covariate within that country:
    for ( i in 1:length(covariates[[icountry]]) ) {
      ## loop over each covariate for a country to calculate zonal stats
      covariate       <- covariates[[icountry]][[i]]
      dataset_path    <- as.character(covariate$path)
      dataset_folder  <- as.character(covariate$dataset_folder)
      dataset_class   <- as.character(covariate$dataset_class)
      dataset_summary <- as.character(covariate$dataset_description)
      
      vrt.gdalinfo <- .rasterInfoGDType(dataset_path)
      rs.NoDataValue <- vrt.gdalinfo$NoDataValue
      rs.GDType <- vrt.gdalinfo$GDType
      
      dataset_NoDataValue   <- as.character(rs.NoDataValue)
      dataset_GDType <- as.character(rs.GDType)      
      
      emptyDfclassPathTmp <- rbind(emptyDfclassPathTmp, data.frame( class = dataset_class, path = dataset_path ))
      
      emptyDfclassFolder <- rbind(emptyDfclassFolder, data.frame( class = dataset_class, 
                                                                  folder = dataset_folder, 
                                                                  summary = dataset_summary,
                                                                  NoDataValue = dataset_NoDataValue,
                                                                  GDType = dataset_GDType
                                                                  ))        
    }
    
    if (icount == 1){
      emptyDfclassPath <- emptyDfclassPathTmp
    }else{
      emptyDfclassPath <- merge(as.data.frame(emptyDfclassPath), as.data.frame(emptyDfclassPathTmp), by="class", sort=FALSE)  
      
    } 
    #
    # If we havew more then one country we need to save a path to census_zones for later mergin this files for RF
    #
    # zonal_raster_flname <- paste0(tolower(icountry),"_grid_100m_","ccidadminl1.tif")
    # adminIdPath <-paste(adminIdPath, paste0(root_path,"/","data","/",icountry,"/",zonal_raster_flname), spr=' ')
    # 
    # 
    # water_mask_raster_flname <- paste0(tolower(icountry),"_grid_100m_","cciwat_cls.tif")
    # water_mask_raster <-paste(water_mask_raster, paste0(root_path,"/","data","/",icountry,"/",water_mask_raster_flname), spr=' ')
    
  } 
  
  
  Dfclass <- data.frame(class = character(), 
                        folder = character(), 
                        summary= character(), 
                        path = character(),
                        NoDataValue = character(), 
                        GDType = character() 
                        ) 
  
  
#** final output folder is based if it we used shp or admin for project
  # if we using shp or admin as input and one country then 
  # final input data will be /output/ISO3
  #
  # if we using shp or admin as input and multiple country then 
  # after merging final input data will be /output/prj_year_ISO3...

  
  for(i in 1:nrow(emptyDfclassFolder)) {
    pat <- ''
    ds <- emptyDfclassPath[emptyDfclassPath$class == as.character(emptyDfclassFolder$class[[i]]), ] 
    
    for(ids in names(ds)){
      if (ids!='class'){
        pat <-paste(pat, as.character(ds[[ids]]), spr=' ')
      }  
    }

        
#**>    
#        if (!is.null(rfg.input.shp)  | !is.null(rfg.input.adminids)){ 
          if ( length(rfg.input.countries ) > 1){
            fnl.dir <- rfg.output.path.countries.tmp
          }else{
            fnl.dir <- as.character(emptyDfclassFolder$folder[[i]])
          }
#        }      
    
    
    Dfclass <- rbind(Dfclass, data.frame(  class = as.character(emptyDfclassFolder$class[[i]]),  
                                           folder = fnl.dir, 
                                           summary = as.character(emptyDfclassFolder$summary[[i]]),
                                           path = pat,
                                           NoDataValue = as.character(emptyDfclassFolder$NoDataValue[[i]]),
                                           GDType = as.character(emptyDfclassFolder$GDType[[i]])
    ))  
    
  }
  
  # Dfclass <- rbind(Dfclass, data.frame(class = "ccidadminl1",  
  #                                      folder = rfg.data.path.countries, 
  #                                      summary = "AdminId",
  #                                      path = adminIdPath ))  
  # 
  # Dfclass <- rbind(Dfclass, data.frame(class = "cciwat_cls",  
  #                                      folder = rfg.data.path.countries, 
  #                                      summary = "WaterMask",
  #                                      path = water_mask_raster ))   
  
  return(Dfclass)
  
}


###############################################################################
###############################################################################

merging_rst_cvr_multy_country <- function(mode="single"){
  ## Merge rasters used on df from get.df.for.mergine.rst.cvr () 
  ## mode can be "single" or "cluster"

  df.for.mergine <- get_df_for_mergine_rst_cvr() 
  
  loginfo("Start merging rasters in a stack for RF ... ")
    
  if (mode == "cluster"){

    cl <- makeCluster(rfg.cluster_workers)
    registerDoParallel(cl)

    clusterExport(cl, "rfg.data.path.countries")
    clusterExport(cl, "rfg.gdal_merge_path")
    clusterExport(cl, "rfg.python_path")
    clusterExport(cl, "rfg.output.path.countries.cvr")

    r <- foreach(i=1:nrow(df.for.mergine) ) %dopar% {
      
      fileOutput <- paste0( as.character( df.for.mergine[i,"class"] ),".tif" )
      
      filePathOutput <- rfg.output.path.countries.cvr
      
      if(!file.exists(filePathOutput)){ dir.create(filePathOutput, recursive = TRUE, showWarnings = FALSE)}
      
      rs.NoDataValue <- as.character(df.for.mergine[i,"NoDataValue"])
      rs.GDType <- as.character(df.for.mergine[i,"GDType"])
      
      print(system(
        paste(rfg.python_path,' ',rfg.gdal_merge_path,
              ' -ot ', rs.GDType ,' -a_nodata ', rs.NoDataValue ,' -of GTiff -init ', rs.NoDataValue , 
              paste('-co',' ','COMPRESS=LZW',' ',sep=''),
              paste('-co',' ','BIGTIFF=YES',' ',sep=''),
              paste('-o',' ',filePathOutput,fileOutput,' ',sep=''),
              as.character( df.for.mergine[i,"path"] ) ), ignore.stdout = FALSE, ignore.stderr = FALSE)     )
      
      
    }
    stopCluster(cl) 

    
  }  # end if mode single
  else 
  {

    for(i in 1:nrow(df.for.mergine)) {
      
      fileOutput <- paste0( as.character( df.for.mergine[i,"class"] ),".tif" )
      
      filePathOutput <- paste0( rfg.output.path.countries.cvr )
      
      if(!file.exists(filePathOutput)){ dir.create(filePathOutput, recursive = TRUE, showWarnings = FALSE)}
      
      
      logdebug(paste0("Mergin "," ", as.character( df.for.mergine[i,"path"])))
      logdebug(paste( "To"," ",filePathOutput,fileOutput," ",sep=""))
      logdebug(paste('----------------------------------------------',sep=''))
      
      
      rs.NoDataValue <- as.character(df.for.mergine[i,"NoDataValue"])
      rs.GDType <- as.character(df.for.mergine[i,"GDType"])      
      
      system(
        paste(rfg.python_path,' ',rfg.gdal_merge_path,
              ' -ot ', rs.GDType ,' -a_nodata ', rs.NoDataValue ,' -of GTiff -init ', rs.NoDataValue , 
              paste('-co',' ','COMPRESS=LZW',' ',sep=''),
              paste('-co',' ','BIGTIFF=YES',' ',sep=''),
              paste('-o',' ',filePathOutput,fileOutput,' ',sep=''),
              as.character( df.for.mergine[i,"path"] ) ), ignore.stdout = FALSE, ignore.stderr = FALSE)     
      
    }  
    
  }
  
  loginfo("Finished merging rasters in a stack for RF ... ")
  
}


###############################################################################
###############################################################################

create_covariates_list_for_RF <- function(){
  
  covariates.new <- list()
  
  df.for.mergine <- get_df_for_mergine_rst_cvr() 
  
  logdebug("Creating a new covariates list for RF ... ")
  
  for(i in 1:nrow(df.for.mergine)) {
    
    class.t <- as.character( df.for.mergine[i,"class"] )
    
    summary.t <- as.character( df.for.mergine[i,"summary"] )
    
    
    # removing AdminId and Watermask from a covariates list
    #if ((class.t != rfg.water.mask) | (class.t != rfg.ccidadminl1)){
      
      if ( length(rfg.input.countries) > 1 ) { 
        filePathOutput <- paste0( rfg.output.path.countries.cvr,"/" )
        fileOutput <- paste0( as.character( df.for.mergine[i,"class"] ),".tif" )  
        fileNamePath <- paste0(filePathOutput,fileOutput)  
        
        folder.t <- rfg.output.path.countries.cvr
      } 
      else
      {
        folder.t <- as.character( df.for.mergine[i,"folder"] )
        fileNamePath <- as.character( df.for.mergine[i,"path"] )
      }          
      
      covariates.new[[ class.t ]] <- list(
        dataset_folder      = folder.t,
        dataset_name        = class.t,
        dataset_description = as.character(covariates[[1]][[class.t]][['dataset_description']]),
        dataset_summary     = summary.t,
        path     = fileNamePath
      )   
      
    #}
    
  }

  
  return(covariates.new)
}

###############################################################################
###############################################################################


 