##  Download the covariates from database 
Download_Covariates <- function(){
  ##  If we have logging turned on:
  if (rfg.DologDEBUG) 
    d.quiet=FALSE
  else
    d.quiet=TRUE
  
  loginfo("Download Covariates files from WP FTP if they dont exist already ")
  ##  For every country declared:
  for ( i in rfg.input.countries ) {
    ##  Retrieve a list of the available covariates:
    df.ListCountryCovariates <- wpgpListCountryDatasets(ISO3=i)
    
    ##  For every covariate the user declared to be of interest in the input.R 
    ##  file:
    for ( j in rfg.input.cvr ) {
      ##  Retrieve only the covariate which name matches the current covariate 
      ##  of iteration:
      df.filtered <- df.ListCountryCovariates[df.ListCountryCovariates$Covariate == j,]
      
      ##  Declare the folder where we will store it
      cvr.folder <-  paste0(root_path,"/","data/", i)
      ##  Declare the name of the covariate raster we will save it as:
      cvr.rst.name <-  paste0(tolower(i),"_",df.filtered$Covariate,".tif")
      ##  Retrieve the covariate name:
      cvr.name <-  df.filtered$Covariate
      ##  Retrieve the covariate description:
      cvr.description <-  df.filtered$Description
      
      ##  I that file does not already exist locally:
      if(!file.exists(paste0(cvr.folder,"/",cvr.rst.name))){ 
        logdebug( paste0('Start downloading Covariate: ',j," for ",i))
        ##  Download it:
        df.download <- wpgpGetCountryDataset(ISO3=i,
                                             covariate=j,
                                             destDir=cvr.folder,
                                             quiet=d.quiet)    
      }else{
        ##  Otherwise note it and pass:
        logdebug( paste0(' Covariate: ',j," for ",i,
                         " already exists in ",cvr.folder))
      }
      
      ##  Assemble a covariates list object which will store the associated 
      ##  metadata and path for each covariate:
      covariates[[i]][[j]] <- list(
        dataset_folder      =  cvr.folder,
        dataset_filename    =  cvr.rst.name,
        dataset_description =  cvr.description,
        dataset_summary     =  "mean",
        dataset_country     = i,
        dataset_class       = j
      )
      covariates[[i]][[j]][["path"]] <- paste0(cvr.folder,"/",cvr.rst.name)
    }

    ##  Download water mask and zonal (L1):
    
    # if we are working with a shp file then we have to recalculate zonal stats for px_area
    # therefore we need to add it to a list of input covariates to download and then 
    # crop it by shape file 
    #
    if (!is.null( rfg.input.shp )){
      adm.cvr <- c(rfg.water.mask,rfg.ccidadminl1,"px_area_100m") 
    }else{
      adm.cvr <- c(rfg.water.mask,rfg.ccidadminl1) 
    }      
    
    for ( k in adm.cvr ) {
      cvr.folder <-  paste0(root_path,"/","data/", as.character(i))
      cvr.rst.name <-  paste0(tolower(i),"_",as.character(k),".tif")
      cvr.name <-  as.character(k)
      cvr.description <-  as.character(k)
      
      if(!file.exists(paste0(cvr.folder,"/",cvr.rst.name))){ 
        
        logdebug( paste0('Start downloading Covariate: ',k," for ",i))
        
        wpgpGetCountryDataset(ISO3=i,
                              covariate=as.character(k),
                              destDir=cvr.folder,
                              quiet=d.quiet)
        
        covariates[[i]][[k]] <- list(
          dataset_folder      =  cvr.folder,
          dataset_filename    =  cvr.rst.name,
          dataset_description =  as.character(k),
          dataset_summary     =  "sum",
          dataset_country     =  as.character(i),
          dataset_class       =  as.character(k)
        )     
        
        covariates[[i]][[k]][["path"]] <- paste0(root_path,"/","data/", i,
                                                 "/",paste0(tolower(i),
                                                            "_",
                                                            as.character(k),
                                                            ".tif"))        
        
      } else{
        
        logdebug( paste0(' Covariate: ',j," for ",i,
                         " alsready exists in ",cvr.folder))
        
        covariates[[i]][[k]] <- list(
          dataset_folder      =  cvr.folder,
          dataset_filename    =  cvr.rst.name,
          dataset_description =  as.character(k),
          dataset_summary     =  "sum",          
          dataset_country     =  as.character(i),
          dataset_class       =  as.character(k)
        )            
        covariates[[i]][[k]][["path"]] <- paste0(root_path,"/","data/", i,"/",
                                                 paste0(tolower(i),
                                                        "_",
                                                        as.character(k),".tif"))        
      } 
    }  # End of downloading water mask and L1
  }  
  

  
  ##  Return the covariates object:
  return(covariates) 
}


###
### Add Custom covariates 
###
add.custom.covariates <- function(){
  
  for ( i in rfg.input.countries ) {
    
    for (j in names(rfg.input.custom.cvr)){
      
    covariates[[i]][[j]] <- list(
      dataset_folder      =  dirname(as.character(rfg.input.custom.cvr[[j]])),
      dataset_filename    =  j,
      dataset_description =  as.character(j),
      dataset_summary     =  "mean",
      dataset_country     =  as.character(i),
      dataset_class       =  as.character(j),
      path                =  as.character(rfg.input.custom.cvr[[j]])
    )
    
    }
  } 
  
  return(covariates)
}  


###
# updated rfg.input.cvr variable with a custom covariates
# if specified in input file
###
update.cvr.list.custom <- function(){
  
  if (!is.null(rfg.input.custom.cvr)){
    for (i in names(rfg.input.custom.cvr)){
      logdebug( paste0('updating rfg.input.cvr variable with a custom covariate: ',i))
      rfg.input.cvr[[ length(rfg.input.cvr) + 1 ]] <- as.character(i)
    }
    return(rfg.input.cvr)
  }
}  


# get.covariates.var.names <- function( ){
#   
#   covariates.var.names <- c()
#   
#   for ( icvritm in 1:length(covariates[[1]]) ) { 
#     covariate <- covariates[[1]][[icvritm]]
#     var_name_class <- covariate$dataset_class
#     covariates.var.names <- c(covariates.var.names, var_name_class)
#   }    
#   
#   sort(covariates.var.names)
#   return(covariates.var.names)
# }
