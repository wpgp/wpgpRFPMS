##  This function checks for common, but not all, errors which could occur in 
##  the input.R file where user supplied parameters are stored and passed to the
##  rest of the program.
check_config_input <- function(){
  
  logdebug('Start checking an input file')
  ##  Check if the number of declared poptables matches the number of declared 
  ##  countries:
  if (!is.null(rfg.input.poptables)) {
    if ( as.integer(length(rfg.input.poptables)) != rfg.nb.countries){
      warning("Error: Number of local poptables is not equal total numbers of countries")
      return(FALSE)
    }
  }
  ##  Check if the number of admin ID vectors matches the number of declared 
  ##  countries:
  if (!is.null(rfg.input.adminids)) {
    if ( as.integer(length(rfg.input.adminids)) != rfg.nb.countries){
      warning("Error: Number of admunitid is not equal total numbers of countries")
      return(FALSE)
    }
  }  
  
  ##  Check if the user is trying to use both the admin ID subset and the 
  ##  shapefile subset options at the same time:
  if (!is.null(rfg.input.adminids) & !is.null(rfg.input.shp)) {
      warning("Error: rfg.input.adminids and rfg.input.shp parameters in input.R file can not be used both at the same time. One of them should be NULL")
      return(FALSE)
  }    
  
  ##  Check that the number of supplied shapefiles for subsetting matches the 
  ##  number of declared countries:
  if (!is.null(rfg.input.shp)) {
    if ( as.integer(length(rfg.input.shp)) != rfg.nb.countries){
      warning("Error: Number of shape files is not equal total numbers of countries")
      return(FALSE)
    }
    ##  For those supplied shapefiles, make sure they actually exist before 
    ##  starting:
    for ( icountry in rfg.input.countries )  {
      if(!file.exists(as.character(rfg.input.shp[icountry])) ){
        warning(paste0("Error: Shape files does not exist ",as.character(rfg.input.shp[icountry]) ))
        return(FALSE)        
      }  
    }  
  }
  
  # check if custom covariates exist 
  if (!is.null(rfg.input.custom.cvr)){
    for (j in names(rfg.input.custom.cvr)){
      if(!file.exists(as.character(rfg.input.custom.cvr[[j]]) ) ){
        warning(paste0("Error: Custom files does not exist ",as.character(rfg.input.custom.cvr[[j]]) ))
        return(FALSE)        
      }  
    }
  } 
  

  if (rfg.DologDEBUG) 
    show.output.on.console=TRUE
  
  # cheking if python installed
  logdebug('checking python')
  py_out <- system(paste(rfg.python_path,"-V",spr=" "), show.output.on.console=show.output.on.console)
  if (py_out != 0){
    warning("ERROR:  There's an error somewhere in your configuration! The system path to Python does not exist.")
    return(FALSE)
  }



  logdebug('checking gdal_merge')

  result = tryCatch({
    gdal_merge.version <- system( paste(rfg.gdal_merge,"--version",spr=" "), show.output.on.console=show.output.on.console)
  }, warning = function(w) {
    warning(w)
    warning("Could not get a version of gdal_merge. Check config.R file")
    return(FALSE)
  }, error = function(e) {
    warning(w)
    warning("Could not get a version of gdal_merge. Check config.R file")
    return(FALSE)
  }, finally = {
    #cat(gdal_merge.version)
  }
  )


# Chekc if gdal_calc is avalible
# 
  logdebug('checking gdal_calc')
  
  result = tryCatch({
    gdal_calc.version <- system( paste(rfg.gdal_calc,"-h",spr=" "), show.output.on.console=FALSE)
  }, warning = function(w) {
    warning(w)
    warning("Could not get gdal_calc. Check config.R file")
    return(FALSE)
  }, error = function(e) {
    warning(e)
    warning("Error: Could not get gdal_calc. Check config.R file")
    return(FALSE)
  }, finally = {
    #cat(gdal_calc.version)
  }
  )

  
  logdebug('checking rfg.fixed if rfg.fixed.set = TRUE in input')
  
  
  if (rfg.fixed.set){
    
    list.of.old.popfits.final <- list.files(paste0(rfg.data.old.popfits.final),
                                            pattern=paste0("\\.Rdata$"),
                                            full.names=TRUE) 

    if ( length(list.of.old.popfits.final) == 0 ){
      err_mess <- paste0('You used fixed_set in input but there is no old popfits.final in : ', rfg.data.old.popfits.final)
      warning(err_mess)
      return(FALSE)
    }

    list.of.old.popfits.quant <- list.files(paste0(rfg.data.old.popfits.quant),
                                            pattern=paste0("\\.Rdata$"),
                                            full.names=TRUE) 

    
    if ( length(list.of.old.popfits.quant) == 0 ){
      err_mess <- paste0('You used fixed_set in input but there is no old popfits.quant in : ', rfg.data.old.popfits.quant)
      warning(err_mess)
      return(FALSE)
    }    
    
    if ( length(list.of.old.popfits.quant) != length(list.of.old.popfits.final) ){
      err_mess <- paste0('Number of files popfits.final and popfits.quant for fix set is not equal. Please check : ', paste0(root_path, "/data/old_popfits/"))
      warning(err_mess)
      return(FALSE)
    }
  }
  
  

return(TRUE)
}