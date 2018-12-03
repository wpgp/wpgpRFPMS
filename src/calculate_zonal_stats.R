###############################################################################
##  This function allows for precision decimal formatting.
specify_decimal <- function(x, k) format(round(x, k), nsmall=k)




## Before calculating a zonal stat for country we  
## should check which zonal stats aval on FTP
checkAvalZoanlStatFTP <- function( ){
  
  df  <- list()
  
  for ( i in rfg.input.countries ) {
    df[[i]][['mean']] <- wpgpGetAvalZonalStats(i,stat='mean')
    df[[i]][['sum']] <- wpgpGetAvalZonalStats(i,stat='sum')    
  }
  
  return(df)
}


###############################################################################

checkExtendRaster <- function( r1, r2 ){
  #####
  # Checking extent of two rasters
  #
  xmin.r1 <- specify_decimal( bbox(r1)[1,1],5 )
  xmax.r1 <- specify_decimal( bbox(r1)[1,2],5 )
  ymin.r1 <- specify_decimal( bbox(r1)[2,1],5 )
  ymax.r1 <- specify_decimal( bbox(r1)[2,2],5 )
  
  xmin.r2 <- specify_decimal( bbox(r2)[1,1],5 )
  xmax.r2 <- specify_decimal( bbox(r2)[1,2],5 )
  ymin.r2 <- specify_decimal( bbox(r2)[2,1],5 )
  ymax.r2 <- specify_decimal( bbox(r2)[2,2],5 )
  
  if ( (xmin.r1) != (xmin.r2) | (xmax.r1) != (xmax.r2) | (ymin.r1) != (ymin.r2) | (ymax.r1) != (ymax.r2)  ) {
    return(FALSE)
  }else{
    return(TRUE)
  }
}

###############################################################################



changeExtendRaster <- function( rFrom, rToPath ){
  ##  Changing extent of two rasters using gdal_warp.
  xmin <- bbox(rFrom)[1,1]
  xmax <- bbox(rFrom)[1,2]
  ymin <- bbox(rFrom)[2,1]
  ymax <- bbox(rFrom)[2,2]
  
  rFileName <- basename(rToPath) 
  rPath <- dirname(rToPath)
  
  system(
    paste("gdalwarp -te ",
          ' ',xmin,
          ' ',ymin,
          ' ',xmax,
          ' ',ymax,
          ' ',rToPath,' ',
          paste0(rPath, "/", "tmp_",rFileName)
    ), ignore.stdout = FALSE, ignore.stderr = FALSE)   
  
  if(file.exists(rToPath)){ unlink(rToPath , recursive = TRUE, force = FALSE)}
  
  file.rename(from=paste0(rPath, "/", "tmp_",rFileName),to=rToPath)
}

###############################################################################



python_zonal <- function(census_file, stats="mean") {
  ##  Function runs zonal statistics through python.
  ##  Load the rPython package:
  pkgLoad('rPython')
  
  ##  Declare the path of the census zones shapefile from which we'll base our 
  ##  zonal summaries:
  shp_file = paste0(root_path,"/","data","/",country,"/",
                    "Census/Derived/census_zones.shp")
  
  ##  If that shapefile does not exist:
  if(!file.exists( shp_file )){
    ##  Go declare the census zoens tif which should exist:
    lRaster = paste0(root_path,"/","data","/",country,"/",
                     "Census/Derived/census_zones.tif")
    ##  Create a polygon shapefile based upon the zonal raster:
    dal_polygonizeR(lRaster, outshape=shp_file)
  }
  
  ##  Declare the type of statistical summary which should be performed:
  stat = tolower(stats)
  ##  If it is modal, perform the majority function:
  if (stat=="modal"){stat = "majority"}
  
  ##  In python, import functions from the package rasterstats and numpy:
  python.exec('from rasterstats import zonal_stats, point_query')
  python.exec('import numpy as np')
  
  ##  Declare our shapefile and census shapefile as variables which can be 
  ##  passed to Python:
  py_inp_x <- shp_file
  py_inp_y <- census_file	
  
  ##  Assign the values of those variables in the Python environment:
  python.assign( "py_inp_x", py_inp_x )
  python.assign( "py_inp_y", py_inp_y )	 
  python.assign( "stat", stat )	
  
  ##  Execute zonal stats within Python:
  python.exec('py_out2 = zonal_stats(py_inp_x, py_inp_y, geojson_out=True, stats=[stat])') 	
  python.exec('import json')
  ##  For every item in the zonal stats output, delete the geometry portion:
  python.exec("for element in py_out2:  del element['geometry']")
  ##  Retrieve the zonal stats from the Python environment back into the R 
  ##  environment:
  r_list <- python.get('py_out2')
  python.exec('del py_out2') 
  
  ##  Set up a matrix object:
  matrix.tmp <-matrix(nrow=0, ncol = 2)
  
  ##  For every admin id corresponding record in the zonal stats output:
  for(i in 1:length(r_list)){
    ##  Extract the admin ID and the corresponding statistical summary value and
    ##  attach to the matrix as a new row:
    matrix.tmp <- rbind(matrix.tmp,
                        c(as.integer(unlist(r_list[[i]]$properties['DN'])), 
                          unlist(r_list[[i]]$properties[stat])
                          )
                        ) 
  }	  
  
  #stats = toupper(stats)
  
  ##  Give the matrix some proper column names:
  colnames(matrix.tmp) <- c("ADMINID", stats)
  ##  Remove any duplicate admin IDs in the matrix:
  matrix.tmp <- subset(matrix.tmp, !duplicated(matrix.tmp[,1]))
  
  ##  Return the matrix:
  return(matrix.tmp)
}




###############################################################################

gdal_polygonizeR <- function(x, 
                             outshape=NULL, 
                             gdalformat = 'ESRI Shapefile',
                             pypath=NULL, 
                             readpoly=TRUE, 
                             quiet=TRUE){
  
  ## gdal_polygonizeR
  ## Getting rasters into shape from R
  ## John Baumgartner's Research
  ## https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/
  ##
  ##  This function turns a raster into a polygon via gdal
  
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}

###############################################################################

check.if.zs.calc.df.ftp <- function(icountry,df,var.name,zs_stat) {
  
  if (!zs_stat %in% c('sum', 'mean')) {
    stop("zs_stat argument in function check.if.zs.calc.df.ftp can be 'sum' or 'mean'")
  }  
  
  select_stat <- switch(zs_stat, "mean" = "ZS_mean", "sum" = "ZS_sum", "min" = "ZS_min" , "max" = "ZS_max")
  
  zs_file_name <- paste0(tolower(icountry),'_',var.name,'_',select_stat,'.csv')
  
  if (zs_file_name %in% df[[icountry]][[zs_stat]] ){
    return(TRUE)
  }else{
    return(FALSE)
  } 

}

###############################################################################

calculate.zonal.stats.country <- function(icvr,icountry,zonal_raster){
  ##  Calculates zonal stats for a given country and covariate given a zonal 
  ##  raster:
  #
  ## icvr - covariat id
  ## icountry - country name
  ## zonal_raster - zonal raster for a country
  #  
  ##  Retrieve the name of the covariate:
  var_name <- names(covariates[[icountry]][icvr])
  ##  Retrieve the corresponding attributes of the covariate:
  covariate <- covariates[[icountry]][[icvr]]
  ##  Retrieve the dataset summary:
  dataset_summary <- covariate$dataset_summary
  ##  Retrieve the corresponding dataset class:
  var_name_class <- covariate$dataset_class
  ##  Retrieve the raster path of the covariate:
  raster_path <- covariate$path
  
  ##  Load the actual covariate raster:
  dataset_raster <- raster(raster_path)
  
  ##  Explicitly retrieve what the covariate is meant to represent:
  covariates.var.names <- covariate$dataset_class

  ##  Declare the directory within which to place the zonal stats CSV:
  file.path <- paste0(root_path, "/output/", icountry, "/zonal_stats/csv/")
  ##  Declare the full path and filename of the output zonal summary:
  file.path.csv <- paste0(file.path,tolower(icountry),"_",var_name_class,"_ZS_mean.csv")
  
  ##  If zonal stats were not calculated prior for this country and covariate 
  ##  then calculate:
  if(!file.exists(file.path.csv )){
    cat("\n")
    logdebug( paste0("Working on ", var_name, " for ",icountry,' ',dataset_summary ))
    logdebug( paste0("Zonal stats has not been calculated before for a covariat ",var_name_class))
    

    #if (var_name_class %in% df.ftp$CvtName & !is.null(df.ftp[df.ftp$CvtName == var_name_class, ]$ZS_mean == TRUE) ){
    
    # Check if zonal stats is avalible on WP FTP
    # if not then > else
    if ( check.if.zs.calc.df.ftp(icountry,
                                 df.aval.zonal.stats.ftp,
                                 var_name_class,"mean"
                                 ) & is.null(rfg.input.shp)){   
    
       loginfo( paste0("Zonal stats for ",var_name_class,
                       " avalible on WP FTP. Downloading...")) 
      
       ##  Retrieve the zonal stats file from the FTP:
       output_stats <-  wpgpGetZonalStats(icountry, var_name_class , stat = "mean" ,file.path) 
       ##  Adjust the column names:
       colnames(output_stats) <- c("ADMINID", var_name_class)
       ##  Sort the stats:
       output_stats.sorted <-  output_stats[sort.list(output_stats[,1]), ]
       
       ##  If we have declared admin IDs we wish to subset our modeling area to:
       if (!is.null(rfg.input.adminids)){
         #output_stats.sorted <- output_stats.sorted[output_stats.sorted$ADMINID %in% rfg.input.adminids[[icountry]],]
         ##  Return only the sorted stats which correspond to those admin IDs:
         output_stats.sorted <- output_stats.sorted[output_stats.sorted$ADMINID 
                                                    %in% 
                                                    as.character(unlist(rfg.input.adminids[icountry])),]
       }  
    }else{
      ##  Check if zonal raster and covariates have the same extents 
      ##  before running zonal stats:
      if ( !checkExtendRaster( zonal_raster,dataset_raster )){ 
        cat("\n")
        loginfo("----------")
        logwarn(paste0("Warning: Zonal raster and covariates have different extends ", raster_path))
        logwarn(paste0("Warning: extends will be changed to match zonal_raster ", raster_path))
        loginfo("----------")
        changeExtendRaster(zonal_raster,raster_path)
        dataset_raster <- raster(raster_path)
      }       
      
      ##  Calculate Zonal stats using R default or Python if rfg.kPythonZonal
      ##  is TRUE:
      if ( rfg.kPythonZonal ) {
        output_stats <- python_zonal(raster_path, stats=dataset_summary)
      }else{
        #output_stats <- zonal(dataset_raster, zonal_raster, fun=dataset_summary)
        ##  Determine the minimum number of blocks needed for processing:
        minblks <- wpGetBlocksNeed (dataset_raster,rfg.cluster_workers, n=2)
        ##  Calculate the stats in parallel:
        output_stats <- wpZonalStatistics (dataset_raster, 
                                           zonal_raster, 
                                           fun=dataset_summary, 
                                           cores=rfg.cluster_workers, 
                                           minblk=minblks)  
      }
      ##  Adjust the column names:
      colnames(output_stats) <- c("ADMINID", var_name_class)
      ##  Sort the stats:
      output_stats.sorted <-  output_stats[sort.list(output_stats[,1]), ]    
      
      ## Saving zonal statiscs per country for each covariate:
      if (rfg.saveZonalStats){
        write.csv( as.data.frame(output_stats.sorted), file = file.path.csv, row.names=FALSE )
      }      
    }
  } else{
    ##  If the zonal stats already exist locally:
    cat("\n")
    logdebug(paste0("Working on ", var_name_class, " for ",icountry,' ',dataset_summary ))    
    logdebug("Zonal stats has been calculated before for a covariat. Loading...")
    ##  Read in the file:
    output_stats <- read.csv( file.path.csv ) 
    ##  Adjust column names:
    colnames(output_stats) <- c("ADMINID", var_name_class)
    ##  Sort the stats:
    output_stats.sorted <-  output_stats[sort.list(output_stats[,1]), ] 
  } 

  #return(output_stats.sorted)  
  ##  Return the stats which do not correspond to "admin ID 0":
  return(output_stats.sorted[output_stats.sorted[,1] != 0, ])
}

###############################################################################



sumzonal <- function(x, na.rm = TRUE) {
  sum(as.numeric(x))
}

###############################################################################



calculate.zonal.stats.rf.pred.sum <- function(){
  ##  Calculates the zonal stat sum for covriates to be used in the RF.
  
  ##  Declare the csv path of the zonal stats:
  file.path.csv <- paste0(rfg.output.path.countries,rfg.zonal.stats.rf.pred.csv)
  
  ##  If zonal stats were not calculated before for this contry and covariate 
  ##  then calculate:
  if(!file.exists(file.path.csv )){
    logdebug( paste0("Zonal stats has not been calculated before for ",rfg.zonal.stats.rf.pred.csv))
    ##  Load the covariate raster:
    dataset_raster <- raster(paste0(rfg.output.path.countries,
                                    rfg.predict.density.rf.pred))
    ##  Load the zonal raster:
    zonal_raster <- raster(censusmaskPathFileName)

    #output_stats <- zonal(dataset_raster, zonal_raster, fun='sum')  
    
    ##  Calculate the zonal stats:
    output_stats <- wpZonalStatistics(dataset_raster, 
                                      zonal_raster, 
                                      fun='sum', 
                                      cores=rfg.cluster_workers)
    
    ##  Sort the zonal stats:
    output_stats <-  output_stats[sort.list(output_stats[,1]), ]
    
    ##  Write the zonal stats to a CSV:
    write.csv( as.data.frame(output_stats),
               file = file.path.csv,
               row.names=FALSE )
  } else{
    ##  If they already exist, just read them from the CSV file:
    logdebug(paste0("Zonal stats has been calculated before for",
                    rfg.predict.density.rf.pred,' Loading...' ))    
    output_stats <- read.csv( file.path.csv ) 
  } 
  ##  Adjust column names:
  colnames(output_stats) <- c("ADMINID", "sum")
  ##  Sort the stats by Admin ID:
  output_stats <-  output_stats[sort.list(output_stats[,1]), ] 
  
  ##  Return the zonal stats excluding "Admin ID 0":
  return(output_stats[output_stats[,1] != 0, ])
}

###############################################################################
