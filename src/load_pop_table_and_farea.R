##########################################################################

load.pop.table <- function(icountry){

  if (!is.null(rfg.input.poptables)){
    
    if(!file.exists(as.character(rfg.input.poptables[icountry])) ){
      logwarn(paste0("Trying to open POP TABLE for ",icountry," from file ", rfg.input.poptables[icountry]))
      stop(paste("Can not load a POP TABLE. Please check your inputs in input.R", sep=""))
      return(FALSE)
    }  
    loginfo(paste0("Will load POP TABLE for ",icountry," from a local file ",rfg.input.poptables[icountry]))
    df <- as.data.frame(read.dbf(rfg.input.poptables[icountry]))    
    return(df)
    
  }else{
    
    # if we alreay have the POP table downloaded then we just load it
    file_local <- paste0(rfg.output.path.countries.tmp,'/', tolower(icountry),'_population_2000_2020.csv')
    
    if(file.exists(file_local) ){
      df <- utils::read.csv(file_local, stringsAsFactors=FALSE,header = TRUE)
      df <- df[ c('GID', paste0('P_',rfg.input.year)) ]
      colnames(df) <-  c("ADMINID", "ADMINPOP") 
      return(df)    
    }  

    # if we do not have the POP table downloaded then we will download from WP FTP
    loginfo(paste0("Will load POP TABLE for ",icountry," from WP FTP "))
    
    df <- wpgpGetPOPTable(ISO3 =icountry, 
                                 year = rfg.input.year,
                                 destDir = rfg.output.path.countries.tmp
    )
    colnames(df) <- c("ADMINID", "ADMINPOP")
    return(df)
  }
}

##########################################################################

load.pop.farea <- function(icountry){
  
  
  
  ##  Declare the directory within which to place the zonal stats CSV:
  file.path <- paste0(root_path, "/output/", icountry, "/zonal_stats/csv/")
  ##  Declare the full path and filename of the output zonal summary:
  file.path.csv <- paste0(file.path,tolower(icountry),"_px_area_100m_ZS_sum.csv") 
  
  logdebug(paste0("Looking for F_AREA for ",icountry)) 
  
  
  if(file.exists(file.path.csv) ){
    
    logdebug(paste0("Zonal stats for a covariat. ",icountry, " is avalible locally. Loading...")) 
    
    df <- utils::read.csv(file.path.csv, stringsAsFactors=FALSE,header = TRUE)
    
  }else{
    
    logdebug(paste0("Zonal stats has NOT been calculated or downloaded before for a covariat. ",icountry)) 
    

    if (check.if.zs.calc.df.ftp(icountry, df.aval.zonal.stats.ftp, "px_area_100m", "sum") ){
      
      loginfo( paste0("Zonal stats sum for ","px_area",
                      " avalible on WP FTP. Downloading...")) 
      
      df <-  wpgpGetZonalStats(ISO3 =icountry, 
                               covariate = "px_area_100m", 
                               stat = "sum", 
                               destDir = file.path
      )
      
    }else{
      
      loginfo( paste0("Zonal stats sum for ","px_area",
                      " is NOT avalible on WP FTP.")) 
      
      cvr.folder <-  paste0(root_path,"/","data/", icountry)
      cvr.rst.name <-  paste0(tolower(icountry),"_px_area_100m.tif")
      
      if(!file.exists(paste0(cvr.folder,"/",cvr.rst.name))){ 
        
        logdebug( paste0('Start downloading Covariate: px_area for ',icountry))
        
        wpgpGetCountryDataset(ISO3=icountry,
                              covariate="px_area_100m",
                              destDir=cvr.folder
        )
      }
      
      logdebug( paste0('Start calculation of px_area zonal stats sum for ',icountry))
      
      dataset_raster <- raster(paste0(cvr.folder,'/',cvr.rst.name))
      
      zonal_raster_path <- covariates[[icountry]][[rfg.ccidadminl1]][["path"]]
      ##  Bring in the zonal raster:
      zonal_raster <- raster(zonal_raster_path) 
      
      minblks <- wpGetBlocksNeed (dataset_raster,rfg.cluster_workers, n=2)
      ##  Calculate the stats in parallel:
      output_stats <- wpZonalStatistics (dataset_raster, 
                                         zonal_raster, 
                                         fun="sum", 
                                         cores=rfg.cluster_workers, 
                                         minblk=minblks)  
      
      ##  Adjust the column names:
      colnames(output_stats) <- c("ADMINID", "F_AREA")
      ##  Sort the stats:
      output_stats.sorted <-  output_stats[sort.list(output_stats[,1]), ]    
      
      ## Saving zonal statiscs per country for each covariate:
      if (rfg.saveZonalStats){
        write.csv( as.data.frame(output_stats.sorted), file = file.path.csv, row.names=FALSE )
      }
      
      df <- output_stats.sorted
      
    }  
    
  }
  
  
  
  # I have add this check as I found some Nan in zonal stats
  # Need to be checked why we are getting this Nan
  #
  if ( check.df.has.na(df) ){
    cat("\n")
    logwarn("----------")
    logwarn("----------")
    logwarn(paste0("F_AREA has NAN valu pleas check the file ", icountry))
    logwarn(paste0("Please check  F_AREA csv file ", file.path.csv))
    logwarn(paste0("The Nan values will be replaced by 1."))
    logwarn("----------")
    logwarn("----------")
    df[is.na(df)] <- 10000
  }
  
  colnames(df) <- c("ADMINID", "F_AREA")
  
  return(df[df$ADMINID != 0, ])   
}

##########################################################################

load.pop.table.and.farea <- function(icountry){

  df <-  merge( as.data.frame(load.pop.table(icountry)), 
                as.data.frame(load.pop.farea(icountry)), 
                by="ADMINID", 
                sort=FALSE)
        
  if (!is.null(rfg.input.adminids)){
    #df <- df[df$ADMINID %in% rfg.input.adminids[[icountry]],]
    df <- df[df$ADMINID %in%  as.character(unlist(rfg.input.adminids[icountry])),]
  }
  
  return(df)

}

##########################################################################
#  if shape file is used then we need to calculate a zonal sum for px_area 
# if not we will download a zoanl stats for this country from FTP
#
get.pop.table.and.farea <- function(icountry, df){
  
  if (!is.null(rfg.input.shp)){
    
    cvr.zn.id <- raster(covariates[[ icountry ]][[rfg.ccidadminl1]][["path"]])
    cvr.zn.dataset_raster <- raster(covariates[[ icountry ]][['px_area_100m']][["path"]])
    
    minbks <- wpGetBlocksNeed (cvr.zn.id, rfg.cluster_workers, n=2)
    out.put.sum <- wpZonalStatistics (cvr.zn.dataset_raster, cvr.zn.id, fun="sum", cores=rfg.cluster_workers, minblk=minbks)  
    colnames(out.put.sum) <-  c("ADMINID", "F_AREA") 
    out.put.sum <- out.put.sum[out.put.sum$ADMINID != 0, ]
    
    
    df <- merge( as.data.frame(df), 
                 as.data.frame(out.put.sum), 
                 by="ADMINID", 
                 sort=FALSE)    
    
    
    
    df <- merge( as.data.frame(df), 
                 as.data.frame(load.pop.table(icountry)), 
                 by="ADMINID",
                 sort=FALSE)           
    
    
    
  }else{
    
    df <- merge( as.data.frame(df), 
                 as.data.frame(load.pop.table.and.farea(icountry)), 
                 by="ADMINID", 
                 sort=FALSE)       
    
  }     
  return(df)
  
}