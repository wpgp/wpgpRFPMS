######################################################################################
## Function to check final results.
## Zonal stats sum will be calculated and compared with POP table.
#

check_final_RF_result <- function() {
  
  if (rfg.check.result){  
    
    print(paste0("Start checking result...")) 
    
    ##  Load final results
    dataset_raster <- raster(paste0(rfg.output.path.countries,rfg.predict.density.rf.pred.final))
    
    ##  Load the zonal raster:
    zonal_raster <- raster(censusmaskPathFileName)    
    
    minblks <- wpGetBlocksNeed(dataset_raster, rfg.cluster_workers, n=2)
    
    output_stats <- wpZonalStatistics(dataset_raster,
                                      zonal_raster,
                                      fun='sum',
                                      cores=rfg.cluster_workers,
                                      minblk=minblks	)
    
    colnames(output_stats) <- c("ADMINID", "PPP_FINAL_RES")
    output_stats.sorted <-  output_stats[sort.list(output_stats[,1]), ] 
    
    output_stats.sorted <-  output_stats.sorted[output_stats.sorted[,1] != 0, ]
    
    
    reqd <- as.vector(c("ADMINID","ADMINPOP"))
    
    df <- census_data[,reqd]
    
    df <- merge( as.data.frame(df), 
                 as.data.frame(output_stats.sorted), 
                 by="ADMINID", 
                 sort=FALSE)   
    

    
    df <- cbind( df, abs(df$ADMINPOP - df$PPP_FINAL_RES)) 
    
    colnames(df) <- c("ADMINID", "ADMINPOP", "PPP", "DIFFERENCE")
    

    file.path.csv <- paste0(rfg.output.path.countries,"/check_result_prj_",rfg.countries.tag,".csv")
    
    write.csv( as.data.frame(df), file = file.path.csv, row.names=FALSE )
    
    if (any(df[,4] > rfg.check.result.eps)){
      logwarn(paste0("Warning: Please check the  results as POP table does not math"))
      logwarn(paste0("the zonal sum stats of filsn results", file.path.csv))
    }
    
  }  
  
  
}
