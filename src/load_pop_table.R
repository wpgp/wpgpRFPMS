load.pop.table.and.farea <- function(icountry){
  
if (!is.null(rfg.input.poptables)){
    
    if(!file.exists(rfg.input.poptables[icountry]) ){
      logwarn(paste0("Trying to open POP TABLE for ",country," from file ", rfg.input.poptables[icountry]))
      stop(paste("Can not load a POP TABLE. Please check your inputs in input.R", sep=""))
      return(FALSE)
    }  
    loginfo(paste0("Will load POP TABLE for ",icountry," from a local file ",rfg.input.poptables[icountry]))
    POP_TABLE <- as.data.frame(read.dbf(rfg.input.poptables[icountry]))    
    
}else{
    loginfo(paste0("Will load POP TABLE for ",icountry," from WP FTP "))
    POP_TABLE <- wpgpGetPOPTable(ISO3 =icountry, 
                                 year = rfg.input.year,
                                 destDir = rfg.output.path.countries.tmp,
                                 username = rfg.ftp.username, 
                                 password = rfg.ftp.password
                                 ) 
}
  
  
  colnames(POP_TABLE) <- c("ADMINID", "ADMINPOP")
  
  
  
  
  
  return(POP_TABLE)
}