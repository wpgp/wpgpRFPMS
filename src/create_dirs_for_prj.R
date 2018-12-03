##  Written by Maksym Bondarenko
##  Commented by Jeremiah J. NIeves
create_dirs_for_prj <- function(){
  ##  Create a subfolder in /Data/ for each country and /output/ for country 
  ##  and countries.
  logdebug('Start creating folders for project')
  
  ##  Declare a character tag to append to the output files:
  countries_tag_output <- paste0('prj_',rfg.input.year)
  
  ##  For every country input into model:
  for ( i in rfg.input.countries ) { 
    ##  Declare the country specific subdirectory:
    subDir.country <- paste0(root_path,"/","data/", i)
    
    ##  If the directory doesn't already exist, create it:
    if(!file.exists(subDir.country)){ 
      dir.create(subDir.country, recursive = TRUE, showWarnings = FALSE) 
      logdebug(paste0('Created a folder: ',subDir.country))
     }
    
    ##  Declare the country specific temporary output folder:
    subDir.country.output.tmp <- paste0(root_path,"/","output/", i , "/tmp/")
    
    ##  If that folder does not already exist, create it:
    if(!file.exists(subDir.country.output.tmp)){ 
      dir.create(subDir.country.output.tmp, 
                 recursive = TRUE,
                 showWarnings = FALSE)
      logdebug(paste0('Created a folder: ',subDir.country.output.tmp))
    }
    
    
    ##  Declare the country specific zonal stats folder for CSV outputs:
    subDir.country.output.zst <- paste0(root_path,"/",
                                        "output/", i , "/zonal_stats/csv/")
    ##  If that folder does not already exist, create it:
    if(!file.exists(subDir.country.output.zst)){ 
      dir.create(subDir.country.output.zst,
                 recursive = TRUE,
                 showWarnings = FALSE)
      logdebug(paste0('Created a folder: ',subDir.country.output.zst))
    }
    
    ##  Declare the country specific zonal stats folder for RData outputs:
    subDir.country.output.zstRData <- paste0(root_path,"/","output/", 
                                             i , "/zonal_stats/RData/")
    ##  If that folder does not already exist, create it:
    if(!file.exists(subDir.country.output.zstRData)){ 
      dir.create(subDir.country.output.zstRData, 
                 recursive = TRUE, 
                 showWarnings = FALSE)
      logdebug(paste0('Created a folder: ',subDir.country.output.zstRData))
    } 
    
    # if working with a shp file or admin ID then croped covariates will be kept in 
    # separate folder to avoid to download original one for other projects
    if (!is.null(rfg.input.shp)  | !is.null(rfg.input.adminids)){ 
      
      subDir.country.output.croped <- paste0(root_path,"/","output/", i , "/croped/")      
      
      if(!file.exists(subDir.country.output.croped)){ 
        dir.create(subDir.country.output.croped, recursive = TRUE, showWarnings = FALSE) 
        logdebug(paste0('Created a folder: ',subDir.country.output.croped))
      }     
    }     
    
    ##  Construct the tag output in a procedural manner so if we have more than
    ##  one country in our model run, we have a tag which includes them all 
    ##  separated by an underscore:
    countries_tag_output <- paste(countries_tag_output, i, sep = "_")    
  }
  
  ##  Declare the path for the output and temporary output folders which can 
  ##  account for multiple countries and is where the merged output files will 
  ##  reside:
  output.path.countries <- paste(root_path, "/output/",
                                 countries_tag_output, "/", sep="")
  output.path.countries.tmp <- paste(root_path, "/output/",
                                     countries_tag_output, "/tmp/", sep="")
  ##  If that folder does not already exist, create it:
  if(!file.exists(output.path.countries.tmp)){ 
    dir.create(output.path.countries.tmp, recursive = TRUE, showWarnings = FALSE)
    logdebug(paste0('Created a folder: ',output.path.countries.tmp))
  }

    output.path.countries.cvr <- paste(root_path, "/output/",
                                     countries_tag_output, "/cvr/", sep="")
    ##  If that folder does not already exist, create it:
    if(!file.exists(output.path.countries.cvr)){ 
      dir.create(output.path.countries.cvr, recursive = TRUE, showWarnings = FALSE)
      logdebug(paste0('Created a folder: ',output.path.countries.cvr))
    }  

  
  
  ##  If we have only one country then the path to /Data/ will be ISO of that
  ##  country otherwise it will be a new folder created based on the combined 
  ##  ISOs [example prj_2000_BTN_NPL]. This folder will contain the merged 
  ##  rasters for RF.
##  if (length(rfg.input.countries) == 1){
    data.path.countries <- paste(root_path, "/data/", 
                                 as.character(rfg.input.countries[[1]]), 
                                 "/", sep="")
    ##  If that folder does not already exist, create it:
    if(!file.exists(data.path.countries)){ 
      dir.create(data.path.countries, recursive = TRUE, showWarnings = FALSE)
      logdebug(paste0('Created a folder: ',data.path.countries))
    }  
##  }else{
##    data.path.countries <- paste(root_path, "/data/", countries_tag_output, "/", sep="")
##    ##  If that folder does not already exist, create it:
##    if(!file.exists(data.path.countries)){ 
##      dir.create(data.path.countries, recursive = TRUE, showWarnings = FALSE)
##    }

##  } 
  
    
    data.path.old.popfits <- paste0(root_path, "/data/old_popfits/")
    ##  If that folder does not already exist, create it:
    if(!file.exists(data.path.old.popfits)){ 
      dir.create(data.path.old.popfits, recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0(data.path.old.popfits, "popfits_final/"), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0(data.path.old.popfits, "popfits_quant/"), recursive = TRUE, showWarnings = FALSE)
      logdebug(paste0('Created a folder: ',data.path.old.popfits))
    }      
    

  ##  Return a list of objects which represent the output path, the data path, 
  ##  and the country(ies) tag:
  return(list(output = output.path.countries, 
              data = data.path.countries, 
              countries_tag = countries_tag_output,
              data_cvr = output.path.countries.cvr,
              data_old_popfits_final = paste0(data.path.old.popfits, "popfits_final/"),
              data_old_popfits_quant = paste0(data.path.old.popfits, "popfits_quant/")
              ))
}
