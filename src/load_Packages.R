load.Packages <- function(){
  
  ##  Load primary packages:
  ##  If there is a discrepancy between the packages listed and the packages 
  ##  installed:
  if (length(setdiff(rfg.pkgs, rownames(installed.packages()))) > 0) {
    warning(paste0("The follwing R packages are not installed: ",
                   setdiff(rfg.pkgs, rownames(installed.packages()))))
    return(FALSE)
  }else{
    ##  Load up the packages:
    sapply(rfg.pkgs, require, character.only = TRUE)
    if (rfg.DologDEBUG) 
      basicConfig(level='DEBUG')
  }
  
  # loading packages to suppor PostGS  Database
  #
  
  # if (rfg.db.type.driver == "RJDBC"){
  #   if ( is.element(rfg.db.type.driver, installed.packages()[,1])){
  #     library("RJDBC", quietly = TRUE)
  #   }else{
  #     warning("The follwing R packages is not installed: RJDBC")
  #     return(FALSE)
  #   }
  #   
  #   if (!file.exists(paste0(rfg.db.type.driver.path))) {
  #     warning("The follwing R packages RJDBC is requred  a valid postgresql-9.4.1211.jre6.jar file. Check Configuration file of RF script")
  #     return(FALSE)
  #   }  
  # }
  # 
  
  
  
  # if (rfg.db.type.driver == "RPostgreSQL"){
  #   if ( is.element(rfg.db.type.driver, installed.packages()[,1])){
  #     library("RPostgreSQL", quietly = TRUE)
  #   }else{
  #     warning("The follwing R packages is not installed: RPostgreSQL")
  #     return(FALSE)
  #   }
  # }
  # 
  # 
  # if (rfg.db.type.driver == "RPostgres"){
  #   if ( is.element(rfg.db.type.driver, installed.packages()[,1])){
  #     library("RPostgres", quietly = TRUE)
  #   }else{
  #     warning("Error: package RPostgres is not installed. ")
  #     print("RPostgres isn't available from CRAN yet, but you can get it from github with:")
  #     print("install.packages('devtools')")
  #     print("devtools::install_github('RcppCore/Rcpp')")
  #     print("devtools::install_github('rstats-db/DBI')")
  #     print("devtools::install_github('rstats-db/RPostgres')")
  #     return(FALSE)
  #   }
  #}
  
  
  
  ###############################################################
  ##  load package to support WP FTP access:
  if ( is.element("wpgpDownloadR", installed.packages()[,1])){
    
      library("wpgpDownloadR", quietly = TRUE)
    
  }else{
        if ( is.element("devtools", installed.packages()[,1])){
            library("devtools", quietly = TRUE)
            devtools::install_github('wpgp/wpgpDownloadR')
        }else{
            install.packages('devtools')
            library("devtools", quietly = TRUE)
            devtools::install_github('wpgp/wpgpDownloadR')
        }  
      
        if ( is.element("wpgpDownloadR", installed.packages()[,1])){
          library("wpgpDownloadR", quietly = TRUE)
          return(TRUE)
        }  
      
      warning("Error: package wpgpDownloadR is not installed. ")
      print("wpgpDownloadR isn't available from CRAN yet, but you can get it from github with:")
      print("install.packages('devtools')")
      print("devtools::install_github('wpgp/wpgpDownloadR')")
      return(FALSE)
      
  }
  
  
  ###############################################################
  ##  load wpUtilities package to support RF script 
  if ( is.element("wpUtilities", installed.packages()[,1])){
    
    library("wpUtilities", quietly = TRUE)
    
  }else{
    if ( is.element("devtools", installed.packages()[,1])){
      library("devtools", quietly = TRUE)
      devtools::install_github('wpgp/wpUtilities')
    }else{
      install.packages('devtools')
      library("devtools", quietly = TRUE)
      devtools::install_github('wpgp/wpUtilities')
    }  
    
    if ( is.element("wpUtilities", installed.packages()[,1])){
      library("wpUtilities", quietly = TRUE)
      return(TRUE)
    }  
    
    warning("Error: package wpUtilities is not installed. ")
    print("wpUtilities isn't available from CRAN yet, but you can get it from github with:")
    print("install.packages('devtools')")
    print("devtools::install_github('wpgp/wpUtilities')")
    return(FALSE)
    
  }
  return(TRUE)
} 
