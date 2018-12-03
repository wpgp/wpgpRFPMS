# wpRFPMS : Random Forests population modelling scripts 

[![N|Solid](http://maps.worldpop.org.uk/img/worldpop-logo.png)](http://maps.worldpop.org.uk)


* [Introduction ](../README.md)
* [Getting started ](GettingStarted.md)
* [Outputs ](Outputs.md)
* [Using Shape file ](Shapefile.md)
* [Using AdminID ](AdminID.md)

 
# Dependencies and installation
wpgpRFPMS script requires the following R packages. 
```
"rgdal" 
"raster" 
"randomForest" 
"quantregForest" 
"foreign"
"snow"
"doParallel"
"gdalUtils"
"jsonlite"
"logging" 
"doSNOW"
"RCurl"
"plyr"
"wpgpDownloadR"
"wpUtilities"
```

Most of the above R packages can be installed from the CRAN repository. 

`wpgpDownloadR` is an R Package interface used for downloading raster datasets from the WorldPop FTP repository. Installation of wpgpCovariates isn't available from CRAN yet, but you can install it directly from github with the following code in the R console:
```
install.packages("devtools")
devtools::install_github("wpgp/wpgpDownloadR")

# load package
library(wpgpDownloadR)
```

`wpUtilities` is an R Package containing the tools to work with raster files, such as zonal stats and rasterizing data, within the context of WorldPop modelling framework and potentially beyond. Installation of wpUtilities isn't available from CRAN yet, but you can install it directly from github with the following code in the R console:
```
install.packages("devtools")
devtools::install_github("wpgp/wpUtilities")

# load package
library(wpUtilities)
```
