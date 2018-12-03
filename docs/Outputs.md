# wpRFPMS : Random Forests population modelling scripts 

[![N|Solid](http://maps.worldpop.org.uk/img/worldpop-logo.png)](http://maps.worldpop.org.uk)


* [Introduction ](../README.md)
* [Dependencies and installation ](Dependencies.md)
* [Getting started ](GettingStarted.md)
* [Using Shape file ](Shapefile.md)
* [Using AdminID ](AdminID.md)

 

# Outputs
When you run the main wpRFPMS script, some folders will be created based on upon user input parameters. For example, if `rfg.input.countries <- c("BTN","NPL")` then you should expect to have the following folders in your project:

  * **Data**
    * BTN
    * NPL
    * old_popfit
  * **output**
    * BTN
        * tmp
        * zonal_stats
    * NPL
        * tmp
        * zonal_stats    
    * prj_2000_BTN_NPL
        * cvr
        * tmp
  
**Data** directory will contain the folders with the name of the country(ies) and the raster files downloaded from FTP. There will also be an empty "old_popfit" directory and this folder will be used if `rfg.fixed.set` is TRUE. If TRUE, the user will also be required to copy the popfit objects, that the user would like to use during RF modelling, into this folder. 

**output** directory will contain files produced by script. All zonal stats calculated by the script or downloaded from the WorldPop FTP will be stored in /**output/[ISO-country]/zonal_stats** 
If `rfg.input.shp` or `rfg.input.adminids` is used to subset the modeling procedure, then all cropped raster files will be stored in /**output/[ISO-country]**

**output** directory will also have folder with a name **prj_[year]_[ISO]**. This folder will have the results of the prediction phase of the model and all R objects saved during the modelling process. 


