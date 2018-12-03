## Check if python is installed:
py_out <- system(paste(rfg.python_path,"-V",spr=" "), show.output.on.console=F)

##  If the check returns a zero:
if (py_out != 0){
  cat("Please check if python path was set corectlly in config.R")
  return(FALSE)
}




##  Check if gdal_merge is avalible and system path for it is set:
result = tryCatch({
  gdal_merge.version <- system( paste(rfg.gdal_merge,"--version",spr=" "), show.output.on.console=F)
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


##  Check if gdal_calc is avalible and system path for it is set:
result = tryCatch({
  gdal_calc.version <- system( paste(rfg.gdal_calc,"-h",spr=" "), show.output.on.console=F)
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

##  Check if gdal_polygonize is avalible and system path for it is set:
result = tryCatch({
  system( paste(rfg.gdal_polygonize,spr=" "), show.output.on.console=T)
}, warning = function(w) {
  #logwarn(w)
}, error = function(e) {
   warning(e)
   return(FALSE)
}, finally = {
  #cat(gdal_calc.version)
}
)



#logdebug('I am a silent child')

##  Check if a given package is installed and available, if not then try to 
##  install it and then load it:
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}