get_ftp_ListCountryCovariates <- function(ISO3){
  
  df <- wpgpListCountryDatasets(ISO3 = ISO3)
  
  return(df)
}

get_ftp_description <- function(cvr){
  
  if (length(rfg.input.countries) > 1){
    
    return(paste0("<a href='ftp://ftp.worldpop.org.uk/GIS/Covariates/Global_2000_2020/'>","ftp://ftp.worldpop.org.uk/GIS/Covariates/Global_2000_2020/</a>"))
    
  }else{
    
    ISO3 =   rfg.input.countries[[1]]
    
    df <- get_ftp_ListCountryCovariates(ISO3 = ISO3)
  
    return(paste0("<a href='ftp://ftp.worldpop.org.uk/GIS/Covariates/Global_2000_2020/'>","ftp://ftp.worldpop.org.uk/WP515640_Global/",Covariate,"</a>"))
    
  }
}


get_cvr_description <- function(cvr){
  
  description <- c("esaccilc_dst011_100m_2000" = "Distance to cultivated areas. </br><b>Data Source</b>: European Space Agency, Climate Change Initiative Land Cover (https://www.esa-landcover-cci.org/), Land Cover Maps v2.0.7 (https://www.esa-landcover-cci.org/?q=node/175)", 
                   "esaccilc_dst040_100m_2000" = "Distance to woody areas  </br><b>Data Source</b>: European Space Agency, Climate Change Initiative Land Cover (https://www.esa-landcover-cci.org/), Land Cover Maps v2.0.7 (https://www.esa-landcover-cci.org/?q=node/175)", 
                   "esaccilc_dst130_100m_2000" = "Distance to shrub areas  </br><b>Data Source</b>: European Space Agency, Climate Change Initiative Land Cover (https://www.esa-landcover-cci.org/), Land Cover Maps v2.0.7 (https://www.esa-landcover-cci.org/?q=node/175)", 
                   "esaccilc_dst140_100m_2000" = "Distance to herbaceous areas  </br><b>Data Source</b>: European Space Agency, Climate Change Initiative Land Cover (https://www.esa-landcover-cci.org/), Land Cover Maps v2.0.7 (https://www.esa-landcover-cci.org/?q=node/175)", 
                   "esaccilc_dst150_100m_2000" = "Distance to sparse vegetation areas </br><b>Data Source</b>: European Space Agency, Climate Change Initiative Land Cover (https://www.esa-landcover-cci.org/), Land Cover Maps v2.0.7 (https://www.esa-landcover-cci.org/?q=node/175)", 
                   "esaccilc_dst160_100m_2000" = "Distance to aquatic vegetation areas  </br><b>Data Source</b>: European Space Agency, Climate Change Initiative Land Cover (https://www.esa-landcover-cci.org/), Land Cover Maps v2.0.7 (https://www.esa-landcover-cci.org/?q=node/175)", 
                   "esaccilc_dst190_100m_2000" = "Distance to Urban area </br><b>Data Source</b>: European Space Agency, Climate Change Initiative Land Cover (https://www.esa-landcover-cci.org/), Land Cover Maps v2.0.7 (https://www.esa-landcover-cci.org/?q=node/175)", 
                   "esaccilc_dst200_100m_2000" = "Distance to bare areas </br><b>Data Source</b>: European Space Agency, Climate Change Initiative Land Cover (https://www.esa-landcover-cci.org/), Land Cover Maps v2.0.7 (https://www.esa-landcover-cci.org/?q=node/175)", 
                   "esaccilc_dst_water_100m_2000_2012"   = "Distance to inland waterbodies </br><b>Data Source</b>: European Space Agency, Climate Change Initiative Land Cover (https://www.esa-landcover-cci.org/), Water Bodies v4.0",
                   "ghsl_esa_dst" = "Distance to settlement/buildup areas  </br><b>Data Source</b>: European Commission Joint Research Centre, Global Human Settlement Layer (http://ghsl.jrc.ec.europa.eu/index.php), GHS Built-Up Grid (http://ghsl.jrc.ec.europa.eu/ghs_bu.php); European Space Agency, Climate Change Initiative Land Cover (https://www.esa-landcover-cci.org/), Land Cover Maps v2.0.7 (https://www.esa-landcover-cci.org/?q=node/175)",
                   "osm_dst_roadintersec_100m_2016"        = "Distance to major road intersetcions, multiple years </br><b>Data Source</b>: OpenStreetMap (https://wiki.openstreetmap.org/wiki/Main_Page), Roads",
                   "osm_dst_waterway_100m_2016"        = "Distance to major waterways, multiple years </br><b>Data Source</b>: OpenStreetMap (https://wiki.openstreetmap.org/wiki/Main_Page), Roads",
                   "osm_dst_road_100m_2016"        = "Distance to major roads, multiple years </br><b>Data Source</b>: OpenStreetMap (https://wiki.openstreetmap.org/wiki/Main_Page), Roads",
                   "wclim_prec"        = "Current average total annula precipitation </br><b>Data Source</b>:  WorldClim - Global Climate Data (http://worldclim.org/), Precipitation v2 (prec 30s)",
                   "wclim_temp"        = "Current average annual temperature </br><b>Data Source</b>:  WorldClim - Global Climate Data (http://worldclim.org/), Average Temperature v2 (tavg 30s)",
                   "srtm_topo_100m"             = "Slope </br><b>Data Source</b>: Viewfinder Panoramas (http://viewfinderpanoramas.org/), Digital Elevation Data (http://viewfinderpanoramas.org/dem3.html)",
                   "srtm_topo_100m"              = "Topo </br><b>Data Source</b>: Viewfinder Panoramas (http://viewfinderpanoramas.org/), Digital Elevation Data (http://viewfinderpanoramas.org/dem3.html)",
                   "dst_coastline_100m_2000_2020"     = "Distance to coastline </br><b>Data Source</b>: CIESIN, Gridded Population of the World v4 (http://sedac.ciesin.columbia.edu/data/collection/gpw-v4)",
                   "cciwat_cls"        = "Binary representation of the waterbodies </br><b>Data Source</b>: European Space Agency, Climate Change Initiative Land Cover (https://www.esa-landcover-cci.org/), Water Bodies v4.0"
  )
  
  if (cvr %in% names(description)){
    return(description[[cvr]])
  }else{
    return("")
  }
 
  
}




get_base64_image <- function(frpath, br=TRUE, bi=FALSE){
  
  rast <- raster(frpath)
  
  jpeg(tf1 <- tempfile(fileext = ".jpg"), width=700, height=500)
  
  #  if (!is.null(maxl)){
  #    plot(rast,col= topo.colors(20), frame.plot=FALSE, axes=F,box=FALSE,zlim=c(0,maxl))
  #  }else{
  #    plot(rast,col=grey(1:100/100),frame.plot=FALSE, axes=F,box=FALSE)
  #  }
  
  if (!bi){
    plot(rast,col=grey(1:100/100),frame.plot=FALSE, axes=F,box=FALSE)     
  }else{
    r.min <- raster:::minValue(rast)
    r.max <- raster:::maxValue(rast)
    plot(rast,col= grey(1:100/100), frame.plot=FALSE, axes=F,box=FALSE, zlim=c(r.min,r.max/4) , axis.args=list(at=c(r.min,r.max/4),labels=c("Low","High"), cex.axis=0.8))  
    
  }
  
  #plot(rast,col=grey(1:100/100),frame.plot=FALSE, axes=F,box=FALSE)
  #plot(rast, col=rev( rainbow( 99, start=0,end=1 ) ),zlim=c(0,10) , frame.plot=FALSE, axes=F,box=FALSE)
  dev.off()
  
  if (br){
    thumbnail <-"img-thumbnail"
  }else{
    thumbnail <-""
  }
  
  txt <- base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")
  html <- sprintf("<img class='center-block img-responsive %s' src='data:image/png;base64,%s'>", thumbnail,txt)
  
  if (file.exists(tf1)) file.remove(tf1)
  
  return(html)
  
}  

get_base64_image_RF <- function(tp="Importance"){
  
  if (tp=="Importance"){
    png(tf1 <- tempfile(fileext = ".png"), width=600, height=600)
    varImpPlot(popfit_final, main=paste0("Covariate for ",get_contry_ISO()," RF Model"))
    dev.off()
  }else{
    png(tf1 <- tempfile(fileext = ".png"), width=600, height=600)
    plot(popfit_final, main=paste0("Prediction Error (MSE) Stability for ",get_contry_ISO()," RF Model"))
    dev.off()
    
  }
  txt <- base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")
  html <- sprintf("<img class='center-block img-responsive img-thumbnail' src='data:image/png;base64,%s'>", txt)
  
  if (file.exists(tf1)) file.remove(tf1)
  
  return(html)
  
}  


get_contry_name <- function(){
  
  df.list.Country <- data.frame(ISO=c('RUS','IDN','USA','VIR','GRL','CHN','AUS','BRA','CAN','CHL','AFG','ALB','ATA','DZA','ASM','AND','AGO',
                                      'ATG','AZE','ARG','AUT','BHS','BHR','BGD','ARM','BRB','BEL','BMU','BTN','BOL','BIH','BWA','BVT','BLZ',
                                      'IOT','SLB','VGB','BRN','BGR','MMR','BDI','BLR','KHM','CMR','CPV','CYM','CAF','LKA','TCD','TWN','COL',
                                      'COM','MYT','COG','COD','COK','CRI','HRV','CUB','CYP','CZE','BEN','DNK','DMA','DOM','ECU','SLV','GNQ',
                                      'ETH','ERI','EST','FRO','FLK','SGS','FJI','FIN','ALA','FRA','GUF','PYF','ATF','DJI','GAB','GEO','GMB',
                                      'PSE','DEU','GHA','GIB','KIR','GRC','GRD','GLP','GUM','GTM','GIN','GUY','HTI','HMD','VAT','HND','HKG',
                                      'HUN','ISL','IND','IRN','IRQ','IRL','ISR','ITA','CIV','JAM','JPN','KAZ','JOR','KEN','PRK','KOR','KWT',
                                      'KGZ','LAO','LBN','LSO','LVA','LBR','LBY','LIE','LTU','LUX','MAC','MDG','MWI','MYS','MDV','MLI','MLT',
                                      'MTQ','MRT','MUS','MEX','MCO','MNG','MDA','MNE','MSR','MAR','MOZ','OMN','NAM','NRU','NPL','NLD','CUW',
                                      'ABW','SXM','BES','NCL','VUT','NZL','NIC','NER','NGA','NIU','NFK','NOR','MNP','UMI','FSM','MHL','PLW',
                                      'PAK','PAN','PNG','PRY','PER','PHL','PCN','POL','PRT','GNB','TLS','PRI','QAT','REU','ROU','RWA','BLM',
                                      'SHN','KNA','AIA','LCA','MAF','SPM','VCT','SMR','STP','SAU','SEN','SRB','SYC','SLE','SGP','SVK','VNM',
                                      'SVN','SOM','ZAF','ZWE','ESP','SSD','SDN','ESH','SUR','SJM','SWZ','SWE','CHE','SYR','TJK','THA','TGO',
                                      'TKL','TON','TTO','ARE','TUN','TUR','TKM','TCA','TUV','UGA','UKR','MKD','EGY','GBR','GGY','JEY','IMN',
                                      'TZA','BFA','URY','UZB','VEN','WLF','WSM','YEM','ZMB','KOS','SPR'),
                                Country=c('Russia','Indonesia','United States','Virgin_Islands_U_S','Greenland','China','Australia','Brazil','Canada',
                                          'Chile','Afghanistan','Albania','Antarctica','Algeria','American Samoa','Andorra','Angola','Antigua and Barbuda',
                                          'Azerbaijan','Argentina','Austria','Bahamas','Bahrain','Bangladesh','Armenia','Barbados','Belgium','Bermuda','Bhutan',
                                          'Bolivia','Bosnia and Herzegovina','Botswana','Bouvet Island','Belize','British Indian Ocean Territory',
                                          'Solomon Islands','British Virgin Islands','Brunei','Bulgaria','Myanmar','Burundi','Belarus','Cambodia','Cameroon',
                                          'Cape Verde','Cayman Islands','Central African Republic','Sri Lanka','Chad','Taiwan','Colombia','Comoros','Mayotte',
                                          'Republic of Congo','Democratic Republic of the Congo','Cook Islands','Costa Rica','Croatia','Cuba','Cyprus','Czech Republic',
                                          'Benin','Denmark','Dominica','Dominican Republic','Ecuador','El Salvador','Equatorial Guinea','Ethiopia','Eritrea','Estonia',
                                          'Faroe Islands','Falkland Islands','South Georgia and the South Sandwich Islands','Fiji','Finland','iland Islands',
                                          'France','French Guiana','French Polynesia','French Southern Territories','Djibouti','Gabon','Georgia','Gambia',
                                          'Palestina','Germany','Ghana','Gibraltar','Kiribati','Greece','Grenada','Guadeloupe','Guam','Guatemala','Guinea',
                                          'Guyana','Haiti','Heard Island and McDonald Islands','Vatican City','Honduras','Hong Kong','Hungary','Iceland',
                                          'India','Iran','Iraq','Ireland','Israel','Italy','CIte dIvoire','Jamaica','Japan','Kazakhstan','Jordan','Kenya',
                                          'North Korea','South Korea','Kuwait','Kyrgyzstan','Laos','Lebanon','Lesotho','Latvia','Liberia','Libya','Liechtenstein',
                                          'Lithuania','Luxembourg','Macao','Madagascar','Malawi','Malaysia','Maldives','Mali','Malta','Martinique','Mauritania',
                                          'Mauritius','Mexico','Monaco','Mongolia','Moldova','Montenegro','Montserrat','Morocco','Mozambique','Oman','Namibia',
                                          'Nauru','Nepal','Netherlands','Curacao','Aruba','Sint Maarten (Dutch part)','Bonaire, Sint Eustatius and Saba',
                                          'New Caledonia','Vanuatu','New Zealand','Nicaragua','Niger','Nigeria','Niue','Norfolk Island','Norway',
                                          'Northern Mariana Islands','United States Minor Outlying Islands','Micronesia','Marshall Islands','Palau','Pakistan',
                                          'Panama','Papua New Guinea','Paraguay','Peru','Philippines','Pitcairn Islands','Poland','Portugal','Guinea-Bissau',
                                          'East Timor','Puerto Rico','Qatar','Reunion','Romania','Rwanda','Saint Barthelemy','Saint Helena','Saint Kitts and Nevis',
                                          'Anguilla','Saint Lucia','Saint Martin (French part)','Saint Pierre and Miquelon','Saint Vincent and the Grenadines',
                                          'San Marino','Sao Tome and Principe','Saudi Arabia','Senegal','Serbia','Seychelles','Sierra Leone','Singapore',
                                          'Slovakia','Vietnam','Slovenia','Somalia','South Africa','Zimbabwe','Spain','South Sudan','Sudan','Western Sahara',
                                          'Suriname','Svalbard and Jan Mayen Islands','Swaziland','Sweden','Switzerland','Syria','Tajikistan','Thailand','Togo',
                                          'Tokelau','Tonga','Trinidad and Tobago','United Arab Emirates','Tunisia','Turkey','Turkmenistan',
                                          'Turks and Caicos Islands','Tuvalu','Uganda','Ukraine','Macedonia','Egypt','United Kingdom','Guernsey','Jersey',
                                          'Isle of Man','Tanzania','Burkina Faso','Uruguay','Uzbekistan','Venezuela','Wallis and Futuna','Samoa','Yemen',
                                          'Zambia','Kosovo','Spratly Islands'),
                                stringsAsFactors=FALSE)
  
  
  if (as.integer(length(rfg.input.countries)) > 1){
    
    for ( i in rfg.input.countries )  {
      if(i==1) next
      contry_name <- paste(as.character(df.list.Country[which(df.list.Country$ISO == rfg.input.countries[[1]]), ]$Country),
                           as.character(df.list.Country[which(df.list.Country$ISO == i), ]$Country),sep = " and ",collapse = NULL)
    }
  }else{
    contry_name <- as.character(df.list.Country[which(df.list.Country$ISO == rfg.input.countries[[1]]), ]$Country)
  } 
  
  return(contry_name)
}


get_contry_ISO <- function(){
  
  if (as.integer(length(rfg.input.countries)) > 1){
    
    for ( i in rfg.input.countries )  {
      if(i==1) next
      contry_name <- paste(as.character(rfg.input.countries[[1]]),
                           as.character(rfg.input.countries[[1]]),sep = " and ",collapse = NULL)
    }
  }else{
    contry_name <- as.character(rfg.input.countries[[1]])
  }   
  return(contry_name)
}  




title_report <- paste(get_contry_name(),"Population Map Metadata Report",sep = " ")
print(paste0("Start creating an report for project ",title_report)) 



sink(paste0(rfg.output.path.countries,"report_",rfg.countries.tag,".html")  )


cat("<!DOCTYPE html>
    <html lang='en'>
    <head>
    <meta charset='utf-8'>
    <meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>\n")

cat(paste0("<meta name='",title_report,"' content='WorldPop'>\n"))

cat(paste0("<title>",title_report,"</title>\n"))


cat("<script src='https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js'></script>
    <!-- Latest compiled and minified CSS -->
    <link rel='stylesheet' href='https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css' integrity='sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u' crossorigin='anonymous'>
    <!-- Optional theme -->
    <link rel='stylesheet' href='https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css' integrity='sha384-rHyoN1iRsVXV4nD0JutlnGaslCJuC7uwjduW9SVrLvRYooPp2bWYgmgJQIXwl/Sp' crossorigin='anonymous'>
    <!-- Latest compiled and minified JavaScript -->
    <script src='https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js' integrity='sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa' crossorigin='anonymous'></script>
    
    <!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
    <!--[if lt IE 9]>
    <script src='https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js'></script>
    <script src='https://oss.maxcdn.com/respond/1.4.2/respond.min.js'></script>
    <![endif]-->
    
    </head>\n\n")

# Start body of html
cat("<body>\n")

cat("<!-- Fixed navbar -->
    <nav class='navbar navbar-default navbar-fixed-top'>
    <div class='container'>
    <div class='navbar-header'>
    <button type='button' class='navbar-toggle collapsed' data-toggle='collapse' data-target='#navbar' aria-expanded='false' aria-controls='navbar'>
    <span class='sr-only'>Toggle navigation</span>
    <span class='icon-bar'></span>
    <span class='icon-bar'></span>
    <span class='icon-bar'></span>
    </button>
    
    
    <img style='height: 50px;' src='http://maps.worldpop.org.uk/img/worldpop-logo.png' alt='WorldPop'>
    
    </div>
    <div id='navbar' class='navbar-collapse collapse'>
    
    <ul class='nav navbar-nav navbar-right'>
    <li><a href='http://www.worldpop.org.uk'>Worldpop</a></li>
    <li><a href='http://www.worldpop.org.uk/data/get_data/'>Data</a></li>
    <li class='active'><a href='http://www.worldpop.org.uk/contact/'>Contact</a></li>
    </ul>
    </div><!--/.nav-collapse -->
    </div>
    </nav>\n\n")

cat("<div class='container'>
    <div class='page-header'>
    <h2 style='padding-top: 35px;'>\n")
cat(title_report)
cat("</h2>
    </div>
    </div> <!-- /container -->\n\n")


cat("<div class='container'>
    <div class='panel panel-primary'>
    <div class='panel-heading'>
    <h3 class='panel-title'></h3>
    </div>
    <div class='panel-body'>
    The weighting layer here below represents the estimated population density in each 3 arc seconds (approximately 100 m at the equator) 
    obtained using the Random Forest (RF)-based dasymetric mapping approach developed by Stevens et al. (2015)*. 
    This weighting layer is then used to dasymetrically disaggregate population counts from administrative units into grid cells 
    to obtain the final population distribution datasets (both remaining unadjusted and being adjusted to match the most recent UNPD estimates# available at the time of production).
    A brief description of the RF model and all its inputs, including the source and spatial distribution of each covariate, is also provided in this metadata report.
   <br/><br/>    
    *Stevens, F. R., Gaughan, A. E., Linard, C. & Tatem, A. J. Disaggregating Census Data for Population Mapping Using Random Forests with Remotely-Sensed and Ancillary Data. PLoS ONE 10, e0107042 (2015).
    <br/>
    <a href='https://esa.un.org/unpd/wpp/Download/Standard/Population/'>https://esa.un.org/unpd/wpp/Download/Standard/Population/</a>
    </div>
    </div>
    </div> <!-- /container -->\n")


## start main cintaner
#
cat("<div class='container'>
    <div class='row my-4'>
    <div class='col-lg-8'>\n")


cat(get_base64_image(paste0(rfg.output.path.countries,rfg.predict.density.rf.pred.final),br=TRUE, bi=TRUE)  )

cat("</br>\n")

cat("<pre><code>\n")
if (rfg.fixed.set) {
  cat(rfg.fixed.set.description)
}
cat("\n")
cat(print(popfit_final))
cat("</code></pre>\n")

#cat(get_base64_image_RF("Importance"))
#cat("</br>\n")
#cat(get_base64_image_RF("Error"))
cat("</div>\n")


cat("<div class='col-lg-4'>
    
    <h3 style='padding-top: 0px;margin-top: 0px;'>Random Forest Diagnostics</h3>  
    
    <div class='list-group'>\n")
cat(paste0("<a href='#' class='list-group-item ' data-toggle='modal' data-target='.Covariateimportance'>"))
cat(paste0("Covariate importance for ",get_contry_ISO()," RF Model"))
cat("</a>\n") 


cat(paste0("<a href='#' class='list-group-item ' data-toggle='modal' data-target='.PredictionError'>"))
cat(paste0("Prediction Error (MSE) Stability for ",get_contry_ISO()," RF Model"))
cat("</a>\n") 

cat("  </div>   
    
    <h3 style='padding-top: 0px;margin-top: 0px;'>Covariate Metadata</h3>  
    
    <div class='list-group'>\n")

for ( i in 1:length(covariates) ) { 
  
  covariate       <- covariates[[i]]
  dataset_path    <- as.character(covariate$path)
  dataset_folder  <- as.character(covariate$dataset_folder)
  dataset_class   <- as.character(covariate$dataset_class)
  dataset_description <- as.character(covariate$dataset_description)
  
  cat(paste0("<a href='#' class='list-group-item ' data-toggle='modal' data-target='.",covariate$dataset_name,"'>"))
  cat(dataset_description)
  cat("</a>\n")
}

cat("
    </div>   
    
    
    </div>
    
    </div>
    </div> <!-- /container -->\n")
#
## End main cintaner


### create modal for importance 

cat(paste0("<div class='modal fade Covariateimportance' tabindex='-1' role='dialog' aria-labelledby='myLargeModalLabel' aria-hidden='true'>\n"))
cat("<div class='modal-dialog modal-lg' role='document'>
    <div class='modal-content'>
    <div class='modal-header'>
    <button type='button' class='close' data-dismiss='modal' aria-label='Close'><span aria-hidden='true'>&times;</span></button>
    \n") 
cat(paste0("<h4 class='modal-title'>",paste0("Covariate importance for ",get_contry_ISO()," RF Model"),"</h4>"))
cat("</div>
    <div class='modal-body'>\n") 


cat("<div class='row'>\n")
cat("<div class='col-md-12'>\n")
cat(get_base64_image_RF("Importance"))
cat("</div>\n")
#cat("<div class='col-md-6'></div>\n")
cat("</div>\n")        

cat("</div>
    <div class='modal-footer'>
    <button type='button' class='btn btn-primary' data-dismiss='modal'>Close</button>
    </div>
    </div><!-- /.modal-content -->
    </div><!-- /.modal-dialog -->
    </div><!-- /.modal -->\n") 

### create modal for importance 

cat(paste0("<div class='modal fade PredictionError' tabindex='-1' role='dialog' aria-labelledby='myLargeModalLabel' aria-hidden='true'>\n"))
cat("<div class='modal-dialog modal-lg' role='document'>
    <div class='modal-content'>
    <div class='modal-header'>
    <button type='button' class='close' data-dismiss='modal' aria-label='Close'><span aria-hidden='true'>&times;</span></button>
    \n") 
cat(paste0("<h4 class='modal-title'>",paste0("Prediction Error (MSE) Stability for ",get_contry_ISO()," RF Model"),"</h4>"))
cat("</div>
    <div class='modal-body'>\n") 


cat("<div class='row'>\n")
cat("<div class='col-md-12'>\n")

if (!rfg.fixed.set) {
  cat(get_base64_image_RF("Error"))  
}else{
  if (rfg.fixed.set.incl.input.countries==TRUE & 
      (nrow(census_data) < rfg.fixed.set.idmin.id.threshold)) {
    cat(get_base64_image_RF("Error"))  
  }
}

cat("</div>\n")
#cat("<div class='col-md-6'></div>\n")
cat("</div>\n")        

cat("</div>
    <div class='modal-footer'>
    <button type='button' class='btn btn-primary' data-dismiss='modal'>Close</button>
    </div>
    </div><!-- /.modal-content -->
    </div><!-- /.modal-dialog -->
    </div><!-- /.modal -->\n") 

### create modal for all covariates 

for ( i in 1:length(covariates)) { 
  
  covariate       <- covariates[[i]]
  dataset_path    <- as.character(covariate$path)
  dataset_folder  <- as.character(covariate$dataset_folder)
  dataset_class   <- as.character(covariate$dataset_class)
  dataset_description <- as.character(covariate$dataset_description)
  
  cat(paste0("<div class='modal fade ",covariate$dataset_name,"' tabindex='-1' role='dialog' aria-labelledby='myLargeModalLabel' aria-hidden='true'>\n"))
  cat("<div class='modal-dialog modal-lg' role='document'>
      <div class='modal-content'>
      <div class='modal-header'>
      <button type='button' class='close' data-dismiss='modal' aria-label='Close'><span aria-hidden='true'>&times;</span></button>
      \n") 
  cat(paste0("<h4 class='modal-title'>",covariate$dataset_description,"</h4>"))
  cat("</div>
      <div class='modal-body'>\n") 
  
  
  
  cat("<div class='row'>\n")
  cat("<div class='col-md-12'>\n")
  cat(get_base64_image(dataset_path,br=FALSE)  )
  cat("</br>\n")
  cat(get_cvr_description(covariate$dataset_name))  
  cat("</br>\n")
  cat(paste0("Download :",get_ftp_description(cvr=covariate$dataset_name),"\n"))
  
  cat("</div>\n")
  #cat("<div class='col-md-6'></div>\n")
  cat("</div>\n")        
  
  cat("</div>
      <div class='modal-footer'>
      <button type='button' class='btn btn-primary' data-dismiss='modal'>Close</button>
      </div>
      </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
      </div><!-- /.modal -->\n")   
  
}


# End body of html
cat("\n</body></html>\n")


sink()
