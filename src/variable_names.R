rfg.countries.fln.Rdata <- paste0("covariates_",rfg.countries.tag, ".Rdata")
rfg.countries.fln.json <- paste0("covariates_",rfg.countries.tag, ".json")
rfg.census.data.fln.Rdata <- paste0("census_data_",rfg.countries.tag, ".Rdata")
rfg.census.data.fln.csv<- paste0("census_data_",rfg.countries.tag, ".csv")
rfg.covariates.RF.Rdata <- paste0("covariates_RF_",rfg.countries.tag, ".Rdata")
rfg.covariates.RF.json<- paste0("covariates_RF_",rfg.countries.tag, ".json")

rfg.init.popfit.RData <- paste0("init_popfit_",rfg.countries.tag, ".Rdata")
rfg.popfit.RData <- paste0("popfit_",rfg.countries.tag, ".Rdata")

rfg.popfit.final.RData <- paste0("popfit_final_",rfg.countries.tag, ".Rdata")
rfg.popfit.quant.RData <- paste0("popfit_quant_",rfg.countries.tag, ".Rdata")


rfg.predict.density.rf.pred <- paste0("predict_density_rf_pred_",rfg.countries.tag, ".tif")
rfg.predict.density.rf.sd <- paste0("predict_density_rf_sd_",rfg.countries.tag, ".tif")

#if (quant_output) {
  rfg.predict.density.rf.pred_05 <- paste0("predict_density_rf_pred_05",rfg.countries.tag, ".tif")
  rfg.predict.density.rf.pred_50 <- paste0("predict_density_rf_pred_50",rfg.countries.tag, ".tif")
  rfg.predict.density.rf.pred_95 <- paste0("predict_density_rf_pred_90",rfg.countries.tag, ".tif")
#}  

rfg.predict.density.rf.pred.final <- paste0("ppp_",rfg.countries.tag, ".tif")

rfg.rst.pop.census.tif <- paste0("pop_census_mask_",rfg.countries.tag, ".tif")

rfg.zonal.stats.rf.pred.csv<- paste0("predict_density_rf_pred_",rfg.countries.tag, "_ZS_sum.csv")

rfg.rst.zonal.stats.rf.pred.tif <- paste0("predict_density_rf_pred_",rfg.countries.tag, "_ZS_sum.tif")



# remove unnecessary variables
rm_unnecessary_var_fln <- function(){
  
  rm("rfg.countries.fln.Rdata",
     "rfg.countries.fln.json",
     "rfg.census.data.fln.Rdata",
     "rfg.census.data.fln.csv",
     "rfg.covariates.RF.Rdata",
     "rfg.covariates.RF.json",envir = .GlobalEnv)  
   
}  