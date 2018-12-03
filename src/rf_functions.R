######################################################################################
get_popfit_final_old <- function(only.names=FALSE) {
  ##  Function which retrieves previously constructed popfit.RData objects.
  err_mess <- ""
  err_bool <- FALSE
  
  list.of.old.popfits.final <- list.files(paste0(rfg.data.old.popfits.final),
                                  pattern=paste0("\\.Rdata$"),
                                  full.names=TRUE) 

  
  logdebug(paste0('Loading old popfit final from: ',rfg.data.old.popfits.final))
  
  if ( length(list.of.old.popfits.final) == 0 ){
    err_mess <- paste0('There is no old popfit Please check the folder : ', rfg.data.old.popfits.final)
    stop(err_mess)
  }

  ##  Load it:
  logdebug(paste0( 'Load old popfit final with  ',list.of.old.popfits.final[[1]] ))
  local_env.Popfit_final = local({load(file=list.of.old.popfits.final[[1]]);environment()})

  popfit.final.old <- local_env.Popfit_final$popfit_final
  popfit.final.old$proximity <- NULL
  popfit.final.old$predicted <- 0  
  
  if (only.names){
    
    fixed.predictors <- row.names(importance(popfit.final.old))    
    return(fixed.predictors)  
  }

  for ( i in 1:length(list.of.old.popfits.final) ) {

    if (i==1) next()

    local_env.Popfit_final = local({load(file=list.of.old.popfits.final[[i]]);environment()})

    local_env.Popfit_final$popfit_final$proximity <- NULL
    local_env.Popfit_final$popfit_final$predicted <- 0

    ##  Combine it with the other popfit finals:
    logdebug(paste0( 'Combine old popfit final with  ',list.of.old.popfits.final[[i]] ))
    popfit.final.old <- combine( popfit.final.old, local_env.Popfit_final$popfit_final )    
  } 
  
  ##  Return it:
  return(popfit.final.old)  
  
}

######################################################################################
get_popfit_quant_old <- function(only.names=FALSE) {
  ##  Function which retrieves previously constructed popfit.RData objects.
  err_mess <- ""

  list.of.old.popfits.quant <- list.files(paste0(rfg.data.old.popfits.quant),
                                          pattern=paste0("\\.Rdata$"),
                                          full.names=TRUE) 
  
  
  logdebug(paste0('Loading old popfit final from: ',rfg.data.old.popfits.quant))
  
  if ( length(list.of.old.popfits.quant) == 0 ){
    err_mess <- paste0('There is no old popfit Please check the folder : ', rfg.data.old.popfits.quant)
    stop(err_mess)
  }
  
  ##  Load it:
  logdebug(paste0( 'Load old popfit quant  with  ',list.of.old.popfits.quant[[1]] ))  
  local_env.Popfit_quant = local({load(file=list.of.old.popfits.quant[[1]]);environment()})
  
  popfit.quant.old <- local_env.Popfit_quant$popfit_quant
  popfit.quant.old$proximity <- NULL
  popfit.quant.old$predicted <- 0  
  
  if (only.names){
    
    fixed.predictors <- row.names(importance(popfit.quant.old))    
    return(popfit.quant.old)  
  }  
  
  
  for ( i in 1:length(list.of.old.popfits.quant) ) {
    
    if (i==1) next()
    
    local_env.Popfit_quant = local({load(file=list.of.old.popfits.quant[[i]]);environment()})
    
    local_env.Popfit_quant$popfit_quant$proximity <- NULL
    local_env.Popfit_quant$popfit_quant$predicted <- 0
    
    ##  Combine it with the other popfit finals:
    logdebug(paste0( 'Combine old popfit final with  ',list.of.old.popfits.quant[[i]] ))
    popfit.quant.old <- combine( popfit.quant.old, local_env.Popfit_quant$popfit_quant )    
  } 
  
  ##  Return it:
  return(popfit.quant.old)  
  
}

# 
# ######################################################################################
# get_popfit_final_old_v2 <- function() {
#   ##  Function which retrieves previously constructed popfit.RData objects.
#   err_mess <- ""
#   err_bool <- FALSE
#   
#   rfg.fixed.set.without.input.countries <- rfg.fixed.set[!rfg.fixed.set %in%
#                                                            rfg.input.countries]
#   
#   ##  Check if a popfit object exists for combined countries (like NPL_BTN):
#   ##    Declare the country tag from the fixed set parameter:
#   countries.tag   <- as.character(rfg.fixed.set.without.input.countries[1]) 
#   
#   ##  For every country in the fixed set:
#   for ( i in 1:length(rfg.fixed.set.without.input.countries) ) {
#     ##  If only one coutnry is specified in the fixed set, pass:
#     if (i==1) next()
#     ##  Construct a multi-country tag to search for the popfit object:
#     countries.tag <- paste(countries.tag, rfg.fixed.set.without.input.countries[i], sep = "_") 
#   }  
#   ##  Declare the directory and path where we should find it:
#   popfit.final.old.folder   <- paste(root_path, '/output/prj_', rfg.input.year,
#                                      '_',countries.tag, '/tmp/',sep="")
#   popfit.final.old.file.path   <- paste0(popfit.final.old.folder,
#                                          'popfit_final_prj_',rfg.input.year,'_',
#                                          countries.tag,'.Rdata') 
#   
#   ##  If it exists where we expect it to:
#   if(file.exists(popfit.final.old.file.path) ){
#     
#     logdebug(paste0('Loading old popfit final from: ',popfit.final.old.file.path))
#     
#     ##  Load it:
#     local_env.Popfit_final = local({load(file=popfit.final.old.file.path);environment()})
#     
#     popfit.final.old <- local_env.Popfit_final$
#       
#     popfit.final.old$proximity <- NULL
#     popfit.final.old$predicted <- 0      
#     
#     ##  Return it:
#     return(popfit.final.old)
#     
#   }else{
#     err_mess <- paste0('Old Popfit final does not exist for: ',countries.tag,'\nPlease check that file exist in : ', popfit.final.old.file.path)
#   }
#   
#   ##  Load popfit for each individual country if combined does not exist:
#   ##  Select fixed countries excluding the current rfg.input.countries:
# 
#  
#   ##  For every one of the input countries:
#   for ( i in 1:length(rfg.fixed.set.without.input.countries) ) {
#     old_counry <-  as.character(rfg.fixed.set.without.input.countries[i])
#     
#     ##  Declare where we should find it:
#     popfit.final.old.folder   <- paste(root_path, '/output/prj_', rfg.input.year,'_',old_counry, '/tmp/',sep="")
#     popfit.final.old.file.path   <- paste0(popfit.final.old.folder,'popfit_final_prj_',rfg.input.year,'_',old_counry,'.Rdata') 
#     
#     ##  If it exists:
#     if(file.exists(popfit.final.old.file.path)){
#       ##  Load it:
#       local_env.Popfit_final = local({load(file=popfit.final.old.file.path); environment()})
#       if ( i == 1 ){
#         logdebug(paste0('Loading old popfit final for: ',old_counry,' from ',popfit.final.old.file.path))
#         popfit.final.old <- local_env.Popfit_final$popfit_final
# 
#         popfit.final.old$proximity <- NULL
#         popfit.final.old$predicted <- 0
#         }else{
#           popfit.final.old$proximity <- NULL
#           popfit.final.old$predicted <- 0
#           ##  Combine it with the other popfit finals:
#           logdebug(paste0('Combine old popfit final for: ',old_counry,' from ',popfit.final.old.file.path))
#           popfit.final.old <- combine(popfit.final.old, local_env.Popfit_final$popfit_final)
#           }
#     }else{
#       err_mess <- paste0('Old Popfit final does not exist for: ',i,'\nPlease check that file exist in : ', popfit.final.old.file.path)
#       err_bool <- TRUE
#     }    
#   }  
#   
#   if (err_bool == FALSE) {
#     
#     return(popfit.final.old)
#     
#   }else{
# 
#     stop(err_mess)  
#     
#   }
# }



######################################################################################
## Set the fixed_set to existing countries if you are using an existing
##    set of randomForest objects to predict from:
#

set_fixed_set_to_existing_countries <- function() {
  
  if (rfg.fixed.set){  
  
    popfit.final.old <- get_popfit_final_old()
    popfit.quant.old <- get_popfit_quant_old()

    if (rfg.fixed.set.incl.input.countries==TRUE & 
        (nrow(census_data) > rfg.fixed.set.idmin.id.threshold)) {
    
      popfit.final.tmp <- popfit_final
      popfit.quant.tmp <- popfit_quant
    
      popfit.final.tmp$proximity <- NULL
      popfit.final.tmp$predicted <- 0

      #popfit.quant.tmp$proximity <- NULL
      popfit.quant.tmp$predicted <- 0

      popfit_final <<- combine(popfit.final.tmp, popfit.final.old)
      popfit_quant <<- combine(popfit.quant.tmp, popfit.quant.old)  
    
    }else{
    
      popfit_final <<- popfit.final.old
      popfit_quant <<- popfit.quant.old
    
    }

  }  
}


# set_fixed_set_to_existing_countries_v2 <- function() {
#   
# if (!is.null(rfg.fixed.set)) {
# 
#   err_mess <- ""
#   err_bool <- FALSE
#   
#   # getting number of creseach countries in fixset
#   rfg.input.countries.in.set <- length(which(rfg.input.countries %in% rfg.fixed.set))
#   
#   # get list of the fixed.set without main countries
#   rfg.fixed.set.without.input.countries <- rfg.fixed.set[!rfg.fixed.set %in% rfg.input.countries]  
#   
#   # first check popfit for the combined countries like NPL_BTN
#   #  
#   if ( length(rfg.fixed.set) > 1 ){
# 
#     if ( rfg.input.countries.in.set >0 ) {
# #      rfg.fixed.set.tmp <- setdiff( rfg.fixed.set, rfg.input.countries)
#       rfg.fixed.set.tmp <- rfg.fixed.set.without.input.countries
#     } else{
#       rfg.fixed.set.tmp <- rfg.fixed.set
#     }
#       countries.tag   <- as.character(rfg.fixed.set.tmp[1]) 
#   
#       for ( i in 1:length(rfg.fixed.set.tmp) ) {
#         if  (i==1)   next()
#         countries.tag <- paste(countries.tag, rfg.fixed.set.tmp[[i]], sep = "_") 
#       }  
#   
#         popfit.final.old.folder   <- paste(root_path, '/output/prj_', rfg.input.year,'_',countries.tag, '/tmp/',sep="")
#         popfit.final.old.file.path   <- paste0(popfit.final.old.folder,'popfit_final_prj_',rfg.input.year,'_',countries.tag,'.Rdata') 
#         popfit.quant.old.file.path   <- paste0(popfit.final.old.folder,'popfit_quant_prj_',rfg.input.year,'_',countries.tag,'.Rdata') 
#                                          
#                                          
#       if( (file.exists(popfit.final.old.file.path))  &  (file.exists(popfit.quant.old.file.path)  )){
#     
#         logdebug(paste0('Laoding old popfit final from: ',popfit.final.old.file.path))
#         logdebug(paste0('Laoding old quant final from: ',popfit.quant.old.file.path))
#         
#         #loading popfit_final in local enviremnt
#         local_env.Popfit_final = local({load(file=popfit.final.old.file.path); environment()})
#         local_env.Popfit_quant = local({load(file=popfit.quant.old.file.path); environment()})
#     
#         local_env.Popfit_final$popfit_final$proximity <- NULL
#         local_env.Popfit_final$popfit_final$predicted <- 0
#         local_env.Popfit_quant$popfit_quant$predicted <- 0
#     
# 
#         
#         # if one of the research countries are included to rfg.fixed.set then we combine
#         # curent popfit with one from rfg.fixed.set
# 
#         if ( rfg.input.countries.in.set > 0 ){
#           
#           local_env.Popfit_final$popfit_final$proximity <- NULL
#           local_env.Popfit_final$popfit_final$predicted <- 0
#           local_env.Popfit_quant$popfit_quant$predicted <- 0
#          
#           popfit_final <<- combine(popfit_final, local_env.Popfit_final$popfit_final)
#           popfit_quant <<- combine(popfit_quant, local_env.Popfit_quant$popfit_quant)         
# 
#         }else{
#           
#           local_env.Popfit_final$popfit_final$proximity <- NULL
#           local_env.Popfit_final$popfit_final$predicted <- 0
#           local_env.Popfit_quant$popfit_quant$predicted <- 0  
#           
#           popfit_final <<- local_env.Popfit_final$popfit_final
#           popfit_quant <<- local_env.Popfit_quant$popfit_quant            
#         
#         }
# 
#         #assign(popfit_final, local_env.Popfit_final$popfit_final, envir = .GlobalEnv) 
#         #assign(popfit_quant, local_env.Popfit_quant$popfit_quant, envir = .GlobalEnv) 
#     
#       return(TRUE)
#     
#       }else{
#         err_mess <- paste0('Old Popfit final does not exist for: ',countries.tag,'\nPlease check that file exist in : ', popfit.final.old.file.path)
#         err_bool <- TRUE
#       }
#   }
#   # 
#   ###########################################################   
#   
#   
#   # load popfit for the each countries if combined does not exist
#   #
#   tmpBool <- FALSE
#   # first check if our main country in the list
#   if ( rfg.input.countries.in.set > 0){
#     
#     #tmpFilePath_final <-  paste0(rfg.output.path.countries.tmp,rfg.popfit.final.RData)
#     #tmpFilePath_quant <-  paste0(rfg.output.path.countries.tmp,rfg.popfit.quant.RData)
#     
#     #if( (file.exists(tmpFilePath_final))  &  (file.exists(tmpFilePath_quant))  ){
#         popfit_final_combined <- popfit_final
#         popfit_quant_combined <- popfit_quant
#         tmpBool <- TRUE
#     #}
#   }  
# 
# 
#   
#   for ( i in 1:length(rfg.fixed.set.without.input.countries) ) {
#     
#     old_counry <-  as.character(rfg.fixed.set.without.input.countries[i])
# 
#     popfit.final.old.folder   <- paste(root_path, '/output/prj_', rfg.input.year,'_',old_counry, '/tmp/',sep="")
#     popfit.final.old.file.path   <- paste0(popfit.final.old.folder,'popfit_final_prj_',rfg.input.year,'_',old_counry,'.Rdata') 
#     popfit.quant.old.file.path   <- paste0(popfit.final.old.folder,'popfit_quant_prj_',rfg.input.year,'_',old_counry,'.Rdata') 
#     
#       if(file.exists(popfit.final.old.file.path)  &  file.exists(popfit.quant.old.file.path )){
#       
#           logdebug(paste0('Laoding old popfit final  for: ',old_counry,' from ', popfit.final.old.file.path))
#           logdebug(paste0('Laoding old popfit quant  for: ',old_counry,' from ', popfit.quant.old.file.path))
#       
#           # loading popfit_final in local enviremnt
#           local_env.Popfit_final = local({load(file=popfit.final.old.file.path); environment()})
#           local_env.Popfit_quant = local({load(file=popfit.quant.old.file.path); environment()})
#       
#           if ( i==1 & tmpBool==FALSE ){
#             
#             popfit_final_combined <- local_env.Popfit_final$popfit_final
#             popfit_quant_combined <- local_env.Popfit_quant$popfit_quant
# 
#             popfit_final_combined$proximity <- NULL
#             popfit_final_combined$predicted <- 0
#             popfit_quant_combined$predicted <- 0
# 
#           }else{
#             
#             local_env.Popfit_final$popfit_final$proximity <- NULL
#             local_env.Popfit_final$popfit_final$predicted <- 0
#             local_env.Popfit_quant$popfit_quant$predicted <- 0
#             
#             popfit_final_combined <- combine(popfit_final_combined, local_env.Popfit_final$popfit_final)
#             popfit_quant_combined <- combine(popfit_quant_combined, local_env.Popfit_quant$popfit_quant)
#             
#              
#           }
#           
#         err_bool <- FALSE 
#         
#       }else{
#         err_mess <- paste0('Old Popfit final does not exist for: ',old_counry,'\nPlease check that file exist in : ', popfit.final.old.file.path)
#         err_bool <- TRUE
#       }    
#  
#    }  
# 
#   
#    if (err_bool == FALSE) {
#      
#      #assign(popfit_final, popfit_final_combined, envir = .GlobalEnv) 
#      #assign(popfit_quant, popfit_quant_combined, envir = .GlobalEnv) 
#     
#      popfit_final <<- popfit_final_combined
#      popfit_quant <<- popfit_quant_combined
#      
#      ##  Save off our popfit object for this set of data:
#      save(popfit_final, file=paste(rfg.output.path.countries.tmp, "popfit_final_combined.RData", sep=""))
#      save(popfit_quant, file=paste(rfg.output.path.countries.tmp, "popfit_quant_combined.RData", sep=""))
# 
#      return(TRUE)
#      
#    }else{
#      
#      stop(err_mess)
#      return(FALSE)
#      
#    }
# 
#   
#  } ## end   if (!is.null(rfg.fixed.set)) {
#   
# }




######################################################################################
######################################################################################
######################################################################################
## getting a list of all covariates
#
get_fixed_predictors <- function() {
  
  if (rfg.fixed.set) {

    fixed.predictors <- get_popfit_final_old(only.names=TRUE)
  
  }else{
    
    fixed.predictors <- get.covariates.var.names() 
  }
  
  return(fixed.predictors)
}


######################################################################################
## Tuning of our randomForest population density regression
#
#
get_init_popfit <- function() {
  
  if (file.exists(paste(rfg.output.path.countries.tmp, rfg.init.popfit.RData, sep=""))) {
    
    loginfo("Tuning of our randomForest population density regression was done before. Loading init_popfit.RData")
    load(file=paste(rfg.output.path.countries.tmp, rfg.init.popfit.RData, sep="")) 

    
  }else{
    loginfo("Start tuning of our randomForest population density regression.")
    
    start_time <- Sys.time()
    
    init_popfit = tuneRF(x=x_data, 
                         y=y_data, 
                         plot=TRUE, 
                         mtryStart=length(x_data)/3, 
                         ntreeTry=length(y_data)/20, 
                         improve=0.0001, 
                         stepFactor=1.20, 
                         trace=TRUE, 
                         doBest=TRUE, 
                         nodesize=length(y_data)/1000, 
                         na.action=na.omit, 
                         importance=TRUE, 
                         proximity=rfg.proximity, 
                         sampsize=min(c(length(y_data), 1000)), 
                         replace=TRUE) 
    
    
    end_time <- Sys.time()
    loginfo(paste("End tuning RF. Elapsed Fitting Time:", tmDiff(start_time,end_time)))
    ##	Save off our init_popfit object for this set of data:
    save(init_popfit, file=paste(rfg.output.path.countries.tmp, rfg.init.popfit.RData, sep=""))
    
  }
 
  return(init_popfit)
}


######################################################################################
## optimize the model
#
#
get_popfit <- function() {
  
  if (file.exists(paste0(rfg.output.path.countries.tmp, rfg.popfit.RData))) {
    
    loginfo(paste0("Loading popfit object from ",rfg.popfit.RData))
    load(file=paste0(rfg.output.path.countries.tmp, rfg.popfit.RData))
    
    varImpPlot(popfit) 
    
    return(popfit)
  }
  
  set.seed(2002)
  
  if (!rfg.fixed.set) {
     
    logdebug("fixed_set is FALSE. Will optimize the model... ") 	
    start_time <- Sys.time()
    init_popfit <- get_init_popfit()
    
    ##	Now we will optimize the model by iteratively removing any 
    ##		covariates with negative increases in node purity:
    
    ##	Get list of covariates that have an importance score greater than 0:
    importance_scores <- importance(init_popfit)[order(importance(init_popfit)[,1], decreasing=TRUE),]
    pos_importance <- rownames(importance_scores)[importance_scores[,1] > 0]
    
    if (length(pos_importance) == length(importance_scores[,1])) {
      
      x_data <- x_data[pos_importance]
      
      popfit = tuneRF(x=x_data, 
                      y=y_data, 
                      plot=TRUE, 
                      mtryStart=length(x_data)/3, 
                      ntreeTry=length(y_data)/20, 
                      improve=0.0001, 
                      stepFactor=1.20, 
                      trace=TRUE, 
                      doBest=TRUE, 
                      nodesize=length(y_data)/1000, 
                      na.action=na.omit, 
                      importance=TRUE, 
                      proximity=rfg.proximity, 
                      sampsize=min(c(length(y_data), 1000)), 
                      replace=TRUE) 
      
    }else{
      
      while (length(pos_importance) < length(importance_scores[,1])) {
        
        logdebug(" Jumping into the [while (length(pos_importance) < length(importance_scores[,1])) ] ... ")
        ##	Subset our x_data to just those columns having positive scores:
        x_data <- x_data[pos_importance]
        
        popfit = tuneRF(x=x_data, 
                        y=y_data, 
                        plot=TRUE, 
                        mtryStart=length(x_data)/3, 
                        ntreeTry=length(y_data)/20, 
                        improve=0.0001, 
                        stepFactor=1.20, 
                        trace=TRUE, 
                        doBest=TRUE, 
                        nodesize=length(y_data)/1000, 
                        na.action=na.omit, 
                        importance=TRUE, 
                        proximity=rfg.proximity, 
                        sampsize=min(c(length(y_data), 1000)), 
                        replace=TRUE) 
        
        ##	Re-check importance scores:
        importance_scores <- importance(popfit)[order(importance(popfit)[,1], decreasing=TRUE),]
        pos_importance <- rownames(importance_scores)[importance_scores[,1] > 0]
        print(popfit)
        
      } ## End of while loop
    }
    
    end_time <- Sys.time()
    loginfo(paste("Elapsed Fitting Time:", tmDiff(start_time,end_time)))
    
  } else {
    
    popfit_final_old <- get_popfit_final_old()
    
    popfit = randomForest(x=x_data, 
                          y=y_data, 
                          mtry=popfit_final_old$mtry, 
                          ntree=popfit_final_old$ntree, 
                          nodesize=length(y_data)/1000, 
                          importance=TRUE, 
                          proximity=rfg.proximity)
    print(popfit)
    
  }  
  
  loginfo(paste0("Saving ",rfg.popfit.RData))
  ##	Save off our popfit object for this set of data:
  save(popfit, file=paste0(rfg.output.path.countries.tmp, rfg.popfit.RData)) 
  
  
  
  ##	For continuous regression, plot observed vs. predicted:
  plot(x=y_data, y=predict(popfit), ylim=c(min(y_data),max(y_data)), xlim=c(min(y_data),max(y_data)))
  abline(a=0, b=1, lty=2)
  
  ###	For continuous regression, plot residuals vs. observed:
  #plot(x=y_data, y=(y_data - predict(popfit)), xlim=c(0,max(y_data)))
  #abline(a=0, b=0, lty=2)
  #
  ###	For continuous regression, plot residuals vs. fitted:
  #plot(x=predict(popfit), y=(y_data - predict(popfit)), xlim=c(0,max(y_data)))
  #abline(a=0, b=0, lty=2)
  
  varImpPlot(popfit)  
  
  return(popfit)
  
}


######################################################################################
##	Another alternative is to use Quantile Regression Forests to generate
##		prediction intervals.  We'll fit a quantile regression using
##		the tuning parameters pulled from the popfit object above:

get_popfit_final <- function() {
  
  if (file.exists(paste0(rfg.output.path.countries.tmp, rfg.popfit.final.RData))) {
    
    loginfo(paste0("Loading popfit object from ",rfg.popfit.final.RData))
    load(file=paste0(rfg.output.path.countries.tmp, rfg.popfit.final.RData))
    
  }else{

    set.seed(2010)
    popfit_final <- randomForest(x=x_data, 
                              y=y_data, 
                              mtry=popfit$mtry, 
                              ntree=popfit$ntree, 
                              nodesize=length(y_data)/1000, 
                              importance=TRUE, 
                              proximity=rfg.proximity,
                              do.trace=F)
  
    loginfo(paste0("Saving popfit_final object  ",rfg.popfit.final.RData))
    save(popfit_final, file=paste(rfg.output.path.countries.tmp, rfg.popfit.final.RData, sep=""))
  
  } 
  
  return(popfit_final)
  
}

######################################################################################
#
#
#
get_popfit_quant <- function() {
  
  if (file.exists(paste0(rfg.output.path.countries.tmp, rfg.popfit.quant.RData))) {
    
    loginfo(paste0("Loading popfit object from ",rfg.popfit.quant.RData))
    load(file=paste0(rfg.output.path.countries.tmp, rfg.popfit.quant.RData))
    
  }else{  
  
    set.seed(2010)
  
    popfit_quant <- quantregForest(x=x_data, 
                                 y=y_data, 
                                 mtry=popfit$mtry, 
                                 ntree=popfit$ntree, 
                                 nodesize=length(y_data)/1000)
  
    loginfo(paste0("Saving popfit_quant object  ",rfg.popfit.quant.RData))
    save(popfit_quant, file=paste(rfg.output.path.countries.tmp, rfg.popfit.quant.RData, sep=""))
  
  } 
  
  return(popfit_quant)
  
}




######################################################################################
#
#
save_rf_pred <- function(rf_pred,mask) {
  
  logdebug(paste0('Saving : ',rfg.predict.density.rf.pred))
  
  sd <- writeStart(mask, 
                   filename=paste0(rfg.output.path.countries,rfg.predict.density.rf.pred), 
                   format="GTiff", 
                   datatype="FLT4S", 
                   overwrite=TRUE, 
                   options=c("COMPRESS=LZW"))
  
  sd <- writeValues(sd, as.matrix(rf_pred),start =1 )
  sd <- writeStop(sd)
  
}

######################################################################################
#
#
save_rf_sd <- function(rf_sd,mask) {
  
  logdebug(paste0('Saving : ',rfg.predict.density.rf.sd))
  
  sd <- writeStart(mask, 
                   filename=paste0(rfg.output.path.countries,rfg.predict.density.rf.sd), 
                   format="GTiff", 
                   datatype="FLT4S", 
                   overwrite=TRUE, 
                   options=c("COMPRESS=LZW"))
  
  sd <- writeValues(sd, as.matrix(rf_sd),start =1 )
  sd <- writeStop(sd)
  
}

######################################################################################
#
#
save_all_pred <- function(rf_sd,mask) {
  
  save_rf_pred (rf_sd$result_rf_pred,mask)
  save_rf_sd(rf_sd$result_rf_sd,mask)
  
  colnames(oper) <-c('result_rf_pred','result_rf_sd','result_rf_05','result_rf_50','result_rf_95')
  
  rfg.predict.density_rf_05 <- paste0("predict_density_rf_05_",rfg.countries.tag, ".tif")
  rfg.predict.density_rf_50 <- paste0("predict_density_rf_50_",rfg.countries.tag, ".tif")
  rfg.predict.density_rf_95 <- paste0("predict_density_rf_95_",rfg.countries.tag, ".tif")
  
  logdebug(paste0('Saving : ',rfg.predict.density_rf_05))
  
  sd <- writeStart(mask, 
                   filename=paste0(rfg.output.path.countries,rfg.predict.density_rf_05), 
                   format="GTiff", 
                   datatype="FLT4S", 
                   overwrite=TRUE, 
                   options=c("COMPRESS=LZW"))
  
  sd <- writeValues(sd, as.matrix(rf_sd),start =1 )
  sd <- writeStop(sd)
  
  logdebug(paste0('Saving : ',rfg.predict.density_rf_50))
  
  sd <- writeStart(mask, 
                   filename=paste0(rfg.output.path.countries,rfg.predict.density_rf_50), 
                   format="GTiff", datatype="FLT4S", 
                   overwrite=TRUE, 
                   options=c("COMPRESS=LZW"))
  
  sd <- writeValues(sd, as.matrix(rf_sd),start =1 )
  sd <- writeStop(sd)
  
  logdebug(paste0('Saving : ',rfg.predict.density_rf_95))
  
  sd <- writeStart(mask, 
                   filename=paste0(rfg.output.path.countries,rfg.predict.density_rf_95), 
                   format="GTiff", 
                   datatype="FLT4S", 
                   overwrite=TRUE, 
                   options=c("COMPRESS=LZW"))
  
  sd <- writeValues(sd, as.matrix(rf_sd),start =1 )
  sd <- writeStop(sd)   
}

######################################################################################
#
#
get_pop_census_all  <- function() {

  census_data <- load.pop.table(rfg.input.countries[1])
  
  for ( i in 1:length(rfg.input.countries) ) {
    if (i==1) next()
    census_data <- rbind(census_data, load.pop.table(rfg.input.countries[i])) 
  }
  
 return(census_data)  
}

######################################################################################
#
#
rasterize_data  <- function(df,flname) {
  
  colnames(df) <- c("ADMINID", "ADMINPOP")
   
    rst <- raster(censusmaskPathFileName)
    
  # ccidadminl1 <- rasterToPoints(rst)
  # colnames(ccidadminl1) <-c('x','y',"ADMINID")
  # 
  # ccidadminl1 <- merge(ccidadminl1, df, all = TRUE, by = "ADMINID")
  # 
  # ccidadminl1 <- ccidadminl1[ ,  c("x","y","ADMINPOP"), drop = FALSE]
  # colnames(ccidadminl1) <- c("x","y","v")
  # 
  # tmpRaster <- rasterFromXYZ(ccidadminl1)
  # 
  # crs(tmpRaster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # projection(tmpRaster)
  # 
  # 
  # writeRaster(tmpRaster, 
  #             filename=paste0(rfg.output.path.countries,flname),
  #             format="GTiff", 
  #             overwrite=TRUE, 
  #             NAflag=-99999, 
  #             datatype='FLT4S', 
  #             options=c("COMPRESS=LZW")  
  # )
  
  tmpRaster <- wpRasterize(rst,
                           df,
                           cores=rfg.cluster_workers,
                           filename=paste0(rfg.output.path.countries, flname),
                           overwrite=TRUE,
                           silent=TRUE)
  
  
  return(tmpRaster)
  
}


######################################################################################
######################################################################################
# Apply population densityto a final RF prediction   (RF_pred +L1_pop)/RF_pred_ZS_sum
# We will need to change it by using gdal_calc
#
apply_population_density <- function() {
  
  # rasterising pop table 
  rst.pop.census <- rasterize_data(get_pop_census_all(), rfg.rst.pop.census.tif)
  
  zonal.stats.rf.pred.sum <- calculate.zonal.stats.rf.pred.sum()
  
  
  
  # rasterising RF prediction
  rst.zonal.stats.rf.pred <- rasterize_data(zonal.stats.rf.pred.sum, rfg.rst.zonal.stats.rf.pred.tif)
  
  rst.predict.density.rf.pred <- raster(paste0(rfg.output.path.countries, rfg.predict.density.rf.pred))  
  
  #s <- stack(rst.predict.density.rf.pred, rst.pop.census, rst.zonal.stats.rf.pred)
  
  #r_calc <- calc(s, fun=function(x){ (x[1]+x[2])/x[3] })
  
  r_calc <- (rst.predict.density.rf.pred * rst.pop.census)/rst.zonal.stats.rf.pred
  
  writeRaster(r_calc, 
              filename=paste0(rfg.output.path.countries,rfg.predict.density.rf.pred.final),
              format="GTiff", 
              overwrite=TRUE, 
              NAflag=-99999, 
              datatype='FLT4S', 
              options=c("COMPRESS=LZW")
  )

}
