# modified by me -------------------------------------------------------------------------------------------

multiplot.wrapper <- function(model.list,
                              siteCovs_unscaled,
                              modelRanking,
                              occasionLength,
                              plotnames.by.rank,
                              plotDir,
                              speciesVector,
                              index_tmp = 1,
                              addRug = TRUE,
                              maximumRankToPlot,
                              plotModelsBelowNullModel,
                              quiet
)
{


# # uncomment objects below to run by single index
# model.list = fms@fits
# siteCovs_unscaled = covariates %>% select( -matches("scale."))
# siteCovs_unscaled = covariates %>% select( matches("scale."))
# modelRanking = modsel
# occasionLength = occasionLength
# #plotnames.by.rank ,
# tmpdir <- tmpDir()
# plotDir = tmpdir
# speciesVector = myspecies #relevantspecies$species[[i]],
# index_tmp = 1
# addRug = TRUE
# #maximumRankToPlot = 200,
# plotModelsBelowNullModel = F
# quiet = T


wd0 <- getwd()
on.exit(setwd(wd0))

p_val_list <- list()

rank_null_model    <- grep(modelRanking@Full$model, pattern = "p(Effort) psi(.) data")   #LP: original: "~ 1$", but makes no sense

for(model_index in 2:length(model.list)){  # LP: starts from two to exclude null model, otherwise not working
#model_index = 5
rank_current_model <- which(modelRanking@Full$model == names(model.list)[model_index])

# container for p-values
p_val_list[[model_index]] <- vector()

# summary to obtain p-values of current model
if(isTRUE(quiet)) sink("nul:")
p.val.tmp <- summary(model.list[[model_index]]@estimates@estimates$state)[,"P(>|z|)"]
if(isTRUE(quiet)) sink()

# extract model estimates
estimates.tmp <- model.list[[model_index]]@estimates@estimates$state@estimates

# --> I commented out
# if there is a quadratic term in formula, remove that one so covariate names in data frame match covariate names in model formula
if(any(sapply(names(estimates.tmp), grepl, pattern = "_sq", fixed = T))) {   #^2
  estimates.tmp.quadratic <- estimates.tmp
  # careful! hier wird _sq herausgenommen..
  #estimates.tmp <- estimates.tmp[-which(sapply(names(estimates.tmp), grepl, pattern = "_sq", fixed = T))]  #^2
}

# extract scaled site site covariates of current model
tmp <- as.data.frame(model.list[[model_index]]@data@siteCovs[,match(names(estimates.tmp)[2:length(estimates.tmp)],names(model.list[[model_index]]@data@siteCovs))])

# calculate quantiles and add mean (= 0) of scaled covariates
tmp2 <- as.data.frame(sapply(tmp, FUN = quantile))
tmp2 <- rbind(tmp2, mean = 0)
tmp2 <- data.frame(tmp2[order(tmp2[,1]),])
names(tmp2) <- names(tmp) <- names(estimates.tmp)[-1]

# extract unscaled covariates
tmp3 <- data.frame(siteCovs_unscaled[,names(tmp2)])

# calculate quantiles and add mean (= 0) of unscaled covariates
tmp3a <- as.data.frame(apply(tmp3, MARGIN = 2, FUN = quantile))
tmp3a <- rbind(tmp3a, mean = sapply(tmp3, FUN = mean))
tmp3b <- as.data.frame(tmp3a[order(tmp3a[,1]),])
rownames(tmp3b) <- rownames(tmp3a)[order(tmp3a[,1])]
names(tmp3a) <- names(tmp3b) <- paste(names(tmp2), "orig", sep = "_")

# container for plots
p <- list()

# for each indiviual covariate (if there is several in the current model)
for(o in 1:ncol(tmp3b)){
#o <- 4
  tmp4plot <- tmp
  if(ncol(tmp3b) >= 2){     # set other columns to 0 (= mean)
    tmp4plot [,-o]  <- 0
  }

  # create sequence from min to max for current covariate
  # sequence length = number of camera trap stations in model
  tmp4plot[,o] <- seq(min(tmp4plot [,o]), max(tmp4plot [,o]), length.out = nrow(tmp4plot))

  # backtransform to unscaled values. tmp4plot is scaled (mean = 0, sd = 1)
  tmp4plot2 <- tmp4plot
  tmp4plot2[,o] <- (tmp4plot[,o] * attributes(scale(tmp3))$`scaled:scale`[o]) + attributes(scale(tmp3))$`scaled:center`[o]

  # create model predictions
  dat4plot <- cbind(tmp4plot2,
                    predict(model.list[[model_index]], newdata = data.frame(tmp4plot), type = "state"))   # prediction creates outtable for plot

  # LP: plot curves, but select respective covariate!
  #plot(dat4plot$Predicted ~ dat4plot$scale.elevation_sq, type = "b")
  
  
  # names of columns of dat4plot
  nms <- paste("`", names(dat4plot), "`", sep="")

  # obtain p-values of current model 

  # if(exists("estimates.tmp.quadratic")) {
  # #  p.val.tmp2 <- p.val.tmp[(o + c(1,4))]   # if there is a quadratic term in the formula, return p-values of linear and quadratic covariate
  #   p.val.tmp2 <- p.val.tmp[o + 4]   # if there is a quadratic term in the formula, return p-values of linear and quadratic covariate
  # } else {
    p.val.tmp2 <- p.val.tmp[o + 1]  # so wird jede richtig ausgewählt
  # }

  # p_val_list[[model_index]][o] <- p.val.tmp2      # this one fails for quadratic terms
  # names(p_val_list[[model_index]])[o] <- nms[o]

  # prepare rugs for plots
  # rug1 = rug for stations with records, rug2 = for staitons without records
  rug1 <- rug2 <- as.data.frame(tmp3)

  rug1[which(rowSums(model.list[[model_index]]@data@y, na.rm = T) == 0),] <- NA   # remove values at stations without records
  rug2[which(rowSums(model.list[[model_index]]@data@y, na.rm = T) >= 1),] <- NA   # remove values at stations with records

  # add rugs to plot data frame
  dat4plot <- cbind(dat4plot, rug1 = rug1[,o], rug2 = rug2[,o])
  
  # create main plot title

  plot_title_tmp <- paste(speciesVector[index_tmp], "; \n",#"Psi (", paste(names(tmp2), collapse = "+") ,") ",
                          format(model.list[[model_index]]@formula),"; \n",

                          #JN EDIT
                          #"rank = ", match(format(model.list[[model_index]]@formula), modelRanking@Full$formula), ";  ",  #  rank = ", index.tmp,
                          "rank = ", rank_current_model, ";  ",
                          #JN EDIT end

                          ifelse(rank_null_model < rank_current_model, "WORSE THAN NULL MODEL", ""),
                          "; AIC = ", round(model.list[[model_index]]@AIC, 2),
                          ";\np = ", paste(round(p.val.tmp2, digits = 3), collapse = " / "), "; ",
                          paste(ifelse(p.val.tmp2 <= 0.001, "***",  #hoch signifikant
                                       ifelse(p.val.tmp2 <= 0.01, "**",
                                              ifelse(p.val.tmp2 <= 0.05, "*",
                                                     ifelse(p.val.tmp2 <= 0.1, ".", "ns")))), collapse = " / "), #nicht sigifikant
                          ";\nconvergence = ", ifelse(model.list[[model_index]]@opt$convergence == 0, "yes", "no"),
                          sep = "")

  # # how to backtransform logit to probability for xaxis?
  # dat4plot_prob <- dat4plot
  # logit2prob(dat4plot_prob$Predicted) 
  
  # generate the plot in ggplot
  p[[o]] <- ggplot(data = dat4plot,  #dat4plot_prob
                   aes_string(x=nms[o], y=nms[grep("Predicted", nms)])) +
    geom_ribbon(aes(ymin=lower, ymax=upper),
                alpha=0.15) +
    # geom_ribbon(aes(ymin=Predicted-SE, ymax=Predicted+SE),
    #             alpha=0.2) +
    geom_line(size = 1) +
    ylim (0,1) +
    theme_bw() +
    theme(plot.title = element_text(size = 7, face="bold"),
          axis.title = element_text(size = 9, face="bold"),
          axis.text  = element_text(size = 9, face="bold")) +
    #xlab(names(tmp2)[o]) +
    xlab(gsub("scale.", "", names(tmp2)[o])) +
    ylab("Psi") +                                                  # me: is Psi same as ocupancy probability?
    labs(title = plot_title_tmp)

  if(isTRUE(addRug)){
    p[[o]] <- p[[o]] +
      geom_rug(mapping = aes(x = rug2), sides = "b", col = "black", alpha = 1/4) +   # stations with non-detection
      geom_rug(mapping = aes(x = rug1), sides = "t", col = "black", alpha = 1/4)     # stations with detections
  }
  rm(plot_title_tmp)
}    # end column loop (o)

dir.create(plotDir, showWarnings = FALSE)
setwd(plotDir)


filename_current_plot <- paste(speciesVector[index_tmp], "_occupancy_",
                               format(model.list[[model_index]]@formula), ".png", sep = "")
filename_current_plot <- gsub("~", "&", filename_current_plot)

try(expr  = {png(filename = filename_current_plot,  # save plot
                 width = 15, height = 10, units = c("cm"), res = 300)
  Rmisc::multiplot(plotlist = p, cols = length(p))
  dev.off()
  Rmisc::multiplot(plotlist = p, cols = length(p))

}
)

# cleanup
suppressWarnings(rm(estimates.tmp.quadratic))
}
#  return(p_val_list)   # this may be incorporated to return p-values of coefficients, but not incorporated yet (doesn't work yet with quadratic terms)
}



# logit2prob <- function(logit){
#   odds <- exp(logit)
#   prob <- odds / (1 + odds)
#   return(prob)
# }



# original -----------------------------------------------------------------------------------------------
# (source somewhere IZW)
#
# multiplot.wrapper <- function(model.list, 
#                               siteCovs_unscaled,
#                               modelRanking,
#                               occasionLength,
#                               plotnames.by.rank,
#                               plotDir,
#                               speciesVector,
#                               index_tmp = 1,
#                               addRug = TRUE,
#                               maximumRankToPlot,
#                               plotModelsBelowNullModel,
#                               quiet
# ){
#   
# 
# # model.list = fms@fits
# # siteCovs_unscaled = covariates %>% select( -matches("scale."))
# # siteCovs_unscaled = covariates %>% select( matches("scale."))
# # modelRanking = modsel
# # occasionLength = occasionLength
# # #plotnames.by.rank ,
# # plotDir = tmpdir
# # speciesVector = myspecies #relevantspecies$species[[i]],
# # index_tmp = 1
# # addRug = TRUE
# # #maximumRankToPlot = 200,
# # plotModelsBelowNullModel = F
# # quiet = T
# 
#   
#   wd0 <- getwd()
#   on.exit(setwd(wd0))
#   
#   p_val_list <- list()
#   
#   rank_null_model    <- grep(modelRanking@Full$model, pattern = "p(Effort) psi(.) data")   #original: "~ 1$"
#   
#   for(model_index in 1:length(model.list)){
#   #model_index = 5  
#     rank_current_model <- which(modelRanking@Full$model == names(model.list)[model_index])
#     
#     # container for p-values
#     p_val_list[[model_index]] <- vector()
#     
#     # summary to obtain p-values of current model
#     if(isTRUE(quiet)) sink("nul:")
#     p.val.tmp <- summary(model.list[[model_index]]@estimates@estimates$state)[,"P(>|z|)"]
#     if(isTRUE(quiet)) sink()
#     
#     # extract model estimates
#     estimates.tmp <- model.list[[model_index]]@estimates@estimates$state@estimates
#     
#     # if there is a quadratic term in formula, remove that one so covariate names in data frame match covariate names in model formula
#     if(any(sapply(names(estimates.tmp), grepl, pattern = "^2", fixed = T))) {
#       estimates.tmp.quadratic <- estimates.tmp
#       estimates.tmp <- estimates.tmp[-which(sapply(names(estimates.tmp), grepl, pattern = "^2", fixed = T))]
#     }
#     # extract scaled site site covariates of current model
#     tmp <- as.data.frame(model.list[[model_index]]@data@siteCovs[,match(names(estimates.tmp)[2:length(estimates.tmp)],
#                                                                         names(model.list[[model_index]]@data@siteCovs))])
#     
#     # calculate quantiles and add mean (= 0) of scaled covariates
#     tmp2 <- as.data.frame(sapply(tmp, FUN = quantile))
#     tmp2 <- rbind(tmp2, mean = 0)
#     tmp2 <- data.frame(tmp2[order(tmp2[,1]),])
#     names(tmp2) <- names(tmp) <- names(estimates.tmp)[-1]
#     
#     # extract unscaled covariates
#     tmp3 <- data.frame(siteCovs_unscaled[,names(tmp2)])
#     
#     # calculate quantiles and add mean (= 0) of unscaled covariates
#     tmp3a <- as.data.frame(apply(tmp3, MARGIN = 2, FUN = quantile))
#     tmp3a <- rbind(tmp3a, mean = sapply(tmp3, FUN = mean))
#     tmp3b <- as.data.frame(tmp3a[order(tmp3a[,1]),])
#     rownames(tmp3b) <- rownames(tmp3a)[order(tmp3a[,1])]
#     names(tmp3a) <- names(tmp3b) <- paste(names(tmp2), "orig", sep = "_")
#     
#     # container for plots
#     p <- list()
#     
#     # for each indiviual covariate (if there is several in the current model)
#     for(o in 1:ncol(tmp3b)){             
#       tmp4plot <- tmp
#       if(ncol(tmp3b) >= 2){     # set other columns to 0 (= mean)
#         tmp4plot [,-o]  <- 0
#       } 
#       
#       # create sequence from min to max for current covariate
#       # sequence length = number of camera trap stations in model
#       tmp4plot[,o] <- seq(min(tmp4plot [,o]), max(tmp4plot [,o]), length.out = nrow(tmp4plot))    
#       
#       # backtransform to unscaled values. tmp4plot is scaled (mean = 0, sd = 1)
#       tmp4plot2 <- tmp4plot    
#       tmp4plot2[,o] <- (tmp4plot[,o] * attributes(scale(tmp3))$`scaled:scale`[o]) + attributes(scale(tmp3))$`scaled:center`[o]
#       
#       # create model predictions
#       dat4plot <- cbind(tmp4plot2, 
#                         predict(model.list[[model_index]], newdata = data.frame(tmp4plot), type = "state"))   # prediction creates outtable for plot
#       
#       # names of columns of dat4plot
#       nms <- paste("`", names(dat4plot), "`", sep="")
#       
#       # obtain p-values of current model
#       
#       if(exists("estimates.tmp.quadratic")) {
#         p.val.tmp2 <- p.val.tmp[(o + c(1,2))]   # if there is a quadratic term in the formula, return p-values of linear and quadratic covariate    
#       } else {      
#         p.val.tmp2 <- p.val.tmp[o + 1] 
#       }
#       # p_val_list[[model_index]][o] <- p.val.tmp2      # this one fails for quadratic terms
#       # names(p_val_list[[model_index]])[o] <- nms[o]
#       
#       # prepare rugs for plots
#       # rug1 = rug for stations with records, rug2 = for staitons without records
#       rug1 <- rug2 <- as.data.frame(tmp3)
#       
#       rug1[which(rowSums(model.list[[model_index]]@data@y, na.rm = T) == 0),] <- NA   # remove values at stations without records
#       rug2[which(rowSums(model.list[[model_index]]@data@y, na.rm = T) >= 1),] <- NA   # remove values at stations with records
#       
#       # add rugs to plot data frame
#       dat4plot <- cbind(dat4plot, rug1 = rug1[,o], rug2 = rug2[,o])
#       
#       # create main plot title
#       
#       plot_title_tmp <- paste(speciesVector[index_tmp], "_",#"Psi (", paste(names(tmp2), collapse = "+") ,") ", 
#                               format(model.list[[model_index]]@formula),"; \n", 
#                               
#                               #JN EDIT
#                               #"rank = ", match(format(model.list[[model_index]]@formula), modelRanking@Full$formula), ";  ",  #  rank = ", index.tmp, 
#                               "rank = ", rank_current_model, ";  ",  
#                               #JN EDIT end
#                               
#                               ifelse(rank_null_model < rank_current_model, "WORSE THAN NULL MODEL", ""),
#                               ";  AIC = ", round(model.list[[model_index]]@AIC, 2),
#                               ";  p = ", paste(round(p.val.tmp2, digits = 3), collapse = " / "), "; ",
#                               paste(ifelse(p.val.tmp2 <= 0.001, "***", 
#                                            ifelse(p.val.tmp2 <= 0.01, "**", 
#                                                   ifelse(p.val.tmp2 <= 0.05, "*", 
#                                                          ifelse(p.val.tmp2 <= 0.1, ".", "ns")))), collapse = " / "),
#                               ";  convergence = ", ifelse(model.list[[model_index]]@opt$convergence == 0, "yes", "no"),
#                               sep = "")
#       
#       # generate the plot in ggplot
#       p[[o]] <- ggplot(data = dat4plot, 
#                        aes_string(x=nms[o], y=nms[grep("Predicted", nms)])) +
#         geom_ribbon(aes(ymin=lower, ymax=upper),
#                     alpha=0.15) +
#         # geom_ribbon(aes(ymin=Predicted-SE, ymax=Predicted+SE),
#         #             alpha=0.2) +
#         geom_line(size = 1) +
#         ylim (0,1) +
#         theme_bw() + 
#         theme(plot.title = element_text(size = 9, face="bold"),
#               axis.title = element_text(size = 9, face="bold"),
#               axis.text  = element_text(size = 9, face="bold")) + 
#         #xlab(names(tmp2)[o]) +
#         xlab(gsub("scale.", "", names(tmp2)[o])) +
#         ylab("Psi") +                                                  # me: is Psi same as ocupancy probability?
#         labs(title = plot_title_tmp)
#       
#       if(isTRUE(addRug)){
#         p[[o]] <- p[[o]] + 
#           geom_rug(mapping = aes(x = rug2), sides = "b", col = "black", alpha = 1/4) +   # stations with non-detection
#           geom_rug(mapping = aes(x = rug1), sides = "t", col = "black", alpha = 1/4)     # stations with detections
#       }
#       rm(plot_title_tmp)
#     }    # end column loop (o)
#     
#     dir.create(plotDir, showWarnings = FALSE)
#     setwd(plotDir)
#     
#     
#     filename_current_plot <- paste(speciesVector[index_tmp], "_occupancy_",
#                                    format(model.list[[model_index]]@formula), ".png", sep = "")
#     filename_current_plot <- gsub("~", "&", filename_current_plot)
#     
#     try(expr  = {png(filename = filename_current_plot,  # save plot
#                      width = 15, height = 10, units = c("cm"), res = 300)
#       Rmisc::multiplot(plotlist = p, cols = length(p))
#       dev.off()    
#       Rmisc::multiplot(plotlist = p, cols = length(p))
#       
#     }
#     )
#     
#     # cleanup
#     suppressWarnings(rm(estimates.tmp.quadratic))
#   } 
#   #  return(p_val_list)   # this may be incorporated to return p-values of coefficients, but not incorporated yet (doesn't work yet with quadratic terms)
# }

