library(xgboost) # Ensure you have the xgboost package loaded
library(data.table) # Keep data.table if the original code relies on its syntax
library(fastshap)
library(shapviz)

#TODO: combine poplars?

climateVariables <- P(sim)$climateVariables
PSPmodelData <- sim$PSPmodelData
PSPvalidationData <- sim$PSPvalidationData

anoms <- setdiff(names(climateVariables), "")
allClim <- c(climateVariables, anoms)

xgb_PSP <- function(trainingData = PSPmodelData,
                    validationData = PSPvalidationData,
                    stat, climVar = c(climateVariables, anoms)) {
  #------------------------------------------------------------
  ## 1. Prepare Data for XGBoost

  # Columns for the predictor matrix X
  xgbXCols <- c(climateVariables, anoms, "biomass", "logAge", "standBiomass", "psp_spp")
  #TODO: delete this maybe
  #psp_sppKey <- trainingData[, .(asInt = as.integer(psp_spp)), .(psp_spp)]
  
  # Training data
  myX <- data.matrix(trainingData[, .SD, .SDcols = xgbXCols])
  myY <- trainingData[[stat]]
  
  # Validation data
  validX <- data.matrix(validationData[, .SD, .SDcols = xgbXCols])
  validY <- validationData[[stat]]
  
  #------------------------------------------------------------
  ## 2. Create xgb.DMatrix datasets
  
  # xgboost uses xgb.DMatrix objects for efficient handling
  dtrain <- xgb.DMatrix(data = as.matrix(myX), label = myY)
  dvalid <- xgb.DMatrix(data = validX, label = validY)
  
  # Define the watchlist for evaluation and early stopping
  watchlist <- list(train = dtrain, validation = dvalid)
  
  #------------------------------------------------------------
  ## 3. Specify Parameters
  etas <- seq(0.03, 0.08, 0.0025)
  min_weights <- seq(1, 5, 1)
  depths <- seq(5, 10, 1)
  hyperparams_Full <- expand.grid(etas, min_weights, depths) |>
    as.data.table() |>
    setnames(new = c("eta", "min_weight", "depth")) |>
    setkey(eta)
  
  #start by tuning on eta
  hyperparam_1 <- hyperparams_Full[, .(min_weight = round(mean(min_weight)), 
                                       depth = round(mean(depth))), 
                                   .(eta)]
  
  tune_xgb <- function(i, hyperparam, train = dtrain, valid = dvalid, 
                       wl = watchlist, returnBST = FALSE) {
    hyperparam <- hyperparam[i,]
    # Use 'reg:squarederror' for L2 regression in xgboost
    params <- list(
      objective = "reg:squarederror",
      lambda = 0.1,
      tree_method = "hist",
      enable_categorical = TRUE,
      eval_metric = "rmse" # Often used for regression in XGBoost
    )
    
    params <- append(hyperparam, params)
    
    #------------------------------------------------------------
    ## 4. Fit Model with Validation and Early Stopping
    # Use xgb.train
    bst <- xgb.train(
      params = params,
      data = train,
      enable_categorical = TRUE,
      evals = wl,
      nrounds = 500,
      early_stopping_rounds = 25,
      verbose = 1, # Print messages during training
      # The 'maximize = FALSE' argument is needed for minimization objectives like RMSE
      maximize = FALSE 
    )
    
    best_iter <- attributes(bst)$early_stop$best_iteration
    best_score <- attributes(bst)$early_stop$best_score
    
    output <- append(hyperparam, list("best_iter" = best_iter, 
                                      "best_score" = best_score))
    
    if (returnBST) {
      output <- bst
    }
    return(output)
  }
  
  tuning1 <- lapply(X = 1:nrow(hyperparam_1), hyperparam = hyperparam_1, FUN = tune_xgb)
  tuning1 <- lapply(tuning1, as.data.table) |>
    rbindlist()
  tuning1_best <- tuning1[best_score == min(best_score)]$eta
  #run with other params
  hyperparam_2 <- hyperparams_Full[eta == tuning1_best]
  
  browser() #verify whether params are correctedly passed in tuning2
  
  tuning2 <- lapply(X = 1:nrow(hyperparam_2), hyperparam = hyperparam_2, FUN = tune_xgb)
  tuning2 <- lapply(tuning2, as.data.table) |>
    rbindlist()
  
  #in case of ties, do this
  finalParams <- tuning2[best_score == min(best_score), 
                         .(eta = mean(eta), 
                           depth = round(mean(depth)), 
                           min_weight = round(mean(min_weight)))]
  
  #final model
  bst <- tune_xgb(1, finalParams, returnBST = TRUE)
  
  eval_df <- attributes(bst)$evaluation_log
  best_iter <- attributes(bst)$early_stop$best_iteration
  best_score <- attributes(bst)$early_stop$best_score

  # Assuming 'rmse' is the evaluation metric (can be changed in params)
  setnames(eval_df, 
           old = c("validation_rmse", "train_rmse"), 
           new = c("valid", "train"))
  setDT(eval_df)
  
  eval_df_long <- melt(eval_df, id.vars = "iter",
                       variable.name = "dataset", value.name = "loss")
  
  #------------------------------------------------------------
  ## 6. Prediction and Plotting
  
  # Prediction with fixed effects only (standard XGBoost prediction)
  pred_fixed <- predict(bst, validX, iterationrange = c(1, best_iter))
  
  #TODO: calculate loglikelihood 
  temp <- copy(validationData)
  temp[, pred := pred_fixed]
  x_sym <- sym(stat)
  
  predPlot <- ggplot(temp, aes(y = pred, x = !!x_sym, col = standAge)) + 
    labs(title = "XGBoost Prediction",
         x = paste("observed", stat), 
         y = paste("predicted", stat)) + 
    geom_point() + 
    geom_smooth(method = "lm", linetype = "dashed", col = "red", se = FALSE) + 
    geom_abline(intercept = 0, slope = 1) + 
    theme_bw()
  
  # The summary(gp_model) is removed as there is no random effect model
  # Calculate SHAP values for the validation data.
  # Use the 'exact' method since the data size is relatively small, or 'tree_path_dependent' for larger data.
  # The 'nsim' argument (number of Monte Carlo simulations) is for non-tree models.

  shap_matrix <- fastshap::explain(object = bst, X = myX, 
                    pred_wrapper = function(object, newdata){predict(object, newdata = newdata)}, 
                    newdata = data.matrix(trainingData[, .SD, .SDcols = xgbXCols]))
  
  # The shapviz function takes the SHAP matrix and the original feature data.
  s_viz <- shapviz::shapviz(
    object = bst, X_pred = dtrain, X = shap_matrix)

  # Plot the SHAP values summary (e.g., using fastshap's plotting utility for a quick overview)
  # This plot shows the magnitude of influence for each feature.
  # Calculate mean absolute SHAP value for each feature
  beeswarm_plot <- sv_importance(s_viz, 
                                 kind = "bee")

  return(list(bst = bst, 
              shapplot = beeswarm_plot, 
              predplot = predPlot, 
              tune1 = tuning1, 
              tune2 = tuning2))
}


growthXG <- xgb_PSP(trainingData = sim$PSPmodelData, validationData = sim$PSPvalidationData, stat = "growth")
