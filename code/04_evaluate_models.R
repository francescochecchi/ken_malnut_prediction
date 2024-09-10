#...............................................................................
### +++++ PREDICTING ACUTE MALNUTRITION IN DROUGHT-PRONE AREAS OF KENYA ++++ ###
#...............................................................................

#...............................................................................
## ------- R SCRIPT TO FIT AND EVALUATE ALTERNATIVE PREDICTIVE MODELS  ------ ##
#...............................................................................


#...............................................................................
### Exploring predictors and performing univariate analysis
#...............................................................................

  #...................................      
  ## Visualise trends in the predictors by county and date

    # Read datasets
    obs <- readRDS(paste0(dir_path, "out/03_obs.rds"))
    df_preds <- readRDS(paste0(dir_path, "out/03_preds.rds"))

    # Identify outcomes
    outcomes <- c("sam", "gam", "zwfl", "zac")
    
    # Identify categorical and non-categorical predictors
    x <- c("adm1", "adm2", "year", "month", "id", "stratum_id", "cluster", 
      "sex", "age", "quality_score", "zwfl", "zac", "gam", "sam")
    preds <- colnames(obs)[! colnames(obs) %in% x]
    preds_cat <- grep("_cat", preds, value = T)    
    preds_cont <- preds[! preds %in% preds_cat]
    
    # Two predictors (ANC and OPD utilisation) don't go as far as 2019 - exclude
    x <- grep("opd_|anc_", colnames(obs), value = T)
    obs <- obs[, ! colnames(obs) %in% x]
    preds <- preds[! preds %in% x]
    preds_cont <- preds_cont[! preds_cont %in% x]
    preds_cat <- preds_cat[! preds_cat %in% x]
    
    # Aggregate to county-date level, taking the means (only 3m running means)
    df <- df_preds[, - grep("_cat|_6m", colnames(df_preds))]
    df <- df[, ! colnames(df) %in% c("mam_admissions_rate", 
      "sam_admissions_rate", "price_sm")]
    df$date <- as.Date(paste(df$year, df$month, "15", sep = "-"))
    df <- aggregate(df[, !colnames(df) %in% c("adm1", "year", "month", "adm2", 
      "date")], by = df[, c("adm1", "date")], FUN = mean, na.rm = T)

    # Reshape long
    x <- colnames(df)[! colnames(df) %in% c("adm1", "date")]
    df <- reshape(df, direction = "long", varying = x, idvar = c("adm1", "date"),
      timevar = "predictor", times = x, v.names = "mean")
    df$predictor <- factor(df$predictor, levels = sort(unique(df$predictor)),
      labels = c("ANC rate", "cholera", "DPT cov.", "insec. events", 
        "insec. deaths", "malaria", "MAM cases", "measles", "MMR cov.",
        "OPD util.", "maize price", "literacy", "safe births", "SAM cases",
        "schooling", "rainfall"))
    
    # Visualise trends
    ggplot(df, aes(x = date, y = mean, colour = adm1)) +
      geom_line(linewidth = 1, linetype = "solid") +
      scale_colour_viridis(discrete = TRUE) +
      scale_x_date("date", breaks = "1 year", date_labels = "%Y") +
      scale_y_continuous("mean predictor value") +
      theme_bw() +
      facet_grid(predictor~adm1, scales = "free_y") +
      theme(legend.position = "none", axis.text.x = element_text(angle = 30,
        hjust = 1, vjust = 1))
    ggsave(paste0(dir_path, "out/04_predictor trends.png"), units = "cm",
      dpi = "print", height = 40, width = 30)    
    
        
  #...................................      
  ## Check for correlation among predictors
  
    # Prepare the data
    df <- obs[, preds]
    for (i in preds_cat) { df[, i] <- as.numeric(df[, i])}    

    # Plot correlation
    ggcorr(df, label = T, layout.exp = 1.3, hjust = 1)
    ggsave(paste0(dir_path, "out/04_preds_correlation.png"), units = "cm",
      dpi = "print", height = 30, width = 30)


  #...................................      
  ## Do univariate analysis for each outcome - GLM + GAM for cont., GLM for cat.
  
    # Initialise output
    out_uni <- expand.grid(outcome = outcomes, predictor = preds)
    for (i in c("glm", "gam")) {
      out_uni[, paste(c("aic", "coeff", "dev_expl"), i, sep = "_")] <- NA
    }
    
    # Univariate analysis
    for (i in outcomes) {
      
      # progress statement
      print(paste0("now doing univariate analysis for outcome: ", i, " ..."))
      
      # set distribution
      family_i <- ifelse(i %in% c("gam", "sam"), "binomial", "gaussian")
      
      # for each predictor
      for (j in preds) {
        
        # progress statement
        print(paste0("    ... and predictor: ", j))
        
        # if categorical...
        if (grepl("_cat", j)) {
          # specify formula
          fx_glm <- formula(paste0(i, " ~ ", j))
          
          # fit model
          mx_glm <- mgcv::bam(formula = fx_glm, data = obs, family = family_i)
          
          # gather statistics
          out_uni[which(out_uni$outcome == i & out_uni$predictor == j),
            c("aic_glm", "coeff_glm", "dev_expl_glm")] <- 
            c(AIC(mx_glm), exp(coef(mx_glm)[2]), summary(mx_glm)$dev.expl)
        }
        
        # if continuous...
        if (! grepl("_cat", j)) {
          # specify GLM formula
          fx_glm <- formula(paste0(i, " ~ ", j))
          
          # specify GAM formula
          fx_gam <- formula(paste0(i, " ~ s(", j, ")"))
          
          # fit models
          mx_glm <- mgcv::bam(formula = fx_glm, data = obs, family = family_i)
          mx_gam <- mgcv::bam(formula = fx_gam, data = obs, family = family_i)
          
          # gather statistics
          out_uni[which(out_uni$outcome == i & out_uni$predictor == j),
            c("aic_glm", "coeff_glm", "dev_expl_glm")] <- 
            c(AIC(mx_glm), 
              ifelse(family_i=="binomial",exp(coef(mx_glm)[2]),coef(mx_glm)[2]),
              summary(mx_glm)$dev.expl
            )
          out_uni[which(out_uni$outcome == i & out_uni$predictor == j),
            c("aic_gam", "coeff_gam", "dev_expl_gam")] <- 
            c(AIC(mx_gam), NA, summary(mx_gam)$dev.expl)
        }
      }
    }

    # Save univariate output
    write.csv(out_uni, paste0(dir_path, "out/04_univariate.csv"), row.names = F)
    
    # Visualise goodness of fit, by outcome and predictor
      
      # prepare data
      df <- out_uni[, c("outcome", "predictor", "aic_glm", "aic_gam")]
      df <- reshape(df, direction = "long", varying = c("aic_glm", "aic_gam"),
        idvar = c("outcome", "predictor"), timevar = "model", 
        times = c("glm", "gam"), v.names = "aic")
      df$outcome <- factor(df$outcome, levels = sort(unique(df$outcome)),
        labels = c("SAM", "GAM", "WHZ", "MUACZ"))
      df$model <- factor(df$model, levels = c("glm", "gam"), 
        labels = c("GLM", "GAM"))
      df$outcome2 <- paste0(df$outcome, " (", df$model, ")")
      x <- aggregate(list(aic_min = df$aic), by = list(outcome = df$outcome),
        min, na.rm = T)
      df <- merge(df, x, by = "outcome", all.x = T)
      df$aic <- df$aic - df$aic_min
      
      # plot difference in AIC from lowest AIC in outcome-predictor combination
      ggplot(df, aes(x = predictor, y = aic, colour = model, shape = model)) +
        geom_point(size = 2, alpha = 0.5, fill = NA, stroke = 2) +
        scale_y_continuous("Akaike Information Criterion value") +
        scale_colour_manual("model", values = palette_gen[c(6,12)]) +
        scale_shape_manual("model", values = c(0, 2)) +
        facet_grid(outcome ~ ., scales = "free_y") +
        theme_bw() +
        theme(legend.position = "top", axis.text.x = element_text(angle = 30, 
          hjust = 1, vjust = 1))
      ggsave(paste0(dir_path, "out/04_univariate.png"), units = "cm",
        dpi = "print", height = 30, width = 20)
      
    
#...............................................................................
### Functions for evaluating alternative models
#...............................................................................

  #...................................      
  ## Leave-one-out cross-validation function
  f_cv <- function(df = obs, model_f = m_try, n_folds = "all", verbose = T,
    out_summary = F) {
    
    # Identify model's dependent variable
    my <- as.character(formula(model_f)[[2]])
    
    # Number of folds (= all stratum_id's if "all" == LOOCV)
    n_units <- length(unique(df$stratum_id))
    n_k <- ifelse(n_folds == "all", n_units, n_folds)

    # Shuffle dataset
    x <- data.frame(stratum_id = unique(df$stratum_id), 
      rank = sample.int(n = n_units, size = n_units))
    df <- merge(df, x, by = "stratum_id", all.x = T)
    df <- df[order(df$rank), ]
    
    # Split data into k folds
    folds <- split(x$rank, sort(x$rank %% n_k))

    # Initialise output
    out <- data.frame(fold = names(folds))
    out$fold_ok <- T
    out[, c("rmse", "obs", "pred", "prop_folded", "adm1", "year")] <- NA
        
    # Fit model for all the folds and track predictive accuracy on each left-out
        # set of observations
    for (ff in 1:nrow(out)) {
      
      # progress statement
      if (verbose) {print(paste0("fold ", ff, " of ", nrow(out)))}
      
      # identify fold observations
      x <- which(df$rank %in% folds[out[ff, "fold"]])

      # add county and year
      out[ff, "adm1"] <- unique(as.character(df[x, "adm1"]))
      out[ff, "year"] <- unique(df[x, "year"])
      
      # try to refit model on training dataset; if no fit, go to next fold
      m_ff <- try(update(model_f, data = df[-x, ]))
      if (class(m_ff)[1] == "try-error") {
        out[ff, "fold_ok"] <- F
        next}

      # try to predict on validation dataset; if cannot predict, go to next fold
      pred <- try(predict(m_ff, newdata = df[x, ], type = "response", 
        allow.new.levels = T))
      if (class(preds)[1] == "try-error" | all(is.na(pred))) {
        out[ff, "fold_ok"] <- F
        next}
      
      # compute RMSE if prediction was successful
      out[ff, "rmse"] <- sqrt(mean((df[x, my] - pred)^2, na.rm = T))
      
      # compute mean observed and predicted, and number of observations in fold
      out[ff, "obs"] <- mean(df[x, my], na.rm = T)
      out[ff, "pred"] <- mean(pred, na.rm = T)
      out[ff, "prop_folded"] <- nrow(df[x, ]) / nrow(df)
      
    }

    # Output results as desired: just mean CV scores, or results per fold
    if (out_summary) {
      x <- c(n_k, colMeans(out[, 2:5], na.rm = T))
      names(x) <- c("n_folds", "prop_fold_ok", colnames(out[, 3:5]))
      return(x)
    } 
    else {
      out_cv <- list(family = as.character(family(m_try))[1],
        out = out)
      return(out_cv)
    }
  }

  
  #...................................      
  ## Plot cross-validation results for a given model
  f_cv_plot <- function(out_cv_f = out_cv, print = T, save = T,
    filename = "04_cv_results.png", dir_path_f = dir_path,
    height_f = 15, width_f = 15) {
    
    # Remove NA values
    df <- na.omit(out_cv_f$out)
    
    # Set plot limits, axis breaks and precision ribbons, depending on model
    if (out_cv_f$family == "binomial") {
      limits <- c(0, max(c(df$obs, df$pred), na.rm = T) + 0.05)
      breaks = seq(0, 1, 0.05)
      ribbons <- data.frame(observed = seq(min(df$obs) - 0.1, max(df$obs) + 0.1, 
        0.01))
      ribbons$minus_a <- ribbons$observed - 0.02
      ribbons$minus_b <- ribbons$observed - 0.05
      ribbons$plus_a <- ribbons$observed + 0.02
      ribbons$plus_b <- ribbons$observed + 0.05
    }
    if (out_cv_f$family == "gaussian") {
      limits <- c(min(c(df$obs, df$pred), na.rm = T) - 0.2, 
        max(c(df$obs, df$pred), na.rm = T) + 0.2)
      breaks = seq(round(limits[1]) - 0.1, round(limits[2], 1) + 0.1, 0.1)
      ribbons <- data.frame(observed = seq(min(df$obs), max(df$obs), 0.10))
      ribbons$minus_a <- ribbons$observed - 0.05
      ribbons$minus_b <- ribbons$observed - 0.20
      ribbons$plus_a <- ribbons$observed + 0.05
      ribbons$plus_b <- ribbons$observed + 0.20
    }

    # Generate plot
    pl <- ggplot() +
      geom_point(data = df, aes(x = obs, y = pred, fill = adm1, colour = adm1,
      size = prop_folded), alpha = 0.5) +
      theme_bw() +
      scale_x_continuous("observed", limits = limits, 
        breaks = breaks, expand = c(0,0),
        labels = ifelse(out_cv_f$family == "binomial", 
          scales::percent, scales::label_number(accuracy = 0.01))) +
      scale_y_continuous("predicted", limits = limits, 
        breaks = breaks, expand = c(0,0),
        labels = ifelse(out_cv_f$family == "binomial",
          scales::percent, scales::label_number(accuracy = 0.01))) +
      theme(legend.position = "top") +
      scale_fill_viridis_d("county") +
      scale_colour_viridis_d("county") +
      geom_abline(intercept = 0, slope = 1, linetype = "11",
        colour = "firebrick") +
      guides(size = "none") +
      geom_ribbon(data = ribbons,aes(x = observed,ymin = minus_a,ymax = plus_a),
        alpha = 0.10, fill = "firebrick") +
      geom_ribbon(data = ribbons,aes(x = observed,ymin = minus_b,ymax = plus_b),
        alpha = 0.05, fill = "firebrick")

    # Print and/or save
    if (print) {print(pl)}
    if (save) {ggsave(paste0(dir_path_f, "out/", filename), units = "cm",
      dpi = "print", height = height_f, width = width_f)}
  }
  

 
#...............................................................................
### Evaluating Global Acute Malnutrition (GAM) models
#...............................................................................

  #...................................      
  ## Models
  obs$adm1 <- factor(obs$adm1)
  m_try <- bam(gam ~ s(spi_6m) + s(dpt3_rate_6m) + s(adm1, bs = "re"), 
    data = obs, family = "binomial")
  AIC(m_try)  

  out_cv <- f_cv()
  f_cv_plot()
      
#...............................................................................  
### ENDS
#...............................................................................
     