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
    
    # Factorise county as a possible random effect
    obs$adm1 <- factor(obs$adm1)
    
    # Identify categorical and non-categorical predictors
    x <- c("adm1", "adm2", "year", "month", "id", "stratum_id", "cluster", 
      "sex", "age", "quality_score", "zwfl", "zac", "gam", "sam")
    preds <- colnames(obs)[! colnames(obs) %in% x]
    preds_cat <- grep("_cat", preds, value = T)    
    preds_cont <- preds[! preds %in% preds_cat]
    
    # Three predictors (ANC, OPD utilisation, malaria) are incomplete - exclude
    x <- grep("opd_|anc_|malaria", colnames(obs), value = T)
    obs <- obs[, ! colnames(obs) %in% x]
    preds <- preds[! preds %in% x]
    preds_cont <- preds_cont[! preds_cont %in% x]
    preds_cat <- preds_cat[! preds_cat %in% x]
    x <- grep("opd_|anc_|malaria", colnames(df_preds), value = T)
    df_preds <- df_preds[, ! colnames(df_preds) %in% x]
    
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
      labels = c("cholera", "DPT cov.", "insec. events", 
        "insec. deaths", "MAM cases", "measles", "MMR cov.", "NDVI",
        "maize price", "literacy", "safe births", "SAM cases",
        "schooling", "SNDVI", "rainfall"))
    
    # Visualise trends
    ggplot(df, aes(x = date, y = mean, colour = adm1)) +
      geom_line(linewidth = 1, linetype = "solid") +
      scale_colour_viridis(discrete = TRUE) +
      scale_x_date("date", breaks = "1 year", date_labels = "%Y") +
      scale_y_continuous("mean predictor value") +
      theme_bw() +
      facet_grid(predictor~adm1, scales = "free_y") +
      annotate("rect", xmin=as.Date("2016-10-01"), xmax=as.Date("2017-12-31"), 
        ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "firebrick") +
      theme(legend.position = "none", axis.text.x = element_text(angle = 30,
        hjust = 1, vjust = 1))
    ggsave(paste0(dir_path, "out/04_predictor trends.png"), units = "cm",
      dpi = "print", height = 38, width = 30)    
    
        
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
    out_uni <- out_uni[order(out_uni$outcome, out_uni$aic_glm), ]
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
        labels = c("generalised linear model", "generalised additive model"))
      df$outcome2 <- paste0(df$outcome, " (", df$model, ")")
      x <- aggregate(list(aic_min = df$aic), by = list(outcome = df$outcome),
        min, na.rm = T)
      df <- merge(df, x, by = "outcome", all.x = T)
      df$aic <- df$aic - df$aic_min
      
      # plot difference in AIC from lowest AIC in outcome-predictor combination
      ggplot(df, aes(x = predictor, y = aic, colour = model, shape = model)) +
        geom_point(size = 2.5, alpha = 0.75, fill = NA, stroke = 1.25) +
        scale_y_continuous("difference in Akaike Information Criterion value") +
        scale_colour_manual("", values = palette_gen[c(6,12)]) +
        scale_shape_manual("", values = c(0, 2)) +
        facet_grid(outcome ~ ., scales = "free_y") +
        theme_bw() +
        theme(legend.position = "top", axis.text.x = element_text(angle = 30, 
          hjust = 1, vjust = 1))
      ggsave(paste0(dir_path, "out/04_univariate.png"), units = "cm",
        dpi = "print", height = 25, width = 20)
      
    
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
      x <- which(df$rank %in% unlist(folds[out[ff, "fold"]]))

      # add county and year
      out[ff, "adm1"] <- paste(unique(as.character(df[x, "adm1"])), 
        collapse = ", ")
      out[ff, "year"] <- paste(unique(df[x, "year"]), collapse = ", ")
      
      # if the model is a random forest...
      if (class(model_f)[1] == "ranger") {
        
        # update dataset
        obs_rf <- na.omit(df[-x, ])
        
        # try to refit model on training dataset; if no fit, go to next fold
        m_ff <- try(eval(model_f$call))
        if (class(m_ff)[1] == "try-error") {
          out[ff, "fold_ok"] <- F
          next}
        
        # try to predict on validation dataset; if cannot, go to next fold
        obs_rf_fold <- na.omit(df[x, ])
        if (nrow(obs_rf_fold) > 0) {
          pred <- try(
            predict(m_ff, data = obs_rf_fold,type = "response")$predictions)
          if (class(pred)[1] == "try-error" | all(is.na(pred))) {
            out[ff, "fold_ok"] <- F
            next}
        }
        if (nrow(obs_rf_fold) == 0) {out[ff, "fold_ok"] <- F; next}
        
        # compute RMSE if prediction was successful
        out[ff, "rmse"] <- sqrt(mean((obs_rf_fold[, my] - pred)^2, na.rm = T))
        
        # compute mean observed and predicted, and number of observations in fold
        out[ff, "obs"] <- mean(obs_rf_fold[, my], na.rm = T)
        out[ff, "pred"] <- mean(pred, na.rm = T)
        out[ff, "prop_folded"] <- nrow(obs_rf_fold) / nrow(na.omit(df))
      }
      
      if (class(model_f)[1] == "bam") {
        # try to refit model on training dataset; if no fit, go to next fold
        m_ff <- try(update(model_f, data = df[-x, ]))
        if (class(m_ff)[1] == "try-error") {
          out[ff, "fold_ok"] <- F
          next}
        
        # try to predict on validation dataset; if cannot, go to next fold
        pred <- try(predict(m_ff, newdata = df[x, ], type = "response", 
          allow.new.levels = T))
        if (class(pred)[1] == "try-error" | all(is.na(pred))) {
          out[ff, "fold_ok"] <- F
          next}
        
        # compute RMSE if prediction was successful
        out[ff, "rmse"] <- sqrt(mean((df[x, my] - pred)^2, na.rm = T))
        
        # compute mean observed and predicted, and number of observations in fold
        out[ff, "obs"] <- mean(df[x, my], na.rm = T)
        out[ff, "pred"] <- mean(pred, na.rm = T)
        out[ff, "prop_folded"] <- nrow(df[x, ]) / nrow(df)
      }
    }

    # Output results as desired: just mean CV scores, or results per fold
    if (out_summary) {
      x <- c(n_k, colMeans(out[, 2:5], na.rm = T))
      names(x) <- c("n_folds", "prop_fold_ok", colnames(out[, 3:5]))
      return(x)
    } 
    else {
      out_cv <- list(outcome = my, family = NA, out = out)
      if (class(model_f)[1] == "ranger") {out_cv$family <- "rf"} else
        {out_cv$family <- as.character(family(model_f))[1]}
      return(out_cv)
    }
  }

  
  #...................................      
  ## Plot cross-validation results for a given model
  f_cv_plot <- function(out_cv_f = out_cv, print = T, save = T, return = F,
    dir_path_f = dir_path, height_f = 15, width_f = 15) {
    
    # Remove NA values
    df <- na.omit(out_cv_f$out)
    
    # Set thresholds for plot visualisation
    if (out_cv_f$outcome=="gam"){t1 <- 0.02; t2 <- 0.05; t3 <- 0.15; t4 <- 0.20}
    if (out_cv_f$outcome=="sam"){t1 <- 0.01; t2 <- 0.02; t3 <- 0.02; t4 <- 0.05}
    if (out_cv_f$outcome=="zwfl"){t1 <- 0.10;t2 <- 0.25;t3 <- -1.00;t4 <- -1.25}
    if (out_cv_f$outcome=="zac"){t1<- 0.10; t2 <- 0.25; t3 <- -1.00;t4 <- -1.25}
    
    # Set plot limits, axis breaks and precision ribbons, depending on model
    if (out_cv_f$outcome %in% c("gam", "sam")) {
      limits <- c(0, max(c(df$obs, df$pred), na.rm = T) + t2)
      breaks = seq(0, 1, t2)
      ribbons <- data.frame(observed = seq(min(df$obs) - 0.1, max(df$obs) + 0.1, 
        0.01))
    }
    if (out_cv_f$outcome %in% c("zwfl", "zac")) {
      limits <- c(min(c(df$obs, df$pred), na.rm = T) - t2, 
        max(c(df$obs, df$pred), na.rm = T) + t2)
      breaks = seq(round(limits[1], 1), round(limits[2], 1), 0.1)
      ribbons <- data.frame(observed = seq(min(df$obs), max(df$obs), 0.10))
    }
    ribbons$minus_a <- ribbons$observed - t1
    ribbons$minus_b <- ribbons$observed - t2
    ribbons$plus_a <- ribbons$observed + t1
    ribbons$plus_b <- ribbons$observed + t2

    # Set precision area colour
    if (out_cv_f$family == "rf") {ribbon_col <- "seagreen3"} else
      {ribbon_col <- "firebrick"}
    
    # Generate plot
    pl <- ggplot() +
      geom_point(data = df, aes(x = obs, y = pred, fill = adm1, colour = adm1,
      size = prop_folded), alpha = 0.5) +
      theme_bw() +
      scale_x_continuous("observed", limits = limits, 
        breaks = breaks, expand = c(0,0),
        labels = ifelse(out_cv_f$outcome %in% c("gam", "sam"), 
          scales::percent, scales::label_number(accuracy = 0.01))) +
      scale_y_continuous("predicted", limits = limits, 
        breaks = breaks, expand = c(0,0),
        labels = ifelse(out_cv_f$outcome %in% c("gam", "sam"),
          scales::percent, scales::label_number(accuracy = 0.01))) +
      theme(legend.position = "top", panel.grid.minor = element_blank()) +
      scale_fill_viridis_d("county") +
      scale_colour_viridis_d("county") +
      geom_abline(intercept = 0,slope = 1,linewidth = 0.5,colour = ribbon_col) +
      guides(size = "none") +
      geom_ribbon(data = ribbons,aes(x = observed,ymin = minus_a,ymax = plus_a),
        alpha = 0.10, fill = ribbon_col) +
      geom_ribbon(data = ribbons,aes(x = observed,ymin = minus_b,ymax = plus_b),
        alpha = 0.05, fill = ribbon_col) +
      geom_vline(xintercept = t3, colour = palette_gen[6], 
        linetype = "22", linewidth = 1) +
      geom_hline(yintercept = t3, colour = palette_gen[6], 
        linetype = "22", linewidth = 1) +
      geom_vline(xintercept = t4, colour = palette_gen[13], 
        linetype = "11", linewidth = 1) +
      geom_hline(yintercept = t4, colour = palette_gen[13], 
        linetype = "11", linewidth = 1)

    # Print and/or save and/or return
    if (print) {print(pl)}
    if (save) {
      if (out_cv_f$family == "rf") {x <- "out/04_cv_results_rf"} else
        {x <- "out/04_cv_results_glm"}
      ggsave(paste0(dir_path_f, x, "_", out_cv_f$outcome, ".png"), units = "cm", 
        dpi = "print", height = height_f, width = width_f)}
    if (return) {return(pl)}
  }
  

  #...................................      
  ## Gather up summary output for any given model
  f_out <- function(out_cv_f = out_cv, model_f = m_try) {
    
    # Set up output
    out <- c()
    
    # Populate output
    out$outcome <- unlist(out_cv_f$outcome)
    out$family <- unlist(out_cv_f$family)
    x <- all.vars(formula(m_try))
    out$predictors <- paste(x[2:length(x)], collapse = ", ")
    out$formula <- as.character(m_try$formula)[3]
    if (out_cv_f$family == "rf") {out$aic <- NA} else {out$aic <- AIC(model_f)}
    x <- out_cv_f$out
    out$n_folds <- nrow(x)
    out$prop_folds_ok <- percent(mean(x$fold_ok), 0.1)
    out$mean_abs_bias <- ifelse(out$outcome %in% c("gam", "sam"),
      percent(mean(x$pred - x$obs, na.rm = T), 0.1),
      round(mean(x$pred - x$obs, na.rm = T), 2))
    out$mean_abs_error <- ifelse(out$outcome %in% c("gam", "sam"),
      percent(mean(abs(x$pred - x$obs), na.rm = T), 0.1),
      round(mean(abs(x$pred - x$obs), na.rm = T), 2))
    if (out$outcome == "gam") {t1 <- 0.02; t2 <- 0.05; t3 <- 0.15; t4 <- 0.20}
    if (out$outcome == "sam") {t1 <- 0.01; t2 <- 0.02; t3 <- 0.02; t4 <- 0.05}
    if (out$outcome == "zwfl") {t1 <- 0.10; t2 <- 0.25; t3 <- -1.00;t4 <- -1.25}
    if (out$outcome == "zac") {t1 <- 0.10; t2 <- 0.25; t3 <- -1.00;t4 <- -1.25}
    for (i in c(t1, t2)) {
      num <- length(which(abs(x$pred - x$obs) < i))
      denom <- nrow(na.omit(x))
      out[paste0("prop_within_", i)] <- paste0(percent(num/denom, 0.1), " (",
        num, "/", denom, ")")
    }
    for (i in c(t3, t4)) {
      if (out$outcome %in% c("gam", "sam")) {
        num <- length(which(x$obs >= i & x$pred >= i))
        denom <- length(which(x$obs >= i))
      }
      if (out$outcome %in% c("zwfl", "zac")) {
        num <- length(which(x$obs < i & x$pred < i))
        denom <- length(which(x$obs < i))
      }
      out[paste0("sens_", i)] <- paste0(percent(num/denom, 0.1), " (", num,
        "/", denom, ")")
    }

    # Return output
    x <- data.frame(characteristic = names(out))
    x$value <- unlist(out)
    return(x)
  }
    
    

#...............................................................................
### Evaluating models
#...............................................................................

  #...................................
  ## Generalised linear/additive models

    # Global Acute Malnutrition (GAM)
    m_try <- bam(gam ~ sndvi_6m + mam_admissions_rate_3m_cat + price_3m +
      cholera_rate_3m_cat + events_rate_3m_cat + 
      mmr1_rate_6m + prop_sba + spi_6m,
      data = obs, family = "binomial")
    out_cv <- f_cv(n_folds = "all")
    pl <- f_cv_plot(return = T)
    assign(paste0("pl_glm_", out_cv$outcome), pl)    
    out_summary <- suppressWarnings(f_out())
    write.csv(out_summary, paste0(dir_path, "out/04_perf_", out_cv$outcome, "_",
      out_cv$family, ".csv"), row.names = F)

    # Severe Acute Malnutrition (GAM)
    m_try <- bam(sam ~ sndvi_6m + sam_admissions_rate_3m_cat + price_3m +
      cholera_rate_3m_cat + events_rate_3m_cat + 
      mmr1_rate_6m + prop_sba + spi_6m, data = obs, family = "binomial")
    summary(m_try)
    out_cv <- f_cv(n_folds = "all")
    pl <- f_cv_plot(return = T)
    assign(paste0("pl_glm_", out_cv$outcome), pl)    
    out_summary <- suppressWarnings(f_out())
    out_summary
    write.csv(out_summary, paste0(dir_path, "out/04_perf_", out_cv$outcome, "_",
      out_cv$family, ".csv"), row.names = F)

    # Weight-for-Height Z-Score
    m_try <- bam(zwfl ~ s(sndvi_6m) + mam_admissions_rate_3m_cat + s(price_3m) +
      cholera_rate_3m_cat + events_rate_3m_cat + 
      s(mmr1_rate_6m) + prop_sba + s(spi_6m), data = obs, family = "gaussian")
    summary(m_try)
    out_cv <- f_cv(n_folds = "all")
    pl <- f_cv_plot(return = T)
    assign(paste0("pl_glm_", out_cv$outcome), pl)    
    out_summary <- suppressWarnings(f_out())
    write.csv(out_summary, paste0(dir_path, "out/04_perf_", out_cv$outcome, "_",
      out_cv$family, ".csv"), row.names = F)

    # MUAC-for-age Z-Score
    m_try <- bam(zac ~ s(sndvi_6m) + mam_admissions_rate_3m_cat + s(price_3m) +
      cholera_rate_3m_cat + events_rate_3m_cat + 
      s(mmr1_rate_6m) + prop_sba + s(spi_6m), data = obs, family = "gaussian")
    summary(m_try)
    out_cv <- f_cv(n_folds = "all")
    pl <- f_cv_plot(return = T)
    assign(paste0("pl_glm_", out_cv$outcome), pl)    
    out_summary <- suppressWarnings(f_out())
    write.csv(out_summary, paste0(dir_path, "out/04_perf_", out_cv$outcome, "_",
      out_cv$family, ".csv"), row.names = F)

    # Combined performance graph
    x <- lapply(ls(pattern = "pl_glm"), get)
    ggarrange(plotlist = x, ncol = 2, nrow = 2, labels = 
      c("global acute malnutrition", "severe acute malnutrition", 
        "weight-for-height Z-score",
        "middle-upper-arm circumference for age Z-score"), 
      align = "hv", font.label = list(size = 11), label.y = 0.95, 
      common.legend = T, hjust = c(-0.4,-0.4,-0.4,-0.21)) + 
      bgcolor("white") + border(NA)
    ggsave(paste0(dir_path, "out/04_glm_combi.png"), units = "cm", 
      dpi = "print", height = 30, width = 30)    
    
  #...................................      
  ## Random forest models
    
    # Select predictors and prepare data
    preds_rf <- c("ndvi_6m", "mam_admissions_rate_3m_cat", "price_3m", 
      "cholera_rate_3m_cat", "measles_rate_3m_cat", "events_rate_3m_cat", 
      "mmr1_rate_6m", "dpt3_rate_6m", "prop_sba", "spi_6m", "prop_lit",
      "schooling_cov")
    obs_rf <- na.omit(obs[, c(outcomes, preds_rf)])
    
    # Grow random forest models for each of the four outcomes
    for (i in outcomes) {
      print(paste0("now growing random forest for outcome: ", i))
      form <- as.formula(paste0(i, " ~ ", paste(preds_rf, collapse = " + ")))
      m_try <- ranger(formula = form, data = obs_rf, num.trees = 1000, mtry = 3)
      out_cv <- f_cv(n_folds = "all")
      pl <- f_cv_plot(return = T)
      assign(paste0("pl_rf_", i), pl)
      out_summary <- suppressWarnings(f_out())   
      write.csv(out_summary, paste0(dir_path, "out/04_perf_", i, "_",
        out_cv$family, ".csv"), row.names = F)
    }
    
    # Combined performance graph
    x <- lapply(ls(pattern = "pl_rf"), get)
    ggarrange(plotlist = x, ncol = 2, nrow = 2, labels = 
      c("global acute malnutrition", "severe acute malnutrition", 
        "weight-for-height Z-score",
        "middle-upper-arm circumference for age Z-score"), 
      align = "hv", font.label = list(size = 11), label.y = 0.95, 
      common.legend = T, hjust = c(-0.4,-0.4,-0.4,-0.21)) + 
      bgcolor("white") + border(NA)
    ggsave(paste0(dir_path, "out/04_rf_combi.png"), units = "cm", 
      dpi = "print", height = 30, width = 30)
    
        

#...............................................................................  
### ENDS
#...............................................................................
     