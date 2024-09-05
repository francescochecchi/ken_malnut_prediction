#...............................................................................
### +++++ PREDICTING ACUTE MALNUTRITION IN DROUGHT-PRONE AREAS OF KENYA ++++ ###
#...............................................................................

#...............................................................................
## ----- R SCRIPT TO MERGE ALL DATA TOGETHER AND CATEGORISE PREDICTORS  ----- ##
#...............................................................................


#...............................................................................
### Merging predictors together and categorise as needed
#...............................................................................

  #...................................      
  ## Merge all predictor data together

    # Time series of subcounty-year-months
    ts <- expand.grid(adm2 = admin$adm2, year = 2015:2019, month = 1:12)
    ts <- merge(ts, admin[, c("adm2", "adm1")], by = "adm2", all.x = T)
    
    # List of predictor datasets and variables
    preds <- list(
      "insecurity_df" = c("adm2", "year", "month", 
        grep("_3m|_6m", colnames(insecurity_df), value = T)),
      "literacy" = c("adm2", "prop_lit"),
      "mam_cases_df" = c("adm2", "year", "month", "mam_admissions_rate",
        "mam_cfr", grep("_3m|_6m", colnames(mam_cases_df), value = T)),
      "mnh_access" = c("adm2", "prop_sba"),
      "morbidity_df" = c("adm1", "year", "month", 
        grep("_3m|_6m", colnames(morbidity_df), value = T)),
      "prices_df" = c("adm2", "year", "month", "price_sm", 
        grep("_3m|_6m", colnames(prices_df), value = T)),
      "sam_cases_df" = c("adm2", "year", "month", "sam_admissions_rate", 
        "sam_cfr", grep("_3m|_6m", colnames(sam_cases_df), value = T)),
      "schooling" = c("adm1", "schooling_cov"),
      "spi" = c("adm2", "year", "month", grep("_3m|_6m",colnames(spi),value=T)),
      "utilisation_df" = c("adm1", "year", "month", 
        grep("_3m|_6m", colnames(utilisation_df), value = T)),
      "vaccination_df" = c("adm1", "year", "month", 
        grep("_3m|_6m", colnames(vaccination_df), value = T))
    )
    
    # Merge each predictor into the time series
    for (i in names(preds)) {
      
      # get dataset
      df_i <- get(i)
      df_i <- df_i[, preds[[i]]]
      
      # identify merge variables
      x <- preds[[i]][which(preds[[i]] %in% c("adm1", "adm2", "year", "month"))]
      
      # merge
      ts <- merge(ts, df_i, by = x, all.x = T)
      
      # check number of rows stays the same
      print(paste0("n rows after merging ", i, " :", nrow(ts)))
    } 
     preds <- ts 
     rm(ts)    

    # Remove some predictors that may be difficult to interpret
    preds <- preds[, -grep("_cfr", colnames(preds))]
     
    # Correct some predictor values that are negligibly below 0 (as in E-17)
      # consequence of rolling means
    x <- grep("_3m|_6m", colnames(preds), value = T) 
    x <- x[! x %in% c("spi_3m", "spi_6m")]
    for (i in x) {
      preds[which(! is.na(preds[, i]) & preds[, i] < 0), i] <- 0
    }
    
#...............................................................................
### Visualising distribution of predictors and creating categories if needed
#...............................................................................

  #...................................      
  ## Visualise distributions
    
    # Prepare output of characteristics of each predictor
    out <- data.frame(
      variable = colnames(preds)[!colnames(preds) %in% c("adm1", "adm2", "year", 
      "month")],
      prop_nonzero = NA,
      mean = NA,
      median = NA,
      min = NA,
      max = NA
    )
    
    # Compute and save characteristics for each predictor
    for (i in 1:nrow(out)) {
      
      # which variable:
      x <- out[i, "variable"]
      
      # proportion of zero values:
      out[i, "prop_nonzero"] <- prop.table(table(preds[, x] == 0))[1]

      # statistics:
      out[i, "mean"] <- round(mean(preds[, x], na.rm = T), 3)
      out[i, "median"] <- round(median(preds[, x], na.rm = T), 3)
      out[i, "min"] <- round(min(preds[, x], na.rm = T), 3)
      out[i, "max"] <- round(max(preds[, x], na.rm = T), 3)
    } 
    write.csv(out, paste0(dir_path, "out/03_characteristics_preds.csv"), 
      row.names = F)
    
    # Graph distributions
    df <- preds
    for (i in out$variable) {
      df$var <- df[, i]
      plot_i <- ggplot(df) +
        geom_density(aes(x = var)) +
        theme_bw() +
        labs(title = i)
      print(plot_i)
    }
    rm(df)
  
    
  #...................................      
  ## Categorise some variables
    
    # Categorise with 0 category and quartiles for positive values
    x <- paste(c("events_rate", "fatalities_rate","cholera_rate","measles_rate",
      "malaria_rate"), collapse="|")
    x <- grep(x, colnames(preds), value = T)
    for (i in x) {
      print(paste0("categorise ", i, " :"))
      preds[, paste0(i, "_cat")] <- NA
      preds[which(preds[, i] == 0), paste0(i, "_cat")] <- "0"
      preds[which(preds[, i] > 0), paste0(i, "_cat")] <-   
        cut(preds[which(preds[, i] > 0), i], include.lowest=T, right=F,
        breaks = quantile(preds[which(preds[, i] > 0), i], 0:4/4, na.rm = T),
        labels = c("1", "2", "3", "4"))
      preds[, paste0(i, "_cat")] <- factor(preds[, paste0(i, "_cat")],
        levels = c("0", "1", "2", "3", "4"), labels = c("0", paste0(1:4, "q")))
      print(table(preds[, paste0(i, "_cat")]))
      print(sum(table(preds[, paste0(i, "_cat")])))
    }

    # Categorise using quartiles
    x <- paste(c("mam_admissions_rate", "sam_admissions_rate"), collapse="|")
    x <- grep(x, colnames(preds), value = T)
    for (i in x) {
      print(paste0("categorise ", i, " :"))
      preds[, paste0(i, "_cat")] <-   
        cut(preds[, i], breaks = quantile(preds[, i], 0:4/4, na.rm = T),
          include.lowest=T, right=F, labels = paste0(1:4,"q"), ordered_result=T)
      print(table(preds[, paste0(i, "_cat")]))
      print(sum(table(preds[, paste0(i, "_cat")])))
    }
    
    
       
#...............................................................................
### Merging predictors and survey data together
#...............................................................................

  #...................................      
  ## Merge and manage survey data and predictor data; save output

    # Merge
    obs <- merge(svy_obs, preds, by = c("adm1", "adm2", "year", "month"), 
      all.x = T)
    
    # Exclude ineligible survey observations
    obs <- subset(obs, flag_crit & age_crit & complete_crit & adm2_crit)
    
    # Create unique stratum variable (possible unit for cross-validation)
    obs$stratum_id <- paste(svy_obs$file, svy_obs$stratum, sep = "_")
    
    # Create additional outcome variables
    obs$sam <- ifelse(obs$zwfl < 3, 1, 0)
    obs$gam <- ifelse(obs$zwfl < 2, 1, 0)
    
    # Streamline variables
    
      # admin variables
      x <- c("adm1", "adm2", "year", "month", "id", "stratum_id")
    
      # + survey variables
      x <- c(x, "cluster", "sex", "age", "quality_score", "zwfl", "zac",
        "gam", "sam")
    
      # + predictor variables
      x <- c(x, colnames(preds)[! colnames(preds) %in% x])
      
      # - non-categorical predictor variables if a categorical version exists
      x <- x[!x %in% gsub("_cat", "", grep("_cat", colnames(preds), value = T))]
    
      # streamline
      obs <- obs[, x]

    # Save dataset
    saveRDS(obs, paste0(dir_path, "out/03_obs.rds"))
    

#...............................................................................  
### ENDS
#...............................................................................
      