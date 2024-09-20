#...............................................................................
### +++++ PREDICTING ACUTE MALNUTRITION IN DROUGHT-PRONE AREAS OF KENYA ++++ ###
#...............................................................................

#...............................................................................
## --- R SCRIPT TO PREPARE AND DESCRIBE SMART SURVEY ANTHROPOMETRIC DATA  --- ##
#...............................................................................


#...............................................................................
### Reading datasets and setting parameters
#...............................................................................

  #...................................      
  ## Read and bind each SMART survey
    
    # Identify survey files
    files <- na.omit(unique(surveys$file))
    
    # For each survey, bind and add to growing number of columns
    for (i in 1:length(files)) {
      
      # read relevant columns of survey dataset
      svy_i <- as.data.frame(read_excel(paste0(dir_path, "in/surveys/", 
        files[i])))
      svy_i <- svy_i[, colnames(svy_i) %in% c("CLUSTER", "SEX", "MONTHS", 
        "WEIGHT", "HEIGHT", "EDEMA", "MUAC")]
      
      # add file name
      svy_i$file <- files[i]
      
      # if first survey, start dataset
      if (i == 1) {svy_obs <- svy_i}
      
      # otherwise, make sure both growing dataset and survey dataset 
          # to be appended have the same columns
      if (i > 1) {
        x <- base::setdiff(colnames(svy_i), colnames(svy_obs))
        if (length(x) > 0 ) {svy_obs[, x] <- NA}
        x <- base::setdiff(colnames(svy_obs), colnames(svy_i))
        if (length(x) > 0 ) {svy_i[, x] <- NA}
        
        # bind
        svy_i <- svy_i[, colnames(svy_obs)]
        svy_obs <- rbind(svy_obs, svy_i)
      }
    }
    
    # Change column names
    colnames(svy_obs) <- c("cluster", "sex", "age", "weight", "lenhei", 
      "oedema", "armc", "file")
    
    # Add subcounties and survey strata where clusters are
    table(svy_obs$file, is.na(svy_obs$cluster))
    svy_obs <- merge(svy_obs, cluster_locations, by = c("file", "cluster"), 
      all.x =T)
    table(svy_obs$file, is.na(svy_obs$adm2))
    table(svy_obs$file, svy_obs$stratum, useNA = "ifany")
    
    # Add subcounties and strata manually where needed
    x <- which(svy_obs$file == "ken_marsabit_0915.xlsx")
    svy_obs[x, "adm2"] <- "Laisamis"
    svy_obs[x, "adm1"] <- "Marsabit"
    svy_obs[x, "stratum"] <- 1

    # Add other metavariables
    svy_obs <- merge(svy_obs, unique(surveys[, c("file", "month", "year", 
      "whole_county", "quality_score")]), by = "file", all.x = T)
    svy_obs[which(svy_obs$whole_county), "stratum"] <- 1
    
    ## Add subcounties that belong to each stratum
    x <- by(svy_obs, svy_obs[, c("file", "stratum")], function (x) {
      data.frame(file = unique(x$file), stratum = unique(x$stratum),
        subcounties_in_stratum = paste(sort(unique(x$adm2)),collapse=", "))
    })
    x <- do.call(rbind, x)
    svy_obs <- merge(svy_obs, x, by = c("file", "stratum"), all.x = T)        
    svy_obs[which(svy_obs$whole_county), "subcounties_in_stratum"] <- 
      "whole adm1"
    
       
  #...................................      
  ## Check completeness and correct categories of key variables

    # adm2 / create adm2 inclusion criterion
    table(svy_obs$adm2, useNA = "ifany")
    svy_obs$adm2_crit <- ifelse(is.na(svy_obs$adm2), FALSE, TRUE)
    table(svy_obs$adm2_crit)
        
    # Sex
    table(svy_obs$sex, useNA = "ifany")
    svy_obs[which(svy_obs$sex %in% c(1, "Male")), "sex"] <- "m"
    svy_obs[which(svy_obs$sex %in% c(2, "Female")), "sex"] <- "f"

    # Age / create age inclusion criterion
    table(is.na(svy_obs$age))    
    range(svy_obs$age, na.rm = T)
    svy_obs$age_crit <- ifelse(svy_obs$age >=6 & svy_obs$age < 60, TRUE, FALSE)
    table(svy_obs$age_crit)

    # Weight
    table(is.na(svy_obs$weight))
    range(svy_obs$weight, na.rm = T)
    
    # Height
    table(is.na(svy_obs$lenhei))
    range(svy_obs$lenhei, na.rm = T)
    
    # Oedema (assume missing oedema are 'no')
    table(svy_obs$oedema, useNA = "ifany")
    svy_obs[which(svy_obs$oedema %in% c(0, 2, "No", "no", NA)), "oedema"] <- "n"
    svy_obs[which(svy_obs$oedema %in% c(1, "Yes", "yes")), "oedema"] <- "y"
    
    # MUAC (convert to cm)
    table(is.na(svy_obs$armc))
    range(svy_obs$armc, na.rm = T)
    svy_obs$armc <- svy_obs$armc / 10
    
    # Create data completeness criterion
    svy_obs$complete_crit <- complete.cases(svy_obs[, c("sex", "age", "weight", 
      "lenhei", "oedema")])
    table(svy_obs$complete_crit)
                    

  #...................................      
  ## Add anthropometric indices and check flags
    
    # Add unique ID for each child
    x <- by(svy_obs, svy_obs$file, function(x) {1:nrow(x)})
    svy_obs$id <- as.vector(unlist(x))
    svy_obs$id <- paste0(svy_obs$file, svy_obs$id)
    
    # Only add indices to observations that meet other criteria
    x <- which(svy_obs$age_crit & svy_obs$complete_crit)
    
    # Add indices
    df <- svy_obs[x, ]
    x <- with(df, anthro_zscores(sex = sex, age = age, weight = weight,
      lenhei = lenhei, oedema = oedema, armc = armc, is_age_in_month = T))
    df <- cbind(df$id, x)
    colnames(df)[1] <- "id"
    svy_obs <- merge(svy_obs, df, by = "id", all.x = T)
    
    # Create flag criterion
    table(svy_obs$fwfl)
    svy_obs$flag_crit <- ifelse(svy_obs$fwfl == 1, F, T)
    table(svy_obs$flag_crit)
    
    # Save file
    saveRDS(svy_obs, file = paste0(dir_path, "out/02_svy_obs.rds"))
    
    
#...............................................................................
### Visualising anthropometric data
#...............................................................................

  #...................................      
  ## Tabulate survey observations and attrition
  
    # Prepare data
    df <- svy_obs
    df$date <- as.Date(paste("15", df$month, df$year, sep = "-"), "%d-%m-%Y")
    df$month_abb <- month.abb[df$month]
    df$n <- 1
    df$adm2_flag_crit <- ifelse(df$flag_crit & df$adm2_crit, T, F)
    df$mmyy <- paste0(df$month_abb, "-", df$year)
    
    # Aggregate
    x <- aggregate(df[, c("n", "age_crit", "complete_crit", "flag_crit", 
      "adm2_flag_crit")], by = df[, c("file", "stratum", "mmyy", "date")], 
      FUN = sum, na.rm = T)
    
    # Add subcounties in each stratum
    x1 <- unique(df[, c("file", "stratum", "adm1", "subcounties_in_stratum")])
    x1 <- na.omit(x1)
    x <- merge(x, x1, by = c("file", "stratum"))
        
    # Save
    write.csv(x, paste0(dir_path, "out/02_svy_attrition.csv"), row.names = F)
    
    
  #...................................      
  ## Estimate SAM and GAM prevalence by survey stratum
  
    # Prepare data
    df <- svy_obs
    df$date <- as.Date(paste("15", df$month, df$year, sep = "-"), "%d-%m-%Y")
    df <- subset(df, age_crit & complete_crit & flag_crit)    
    
    # Classify GAM and SAM
      # gam
      df$gam <- 0
      df[which(df$zwfl < -2 | df$oedema == "y"), "gam"] <- 1
      table(df$gam)    

      # sam
      df$sam <- 0
      df[which(df$zwfl < -3 | df$oedema == "y"), "sam"] <- 1
      table(df$sam)    
      
    # Estimate GAM/SAM prevalence and 95%CI by stratum-date instance
    x <- by(df, df[, c("file", "stratum", "date")], function(x) {
      
      # restrict to non-missing clusters - if all clusters missing (1 survey,
          # assume random sampling)
      if (all(is.na(x$cluster))) {
        svyd <- suppressWarnings(svydesign(id = ~ 1, data = x))
      }
      if (! all(is.na(x$cluster))) {
        svyd <- suppressWarnings(svydesign(id = ~ cluster, 
          data = subset(x, ! is.na(cluster))))
      }

      # estimate SAM prevalence
      est_sam <- survey::svyglm(sam ~ NULL, design = svyd, deff = T,
        family = "quasibinomial")
      summary_est_sam <- summary(est_sam)$coefficients
            
      # estimate GAM prevalence
      est_gam <- survey::svyglm(gam ~ NULL, design = svyd, deff = T,
        family = "quasibinomial")
      summary_est_gam <- summary(est_gam)$coefficients

      # return output
      out <- data.frame(file = unique(x$file), stratum = unique(x$stratum),
        date = unique(x$date), 
        est_sam = boot::inv.logit(summary_est_sam[1]),
        lci_sam = boot::inv.logit(summary_est_sam[1] - 1.96*summary_est_sam[2]),
        uci_sam = boot::inv.logit(summary_est_sam[1] + 1.96*summary_est_sam[2]),
        deff_sam = deff(est_sam),
        est_gam = boot::inv.logit(summary_est_gam[1]),
        lci_gam = boot::inv.logit(summary_est_gam[1] - 1.96*summary_est_gam[2]),
        uci_gam = boot::inv.logit(summary_est_gam[1] + 1.96*summary_est_gam[2]),
        deff_gam = deff(est_gam)
      )
      return(out)
    })
    x <- do.call(rbind, x)

    # Add other variables
    x1 <- unique(df[, c("file", "stratum", "adm1", "subcounties_in_stratum")])
    x1 <- na.omit(x1)
    x <- merge(x, x1, by = c("file", "stratum"))
    x$mmyy <- paste0(month.abb[month(x$date)], "-", year(x$date))
    
  #...................................      
  ## Save GAM and SAM prevalences as a table
    
    # Format numbers
    for (i in c("est_sam","lci_sam","uci_sam","est_gam","lci_gam","uci_gam")) {
      x[, paste0(i, "_pretty")] <- percent(x[, i], accuracy = 0.1)
    }
    x$prev_sam <- paste0(x$est_sam_pretty, " (", x$lci_sam_pretty, " to ", 
      x$uci_sam_pretty,")")
    x$deff_sam_pretty <- sprintf("%.2f", x$deff_sam)   
    x$prev_gam <- paste0(x$est_gam_pretty, " (", x$lci_gam_pretty, " to ", 
      x$uci_gam_pretty,")")
    x$deff_gam_pretty <- sprintf("%.2f", x$deff_gam)   
    out <- x[, c("adm1", "stratum", "date", "mmyy", "subcounties_in_stratum",
      "prev_sam", "deff_sam_pretty", "prev_gam", "deff_gam_pretty")]      
    colnames(out) <- gsub("_pretty", "", colnames(out))
    
    # Save
    write.csv(out, paste0(dir_path, "out/02_svy_est_sam_gam.csv"),row.names = F)        


  #...................................      
  ## Graph GAM and SAM prevalences

    # Prepare data
    df <- x[, c("adm1", "stratum", "subcounties_in_stratum", "date",
      "est_sam", "lci_sam", "uci_sam", "est_gam", "lci_gam", "uci_gam")]
    df <- reshape(df, direction = "long", 
      varying = c("est_sam", "lci_sam", "uci_sam", "est_gam", 
        "lci_gam", "uci_gam"),
      idvar = c("adm1", "stratum", "subcounties_in_stratum", "date"),
      timevar = "indicator", times = c("sam", "gam"),
      v.names = c("est", "lci", "uci")
    )
    df$stratum <- factor(df$stratum)
    df$indicator <- factor(df$indicator, levels = c("sam", "gam"),
      labels = c("severe acute malnutrition", "global acute malnutrition"))
        
    # Plot GAM (just point estimates, otherwise it's confusing)
    plot_gam <- ggplot(subset(df, indicator == "global acute malnutrition"), 
      aes(x = date, y = est, colour = stratum, fill = stratum, group = stratum)) +
      geom_point(alpha = 0.5, position = position_jitter(width = 45)) +
      geom_line(alpha = 0.25, linetype = "11", linewidth = 0.5) +
      scale_x_date("date") +
      scale_y_continuous("prevalence", labels = scales::percent,
        breaks = seq(0, 0.5, 0.10), limits = c(0, 0.45)) +
      scale_colour_manual("stratum", values = palette_gen[c(2,6,10,14)]) +
      scale_fill_manual("stratum", values = palette_gen[c(2,6,10,14)]) +
      annotate("rect", xmin=as.Date("2016-10-01"), xmax=as.Date("2017-12-31"), 
        ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "firebrick") +      
      facet_grid(adm1~.) +
      theme_bw() +
      theme(legend.position = "top", plot.margin = margin(10,5,0,0))
    ggsave(paste0(dir_path, "out/02_svy_estimates_gam.png"), units = "cm",
      dpi = "print", height = 27, width = 12)

    # Plot SAM (just point estimates, otherwise it's confusing)
    plot_sam <- ggplot(subset(df, indicator == "severe acute malnutrition"), 
      aes(x = date, y = est, colour = stratum, fill = stratum, group = stratum)) +
      geom_point(alpha = 0.5, position = position_jitter(width = 45)) +
      geom_line(alpha = 0.25, linetype = "11", linewidth = 0.5) +
      scale_x_date("date") +
      scale_y_continuous("prevalence", labels = scales::percent,
        breaks = seq(0, 0.14, 0.02), limits = c(0, 0.14)) +
      scale_colour_manual("stratum", values = palette_gen[c(2,6,10,14)]) +
      scale_fill_manual("stratum", values = palette_gen[c(2,6,10,14)]) +
      annotate("rect", xmin=as.Date("2016-10-01"), xmax=as.Date("2017-12-31"), 
        ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "firebrick") +      
      facet_grid(adm1~.) +
      theme_bw() +
      theme(legend.position = "top", plot.margin = margin(10,5,0,0))
    ggsave(paste0(dir_path, "out/02_svy_estimates_sam.png"), units = "cm",
      dpi = "print", height = 27, width = 12)

    # Combination plot
    plot_combi <- ggarrange(
      plot_sam + theme(strip.background.y = element_blank(), 
        strip.text.y = element_blank()), 
      plot_gam + theme(axis.title.y = element_blank()), 
      ncol = 2, 
      labels = levels(df$indicator), font.label = list(face="plain",size=10.5))
    ggsave(paste0(dir_path, "out/02_svy_estimates_combi1.png"), units = "cm",
      dpi = "print", height = 23, width = 17)
     
    
  #...................................      
  ## Estimate mean WHZ and mean MUAC-for-age Z-score by survey stratum and time
  
    # Prepare data
    df <- svy_obs
    df$date <- as.Date(paste("15", df$month, df$year, sep = "-"), "%d-%m-%Y")
    df <- subset(df, age_crit & complete_crit & flag_crit)    
    
    # Estimate WHZ and MUAC-by-age Z-score and 95%CI by stratum-date instance
    x <- by(df, df[, c("file", "stratum", "date")], function(x) {
      
      # restrict to non-missing clusters - if all clusters missing (1 survey,
          # assume random sampling)
      if (all(is.na(x$cluster))) {
        svyd <- suppressWarnings(svydesign(id = ~ 1, data = x))
      }
      if (! all(is.na(x$cluster))) {
        svyd <- suppressWarnings(svydesign(id = ~ cluster, 
          data = subset(x, ! is.na(cluster))))
      }

      # estimate WHZ
      est_whz <- survey::svyglm(zwfl ~ NULL, design = svyd, deff = T,
        family = "gaussian")
      summary_est_whz <- summary(est_whz)$coefficients
            
      # estimate MUAC-for-age
      est_acz <- survey::svyglm(zac ~ NULL, design = svyd, deff = T,
        family = "gaussian")
      summary_est_acz <- summary(est_acz)$coefficients

      # return output
      out <- data.frame(file = unique(x$file), stratum = unique(x$stratum),
        date = unique(x$date), 
        est_whz = summary_est_whz[1],
        lci_whz = summary_est_whz[1] - 1.96*summary_est_whz[2],
        uci_whz = summary_est_whz[1] + 1.96*summary_est_whz[2],
        deff_whz = deff(est_whz),
        est_acz = summary_est_acz[1],
        lci_acz = summary_est_acz[1] - 1.96*summary_est_acz[2],
        uci_acz = summary_est_acz[1] + 1.96*summary_est_acz[2],
        deff_acz = deff(est_acz)
      )
      return(out)
    })
    x <- do.call(rbind, x)

    # Add other variables
    x1 <- unique(df[, c("file", "stratum", "adm1", "subcounties_in_stratum")])
    x1 <- na.omit(x1)
    x <- merge(x, x1, by = c("file", "stratum"))
    x$mmyy <- paste0(month.abb[month(x$date)], "-", year(x$date))

        
  #...................................      
  ## Save WHZ and MUAC-Z score estimates as a table
    
    # Format numbers
    for (i in c("est_whz","lci_whz","uci_whz","est_acz","lci_acz","uci_acz")) {
      x[, paste0(i, "_pretty")] <- sprintf("%.2f", x[, i])
    }
    x$all_whz <- paste0(x$est_whz_pretty, " (", x$lci_whz_pretty, " to ", 
      x$uci_whz_pretty,")")
    x$deff_whz_pretty <- sprintf("%.2f", x$deff_whz)   
    x$all_acz <- paste0(x$est_acz_pretty, " (", x$lci_acz_pretty, " to ", 
      x$uci_acz_pretty,")")
    x$deff_acz_pretty <- sprintf("%.2f", x$deff_acz)   
    out <- x[, c("adm1", "stratum", "date", "mmyy", "subcounties_in_stratum",
      "all_whz", "deff_whz_pretty", "all_acz", "deff_acz_pretty")]      
    colnames(out) <- gsub("_pretty", "", colnames(out))
    
    # Save
    write.csv(out, paste0(dir_path, "out/02_svy_est_whz_muacz.csv"),row.names=F)        


  #...................................      
  ## Graph WHZ and MUAC Z-score point estimates

    # Prepare data
    df <- x[, c("adm1", "stratum", "subcounties_in_stratum", "date",
      "est_whz", "lci_whz", "uci_whz", "est_acz", "lci_acz", "uci_acz")]
    df <- reshape(df, direction = "long", 
      varying = c("est_whz", "lci_whz", "uci_whz", "est_acz", 
        "lci_acz", "uci_acz"),
      idvar = c("adm1", "stratum", "subcounties_in_stratum", "date"),
      timevar = "indicator", times = c("whz", "acz"),
      v.names = c("est", "lci", "uci")
    )
    df$stratum <- factor(df$stratum)
    df$indicator <- factor(df$indicator, levels = c("whz", "acz"),
      labels = c("weight-for-height Z-score", "MUAC-for-age Z-score"))
        
    # Plot WHZ (just point estimates, otherwise it's confusing)
    plot_whz <- ggplot(subset(df, indicator == "weight-for-height Z-score"), 
      aes(x = date, y = est, colour = stratum, fill = stratum, group = stratum)) +
      geom_point(alpha = 0.5, position = position_jitter(width = 45)) +
      geom_line(alpha = 0.25, linetype = "11", linewidth = 0.5) +
      scale_x_date("date") +
      scale_y_continuous("point estimate",
        breaks = seq(-3, 0, 0.25), limits = c(-1.75, -0.25)) +
      scale_colour_manual("stratum", values = palette_gen[c(2,6,10,14)]) +
      scale_fill_manual("stratum", values = palette_gen[c(2,6,10,14)]) +
      annotate("rect", xmin=as.Date("2016-10-01"), xmax=as.Date("2017-12-31"), 
        ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "firebrick") +      
      facet_grid(adm1~.) +
      theme_bw() +
      theme(legend.position = "top", plot.margin = margin(10,5,0,0))
    ggsave(paste0(dir_path, "out/02_svy_estimates_whz.png"), units = "cm",
      dpi = "print", height = 27, width = 12)

    # Plot MUAC Z-score (just point estimates, otherwise it's confusing)
    plot_acz <- ggplot(subset(df, indicator == "MUAC-for-age Z-score"), 
      aes(x = date, y = est, colour = stratum, fill = stratum, group = stratum)) +
      geom_point(alpha = 0.5, position = position_jitter(width = 45)) +
      geom_line(alpha = 0.25, linetype = "11", linewidth = 0.5) +
      scale_x_date("date") +
      scale_y_continuous("point estimate",
        breaks = seq(-3, 0, 0.25), limits = c(-1.75, -0.25)) +
      scale_colour_manual("stratum", values = palette_gen[c(2,6,10,14)]) +
      scale_fill_manual("stratum", values = palette_gen[c(2,6,10,14)]) +
      annotate("rect", xmin=as.Date("2016-10-01"), xmax=as.Date("2017-12-31"), 
        ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "firebrick") +
      facet_grid(adm1~.) +
      theme_bw() +
      theme(legend.position = "top", plot.margin = margin(10,5,0,0))
    ggsave(paste0(dir_path, "out/02_svy_estimates_acz.png"), units = "cm",
      dpi = "print", height = 27, width = 12)

    # Combination plot
    plot_combi <- ggarrange(
      plot_whz + theme(strip.background.y = element_blank(), 
        strip.text.y = element_blank()), 
      plot_acz + theme(axis.title.y = element_blank()), 
      ncol = 2, 
      labels = levels(df$indicator), font.label = list(face="plain",size=10.5))
    ggsave(paste0(dir_path, "out/02_svy_estimates_combi2.png"), units = "cm",
      dpi = "print", height = 23, width = 17)
     

  #...................................      
  ## Graph survey data availability by sub-county
    
    # Prepare data
    x <- table(svy_obs$adm2)
    x <- data.frame(adm2 = names(x), n_obs = as.vector(x))
    df <- merge(shp_adm2_trans, x, by = "adm2", all.x = T)
    df[which(is.na(df$n_obs)), "n_obs"] <- 0
    pnts <- unique(prices_left[, c("market", "lat", "long")])
    pnts_sf <- st_as_sf(pnts, coords = c("long", "lat"), crs = "EPSG:4326") 
    pnts_sf <- st_transform(pnts_sf, "EPSG:4326")
    nudge_x_adm2 <- vector("numeric", length = nrow(df))
    names(nudge_x_adm2) <- df$adm2 
    nudge_x_adm2[] <- 0
    nudge_x_adm2["Baringo South"] <- 0.35
    nudge_x_adm2["Turkana Central"] <- 0.2
    nudge_x_adm2["Saku"] <- 0.2
    nudge_x_adm2["Dujis"] <- 0.1
        
    # Produce a map of counties and subcounties, shaded based on n observations
        # with market locations
    ggplot() +
      geom_sf(data = df, aes(fill = n_obs), lwd = 0.25, colour = "grey80", 
        alpha = 0.8) +
      geom_sf(data = shp_adm1_trans, lwd = 0.5, colour = "grey20", 
        alpha = 0.3, fill = NA) +
      geom_sf_text(data = df, aes(label = adm2), 
        colour = "black", size = 1.5, nudge_x = as.vector(nudge_x_adm2) ) +
      geom_sf_label(data = shp_adm1_trans, aes(label = adm1), size = 2, 
        colour = "black", alpha = 0.5,
        nudge_y = c(-0.1,0,0,0,0,0,-0.2,0.2,0,-0.2)) +
      geom_point(data = pnts, aes(x = long, y = lat), fill = palette_gen[13], 
        colour = palette_gen[9], shape = 22) +
      theme_bw() +
      theme(axis.title = element_blank(), legend.position = "inside",
        legend.position.inside = c(0.18, 0.18),
        axis.text = element_text(size = 6), 
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 6)) +
      scale_fill_gradient(name = "number of observations", 
        low = "white", high = palette_gen[6])
    ggsave(paste0(dir_path, "out/02_map_obs.png"), height = 13, width = 15, 
      units = "cm", dpi = 300)
    
       
#...............................................................................  
### ENDS
#...............................................................................
     