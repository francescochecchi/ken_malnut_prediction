#...............................................................................
### +++++ PREDICTING ACUTE MALNUTRITION IN DROUGHT-PRONE AREAS OF KENYA ++++ ###
#...............................................................................

#...............................................................................
## --- R SCRIPT TO READ AND HARMONISE NOMENCLATURE OF DIFFERENT DATASETS  --- ##
#...............................................................................


#...............................................................................
### Reading administrative datasets, population denominators and survey metadata
#...............................................................................

  #...................................      
  ## Read administrative divisions, inflation and survey metadata
    
    # Filename and metadata
    file_other <- "ken_other_data.xlsx"
    metadata_other <- as.data.frame(read_excel(
      paste0(dir_path, "in/", file_other), sheet = "metadata"))
    
    # Read each dataset
    for (i in metadata_other$worksheet) {
      x <- as.data.frame(read_excel(
        paste0(dir_path, "in/", file_other), sheet = i))
      assign(i, x)
    }

  #...................................      
  ## Read and prepare administrative divisions shapefile

    # Read administrative shapefile for admin1 (county)
    shp_adm1 <- sf::st_read(paste0(dir_path, 
      "in/admin_boundaries/ken_admbnda_adm1_iebc_20191031.shp"))
    colnames(shp_adm1) <- tolower(colnames(shp_adm1))
    shp_adm1 <- shp_adm1[, ! colnames(shp_adm1) %in% c("adm1_ref",
      "adm1alt1en", "adm1alt2en")]
    colnames(shp_adm1) <- gsub("_en", "", colnames(shp_adm1))
    
      # restrict to counties of interest
      shp_adm1 <- subset(shp_adm1, adm1 %in% unique(admin$adm1))
 
      # apply planar transformation
      st_crs(shp_adm1)
      shp_adm1_trans <- st_transform(shp_adm1, 4326)
      table(sf::st_is_valid(shp_adm1_trans))

    # Read administrative shapefile for admin2 (subcounty)
    shp_adm2 <- sf::st_read(paste0(dir_path, 
      "in/admin_boundaries/ken_admbnda_adm2_iebc_20191031.shp"))
    colnames(shp_adm2) <- tolower(colnames(shp_adm2))
    shp_adm2 <- shp_adm2[, ! colnames(shp_adm2) %in% c("adm2_ref",
      "adm2alt1en", "adm2alt2en")]
    colnames(shp_adm2) <- gsub("_en", "", colnames(shp_adm2))

      # restrict to counties of interest
      shp_adm2 <- subset(shp_adm2, adm2 %in% unique(admin$adm2))
    
      # apply planar transformation and fix a non-valid vertex
      st_crs(shp_adm2)
      shp_adm2_trans <- st_transform(shp_adm2, 4326)
      table(sf::st_is_valid(shp_adm2_trans))
      shp_adm2 <- sf::st_make_valid(shp_adm2)
      shp_adm2_trans <- sf::st_make_valid(shp_adm2_trans)
  
      
  #...................................
  ## Read and prepare WorldPop population data for each year
  if (file.exists(paste0(dir_path, "in/pop.rds"))) 
    {pop <- readRDS(paste0(dir_path, "in/pop.rds"))
     pop_adm1 <- readRDS(paste0(dir_path, "in/pop_adm1.rds"))
    }

  if (! file.exists(paste0(dir_path, "in/pop.rds"))) {
    # Prepare output
    pop <- expand.grid(adm2 = admin$adm2, year = 2015:2019, month = 1:12, 
      pop = NA)
    pop <- pop[order(pop$adm2), ]
    
    # For each year...  
    for (i in 2015:2019) {
    
      # progress statement
      print(paste0("now working on year ", i))
      
      # download yearly dataset  
      file_i <- terra::rast(paste0("https://data.worldpop.org/GIS/Population/",
        "Global_2000_2020/", i, "/KEN/ken_ppp_", i, "_UNadj.tif"))

      # make sure admin shape file has the same projection
      shp_adm2_pop <- st_transform(shp_adm2, terra::crs(file_i))
  
      # clip and mask to restrict to subcounties of interest
      shp_adm2_pop <- shp_adm2_pop[order(shp_adm2_pop$adm2), ]
      file_i <- terra::crop(x = file_i, y = shp_adm2_pop)
      file_i <- terra::mask(x = file_i, mask = shp_adm2_pop)
      
      # take means for each subcounty and add to output
      pop[which(pop$year == i & pop$month == 6), "pop"] <- 
        exact_extract(file_i, shp_adm2_pop, "sum")
      rm(file_i)
    } 
    
    # Interpolate population linearly to obtain monthly values, and save file
    pop <- pop[order(pop$adm2, pop$year, pop$month), ]
    pop_ipol <- by(pop, pop$adm2, function(xx) {
      xx$tm <- 1:nrow(xx)
      return(Hmisc::approxExtrap(x = xx[!is.na(xx$pop), "tm"], 
        y = xx[!is.na(xx$pop), "pop"], xout = xx$tm)$y)
    })
    pop$pop_ipol <- as.vector(unlist(pop_ipol))
    pop$pop <- pop$pop_ipol
    pop <- subset(pop, select = -pop_ipol)
    saveRDS(pop, paste0(dir_path, "in/pop.rds"))
    
    # Aggregate population to county level (for county-level predictors)
    pop_adm1 <- merge(pop, admin[, c("adm1", "adm2")], by = "adm2", all.x = T)
    pop_adm1 <- aggregate(list(pop = pop_adm1$pop), 
      by = pop_adm1[, c("adm1", "year", "month")], FUN = sum) 
    saveRDS(pop_adm1, paste0(dir_path, "in/pop_adm1.rds"))
  }

    
#...............................................................................
### Reading and preparing predictor datasets
#...............................................................................
    
  #...................................      
  ## Read Excel-format predictor datasets
    
    # Filename and metadata
    file_pred <- "ken_predictor_data.xlsx"
    metadata_pred <- as.data.frame(read_excel(
      paste0(dir_path, "in/", file_pred), sheet = "metadata"))
    
    # Read each dataset
    for (i in metadata_pred$worksheet) {
      x <- as.data.frame(read_excel(
        paste0(dir_path, "in/", file_pred), sheet = i))
      assign(i, x)
    }
    

  #...................................      
  ## Read static spatial predictor datasets
    
    # Proportion of women aged 15-49 per grid square literate in 2008/9
    
      # extract data
      archive::archive_extract(paste0(dir_path, "in/literacy/literacy.7z"),
        paste0(dir_path, "in/literacy/"))
    
      # compute mean literacy proportion per subcounty
      literacy<-terra::rast(paste0(dir_path, "in/literacy/KEN_literacy_F.tif"))

      # make sure admin shape file has the same projection
      shp_adm2_lit <- st_transform(shp_adm2, terra::crs(literacy))
  
      # clip and mask to restrict to subcounties of interest
      shp_adm2_lit <- shp_adm2_lit[order(shp_adm2_lit$adm2), ]
      literacy <- terra::crop(x = literacy, y = shp_adm2_lit)
      literacy <- terra::mask(x = literacy, mask = shp_adm2_lit)
      
      # take means for each subcounty and add to output
      literacy <- data.frame(adm2 = shp_adm2_lit$adm2, prop_lit =
        exact_extract(literacy, shp_adm2_lit, "mean"))
      
    # MNH service access (probability of skilled birth attendance, SBA, in 2015)
    
      # extract data
      archive::archive_extract(paste0(dir_path, "in/mnh_access/mnh.7z"),
        paste0(dir_path, "in/mnh_access/"))
    
      # compute mean SBA proportion per subcounty
      mnh_access<-terra::rast(paste0(dir_path, "in/mnh_access/KEN_MNH_SBA.tif"))

      # make sure admin shape file has the same projection
      shp_adm2_mnh <- st_transform(shp_adm2, terra::crs(mnh_access))
  
      # clip and mask to restrict to subcounties of interest
      shp_adm2_mnh <- shp_adm2_mnh[order(shp_adm2_mnh$adm2), ]
      mnh_access <- terra::crop(x = mnh_access, y = shp_adm2_mnh)
      mnh_access <- terra::mask(x = mnh_access, mask = shp_adm2_mnh)
      
      # take means for each subcounty and add to output
      mnh_access <- data.frame(adm2 = shp_adm2_mnh$adm2, prop_sba =
        exact_extract(mnh_access, shp_adm2_mnh, "mean"))
    
      
  #...................................      
  ## Read and prepare monthly standardised precipitation index, from CHIRPS data
  if (! file.exists(paste0(dir_path, "in/spi.rds"))) {    
    # Version
    v_chirps <- "2.0"
    
    # Source URL
    url <- paste0("https://data.chc.ucsb.edu/products/CHIRPS-", v_chirps, 
      "/africa_monthly/tifs/")
    
    # File names to download from website
      # file names
      file_names <- c()
      for (i in 1981:2019) {
        for (j in sprintf("%02d", 1:12)) {
          file_names <- c(file_names, paste0("chirps-v", v_chirps, ".", i, ".", j, 
            ".tif.gz"))
        }
      }
      
      # unzipped file names  
      file_names_unzip <- gsub(".gz", "", file_names)

      # individual file URL links
      file_urls <- paste0(url, file_names)

    # Make sure the admin shapefile has the same projection as CHIRPS rasters
    x <- paste0(dir_path, "in/", gsub(url, "", file_urls[1]))
    download.file(file_urls[1], x, method = "curl")
    gunzip(x)
    df <- terra::rast(gsub(".gz", "", x))
    shp_adm2_chirps <- st_transform(shp_adm2, terra::crs(df))
    file.remove(gsub(".gz", "", x))
    rm(df)
    
    # Prepare output
    spi <- expand.grid(adm2_pcode= shp_adm2$adm2_pcode, file = file_names_unzip)
    spi$year <- as.integer(substr(spi$file, 13, 16))
    spi$month <- as.integer(substr(spi$file, 18, 19))
    spi$rainfall <- NA

    # Download and process each CHIRPS monthly file
    for (i in seq_along(file_urls)) {
      
      # progress
      print(paste0("now working on CHIRPS file ", i, " of ", length(file_urls)))
      
      # where to download
      target_i <- paste0(dir_path, "in/", gsub(url, "", file_urls[i]))
      
      # download file (if doesn't work, ignore error)
      x <- tryCatch(download.file(file_urls[i], target_i, method = "curl"))
      if (inherits(x, "error")) {print(x); next}
      
      # unzip file
      gunzip(target_i)
      target_i_unzip <- gsub(".gz", "", target_i)
      
      # source raster file
      file_i <- terra::rast(target_i_unzip)

      # clip and mask to restrict to subcounties of interest
      file_i <- terra::crop(x = file_i, y = shp_adm2_chirps)
      file_i <- raster::mask(x = file_i, mask = shp_adm2_chirps)
        
      # take means for each subcounty and add to output
      spi[which(spi$file == file_names_unzip[i]), "rainfall"] <- 
        exact_extract(file_i, shp_adm2_chirps, "mean")
      
      # delete CHIRPS file from directory so as to create space
      if(file.exists(target_i_unzip)) {file.remove(target_i_unzip)}
    }
    
    # Save rainfall dataset
    saveRDS(spi, paste0(dir_path, "in/spi.rds"))
  }  

    # Source rainfall data
    spi <- readRDS(paste0(dir_path, "in/spi.rds"))
      
    # Compute standardised precipitation index      
    spi <- merge(spi, admin[, c("adm2_pcode", "adm2")], 
      by = "adm2_pcode")
    spi$date <- as.Date(paste(spi$year, spi$month, "15",sep = "-"))
    spi <- spi[order(spi$adm2, spi$date), ]
    x <- by(spi, spi$adm2, function(x) {
      out <- SPEI::spi(x$rainfall, scale = 1, verbose = F)
      return(out$fitted)
    })
    spi$spi <- as.vector(unlist(x))
  
    # Add 3- and 6-monthly rolling SPI means, aligned right
    x <- by(spi, spi$adm2, function(x) {rollmean(x$spi, k = 3, align = "right",
      na.pad = T)})
    spi$spi_3m <- as.vector(unlist(x))
    x <- by(spi, spi$adm2, function(x) {rollmean(x$spi, k = 6, align = "right",
      na.pad = T)})
    spi$spi_6m <- as.vector(unlist(x))

  
  # #...................................      
  # ## Read and prepare monthly NDVI index, from MODIS data
  #   
  #   MODIStsp_get_prodnames()
  #   MODIStsp_get_prodlayers("M*D13A3")
  #  
  #   st_write(shp_adm2[which(shp_adm2$adm2 == "Tiaty"), ], 
  #     paste0(dir_path, "in/ndvi/tiaty.shp")) 
  # 
  #   
  #   MODIStsp(
  #     gui = F,
  #     out_folder = paste0(dir_path, "in/ndvi"),
  #     selprod = "Vegetation_Indexes_Monthly_1Km (M*D13A3)",
  #     bandsel = "NDVI",
  #     user = "francescochecchi",
  #     password = "Kenya_Malnut2024!",
  #     start_date = "2015.01.01",
  #     end_date = "2015.12.31",
  #     spatmeth = "file",
  #     spafile = paste0(dir_path, "in/ndvi/tiaty.shp"),
  #     parallel = F,
  #     out_format = "GTiff",
  #     out_projsel = "User Defined",
  #     output_prof = "4326",
  #     scale_val = F,
  #     verbose = T
  #   )

      
    
    
#...............................................................................
### Harmonising administrative units (nomenclature, least common denominator...)
#...............................................................................

  #...................................      
  ## Standardise subcounty names
    
    # Produce a map of reference OCHA counties and subcounties
    map_adm2 <-  ggplot() +
      geom_sf(data = shp_adm1_trans, lwd = 0.5, colour = palette_gen[8], 
        alpha = 0.8, fill = NA) +
      geom_sf(data = shp_adm2_trans, lwd = 0.25, colour = "grey80", 
        alpha = 0.8, fill = NA) +
      geom_sf_text(data = shp_adm2_trans, aes(label = adm2), 
        colour = "black", size = 1.5) +
      geom_sf_text(data = shp_adm1_trans, aes(label = adm1), size = 2, 
        colour = palette_gen[8], nudge_y = c(-0.1,0,0,0,0,0,-0.2,0.2,0,-0.2)) +
      theme_bw() +
      theme(axis.title = element_blank())
    ggsave(paste0(dir_path, "out/01_map_adm2.png"), height = 15, width = 15, 
      units = "cm", dpi = 300)

    # Incorrect and correct nomenclature
    df_correct <- data.frame(
      adm2 = c("Banisa", "Baringo East - Tiaty", "Central Pokot", 
        "Garbatulla", "Garissa", "Habaswein", "Hulugho", "Isiolo", 
        "Kibish", "Kotulo", "Merti", "North Pokot", 
        "South Pokot", "Tana Delta", "Tana North", "Tana River", "West Pokot",
        "East Pokot", "Garbatula", "Kakimat Community Health Unit", "Koibatek", 
        "Kulamawe medical services", "Kutulo", "Kutulo Sub-County",
        "Lengusaka Dispensary", "Mandera Central", "Marigat",
        "Nyatalio Dispensary", "Samburu Central", "Tiaty East"
        ),
      adm2_correct = c("Banissa", "Tiaty", "Kapenguria",
        "Isiolo South", "Dujis", "Wajir South", "Ijara", "Isiolo South",
        "Turkana North", "Mandera South", "Isiolo North", "Kacheliba", 
        "Pokot South", "Garsen", "Bura", "Galole", "Kapenguria",
        "Tiaty", "Isiolo South", "Turkana Central", "Eldama Ravine",
        "Isiolo South", "Mandera South", "Mandera South",
        "Samburu East", "Mandera South", "Baringo South",
        "Mandera South", "Samburu West", "Tiaty"
       )
    )
    
    # Correct nomenclature
    for (i in c(metadata_other$worksheet, metadata_pred$worksheet)) {
      
      # get dataset
      df <- get(i)
      
      # if subcounty is one of the variables...
      if ("adm2" %in% colnames(df)) {
        x <- which(df$adm2 %in% df_correct$adm2)
        if (length(x) > 0) {
          df1 <- merge(df[x, ], df_correct, by = "adm2", all.x = T)
          df2 <- df[-x, ]
          df2$adm2_correct <- df2$adm2
          df <- rbind(df1, df2)
          df$adm2 <- df$adm2_correct
          df <- subset(df, select = -adm2_correct)
        }
      }
      
      # update dataset
      assign(i, df)
    }
    
  #...................................      
  ## Add adm2 (subcounty) to datasets where it is missing or needs to be checked
  for (i in c("insecurity", "prices")) {
    
    # Get dataset
    df <- get(i)
    
    # Prepare a points collection
    pnts <- df[, c("long", "lat")]
    colnames(pnts) <- c("x", "y")
    pnts_sf <- do.call("st_sfc", c(lapply(1:nrow(pnts), 
      function(x) {st_point(as.numeric(pnts[x, ]))}), list("crs" = 4326))) 

    # Apply planar transformation
    pnts_trans <- st_transform(pnts_sf, 4326)

    # Find which subcounty each point is in
    x <- apply(st_intersects(shp_adm2_trans, pnts_trans, sparse = F), 2, 
      function(x) { shp_adm2_trans[which(x), "adm2_pcode"]})
    df$adm2_pcode <- as.vector(unlist(lapply(x, st_drop_geometry)))
    assign(i, df)
    
    # Check completeness of subcounty values
    print(paste0("are any subcounties missing for dataset '", i, "'?"))
    print(table(is.na(df$adm2_pcode)))
  }
    
    
  #...................................      
  ## Add to any dataset pcodes or administrative unit names, if missing
  for (i in c(metadata_other$worksheet, metadata_pred$worksheet)) {
    
    # Get dataset
    df <- get(i)

    # Add missing pcodes    
    if ("adm1" %in% colnames(df) & ! "adm1_pcode" %in% colnames(df)) {
      df <- merge(df, unique(admin[, c("adm1", "adm1_pcode")]), by = "adm1", 
        all.x = T)
    }
    if ("adm2" %in% colnames(df) & ! "adm2_pcode" %in% colnames(df)) {
      df <- merge(df, admin[, c("adm2", "adm2_pcode")], by = "adm2",  all.x = T)
    }
      # check completeness
      print(paste0("are any county pcodes missing for dataset '", i, "'?"))
      print(table(is.na(df$adm1_pcode)))
      print(paste0("are any subcounty pcodes missing for dataset '", i, "'?"))
      print(table(is.na(df$adm2_pcode)))
      print("#############")
    
    # Add missing names
    if ("adm1_pcode" %in% colnames(df) & ! "adm1" %in% colnames(df)) {
      df <- merge(df, admin[, c("adm1", "adm1_pcode")],by="adm1_pcode", all.x=T)
    }
    if ("adm2_pcode" %in% colnames(df) & ! "adm2" %in% colnames(df)) {
      df <- merge(df, admin[, c("adm2", "adm2_pcode")],by="adm2_pcode", all.x=T)
    }
      # check completeness
      print(paste0("are any county names missing for dataset '", i, "'?"))
      print(table(is.na(df$adm1)))
      print(paste0("are any subcounty names missing for dataset '", i, "'?"))
      print(table(is.na(df$adm2)))
      print("#############")
    
    # Update dataset
    assign(i, df)
  }    


  #...................................      
  ## Make sure each time-varying dataset has a numeric month and year variable
    
    # Which datasets
    x <- c("insecurity", "mam_cases", "morbidity", "pop", "prices", "sam_cases",
      "spi", "surveys", "utilisation", "vaccination")
    
    # For each dataset...
    for (i in x) {
      
      # which dataset
      print(paste0("now checking month and year in dataset ", i))
      
      # grab dataset
      df_i <- get(i)
      
      # if there is a date variable...
      if ("date" %in% colnames(df_i)) {
        df_i$month <- month(df_i$date)
        df_i$year <- year(df_i$date)
      }

      # if there is a month variable...
      if ("month" %in% colnames(df_i)) {
        if (!typeof(df_i$month) %in% c("numeric", "integer")) {
          df_i$month <- tolower(df_i$month)
          df_i[which(df_i$month %in% c("1", "jan", "january")), "month"] <- 1
          df_i[which(df_i$month %in% c("2", "feb", "february")), "month"] <- 2
          df_i[which(df_i$month %in% c("3", "mar", "march")), "month"] <- 3
          df_i[which(df_i$month %in% c("4", "apr", "april")), "month"] <- 4
          df_i[which(df_i$month %in% c("5", "may", "may")), "month"] <- 5
          df_i[which(df_i$month %in% c("6", "jun", "june")), "month"] <- 6
          df_i[which(df_i$month %in% c("7", "jul", "july")), "month"] <- 7
          df_i[which(df_i$month %in% c("8", "aug", "august")), "month"] <- 8
          df_i[which(df_i$month %in% c("9", "sep", "september")), "month"] <- 9
          df_i[which(df_i$month %in% c("10", "oct", "october")), "month"] <- 10
          df_i[which(df_i$month %in% c("11", "nov", "november")), "month"] <- 11
          df_i[which(df_i$month %in% c("12", "dec", "december")), "month"] <- 12
          df_i$month <- as.integer(df_i$month)
        }
      }
      
      # check
      print(table(df_i$month))
      print(table(df_i$year))
            
      # update dataset  
      assign(i, df_i)        
    }
      

    
#...............................................................................
### Managing the food prices dataset
#...............................................................................
     
  #...................................      
  ## Decide which food price time series are complete enough to keep
    
    # Restrict to 2014-2019
    prices <- subset(prices, year %in% 2014:2019)
    
    # Check for unique markets - all may be unique
    unique(prices[, c("market", "adm2", "lat", "long")])
    
    # Which unique food items
    unique(prices[, c("commodity", "unit")])
    prices$item <- paste(prices$commodity, prices$unit)
    
    # Check for any duplicates (no)
    table(duplicated(prices))
    
    # Time series of markets-items-years-months
    ts <- expand.grid(item = unique(prices$item), year = 2014:2019, month= 1:12)
    ts <- merge(unique(prices[, c("market", "adm2", "lat", "long")]), ts)
    ts <- ts[order(ts$market, ts$item, ts$year, ts$year), ]
    
    # Tabulate annual completeness of each market - food price series
    x <- na.omit(prices[, c("market", "item", "year", "month", "price")])
    ts <- merge(ts, x, by=c("market", "item", "year", "month"), all.x = T)
    ts$value <- ifelse(is.na(ts$price), 0, 1)    
    x <- aggregate(list(value = ts$value), 
      by = ts[, c("market", "adm2", "item", "year")], FUN = mean)
    
    # Apply threshold of > 25% completeness and inspect what is left
    x <- subset(x, value >= 0.25)
    
    # Can only keep 'Maize (white) KG', and for a few markets only, from 2015-
    x <- subset(x, item == "Maize (white) KG")
    table(x[, c("market", "year")])

    # Subset prices dataset accordingly
    prices_left <- subset(ts, year %in% 2015:2019 & 
      item == "Maize (white) KG" & market %in% unique(x$market))
    prices_left <- prices_left[, c("market", "adm2", "lat", "long", 
      "year", "month", "item", "price")]    

  #...................................      
  ## Adjust for inflation, smooth series and compute running means
    
    # Adjust for inflation
    deflator$adj <- deflator$value
    prices_left <- merge(prices_left, 
      deflator[which(deflator$currency == "KES"), c("year", "adj")], 
      by = "year", all.x = T)
    prices_left$price_adj <- prices_left$price * prices_left$adj / 100
    
    # Prepare smoothing output
    prices_left$tm <- (prices_left$year - 2015) * 12 + prices_left$month
    prices_left$market <- paste0(prices_left$market, " (", prices_left$adm2,")")
    prices_left$date <- as.Date(paste(prices_left$year, prices_left$month, "15",
      sep = "-"))
    prices_left <- prices_left[order(prices_left$market, prices_left$year,
      prices_left$month), ]
    
    # Smooth
    x <- by(prices_left, prices_left$market, function(xx) {
        return(predict(smooth.spline(
          as.matrix(na.omit(xx[, c("tm", "price_adj")])), spar = 0.2), xx$tm)$y)
      })
    prices_left$price_sm <- as.vector(unlist(x))   
    
    # Graph smoothing output
    ggplot(prices_left, aes(x = date, group = market, colour = market, 
      fill = market)) +
      geom_point(aes(y = price_adj), alpha = 0.5, size = 3) +
      geom_line(aes(y = price_sm), linewidth = 1, linetype = "11") +
      scale_x_date("date", breaks = "6 months", expand = c(0, 0), 
        date_labels = "%b-%Y") +
      scale_y_continuous("price of white maize per Kg (KES, 2015)") +
      scale_colour_manual("market", values = palette_gen[c(1,4,7,10,13,16)]) +
      scale_fill_manual("market", values = palette_gen[c(1,4,7,10,13,16)]) +
      facet_grid(market ~ .) +
      theme_bw() +
      theme(legend.position = "none")
    ggsave(paste0(dir_path, "out/01_price_series.png"), dpi = "print", 
      units = "cm", width = 20, height = 30)
    
    # Compute running means
    prices_left <- prices_left[order(prices_left$market, prices_left$year,
      prices_left$month), ]
    x <- by(prices_left, prices_left$market, function(x) {rollmean(x$price_sm, 
      k = 3, align = "right", na.pad = T)})
    prices_left$price_3m <- as.vector(unlist(x))
    x <- by(prices_left, prices_left$market, function(x) {rollmean(x$price_sm, 
      k = 6, align = "right", na.pad = T)})
    prices_left$price_6m <- as.vector(unlist(x))
    
    
  #...................................      
  ## Find the nearest market to each subcounty and pair the two
    
    # Centroids of subcounties
    x <- shp_adm2_trans[which(shp_adm2_trans$adm2 %in% admin$adm2), ]
    centroids <- st_centroid(x)
    
    # Points collection for the markets
    pnts <- unique(prices_left[, c("market", "lat", "long")])
    pnts_sf <- st_as_sf(pnts, coords = c("long", "lat"), crs = "EPSG:4326") 
    pnts_sf <- st_transform(pnts_sf, "EPSG:4326")

    # Find closest point (market) for each subcounty
    x$market <- pnts[st_nearest_feature(centroids, pnts_sf), "market"]
    x <- x[, c("adm2", "market")]
    x <- st_drop_geometry(x)

    # Create dataset with each subcounty and its nearest-market price values
    prices_left <- subset(prices_left, select = -adm2)
    prices_df <- expand.grid(adm2 = admin$adm2, year = 2015:2019, month = 1:12)
    prices_df <- merge(prices_df, x, by = "adm2", all.x = T)
    prices_df <- merge(prices_df, prices_left[, c("market", "year", "month",
      "price_sm", "price_3m", "price_6m")], by = c("market", "year", "month"),
      all.x = T)
        
    
#...............................................................................
### Prepare other predictors so that all have a value for each subcounty-time
#...............................................................................

  #...................................      
  ## Prepare insecurity dataset
    
    # Aggregate events by subcounty-year-month
    insecurity$events <- 1
    insecurity_df <- aggregate(insecurity[, c("events", "fatalities")],
      by = insecurity[, c("adm2", "year", "month")], FUN = sum)
    
    # Merge with overall sub-county time series and set NA values to 0
    ts <- expand.grid(adm2 = admin$adm2, year = 2015:2019, month = 1:12)
    insecurity_df <- merge(ts, insecurity_df[, c("adm2", "year", "month", 
      "events", "fatalities")], by = c("adm2", "year", "month"),
      all.x = T)  
    insecurity_df <- na.replace(insecurity_df, 0)
    
    # Compute incidence rates and compute running means
    insecurity_df <- merge(insecurity_df, 
      pop[, c("adm2", "year", "month", "pop")], by = c("adm2", "year", "month"),
      all.x = T)
    insecurity_df <- insecurity_df[order(insecurity_df$adm2, insecurity_df$year,
      insecurity_df$month), ]
    for (i in c("events", "fatalities")) {
      insecurity_df[, paste0(i, "_rate")] <-
      insecurity_df[, i] * 100000 / insecurity_df$pop
      x <- by(insecurity_df, insecurity_df$adm2, function(x) {
        rollmean(x[, paste0(i, "_rate")], k = 3, align = "right", na.pad = T)})
      insecurity_df[, paste0(paste0(i, "_rate_3m"))] <- as.vector(unlist(x))
      x <- by(insecurity_df, insecurity_df$adm2, function(x) {
        rollmean(x[, paste0(i, "_rate")], k = 6, align = "right", na.pad = T)})
      insecurity_df[, paste0(paste0(i, "_rate_6m"))] <- as.vector(unlist(x))
    }

    
  #...................................      
  ## Prepare MAM cases dataset
    
    # Aggregate new cases by subcounty-year-month
    mam_cases_df <- aggregate(mam_cases[, c("admissions", "died")],
      by = mam_cases[, c("adm2", "year", "month")], FUN = sum)
    
    # Create CFR variable
    mam_cases_df$mam_cfr <- ifelse(mam_cases_df$admissions == 0, 0,
      mam_cases_df$died / mam_cases_df$admissions)
    
    # Rename variables
    mam_cases_df$mam_admissions <- mam_cases_df$admissions
        
    # Merge with overall sub-county time series (NA values stay as NA)
    ts <- expand.grid(adm2 = admin$adm2, year = 2015:2019, month = 1:12)
    mam_cases_df <- merge(ts, mam_cases_df[, c("adm2", "year", "month", 
      "mam_admissions", "mam_cfr")], by = c("adm2", "year", "month"), all.x = T)

    # Compute incidence rates and compute running means
    mam_cases_df <- merge(mam_cases_df,pop[, c("adm2", "year", "month", "pop")], 
      by = c("adm2", "year", "month"), all.x = T)
    mam_cases_df <- mam_cases_df[order(mam_cases_df$adm2, mam_cases_df$year,
      mam_cases_df$month), ]
    mam_cases_df$mam_admissions_rate <- mam_cases_df$mam_admissions * 100000 /
      mam_cases_df$pop
    for (i in c("mam_admissions_rate", "mam_cfr")) {
      for (j in c(3, 6)) {
        x <- by(mam_cases_df, mam_cases_df$adm2, function(x) {
          rollmean(x[, i], k = j, align = "right", na.pad = T)})
        mam_cases_df[, paste0(i, "_", j, "m")] <- as.vector(unlist(x))
      }
    }

    
  #...................................      
  ## Prepare SAM cases dataset
    
    # Exclude some cases
    sam_cases <- subset(sam_cases, group != "Under 6 months")
    
    # Aggregate new cases by subcounty-year-month
    sam_cases_df <- aggregate(sam_cases[, c("new_cases", "died")],
      by = sam_cases[, c("adm2", "year", "month")], FUN = sum)
    
    # Create CFR variable
    sam_cases_df$sam_cfr <- ifelse(sam_cases_df$new_cases == 0, 0,
      sam_cases_df$died / sam_cases_df$new_cases)
    
    # Rename variables
    sam_cases_df$sam_admissions <- sam_cases_df$new_cases
        
    # Merge with overall sub-county time series (NA values stay as NA)
    ts <- expand.grid(adm2 = admin$adm2, year = 2015:2019, month = 1:12)
    sam_cases_df <- merge(ts, sam_cases_df[, c("adm2", "year", "month", 
      "sam_admissions", "sam_cfr")], by = c("adm2", "year", "month"), all.x = T)

    # Compute incidence rates and compute running means
    sam_cases_df <- merge(sam_cases_df, pop[,c("adm2", "year", "month", "pop")], 
      by = c("adm2", "year", "month"), all.x = T)
    sam_cases_df <- sam_cases_df[order(sam_cases_df$adm2, sam_cases_df$year,
      sam_cases_df$month), ]
    sam_cases_df$sam_admissions_rate <- sam_cases_df$sam_admissions * 100000 /
      sam_cases_df$pop
    for (i in c("sam_admissions_rate", "sam_cfr")) {
      for (j in c(3, 6)) {
        x <- by(sam_cases_df, sam_cases_df$adm2, function(x) {
          rollmean(x[, i], k = j, align = "right", na.pad = T)})
        sam_cases_df[, paste0(i, "_", j, "m")] <- as.vector(unlist(x))
      }
    }
    
    
  #...................................      
  ## Prepare morbidity dataset
    
    # Aggregate cases by subcounty-year-month
    morbidity_df <- aggregate(morbidity[, c("cholera", "malaria", "measles")],
      by = morbidity[, c("adm1", "year", "month")], FUN = sum)
    
    # Merge with overall sub-county time series and set NA values to 0
    ts <- expand.grid(adm1 = unique(admin$adm1), year = 2015:2019, month = 1:12)
    morbidity_df <- merge(ts, morbidity_df[, c("adm1", "year", "month", 
      "cholera", "malaria", "measles")], by = c("adm1", "year", "month"),
      all.x = T)  
    morbidity_df <- na.replace(morbidity_df, 0)
    
    # Compute incidence rates and compute running means
    morbidity_df <- merge(morbidity_df, 
      pop_adm1[, c("adm1", "year", "month", "pop")], 
      by = c("adm1", "year", "month"), all.x = T)
    morbidity_df <- morbidity_df[order(morbidity_df$adm1, morbidity_df$year,
      morbidity_df$month), ]
    for (i in c("cholera", "malaria", "measles")) {
      morbidity_df[, paste0(i, "_rate")] <- 
        morbidity_df[, i] * 100000 / morbidity_df$pop
      for (j in c(3, 6)) {
        x <- by(morbidity_df, morbidity_df$adm1, function(x) {
          rollmean(x[, paste0(i, "_rate")], k = j, align= "right", na.pad = T)})
        morbidity_df[, paste0(paste0(i,"_rate_",j,"m"))] <- as.vector(unlist(x))
      }
    }
           
    
  #...................................      
  ## Prepare utilisation dataset
    
    # Rename / define some variables
    utilisation$anc <- utilisation$`ANC Attendance`
    utilisation$opd <- utilisation$`OPD Attendance <5yrs Female` +
      utilisation$`OPD Attendance <5yrs Male`
    
    # Aggregate cases by subcounty-year-month
    utilisation_df <- aggregate(utilisation[, c("anc", "opd")],
      by = utilisation[, c("adm1", "year", "month")], FUN = sum)
    
    # Merge with overall sub-county time series and set NA values to 0
    ts <- expand.grid(adm1 = unique(admin$adm1), year = 2015:2019, month = 1:12)
    utilisation_df <- merge(ts, utilisation_df[, c("adm1", "year", "month",
      "anc", "opd")], by = c("adm1", "year", "month"), all.x = T)  
    utilisation_df <- na.replace(utilisation_df, 0)
    
    # Compute incidence rates and compute running means
    utilisation_df <- merge(utilisation_df, 
      pop_adm1[, c("adm1", "year", "month", "pop")], 
      by = c("adm1", "year", "month"), all.x = T)
    utilisation_df <- utilisation_df[order(utilisation_df$adm1, 
      utilisation_df$year, utilisation_df$month), ]
    for (i in c("anc", "opd")) {
      utilisation_df[, paste0(i, "_rate")] <-
      utilisation_df[, i] * 100000 / utilisation_df$pop
      for (j in c(3, 6)) {
        x <- by(utilisation_df, utilisation_df$adm1, function(x) {
          rollmean(x[, paste0(i, "_rate")], k = j, align= "right", na.pad = T)})
        utilisation_df[, paste0(paste0(i,"_rate_",j,"m"))]<-as.vector(unlist(x))
      }
    }
            

  #...................................      
  ## Prepare vaccination dataset
    
    # Rename / define some variables
    vaccination$dpt3 <- vaccination$`DPT/Hep+HiB3 doses Administered`
    vaccination$mmr1 <- vaccination$`Measles-Rubella 1 doses Administered`
    
    # Aggregate cases by subcounty-year-month
    vaccination_df <- aggregate(vaccination[, c("dpt3", "mmr1")],
      by = vaccination[, c("adm1", "year", "month")], FUN = sum)
    
    # Merge with overall sub-county time series and set NA values to 0
    ts <- expand.grid(adm1 = unique(admin$adm1), year = 2015:2019, month = 1:12)
    vaccination_df <- merge(ts, vaccination_df[, c("adm1", "year", "month",
      "dpt3", "mmr1")], by = c("adm1", "year", "month"), all.x = T)  
    vaccination_df <- na.replace(vaccination_df, 0)
    
    # Compute incidence rates and compute running means
    vaccination_df <- merge(vaccination_df, 
      pop_adm1[, c("adm1", "year", "month", "pop")], 
      by = c("adm1", "year", "month"), all.x = T)
    vaccination_df <- vaccination_df[order(vaccination_df$adm1, 
      vaccination_df$year, vaccination_df$month), ]
    for (i in c("dpt3", "mmr1")) {
      vaccination_df[, paste0(i, "_rate")] <-
      vaccination_df[, i] * 100000 / vaccination_df$pop
      for (j in c(3, 6)) {
        x <- by(vaccination_df, vaccination_df$adm1, function(x) {
          rollmean(x[, paste0(i, "_rate")], k = j, align= "right", na.pad = T)})
        vaccination_df[, paste0(paste0(i,"_rate_",j,"m"))]<-as.vector(unlist(x))
      }
    }
    

  #...................................      
  ## Prepare static datasets
    
    # Schooling
    schooling$schooling_cov <- schooling$n_schooled / schooling$n_children
    

                
#...............................................................................
### ENDS
#...............................................................................

    
    