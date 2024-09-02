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
 
      # apply planar transformation
      st_crs(shp_adm1)
      shp_adm1_trans <- st_transform(shp_adm1, 4981)
      table(sf::st_is_valid(shp_adm1_trans))

    # Read administrative shapefile for admin2 (subcounty)
    shp_adm2 <- sf::st_read(paste0(dir_path, 
      "in/admin_boundaries/ken_admbnda_adm2_iebc_20191031.shp"))
    colnames(shp_adm2) <- tolower(colnames(shp_adm2))
    shp_adm2 <- shp_adm2[, ! colnames(shp_adm2) %in% c("adm2_ref",
      "adm2alt1en", "adm2alt2en")]
    colnames(shp_adm2) <- gsub("_en", "", colnames(shp_adm2))

      # apply planar transformation and fix a non-valid vertex
      st_crs(shp_adm2)
      shp_adm2_trans <- st_transform(shp_adm2, 4981)
      table(sf::st_is_valid(shp_adm2_trans))
      shp_adm2 <- sf::st_make_valid(shp_adm2)
      shp_adm2_trans <- sf::st_make_valid(shp_adm2_trans)
  
      
  #...................................
  ## Read and prepare WorldPop population data for each year
  if (file.exists(paste0(dir_path, "in/pop.rds"))) 
    {pop <- readRDS(paste0(dir_path, "in/pop.rds"))}

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
      shp_adm2_pop <- shp_adm2_pop[which(shp_adm2_pop$adm2 %in% admin$adm2), ]
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
    spi <- expand.grid(adm2_pcode = shp_adm2$adm2_pcode, file = file_names_unzip)
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

      # clip and mask to restrict to Kenya subcounties
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
      by = "adm2_pcode") # this will restrict to region of interest  
    spi$date <- as.Date(paste(spi$year, spi$month, "15",sep = "-"))
    spi <- spi[order(spi$adm2, spi$date), ]
    x <- by(spi, spi$adm2, function(x) {
      out <- SPEI::spi(x$rainfall, scale = 1, verbose = F)
      return(out$fitted)
    })
    spi$spi <- as.vector(unlist(x))
  
    # Add 3- and 6-monthly rolling SPI means, aligned right
    x <- by(spi, spi$adm2, function(x) {rollmean(x$spi, k = 3, align = "right",
      na.pad = TRUE)})
    spi$spi3m <- as.vector(unlist(x))
    x <- by(spi, spi$adm2, function(x) {rollmean(x$spi, k = 6, align = "right",
      na.pad = TRUE)})
    spi$spi6m <- as.vector(unlist(x))

  
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
  #     gui = FALSE,
  #     out_folder = paste0(dir_path, "in/ndvi"),
  #     selprod = "Vegetation_Indexes_Monthly_1Km (M*D13A3)",
  #     bandsel = "NDVI",
  #     user = "francescochecchi",
  #     password = "Kenya_Malnut2024!",
  #     start_date = "2015.01.01",
  #     end_date = "2015.12.31",
  #     spatmeth = "file",
  #     spafile = paste0(dir_path, "in/ndvi/tiaty.shp"),
  #     parallel = FALSE,
  #     out_format = "GTiff",
  #     out_projsel = "User Defined",
  #     output_prof = "4326",
  #     scale_val = FALSE,
  #     verbose = TRUE
  #   )

      
    
    
#...............................................................................
### Harmonising administrative units (nomenclature, least common denominator...)
#...............................................................................

  #...................................      
  ## Standardise subcounty names
    
    # Produce a map of reference OCHA counties and subcounties
    x1 <- subset(shp_adm1_trans, adm1 %in% names(table(admin$adm1)))
    x2 <- subset(shp_adm2_trans, adm2 %in% names(table(admin$adm2)))
    map_adm2 <-  ggplot() +
      geom_sf(data = x1, lwd = 0.5, colour = palette_gen[8], alpha = 0.8, 
        fill = NA) +
      geom_sf(data = x2, lwd = 0.25, colour = "grey80", alpha = 0.8, 
        fill = NA) +
      geom_sf_text(data = x2, aes(label = adm2), colour = "black",
        size = 1.5) +
      geom_sf_text(data = x1, aes(label = adm1), colour = palette_gen[8],
        size = 2, nudge_y = c(-0.1, 0, 0, 0, 0, 0, -0.2, 0.2, 0, -0.2)) +
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
          df2 <- df[!x, ]
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
  for (i in c("insecurity", "prices", "water_points")) {
    
    # Get dataset
    df <- get(i)
    
    # Prepare a points collection
    pnts <- df[, c("long", "lat")]
    colnames(pnts) <- c("x", "y")
    pnts_sf <- do.call("st_sfc", c(lapply(1:nrow(pnts), 
      function(x) {st_point(as.numeric(pnts[x, ]))}), list("crs" = 4326))) 

    # Apply planar transformation
    pnts_trans <- st_transform(pnts_sf, 4981)

    # Find which subcounty each point is in
    x <- apply(st_intersects(shp_adm2_trans, pnts_trans, sparse = FALSE), 2, 
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
      df <- merge(df, admin[, c("adm1", "adm1_pcode")], by = "adm1", all.x = T)
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
      
 
  #...................................      
  ## Make sure each time-varying dataset has a numeric month and year variable
    
     
    
       
#...............................................................................
### ENDS
#...............................................................................

    
    