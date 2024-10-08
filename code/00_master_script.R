#...............................................................................
### +++++ PREDICTING ACUTE MALNUTRITION IN DROUGHT-PRONE AREAS OF KENYA ++++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO LOAD PACKAGES AND SOURCE OTHER ANALYSIS SCRIPTS  ------ ##
#...............................................................................



#...............................................................................
### Preparatory steps
#...............................................................................

  #...................................      
  ## Install or load required R packages
  if (!"pacman" %in% rownames(installed.packages())){install.packages("pacman")}
  
    # Install or load packages from CRAN
    pacman::p_load(
      anthro,        # Calculate anthropometric indices
      archive,       # Unzip 7z files
      chirps,        # Download CHIRPS rainfall data
      exactextractr, # Extract statistics for each polygon from raster data
      GGally,        # Produce correlation plots
      ggplot2,       # Visualise data
      ggpubr,        # Arrange multiple plots into a single plot
      ggrepel,       # Improve labelling of plots
      gtools,        # Assist various programming tasks
      Hmisc,         # Interpolate and extrapolate linearly
      lubridate,     # Work with dates and times
      MASS,          # Implement various statistical methods
      mgcv,          # Fit generalised additive models
      parallel,      # Speed up code implementation
      psych,         # Implement Principal Components Analysis
      ranger,        # Grow random forest machine learning models
      readxl,        # Read Excel files
      remotes,       # Install packages from github
      R.utils,       # Unzip .gz files
      scales,        # Scale and format data for visualisation
      sf,            # Work with shape files and maps
      SPEI,          # Compute standardised precipitation index
      survey,        # Estimate based on complex survey designs
      terra,         # Manage and analyse spatial datasets
      tidyverse,     # Tidyverse suite of packages
      tmap,          # Produce maps and work with GIS data
      viridis,       # Colour-blind palette
      zoo)           # Compute running means


  #...................................      
  ## Starting setup

    # Clean up from previous code / runs
    rm(list=ls(all=TRUE) )
  
    # Set font for Windows or Mac
    suppressWarnings(windowsFonts(Arial = windowsFont("Arial")))
    suppressWarnings(par(family = "Arial"))

    # Set working directory to where this file is stored
    dir_path <- paste(dirname(rstudioapi::getActiveDocumentContext()$path  )
      , "/", sep = "")
    setwd(dir_path)
    print( getwd() )
    dir_path <- gsub("/code", "", dir_path)
    suppressWarnings(dir.create(paste0(dir_path, "out")))
    
    # Initialise random numbers
    set.seed(123)
    
    # Colour-blind palette for graphing
      # general palette
      palette_gen <- viridis(16)
      show_col(palette_gen)
          

#...............................................................................
### Sourcing dependent scripts
    # scripts 01 to 03 need only be run once; thereafter, can start from 04
#...............................................................................

  #...................................      
  ## Read and harmonise datasets (except for survey raw data)
  source(paste0(dir_path, "code/01_read_harmonise_data.r") )
          
  #...................................      
  ## Prepare SMART survey anthropometric data
  source(paste0(dir_path, "code/02_prepare_survey_data.r") )

  #...................................      
  ## Merge all data together and categorise predictors
  source(paste0(dir_path, "code/03_merge_categorise_predictors.r") )

  #...................................      
  ## Evaluate candidate models
  source(paste0(dir_path, "code/04_evaluate_models.r") )
    
      
            
#...............................................................................  
### ENDS
#...............................................................................
     
