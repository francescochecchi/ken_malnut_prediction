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
      ggplot2,       # Visualise data
      ggpubr,        # Arrange multiple plots into a single plot
      ggrepel,       # Improve labelling of plots
      gtools,        # Assist various programming tasks
      Hmisc,         # Interpolate and extrapolate linearly
      lubridate,     # Work with dates and times
      MASS,          # Implement various statistical methods
      mgcv,          # Fit generalised additive models
#      mice,          # Impute missing variables
      parallel,      # Speed up code implementation
      psych,         # Implement Principal Components Analysis
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

    # # Install and load other packages not available from CRAN
    #   # package to download and manage NDVI data
    #   remotes::install_github("francescochecchi/MODIStsp", force = T)
    #   library(MODIStsp)
    #     # repo was merged with pull request 'devel' from user 'pkautio'

    
  #...................................      
  ## Starting setup

    # Clean up from previous code / runs
    rm(list=ls(all=TRUE) )
  
    # Set font
    windowsFonts(Arial=windowsFont("Arial"))

    # Set working directory to where this file is stored
    dir_path <- paste(dirname(rstudioapi::getActiveDocumentContext()$path  )
      , "/", sep = "")
    setwd(dir_path)
    print( getwd() )
    dir_path <- gsub("/code", "", dir_path)
    
    # Initialise random numbers
    set.seed(123)
    
    # Colour-blind palette for graphing
      # general palette
      palette_gen <- viridis(16)
      show_col(palette_gen)
          

#...............................................................................
### Sourcing dependent scripts
#...............................................................................

  #...................................      
  ## Read and harmonise datasets (except for survey raw data)
  source(paste0(dir_path, "code/01_read_harmonise_data.r") )
          
  #...................................      
  ## Prepare SMART survey anthropometric data
  source(paste0(dir_path, "code/02_prepare_survey_data.r") )


          
#...............................................................................  
### ENDS
#...............................................................................
     