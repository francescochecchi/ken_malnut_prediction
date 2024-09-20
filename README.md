## Predicting the burden of acute malnutrition in drought-prone regions of Kenya: a statistical analysis
### Description of input data and R analysis scripts
September 2024

## Input data
The following input datasets are included in the `\in` directory of the repository:
* `surveys.zip`, which users should unzip such that all files therein are contained within a `in\surveys\` sub-directory. Each unzipped .csv file contains a single survey dataset, though some datasets contain multiple explicit survey strata. FIle names should not be altered. All datasets are fully anonymised;
* `ken_predictor_data.xlsx`, each worksheet of which contains a single predictor dataset. the `metadata` tab lists each worksheet (predictor) and its characteristics, and is read by R so as to then read the individual worksheets. More detail on each dataset is provided in the paper;
* `ken_other_data.xlsx`, which contains in each worksheet other datasets needed for the analysis, again listed in a `metadata` worksheet;
* `admin_boundaries.zip`, which users should unzip to a `in\admin-boundaries\` sub-directory. This file contains shape files delineating county and sub-county boundaries for Kenya;
A few additional predictor datasets are not uploaded to the repository due to their large size. Users should therefore download them first, as follows:
* normalised difference vegetation index (NDVI) data should be downloaded from [https://gmes.icpac.net/data-center/vgt-ndvi] to a `in\ndvi\` sub-directory. The data platform requires users to sign up and request specific data for download, which they will then receive an email link for. Users should select the 'Monthly NDVI' and 'Monthly SNDVI (Anomaly)' products from 2014 to 2019 in GeoTiff format for all of Kenya. The download will be a single zipped file. Users should unzip this file to the above sub-directory.
* Data on estimated access to safe assisted births should be downloaded from [https://hub.worldpop.org/geodata/summary?id=1263] to a `in\mnh_access\` sub-directory. The R script reading the data will unzip this file;
* Data on literacy should be downloaded from [https://hub.worldpop.org/geodata/summary?id=1261] to a `in\literacy\` sub-directory. The R script reading the data will unzip this file;
* Rainfall data will be downloaded automatically by one of the R scripts.

## R scripts
The `00_master_script.R` code installs and/or loads required R packages, initialises certain parameters and then sources each of the following scripts in order. 
* `01_read_harmonise_data.R` reads all the datasets, calculates additional variables, standardises place names and dates and prepares predictor datasets for merging;
* `02_prepare_survey_data.R` prepares, merges and describes individual survey data, while also generating descriptive tables and graphs;
* `03_merge_categorise_predictors.R` merges all datasets together into one file and explores the distributions of predictors, categorising them if needed;
* `04_evaluate_models.R` explores predictor patterns and collinearity, does univariate analysis and fits borth generalised linear/additive and random forest models for each anthropometric outcome included in the analysis, while generating tables and graphs of model performance.
Scripts 01, 02 and 03 need only be run once, upon which the dataset will be readied for analysis by script 04.

## Replicating the analysis
To replicate the analysis, follow these steps:
* Download and unzip the repository to any folder in your computer (other than the Downloads folder, which usually gets wiped automatically). The folder is identified automatically when the code is run.
* Download R and RStudio (see download links on [https://posit.co/download/rstudio-desktop/]). While R is sufficient to run the analysis, it is recommended to instead run the scripts from the RStudio interface.
* Open and run the entire `00_master_script.R` script (just press Alt+Ctrl+R). This will create an `\out` folder, to which output tables and graphs will be saved automatically. As this scripts calls all the others, it alone is sufficient to replicate the analysis. On a cheap laptop, it should take about max. 10-15 minutes to run the analysis.
