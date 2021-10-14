# hemlock_decline_SDM_project
An ecological palaeoecology project for undergraduate/ masters students which uses species distribution modelling to investigate the climatic drivers of the hemlock decline.

Contact alistair.seddon@uib.no for more information

Note that the project uses the renv() package to run analyses. If working from your own machine you can install renv() and then use renv::restore() to lone the packages that were used when the project was created. See https://rstudio.github.io/renv/ for more details.

# data
## CCSM 
Contains time slices of the CCSM3 model outputs, from Lorenz et al. 2016. (https://www.nature.com/articles/sdata201648). See the article in Scientific Data for more information. 
Data stored in individual 500 year time slices.

## NA_pollen_climate
Pollen-count data from Neotoma Palaeoecology Database downloaded in spring 2020 for eastern North America using a preliminary version of the HOPE (Humans on Planet Earth) data workflow developed by Ondrej Mottl at the University of Bergen. All variables names are from Neotoma database.
Age models are preliminary versions of BChron age models run within the HOPE project in spring 2020. *NB. These are not the final versions and should be used with caution"" 

# output
Directory to store outputs from script using functions created in function file

# script.R
Annotated workflow script for the exercise to investigate species distrution modelling.

# function_script.R
Needs commenting. Functions used to process data and map data in R script.