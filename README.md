
### Spatiotemporal analysis of opioid prescription in Indiana
This repository contains the R Markdown for the demographic-based and the spatiotemporal analysis in Indiana Medicaid patients who received at least one opioid prescription between 2015 to 2019. 

### Prerequisites 
The following libraries must be installed. 

```
library(dplyr)
library(data.table)
library(tableone)
library(chisq.posthoc.test)
library(tidycensus)
library(readr)
library(ggplot2)
library(tidyr)
library(sf)
library(sfdep)
library(spdep)
library(sp)
library(ggpubr)
library(rgeoda)
library(gridExtra)
library(grid)
library(cowplot)
library(gridGraphics)
library(tmap)
library(readxl)
library(purrr)
library(tseries)
library(spatialreg)
library(lmtest)


```
### Authorization 
Data used in this project is not available for public access. 

### How to use it
The first R markdown (StatisticalTests_AgeAndGender) contains the code for all demographic estimations and statistical analysis by demographic groups. 

The second R markdown (SpatiotemporalAnalysis) comprises the code to generate the necessary shapefiles and proportions of individuals receiving an opioid prescription by 3-digit ZIP code zones, and the steps to calculate the Global and Local's Moran I in order to identify the spatially clustered zones.   

The third R markdown (MAUP_impact) comprises the code to evaluate the MAUP at the county-level.    
### Publication 
This code supports the analysis of the paper "A spatiotemporal analysis of opioid prescriptions in Indiana from 2015 to 2019", DOI: 10.1186/s13011-025-00664-8
.  



