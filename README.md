
### OpioidPrescriptionIndiana
This repository contains the R Markdown for the demographic-based and the spatiotemporal analysis in Indiana Medicaid patients who received at least one opioid prescription between 2015 to 2019. 

### Prerequisites 
The following libraries must be installed. 

```
library(rafalib)
library(dplyr)
library(stringr)
library(ggplot2)
library(gsubfn)
library(proto)
library(tidyverse)
library(nortest)
library(data.table)
library(sf)
library(sfdep)
library(spdep)
library(tidyr)
library(sp)
library(tidycensus)
library(ggpubr)
library(rgeoda)
library(gridExtra)
library(grid)
library(cowplot)
library(gridGraphics)
library(tmap)

```
### Authorization 
Data used in this project is not available for public access. 

### How to use it
The first R markdown (Pairedt-test_AgeAndGender) contains the code for all demographic estimations and statistical analysis by demographic groups. 

The second R markdown (SpatiotemporalAnalysis) comprises the code to generate the necessary shapefiles and proportions of individuals receiving an opioid prescription by 3-digit ZIP code zones, and the steps to calculate the Global and Local's Moran I in order to identify the spatially clustered zones.   

### Publication 
This code supports the analysis intended to be published in a paper in the next couple of months.  


