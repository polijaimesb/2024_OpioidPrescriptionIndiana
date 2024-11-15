---
title: "Spatiotemporal analysis - Indiana prescription"
author: "Paula Jaimes"
date: "2024-05-23"
output: html_document
---

This script contains all the necessary steps for the spatiotemporal analysis for the paper called "A spatiotemporal analysis of opioid prescriptions in Indiana from 2015 to 2019".

## Libraries

The following libraries are necessary to run the script.



## Functions

Necessary functions to extract information from the last prescription and to obtain the Indiana Medicaid population from American Community Survey (ACS).


``` r
SelectCensus<-function(Year){ 
  
  female_estimation<- get_acs(
    geography = "zcta",
    variables = "C27007_017",
    state = "IN", 
    survey = "acs5",
    year = as.numeric(Year)
  )
  
  female<-data.frame(NAME=female_estimation$NAME, estimate_female=female_estimation$estimate)
  
  male_estimation<- get_acs(
    geography = "zcta",
    variables = "C27007_007",
    state = "IN", 
    survey = "acs5",
    year = as.numeric(Year)
  )
  
  male<-data.frame(NAME=male_estimation$NAME, estimate_male=male_estimation$estimate)
  
  total_pop_Ind<- get_acs(
    geography = "zcta",
    variables =  "B01001_001", #"C27007_007",
    state = "IN", 
    survey = "acs5",
    geometry = TRUE,
    year = as.numeric(Year)
  )
  
  total_pop_Ind <- merge(total_pop_Ind,female, by="NAME")
  total_pop_Ind <- merge(total_pop_Ind,male, by="NAME")
  total_pop_Ind$estimate <- total_pop_Ind$estimate_male + total_pop_Ind$estimate_female
  
  return(total_pop_Ind)
}
```

## Dataset and necessary arrays

Initialize necessary arrays and vectors, and load the dataset. First step of the dataset is to include the patients between 19 to 64.




``` r
ext<-c("2015","2016","2017","2018","2019")
max_date_total<-c(732471,732837,733202,733567,733932)
min_date_total<-c(732107,732472,732838,733203,733568)


global_MoranI_wnormal<-vector("numeric", length(ext))
global_MoranI_wNormal_MC<-vector("numeric", length(ext))
global_MoranI_normal<-vector("numeric", length(ext))
global_MoranI_Normal_MC<-vector("numeric", length(ext))
global_GTest_Normal<-vector("numeric", length(ext))


Indiana_final_filter <- read.csv(path)
```

## Spatiotemporal analysis: Global and Local Moran's I

The following `for statement` contains:

-   The identification of the cohort for each year, where individuals part of the study must aged between 19 to 64 years.

-   Extraction of Indiana Medicaid estimations from the ACS and the necessary shapefiles by 5-digit ZIP codes.

-   Obtain the final prescription for each patient, to obtain the age, gender, and race for each year.

-   Aggregate the estimation per 5-digit ZIP code zone to 3-digit ZIP code areas.

-   Calculate the global and the local Moran's I.


```
## Getting data from the 2011-2015 5-year ACS
```

```
## Using FIPS code '18' for state 'IN'
```

```
## Getting data from the 2011-2015 5-year ACS
```

```
## Using FIPS code '18' for state 'IN'
```

```
## Getting data from the 2011-2015 5-year ACS
```

```
## Using FIPS code '18' for state 'IN'
```

```
## [1] "Cohorts"
## [1] 34889
```

```
## Adding missing grouping variables: `Recipient_Addr_Zip5`
```

```
## [1] "Most prescribed opioid - rate of prescription2015"
## [1] "#### Global Moran's I - Not normalized data ####"
## 
## 	Moran I test under randomisation
## 
## data:  merged_data$n  
## weights: lw    
## 
## Moran I statistic standard deviate = 1.1757, p-value = 0.1198
## alternative hypothesis: greater
## sample estimates:
## Moran I statistic       Expectation          Variance 
##        0.10538197       -0.05263158        0.01806203 
## 
## [1] "#### Global Moran's I MonteCarlo Simulation - Not normalized data ####"
## 
## 	Monte-Carlo simulation of Moran I
## 
## data:  merged_data$n 
## weights: lw  
## number of simulations + 1: 1e+05 
## 
## statistic = 0.10538, observed rank = 89263, p-value = 0.1074
## alternative hypothesis: greater
## 
## [1] "#### Global Moran's I - Normalized data ####"
## 
## 	Moran I test under randomisation
## 
## data:  merged_data$normal_count  
## weights: lw    
## 
## Moran I statistic standard deviate = 0.36955, p-value = 0.3559
## alternative hypothesis: greater
## sample estimates:
## Moran I statistic       Expectation          Variance 
##       0.009781576      -0.052631579       0.028523163 
## 
## [1] "#### Global Moran's I MonteCarlo Simulation - Normalized data ####"
## 
## 	Monte-Carlo simulation of Moran I
## 
## data:  merged_data$normal_count 
## weights: lw  
## number of simulations + 1: 1e+05 
## 
## statistic = 0.0097816, observed rank = 65635, p-value = 0.3437
## alternative hypothesis: greater
## 
## [1] "#### Global G test - Normalized data ####"
```

```
## Warning in globalG.test(merged_data$normal_count, lw): Binary weights recommended (especially for distance bands)
```

```
## 
## 	Getis-Ord global G statistic
## 
## data:  merged_data$normal_count 
## weights: lw   
## 
## standard deviate = 1.178, p-value = 0.1194
## alternative hypothesis: greater
## sample estimates:
## Global G statistic        Expectation           Variance 
##       5.515437e-02       5.263158e-02       4.586413e-06 
## 
## [1] "Significant areas -2015"
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)

```
## Getting data from the 2012-2016 5-year ACS
## Using FIPS code '18' for state 'IN'
```

```
## Getting data from the 2012-2016 5-year ACS
```

```
## Using FIPS code '18' for state 'IN'
```

```
## Getting data from the 2012-2016 5-year ACS
```

```
## Using FIPS code '18' for state 'IN'
```

```
## [1] "Cohorts"
## [1] 46571
```

```
## Adding missing grouping variables: `Recipient_Addr_Zip5`
```

```
## [1] "Most prescribed opioid - rate of prescription2016"
## [1] "#### Global Moran's I - Not normalized data ####"
## 
## 	Moran I test under randomisation
## 
## data:  merged_data$n  
## weights: lw    
## 
## Moran I statistic standard deviate = 0.88844, p-value = 0.1872
## alternative hypothesis: greater
## sample estimates:
## Moran I statistic       Expectation          Variance 
##        0.06652912       -0.05263158        0.01798894 
## 
## [1] "#### Global Moran's I MonteCarlo Simulation - Not normalized data ####"
## 
## 	Monte-Carlo simulation of Moran I
## 
## data:  merged_data$n 
## weights: lw  
## number of simulations + 1: 1e+05 
## 
## statistic = 0.066529, observed rank = 82562, p-value = 0.1744
## alternative hypothesis: greater
## 
## [1] "#### Global Moran's I - Normalized data ####"
## 
## 	Moran I test under randomisation
## 
## data:  merged_data$normal_count  
## weights: lw    
## 
## Moran I statistic standard deviate = 0.63299, p-value = 0.2634
## alternative hypothesis: greater
## sample estimates:
## Moran I statistic       Expectation          Variance 
##        0.05443102       -0.05263158        0.02860743 
## 
## [1] "#### Global Moran's I MonteCarlo Simulation - Normalized data ####"
## 
## 	Monte-Carlo simulation of Moran I
## 
## data:  merged_data$normal_count 
## weights: lw  
## number of simulations + 1: 1e+05 
## 
## statistic = 0.054431, observed rank = 74741, p-value = 0.2526
## alternative hypothesis: greater
## 
## [1] "#### Global G test - Normalized data ####"
```

```
## Warning in globalG.test(merged_data$normal_count, lw): Binary weights recommended (especially for distance bands)
```

```
## 
## 	Getis-Ord global G statistic
## 
## data:  merged_data$normal_count 
## weights: lw   
## 
## standard deviate = 1.4119, p-value = 0.07899
## alternative hypothesis: greater
## sample estimates:
## Global G statistic        Expectation           Variance 
##       5.567214e-02       5.263158e-02       4.637612e-06 
## 
## [1] "Significant areas -2016"
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-2.png)

```
## Getting data from the 2013-2017 5-year ACS
## Using FIPS code '18' for state 'IN'
```

```
## Getting data from the 2013-2017 5-year ACS
```

```
## Using FIPS code '18' for state 'IN'
```

```
## Getting data from the 2013-2017 5-year ACS
```

```
## Using FIPS code '18' for state 'IN'
```

```
## [1] "Cohorts"
## [1] 17134
```

```
## Adding missing grouping variables: `Recipient_Addr_Zip5`
```

```
## [1] "Most prescribed opioid - rate of prescription2017"
## [1] "#### Global Moran's I - Not normalized data ####"
## 
## 	Moran I test under randomisation
## 
## data:  merged_data$n  
## weights: lw    
## 
## Moran I statistic standard deviate = 1.6307, p-value = 0.05148
## alternative hypothesis: greater
## sample estimates:
## Moran I statistic       Expectation          Variance 
##        0.11777634       -0.05263158        0.01092056 
## 
## [1] "#### Global Moran's I MonteCarlo Simulation - Not normalized data ####"
## 
## 	Monte-Carlo simulation of Moran I
## 
## data:  merged_data$n 
## weights: lw  
## number of simulations + 1: 1e+05 
## 
## statistic = 0.11778, observed rank = 93202, p-value = 0.06798
## alternative hypothesis: greater
## 
## [1] "#### Global Moran's I - Normalized data ####"
## 
## 	Moran I test under randomisation
## 
## data:  merged_data$normal_count  
## weights: lw    
## 
## Moran I statistic standard deviate = 1.3094, p-value = 0.0952
## alternative hypothesis: greater
## sample estimates:
## Moran I statistic       Expectation          Variance 
##        0.17058217       -0.05263158        0.02905976 
## 
## [1] "#### Global Moran's I MonteCarlo Simulation - Normalized data ####"
## 
## 	Monte-Carlo simulation of Moran I
## 
## data:  merged_data$normal_count 
## weights: lw  
## number of simulations + 1: 1e+05 
## 
## statistic = 0.17058, observed rank = 89933, p-value = 0.1007
## alternative hypothesis: greater
## 
## [1] "#### Global G test - Normalized data ####"
```

```
## Warning in globalG.test(merged_data$normal_count, lw): Binary weights recommended (especially for distance bands)
```

```
## 
## 	Getis-Ord global G statistic
## 
## data:  merged_data$normal_count 
## weights: lw   
## 
## standard deviate = 1.6371, p-value = 0.05081
## alternative hypothesis: greater
## sample estimates:
## Global G statistic        Expectation           Variance 
##        0.058365598        0.052631579        0.000012268 
## 
## [1] "Significant areas -2017"
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-3.png)

```
## Getting data from the 2014-2018 5-year ACS
## Using FIPS code '18' for state 'IN'
```

```
## Getting data from the 2014-2018 5-year ACS
```

```
## Using FIPS code '18' for state 'IN'
```

```
## Getting data from the 2014-2018 5-year ACS
```

```
## Using FIPS code '18' for state 'IN'
```

```
## [1] "Cohorts"
## [1] 20274
```

```
## Adding missing grouping variables: `Recipient_Addr_Zip5`
```

```
## [1] "Most prescribed opioid - rate of prescription2018"
## [1] "#### Global Moran's I - Not normalized data ####"
## 
## 	Moran I test under randomisation
## 
## data:  merged_data$n  
## weights: lw    
## 
## Moran I statistic standard deviate = 1.4834, p-value = 0.06899
## alternative hypothesis: greater
## sample estimates:
## Moran I statistic       Expectation          Variance 
##        0.16092886       -0.05263158        0.02072670 
## 
## [1] "#### Global Moran's I MonteCarlo Simulation - Not normalized data ####"
## 
## 	Monte-Carlo simulation of Moran I
## 
## data:  merged_data$n 
## weights: lw  
## number of simulations + 1: 1e+05 
## 
## statistic = 0.16093, observed rank = 92750, p-value = 0.0725
## alternative hypothesis: greater
## 
## [1] "#### Global Moran's I - Normalized data ####"
## 
## 	Moran I test under randomisation
## 
## data:  merged_data$normal_count  
## weights: lw    
## 
## Moran I statistic standard deviate = 1.7988, p-value = 0.03603
## alternative hypothesis: greater
## sample estimates:
## Moran I statistic       Expectation          Variance 
##        0.25499876       -0.05263158        0.02924774 
## 
## [1] "#### Global Moran's I MonteCarlo Simulation - Normalized data ####"
## 
## 	Monte-Carlo simulation of Moran I
## 
## data:  merged_data$normal_count 
## weights: lw  
## number of simulations + 1: 1e+05 
## 
## statistic = 0.255, observed rank = 96194, p-value = 0.03806
## alternative hypothesis: greater
## 
## [1] "#### Global G test - Normalized data ####"
```

```
## Warning in globalG.test(merged_data$normal_count, lw): Binary weights recommended (especially for distance bands)
```

```
## 
## 	Getis-Ord global G statistic
## 
## data:  merged_data$normal_count 
## weights: lw   
## 
## standard deviate = 1.833, p-value = 0.0334
## alternative hypothesis: greater
## sample estimates:
## Global G statistic        Expectation           Variance 
##       5.804157e-02       5.263158e-02       8.711264e-06 
## 
## [1] "Significant areas -2018"
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-4.png)

```
## Getting data from the 2015-2019 5-year ACS
## Using FIPS code '18' for state 'IN'
```

```
## Getting data from the 2015-2019 5-year ACS
```

```
## Using FIPS code '18' for state 'IN'
```

```
## Getting data from the 2015-2019 5-year ACS
```

```
## Using FIPS code '18' for state 'IN'
```

```
## [1] "Cohorts"
## [1] 13441
```

```
## Adding missing grouping variables: `Recipient_Addr_Zip5`
```

```
## [1] "Most prescribed opioid - rate of prescription2019"
## [1] "#### Global Moran's I - Not normalized data ####"
## 
## 	Moran I test under randomisation
## 
## data:  merged_data$n  
## weights: lw    
## 
## Moran I statistic standard deviate = 1.6475, p-value = 0.04972
## alternative hypothesis: greater
## sample estimates:
## Moran I statistic       Expectation          Variance 
##        0.17997967       -0.05263158        0.01993374 
## 
## [1] "#### Global Moran's I MonteCarlo Simulation - Not normalized data ####"
## 
## 	Monte-Carlo simulation of Moran I
## 
## data:  merged_data$n 
## weights: lw  
## number of simulations + 1: 1e+05 
## 
## statistic = 0.17998, observed rank = 93269, p-value = 0.06731
## alternative hypothesis: greater
## 
## [1] "#### Global Moran's I - Normalized data ####"
## 
## 	Moran I test under randomisation
## 
## data:  merged_data$normal_count  
## weights: lw    
## 
## Moran I statistic standard deviate = 1.4629, p-value = 0.07175
## alternative hypothesis: greater
## sample estimates:
## Moran I statistic       Expectation          Variance 
##        0.17675231       -0.05263158        0.02458772 
## 
## [1] "#### Global Moran's I MonteCarlo Simulation - Normalized data ####"
## 
## 	Monte-Carlo simulation of Moran I
## 
## data:  merged_data$normal_count 
## weights: lw  
## number of simulations + 1: 1e+05 
## 
## statistic = 0.17675, observed rank = 92611, p-value = 0.07389
## alternative hypothesis: greater
## 
## [1] "#### Global G test - Normalized data ####"
```

```
## Warning in globalG.test(merged_data$normal_count, lw): Binary weights recommended (especially for distance bands)
```

```
## 
## 	Getis-Ord global G statistic
## 
## data:  merged_data$normal_count 
## weights: lw   
## 
## standard deviate = 1.7945, p-value = 0.03637
## alternative hypothesis: greater
## sample estimates:
## Global G statistic        Expectation           Variance 
##       5.936237e-02       5.263158e-02       1.406899e-05 
## 
## [1] "Significant areas -2019"
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-5.png)![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-6.png)
Values for the Global Moran's I. 


``` r
global_table <- matrix(global_MoranI_Normal_MC, ncol = 5, nrow = 1)
colnames(global_table) <-as.character(c(2015:2019))

print(global_table)
```

```
##             2015       2016      2017      2018      2019
## [1,] 0.009781576 0.05443102 0.1705822 0.2549988 0.1767523
```


## Plots of Local Moran's I by year 



Maximum of each rate of prescription by 3-digit ZIP code


General rate of prescription by each 3-digit ZIP code. 


