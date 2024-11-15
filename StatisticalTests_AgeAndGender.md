---
title: "Paired t-test - Gender and Age"
author: "Paula Jaimes"
date: "2024-05-23"
output: html_document
---

The script contains all the necessary steps for the statistical tests between demographic group estimations from Indiana Medicaid reimbursement claims data from individuals receiving an opioid prescription, from 2015 to 2019. This script is part of the methodology for the paper called "A spatiotemporal analysis of opioid prescriptions in Indiana from 2015 to 2019".

## Libraries

The following libraries are necessary to run the script.



## Processing of the dataset



First, load the data and start to delete repeated claims. 


``` r
Indiana_df <-  read_csv(path, col_types = cols(.default = "c"))

#Eliminating duplicates
Indiana_df <- Indiana_df[!duplicated(Indiana_df),]
 
print(paste0("Patients after eliminating duplicated rows: ", length(unique(Indiana_df$DE_Identified_Recipient_ID))))
```

```
## [1] "Patients after eliminating duplicated rows: 156916"
```

Then, eliminate NA values. 



``` r
print(paste0("Rows with NA values: ", sum(is.na(Indiana_df))))
```

```
## [1] "Rows with NA values: 845"
```

``` r
Indiana_df <- na.omit(Indiana_df)
```

``` r
print(paste0("After deleting NA values: ", length(unique(Indiana_df$DE_Identified_Recipient_ID))))
```

```
## [1] "After deleting NA values: 156503"
```
Some patients appear with 2 gender. We are going to delete those. Also, the patients with unknown gender. 


``` r
#### Delete the patients with more than 2 gender 
Patients_Gender <-Indiana_df %>% 
  group_by(DE_Identified_Recipient_ID,Recipient_Gender) %>% 
  count %>% 
  ungroup %>% 
  group_by(DE_Identified_Recipient_ID) %>% 
  count %>%
  ungroup() %>% 
  filter(n > 1) %>% 
  select(DE_Identified_Recipient_ID) %>% 
  unlist 

Indiana_df <- Indiana_df %>% 
  filter(! DE_Identified_Recipient_ID %in% Patients_Gender & 
           Recipient_Gender != "U")

print(paste0("Patients reported with more than 1 gender: ", length(Patients_Gender)))
```

```
## [1] "Patients reported with more than 1 gender: 28"
```

``` r
print(paste0("Patients after gender filtering: ", length(unique(Indiana_df$DE_Identified_Recipient_ID))))
```

```
## [1] "Patients after gender filtering: 156473"
```
Now, we deleted invalid entries of race. 


``` r
invalid_races <- Indiana_df %>% 
  group_by(Recipient_Race) %>% 
  count

print(invalid_races)
```

``` r
Indiana_df <- Indiana_df %>% 
  filter(!Recipient_Race %in% c("7", "8"))

print(paste0("Patients after race filtering: ", length(unique(Indiana_df$DE_Identified_Recipient_ID))))
```

```
## [1] "Patients after race filtering: 153852"
```
Now, we only want to keep patients aged 18 to 64. 


``` r
Indiana_df$Date_Birth_Recipient <- as.numeric(Indiana_df$Date_Birth_Recipient)

Indiana_df<-Indiana_df %>% 
  filter(Date_Birth_Recipient >= 18 & Date_Birth_Recipient <= 64)

print(paste0("Patients after age filtering: ", length(unique(Indiana_df$DE_Identified_Recipient_ID))))
```

```
## [1] "Patients after age filtering: 126615"
```
Filtering also patients who resided outside Indiana. 

``` r
Indiana_df$Recipient_Addr_Zip5 <- as.numeric(Indiana_df$Recipient_Addr_Zip5)

Indiana_df <- Indiana_df %>% 
  filter(Recipient_Addr_Zip5 <= 479 & Recipient_Addr_Zip5 >= 460) 

print(paste0("Patients after residence filtering: ", length(unique(Indiana_df$DE_Identified_Recipient_ID))))
```

```
## [1] "Patients after residence filtering: 126247"
```
Finally, patients with records between 2015 to 2019. 

``` r
Indiana_df$Date_Begin_Service_Header <- as.numeric(Indiana_df$Date_Begin_Service_Header)

Indiana_df <- Indiana_df %>% 
  filter(Date_Begin_Service_Header >= 732107 & Date_Begin_Service_Header <= 733932)

print(paste0("Patients with a record between 2015 to 2019: ", length(unique(Indiana_df$DE_Identified_Recipient_ID))))
```

```
## [1] "Patients with a record between 2015 to 2019: 107574"
```
Change the NDC number by their respective drug name. 




``` r
Allndc_opioids <- as.numeric(unlist(opioidNDC$`NDC 9 digit code`))
OpioidNames <- unlist(opioidNDC$`Drug Name`)
ndc_df <- unlist(unique(Indiana_df$NDC_Code))
non_ndc  <- c()

for(i in 1:length(ndc_df)){
  is_ndc = FALSE
  for(j in 1:length(Allndc_opioids)){
    if(grepl(Allndc_opioids[j], ndc_df[i])){
      is_ndc = TRUE
      Indiana_df$NDC_Code <- replace(Indiana_df$NDC_Code,Indiana_df$NDC_Code == ndc_df[i], OpioidNames[j])
      break
    }
  }
  if(is_ndc == FALSE){
    non_ndc <- c(non_ndc, ndc_df[i])
  }
}

Indiana_df <- Indiana_df %>% 
  filter(!NDC_Code %in% non_ndc)

print(paste0("Patients after filtering just the ones who received opioid prescription: ", length(unique(Indiana_df$DE_Identified_Recipient_ID))))
```

```
## [1] "Patients after filtering just the ones who received opioid prescription: 107574"
```

## Prepare the data

First, we obtained the corresponding date according with the sequence of numbers.


``` r
######## Convert number in respective dates ############

ConvertNumber2Date <- function(lower, upper, year){

  start_date <- paste0(year,"-01-01")
  origin_date <- as.Date(start_date) - (lower - 1) 

  # Generate the sequence of dates from lower limit to upper limit
  date_sequence <- seq.Date(from = origin_date + lower - 1, to = origin_date + upper - 1, by = "day")

  number2date <- data.frame(Date_Begin_Service_Header = lower:upper, 
                            AllDate = date_sequence)
  
  return(number2date)
}

year_list <-c(2015,2016,2017,2018,2019)
upper_date<-c(732471,732837,733202,733567,733932)
lower_date<-c(732107,732472,732838,733203,733568)


ConvertNumber2Date_df <- bind_rows(lapply(1:length(year_list), function(x) ConvertNumber2Date(lower_date[x],
                                                                               upper_date[x], 
                                                                               year_list[x])))
indiana_df_filter <- Indiana_df %>% 
  left_join(ConvertNumber2Date_df, 
            by = "Date_Begin_Service_Header")


######## Categorized dates in years , and respective letter in race ############


indiana_df_filter <- indiana_df_filter %>% 
  mutate(
    year = case_when(
      Date_Begin_Service_Header >= 732107 & Date_Begin_Service_Header <=  732471 ~ 2015,
      Date_Begin_Service_Header >= 732472 & Date_Begin_Service_Header <=  732837 ~ 2016,
      Date_Begin_Service_Header >= 732838 & Date_Begin_Service_Header <=  733202 ~ 2017,
      Date_Begin_Service_Header >= 733203 & Date_Begin_Service_Header <=  733567 ~ 2018,
      Date_Begin_Service_Header >= 733568 & Date_Begin_Service_Header <=  733932 ~ 2019,
      TRUE ~ 0
    ), 
    race = case_when( 
      Recipient_Race == "H" ~ "Hispanic", 
      Recipient_Race == "B" ~ "Black",
      Recipient_Race == "C" ~ "White", 
      TRUE ~ "Other"
      ), 
    age = case_when(
      Date_Birth_Recipient >= 18 & Date_Birth_Recipient <= 25 ~ "18 to 25",
      Date_Birth_Recipient >= 26 & Date_Birth_Recipient <= 34 ~ "26 to 34",
      Date_Birth_Recipient >= 35 & Date_Birth_Recipient <= 44 ~ "35 to 44",
      Date_Birth_Recipient >= 45 & Date_Birth_Recipient <= 54 ~ "45 to 54",
      Date_Birth_Recipient >= 55 & Date_Birth_Recipient <= 64 ~ "55 to 64",
      TRUE ~ "Unknown"
    )
  )
```





``` r
######## Extract last entry for each year by patient ############
indiana_df_LastPrescription <- indiana_df_filter %>% 
  arrange(desc(AllDate), DE_Identified_Recipient_ID) %>% 
  group_by(DE_Identified_Recipient_ID, year) %>% 
  slice(1) %>% 
  ungroup()

print(paste0("Patients receiving an opioid prescription: ", length(unique(indiana_df_LastPrescription$DE_Identified_Recipient_ID))))
```

```
## [1] "Patients receiving an opioid prescription: 107574"
```




Then, we identify the urban/rural communities utilizing Rural-Urban Commuting Area Codes (RUCA) using the 3-digit ZIP code zones from Indiana.

The first plan consisted in convert RUCA which are in census tracts, to ZIP codes, and then to 3-digit ZIP codes, using the relationship files from US Census Bureau <https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.2010.html#par_textimage_674173622>.

However, RUCA webpage already did it. Citing: "A second dataset applies 2010 RUCA classifications to ZIP code areas by transferring RUCA values from the census tracts that comprise them."

So, we are going to use the classification of RUCA using the ZIP code areas.




``` r
RUCA_ZipCodes <- readxl::read_excel(path, 
                                    col_types = "text", 
                                    sheet = "Data")
```

RUCA has two columns depending of the primary and secondary RUCA codes. Primary represents the whole number, eg., if you have 4.1, the whole number would be the 4. The primary is the principal classification of that census tract or, in this case, the ZIP code. But secondary are also provided, citing:

"These 10 codes offer a relatively straightforward and complete delineation of metropolitan and nonmetropolitan areas based on the size and direction of primary commuting flows. However, secondary flows may indicate other connections among rural and urban places. Thus, the primary RUCA codes are further subdivided to identify areas where classifications overlap, based on the size and direction of the secondary, or second largest, commuting flow (table 2). For example, 1.1 and 2.1 codes identify areas where the primary flow is within or to a metropolitan core, but another 30 percent or more commute to a larger metropolitan core. Similarly, 10.1, 10.2, and 10.3 identify rural tracts for which the primary commuting share is local, but more than 30 percent also commute to a nearby metropolitan, micropolitan, or small town core, respectively."

In this case, the RUCA1 is the primary and the RUCA2 is the primary+secondary. We used RUCA1. For more documentation, visit the webpage <https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes/documentation/>

Now, because we are going to work with Indiana, filter just the zip codes for that zone.


``` r
RUCA_ZipCodes_IN <- RUCA_ZipCodes %>% 
  filter(STATE == "IN")

#Create same column in RUCA IN
RUCA_ZipCodes_IN$FirstThreeDigits <- substr(RUCA_ZipCodes_IN$ZIP_CODE, 1,3)
```

However, if you observed, for example, the 3-digit ZIP code 460, you realized that there were some of them with a RUCA value ranging from 1-3 (metropolitan) and other to 4-10, which it is known as rural.

**How can we summarize the RUCA for multiple 5-digit ZIP codes?**

A possible idea is to get a weighted average considering the population for each 5-digit ZIP code.

$$ 
RUCA_{3-digit} = /frac{/sum RUCA_{5-digit} * Population_{5-digit}}{/sum Population_{5-digit}}
$$

And this is for each 3-digit ZIP code zone. First, eliminate all the post office locations, where they are located P.O. boxes, and also, exclude the 99 RUCA (which is the zones where "Census tract has zero population and no rural-urban identifier information").


``` r
RUCA_ZipCodes_IN <- RUCA_ZipCodes_IN %>% 
  filter(ZIP_TYPE == "Zip Code Area" & 
           RUCA1 != 99)
```


For the weighted average, we must be careful about manipulating the decimals. Because the secondary numbers are pre-defined, we can:/
/
**Review RUCA Code Definitions:**

-   RUCA code 2: Metropolitan area high commuting (primary flow 30% or more to a UA).

-   RUCA code 3: Metropolitan area low commuting (primary flow 10% to 30% to a UA).

1.  **Rounding to Nearest Primary RUCA Code:**

    -   Since the RUCA classification system uses whole numbers and, in some cases, a single decimal place to indicate secondary commuting patterns, you need to round the weighted average to the nearest allowable RUCA code.

2.  **Apply Rounding Logic:**

    -   RUCA code 2.73 should be rounded to the nearest whole number or primary classification. In this case, the nearest whole number is 3.

Because we are more interested in the whole numbers, i.e., the primary classification, we are going to round up the number obtained in the weighted average.

Now, after creating the weighted average of the RUCA, we can create a group depending of the RUCA obtained such as:

-   RUCA 10: Isolated

-   RUCA 7-9: Small Town

-   RUCA 4-6 Micropolitan

-   RUCA 1-3: Metropolitan

Or we can also use the other definition, where isolated, small town and micropolitan are called "rural" and the rest, "urban".

This process has to be made by each year, in order to accurately represent the total population of each year. Is this necessary?


``` r
RUCA_Aggregated_calculation <- function(year_select, RUCA_IN){
  
  #Extract the estimation by year for the 5-digit ZIP codes 
  total_pop_Ind<- get_acs(
      geography = "zcta",
      variables =  "B01001_001", #"C27007_007",
      state = "IN", 
      survey = "acs5",
      geometry = TRUE,
      year = year_select
    )
  
  total_pop_Ind$FirstThreeDigits <- substr(total_pop_Ind$GEOID, 1, 3)
  
  #Aggregate by first 3-digits 
  population_counts <- total_pop_Ind %>%
      group_by(FirstThreeDigits) %>%
      summarize(total=sum(estimate))
  
  
  total_pop_Ind_df <- total_pop_Ind %>% 
    mutate(ZIP_CODE = GEOID) %>% 
    select(-GEOID) 

  RUCA_ZipCodes_IN <- RUCA_ZipCodes_IN %>% 
    left_join(select(total_pop_Ind_df, ZIP_CODE, estimate))
  
  #Calculate weighted average
  RUCA_ZipCodes_IN_wa<- RUCA_ZipCodes_IN %>% 
    filter(!is.na(estimate)) %>%
    group_by(FirstThreeDigits) %>%
    summarise(RUCA_3Digit = sum(as.numeric(RUCA1) * estimate)/sum(estimate)) %>%
    ungroup()

  RUCA_ZipCodes_IN_wa_shp <- population_counts %>%
    left_join(RUCA_ZipCodes_IN_wa, by = "FirstThreeDigits")
  
  #Round up and classify 
  RUCA_ZipCodes_IN_wa_shp <- RUCA_ZipCodes_IN_wa_shp %>%
    mutate(RUCA_3Digit_round = round(RUCA_3Digit))
  
    RUCA_ZipCodes_IN_year <- RUCA_ZipCodes_IN_wa_shp %>% 
      mutate(group = ifelse(RUCA_3Digit_round >= 4 & 
                            RUCA_3Digit_round <= 10, 
                            "Rural", "Urban"), 
             year = year_select)
    
  return(RUCA_ZipCodes_IN_year)
}
```

After generating the urban/rural classification, now we can join it with the our study cohort.


```
## Getting data from the 2011-2015 5-year ACS
```

```
## Downloading feature geometry from the Census website.  To cache shapefiles for use in future sessions, set `options(tigris_use_cache = TRUE)`.
```

```
## Using FIPS code '18' for state 'IN'
```

```
## Joining with `by = join_by(ZIP_CODE)`
## Getting data from the 2012-2016 5-year ACS
## Downloading feature geometry from the Census website.  To cache shapefiles for use in future sessions, set `options(tigris_use_cache
## = TRUE)`.
## Using FIPS code '18' for state 'IN'
## Joining with `by = join_by(ZIP_CODE)`
## Getting data from the 2013-2017 5-year ACS
## Downloading feature geometry from the Census website.  To cache shapefiles for use in future sessions, set `options(tigris_use_cache
## = TRUE)`.
## Using FIPS code '18' for state 'IN'
## Joining with `by = join_by(ZIP_CODE)`
## Getting data from the 2014-2018 5-year ACS
## Downloading feature geometry from the Census website.  To cache shapefiles for use in future sessions, set `options(tigris_use_cache
## = TRUE)`.
## Using FIPS code '18' for state 'IN'
## Joining with `by = join_by(ZIP_CODE)`
## Getting data from the 2015-2019 5-year ACS
## Downloading feature geometry from the Census website.  To cache shapefiles for use in future sessions, set `options(tigris_use_cache
## = TRUE)`.
## Using FIPS code '18' for state 'IN'
## Joining with `by = join_by(ZIP_CODE)`
```





# Descriptive statistics

After that, create table with estimations.


``` r
######## Estimations by year ############

vars <- c("Recipient_Gender", "race", "age", "group")
strata <- "year"
indiana_Df_estimations <- CreateTableOne(vars = vars, 
                                         strata = strata, 
                                         data = indiana_df_LastPrescription, 
                                         test = TRUE)

print(indiana_Df_estimations, pDigits = 3, showAllLevels = TRUE, quote = TRUE, formatOptions = list(big.mark = ","))
```

```
##                         "Stratified by year"
##  ""                      "level"    "2015"           "2016"           "2017"           "2018"           "2019"           "p"     
##   "n"                    ""         "34,889"         "46,571"         "17,134"         "20,274"         "13,441"         ""      
##   "Recipient_Gender (%)" "F"        " 24214 (69.4) " " 31465 (67.6) " " 11628 (67.9) " " 13719 (67.7) " "  8985 (66.8) " "<0.001"
##   ""                     "M"        " 10675 (30.6) " " 15106 (32.4) " "  5506 (32.1) " "  6555 (32.3) " "  4456 (33.2) " ""      
##   "race (%)"             "Black"    "  6475 (18.6) " "  8199 (17.6) " "  3209 (18.7) " "  3808 (18.8) " "  2189 (16.3) " "<0.001"
##   ""                     "Hispanic" "  1367 ( 3.9) " "  2073 ( 4.5) " "   718 ( 4.2) " "   863 ( 4.3) " "   525 ( 3.9) " ""      
##   ""                     "Other"    "   216 ( 0.6) " "   356 ( 0.8) " "   375 ( 2.2) " "   342 ( 1.7) " "   250 ( 1.9) " ""      
##   ""                     "White"    " 26831 (76.9) " " 35943 (77.2) " " 12832 (74.9) " " 15261 (75.3) " " 10477 (77.9) " ""      
##   "age (%)"              "18 to 25" "  4146 (11.9) " "  5632 (12.1) " "  2110 (12.3) " "  2136 (10.5) " "  1239 ( 9.2) " "<0.001"
##   ""                     "26 to 34" "  6736 (19.3) " "  9463 (20.3) " "  4101 (23.9) " "  4379 (21.6) " "  2773 (20.6) " ""      
##   ""                     "35 to 44" "  7857 (22.5) " " 10387 (22.3) " "  4080 (23.8) " "  4873 (24.0) " "  3179 (23.7) " ""      
##   ""                     "45 to 54" "  8812 (25.3) " " 11527 (24.8) " "  3898 (22.8) " "  4798 (23.7) " "  3312 (24.6) " ""      
##   ""                     "55 to 64" "  7338 (21.0) " "  9562 (20.5) " "  2945 (17.2) " "  4088 (20.2) " "  2938 (21.9) " ""      
##   "group (%)"            "Rural"    "  6080 (17.4) " "  8785 (18.9) " "  2650 (15.5) " "  3236 (16.0) " "  2236 (16.6) " "<0.001"
##   ""                     "Urban"    " 28809 (82.6) " " 37786 (81.1) " " 14484 (84.5) " " 17038 (84.0) " " 11205 (83.4) " ""      
##                         "Stratified by year"
##  ""                      "test"
##   "n"                    ""    
##   "Recipient_Gender (%)" ""    
##   ""                     ""    
##   "race (%)"             ""    
##   ""                     ""    
##   ""                     ""    
##   ""                     ""    
##   "age (%)"              ""    
##   ""                     ""    
##   ""                     ""    
##   ""                     ""    
##   ""                     ""    
##   "group (%)"            ""    
##   ""                     ""
```
Chi-square by row. 

``` r
year_count <- c(34889, 46571, 17134, 20274, 13441)
ExtractTable <- function(year, variable, table_df){
  estimations <- table_df[["CatTable"]][[as.character(year)]][[variable]][,5]
  return(estimations)
}


gender_estimations <- unlist(lapply(2015:2019, 
                                      ExtractTable,  
                                      variable = "Recipient_Gender", 
                                      table_df = indiana_Df_estimations))

gender_estimations_mx_female <- matrix(gender_estimations,nrow=2,ncol=5)
colnames(gender_estimations_mx_female) <- 2015:2019
rownames(gender_estimations_mx_female) <- c("F", "NoF")

gender_estimations_mx_female["NoF",] <- year_count - gender_estimations_mx_female["F",] 
                               

chi_square_gender <- chisq.test(gender_estimations_mx_female)
```

For ages. 

``` r
age_estimations <- unlist(lapply(2015:2019, 
                                      ExtractTable,  
                                      variable = "age", 
                                      table_df = indiana_Df_estimations))

age_estimations_mx <- matrix(age_estimations,nrow=5,ncol=5)
colnames(age_estimations_mx) <- 2015:2019
rownames(age_estimations_mx) <- c("18 to 25", 
                                    "26 to 34",
                                    "35 to 44", 
                                    "45 to 54", 
                                    "55 to 64")
#18 to 25
age_estimations_mx_18to25 <- age_estimations_mx[c("18 to 25", "26 to 34"), ]
rownames(age_estimations_mx_18to25) <- c("18 to 25", "Other")

age_estimations_mx_18to25["Other",] <- year_count - age_estimations_mx_18to25["18 to 25",] 
                               
chi_square_18to25 <- chisq.test(age_estimations_mx_18to25)

#26 to 34
age_estimations_mx_26to34 <- age_estimations_mx[c("26 to 34", "35 to 44"), ]
rownames(age_estimations_mx_26to34) <- c("26 to 34", "Other")

age_estimations_mx_26to34["Other",] <- year_count - age_estimations_mx_26to34["26 to 34",] 
                               
chi_square_26to34<-chisq.test(age_estimations_mx_26to34)

#35 to 44
age_estimations_mx_35to44 <- age_estimations_mx[c("35 to 44", "45 to 54"), ]
rownames(age_estimations_mx_35to44) <- c("35 to 44", "Other")

age_estimations_mx_35to44["Other",] <- year_count - age_estimations_mx_35to44["35 to 44",] 
                               
chi_square_35to44<-chisq.test(age_estimations_mx_35to44)

#45 to 54
age_estimations_mx_45to54 <- age_estimations_mx[c("45 to 54", "55 to 64"), ]
rownames(age_estimations_mx_45to54) <- c("45 to 54", "Other")

age_estimations_mx_45to54["Other",] <- year_count - age_estimations_mx_45to54["45 to 54",] 
                               
chi_square_45to54<-chisq.test(age_estimations_mx_45to54)

#55 to 64
age_estimations_mx_55to64 <- age_estimations_mx[c("55 to 64", "45 to 54"), ]
rownames(age_estimations_mx_55to64) <- c("55 to 64", "Other")

age_estimations_mx_55to64["Other",] <- year_count - age_estimations_mx_55to64["55 to 64",] 
                               
chi_square_55to64<-chisq.test(age_estimations_mx_55to64)
```
Race. 


``` r
race_estimations <- unlist(lapply(2015:2019, 
                                      ExtractTable,  
                                      variable = "race", 
                                      table_df = indiana_Df_estimations))

race_estimations_mx <- matrix(race_estimations,nrow=4,ncol=5)
colnames(race_estimations_mx) <- 2015:2019
rownames(race_estimations_mx) <- c("Black", 
                                     "Hispanic",
                                     "Other", 
                                     "White")

#White 
race_estimations_mx_White <- race_estimations_mx[c("White", "Black"), ]
rownames(race_estimations_mx_White) <- c("White", "Other")

race_estimations_mx_White["Other",] <- year_count - race_estimations_mx_White["White",] 
                               
chi_square_White<-chisq.test(race_estimations_mx_White)

#Black 
race_estimations_mx_black <- race_estimations_mx[c("Black", "White"), ]
rownames(race_estimations_mx_black) <- c("Black", "Other")

race_estimations_mx_black["Other",] <- year_count - race_estimations_mx_black["Black",] 
                               
chi_square_black<-chisq.test(race_estimations_mx_black)

#Hispanic 
race_estimations_mx_hispanic <- race_estimations_mx[c("Hispanic", "White"), ]
rownames(race_estimations_mx_hispanic) <- c("Hispanic", "Other")

race_estimations_mx_hispanic["Other",] <- year_count - race_estimations_mx_hispanic["Hispanic",] 
                               
chi_square_hispanic<-chisq.test(race_estimations_mx_hispanic)

#Other 
race_estimations_mx_other <- race_estimations_mx[c("Other", "White"), ]
rownames(race_estimations_mx_other) <- c("OtherRaces", "Other")

race_estimations_mx_other["Other",] <- year_count - race_estimations_mx_other["OtherRaces",] 
                               
chi_square_other<-chisq.test(race_estimations_mx_other)
```
# Line chart for the proportion of urban and rural 


``` r
urbanVsRural <- unlist(lapply(as.character(2015:2019), function(x) indiana_Df_estimations[["CatTable"]][[x]][["group"]][["percent"]]))

urbanVsRural <- as.data.frame(matrix(urbanVsRural, nrow = 2, ncol = 5)) %>% 
  mutate(group = c("Rural", "Urban"))

colnames(urbanVsRural) <- c(as.character(2015:2019), "group")

urbanVsRural <- urbanVsRural %>% 
  pivot_longer(
    cols = c(as.character(2015:2019)), 
    names_to = "year", 
    values_to = "Byyear"
  )

urbanVsRural %>% 
  ggplot(aes(x = year, y = Byyear, group = group, color = group)) + 
  geom_line(color = "grey", lwd = 2) + 
  geom_point(aes(fill = group), shape = 21, color = "black", size = 5) +
  scale_fill_manual(values = c("Rural" = "#8babf1", "Urban" = "#029356")) + 
  theme_bw(base_size = 22) + 
  theme(
   panel.grid.major = element_blank(),
   # explicitly set the horizontal lines (or they will disappear too)
   panel.grid.minor = element_blank(),
   legend.position = "top"
  ) +
  labs(title = "",
       x = "Year", 
       y = "Proportion of patients (%)", 
       size = "", 
       fill = "") 
```

![plot of chunk unnamed-chunk-79](figure/unnamed-chunk-79-1.png)




#Post hoc chi-squared analysis 
According with the Chi-square test, all the demographics are significantly different across years. To find the most significant group, we are going to do a post-hoc test.


``` r
######## Extractions of frequencies for post-hoc by gender ############

ExtractTable <- function(year, variable, table_df){
  estimations <- table_df[["CatTable"]][[as.character(year)]][[variable]][,5]
  return(estimations)
}


gender_Estimatations <- unlist(lapply(2015:2019, 
                                      ExtractTable,  
                                      variable = "Recipient_Gender", 
                                      table_df = indiana_Df_estimations))

gender_Estimatations_mx <- matrix(gender_Estimatations,nrow=2,ncol=5)
colnames(gender_Estimatations_mx) <- 2015:2019
rownames(gender_Estimatations_mx) <- c("F", "M")
                               
gender_posthoc_year <- chisq.posthoc.test(gender_Estimatations_mx, method = "bonferroni")
```





``` r
######## Extractions of frequencies for post-hoc by age ############


age_Estimatations <- unlist(lapply(2015:2019, 
                                      ExtractTable,  
                                      variable = "age", 
                                      table_df = indiana_Df_estimations))

age_Estimatations_mx <- matrix(age_Estimatations,nrow=5,ncol=5)
colnames(age_Estimatations_mx) <- 2015:2019
rownames(age_Estimatations_mx) <- c("18 to 25", 
                                    "26 to 34",
                                    "35 to 44", 
                                    "45 to 54", 
                                    "55 to 64")
                               
# chisq.posthoc.test(age_Estimatations_mx, method = "bonferroni")
age_posthoc_df <- chisq.posthoc.test(age_Estimatations_mx, method = "bonferroni")
```





``` r
######## Extractions of frequencies for post-hoc by race ############


race_Estimatations <- unlist(lapply(2015:2019, 
                                      ExtractTable,  
                                      variable = "race", 
                                      table_df = indiana_Df_estimations))

race_Estimations_mx <- matrix(race_Estimatations,nrow=4,ncol=5)
colnames(race_Estimations_mx) <- 2015:2019
rownames(race_Estimations_mx) <- c("Black", 
                                     "Hispanic",
                                     "Other", 
                                     "White")
                               
race_posthoc_df <- chisq.posthoc.test(race_Estimations_mx, method = "bonferroni")
```


``` r
######## Extractions of frequencies for post-hoc by urban/rural ############


urbanRural_Estimatations <- unlist(lapply(2015:2019, 
                                      ExtractTable,  
                                      variable = "group", 
                                      table_df = indiana_Df_estimations))

urbanRural_Estimatations_mx <- matrix(urbanRural_Estimatations,nrow=2,ncol=5)
colnames(urbanRural_Estimatations_mx) <- 2015:2019
rownames(urbanRural_Estimatations_mx) <- c("Rural", 
                                     "Urban")
                               
ruralUrban_posthoc_df <- chisq.posthoc.test(urbanRural_Estimatations_mx, method = "bonferroni")
```

Overall statistics. 


``` r
gender_overall <- indiana_df_filter %>% 
  select(DE_Identified_Recipient_ID, Recipient_Gender) %>% 
  unique 

sum(gender_overall$Recipient_Gender == "F")
```

```
## [1] 72181
```

``` r
sum(gender_overall$Recipient_Gender == "M")
```

```
## [1] 35393
```

``` r
sum(gender_overall$Recipient_Gender == "F")/(nrow(gender_overall))
```

```
## [1] 0.6709893
```

``` r
sum(gender_overall$Recipient_Gender == "M")/(nrow(gender_overall))
```

```
## [1] 0.3290107
```


``` r
race_overall <- indiana_df_filter %>% 
  select(DE_Identified_Recipient_ID, year, race) %>% 
  arrange(desc(year)) %>% 
  group_by(DE_Identified_Recipient_ID) %>% 
  slice(1)

sum(race_overall$race == "White")
```

```
## [1] 82073
```

``` r
sum(race_overall$race == "Black")
```

```
## [1] 19311
```

``` r
sum(race_overall$race == "Hispanic")
```

```
## [1] 4768
```

``` r
sum(race_overall$race == "Other")
```

```
## [1] 1422
```

``` r
sum(race_overall$race == "White")/nrow(race_overall)
```

```
## [1] 0.7629446
```

``` r
sum(race_overall$race == "Black")/nrow(race_overall)
```

```
## [1] 0.1795136
```

``` r
sum(race_overall$race == "Hispanic")/nrow(race_overall)
```

```
## [1] 0.04432298
```

``` r
sum(race_overall$race == "Other")/nrow(race_overall)
```

```
## [1] 0.01321881
```
Classification of rural/urban by each year. 

![plot of chunk unnamed-chunk-89](figure/unnamed-chunk-89-1.png)![plot of chunk unnamed-chunk-89](figure/unnamed-chunk-89-2.png)![plot of chunk unnamed-chunk-89](figure/unnamed-chunk-89-3.png)![plot of chunk unnamed-chunk-89](figure/unnamed-chunk-89-4.png)

```
## Warning in layer_sf(data = data, mapping = mapping, stat = stat, geom = GeomLabel, : Ignoring unknown parameters: `check_overlap`
```

```
## Warning in st_point_on_surface.sfc(sf::st_zm(x)): st_point_on_surface may not give correct results for longitude/latitude data
```

![plot of chunk unnamed-chunk-89](figure/unnamed-chunk-89-5.png)

# Supplementary table: 3-digit ZIP code, 5 -digit ZIP code contained and county. 
To determine the 5-digit ZIP codes aggregated by 3-digit ZIP code, and also the county related to that 3-digit ZIP code, we are going to use the relationship files from ZCTA to county, using also the land area info. First, identify all the 5-digit ZIP codes aggregated by 3-digit ZIP codes.


```
## Getting data from the 2015-2019 5-year ACS
```

```
## Using FIPS code '18' for state 'IN'
```

Now, we are going to load the relationship file. 


``` r
ZCTA2County <- read.delim("C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/CSV files/tab20_zcta520_county20_natl.txt", 
                          sep = "|", 
                          colClasses = c("GEOID_ZCTA5_20" = "character"))
```

Extract the ZIP codes from Indiana. 


``` r
IN_ZCTA2County <- ZCTA2County %>% 
  filter(GEOID_ZCTA5_20 %in% unique(ZipCode_info$GEOID))

IN_ZCTA2County$FirstThreeDigits <- substr(IN_ZCTA2County$GEOID_ZCTA5_20, 1, 3) 
```

To know how many counties are repeated in multiple 3-digit ZIP codes. 


``` r
Counties_IN_ZIP3 <- IN_ZCTA2County %>% 
  select(FirstThreeDigits, NAMELSAD_COUNTY_20) %>% 
  unique %>% 
  group_by(NAMELSAD_COUNTY_20) %>% 
  count %>% 
  ungroup()
```

To classify the county in a 3-digit ZIP code it is necessary to aggregate the land area of all the 5-digit ZIP codes by 3-digit ZIP code, and the one that has the highest land area, is the assigned 3-digit ZIP code to that county. 


``` r
#Create the dataframe for the counties who just have one 3-digit ZIP code 
ZIP32County_One <- Counties_IN_ZIP3 %>% 
  filter( n == 1) %>% 
  select(NAMELSAD_COUNTY_20) %>% 
  unlist 

ZIP32County_One_df <- IN_ZCTA2County %>% 
  filter(NAMELSAD_COUNTY_20 %in% ZIP32County_One) %>% 
  select(NAMELSAD_COUNTY_20, FirstThreeDigits) %>% 
  unique 

#Identify the counties which have more than 1 3-digit ZIP code assigned. 
Counties_IN_ZIP3_MoreThan1 <- Counties_IN_ZIP3 %>% 
  filter(n > 1)

CountiMaxLandArea_ZIP3 <- bind_rows(lapply(unique(Counties_IN_ZIP3_MoreThan1$NAMELSAD_COUNTY_20), function(x){
  IN_ZCTA2County_eachCounty <- IN_ZCTA2County %>% 
    filter(NAMELSAD_COUNTY_20 == x) 
  
  County_TotalArea <- IN_ZCTA2County_eachCounty %>% 
    select(AREALAND_COUNTY_20) %>% 
    unique %>% 
    unlist 
  
  IN_ZCTA2County_eachCounty <- IN_ZCTA2County_eachCounty %>% 
    group_by(FirstThreeDigits) %>% 
    summarise(land_area_aggregated = sum(AREALAND_PART)) %>% 
    ungroup() %>% 
    mutate(
      land_area_aggregated_prop = land_area_aggregated/County_TotalArea
    ) %>% 
    filter(land_area_aggregated_prop == max(land_area_aggregated_prop)) 
  

  CountAndZip3 <- IN_ZCTA2County %>% 
    select(FirstThreeDigits, NAMELSAD_COUNTY_20) %>% 
    unique %>% 
    filter(NAMELSAD_COUNTY_20 == x & 
             FirstThreeDigits == IN_ZCTA2County_eachCounty$FirstThreeDigits)
  
  return(CountAndZip3)

}))
```

There are some 3-digit ZIP codes which does not have an assigned county, because the only county they have is sharing bigger land area to their neighboring 3-digit ZIP codes. Those cases are 464, 466, and 468. For those 3-digit ZIP codes that only have one county assigned, just leave it and not assigned it to other 3-digit ZIP codes.  To do that, identify the 3-digit  ZIP code with just one or two county assigned, and keep the one with the highest proportion. 


``` r
#Join the two dataframes
ZIP3_to_Counties <- bind_rows(ZIP32County_One_df, CountiMaxLandArea_ZIP3)


NoZIP3_county <- ZipCode_info_2 %>% 
  filter(!FirstThreeDigits %in% ZIP3_to_Counties$FirstThreeDigits) %>% 
  select(FirstThreeDigits) %>% 
  unique %>% 
  unlist 
  
  
Proporiton_ByCounty <- bind_rows(lapply(unique(Counties_IN_ZIP3_MoreThan1$NAMELSAD_COUNTY_20), function(x){
  IN_ZCTA2County_eachCounty <- IN_ZCTA2County %>% 
    filter(NAMELSAD_COUNTY_20 == x) 
  
  County_TotalArea <- IN_ZCTA2County_eachCounty %>% 
    select(AREALAND_COUNTY_20) %>% 
    unique %>% 
    unlist 
  
  IN_ZCTA2County_eachCounty <- IN_ZCTA2County_eachCounty %>% 
    group_by(FirstThreeDigits) %>% 
    mutate(land_area_aggregated = sum(AREALAND_PART)) %>% 
    ungroup() %>% 
    mutate(
      land_area_aggregated_prop = land_area_aggregated/County_TotalArea
    ) %>% 
    select(FirstThreeDigits, NAMELSAD_COUNTY_20, land_area_aggregated_prop) %>% 
    unique
  
  return(IN_ZCTA2County_eachCounty)

})) 


#Keep the highest proportion 
ZIP3_highProp <- Proporiton_ByCounty %>% 
  filter(FirstThreeDigits %in% NoZIP3_county) %>% 
  arrange(FirstThreeDigits, desc(land_area_aggregated_prop)) %>% 
  group_by(FirstThreeDigits) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(NAMELSAD_COUNTY_20, FirstThreeDigits)

#Eliminate the counties for the other ZIP code
ZIP3_to_Counties <- ZIP3_to_Counties %>% 
  filter(!NAMELSAD_COUNTY_20 %in% ZIP3_highProp$NAMELSAD_COUNTY_20) %>% 
  bind_rows(ZIP3_highProp)

ZIP3_to_Counties_df <- ZIP3_to_Counties %>% 
  select(FirstThreeDigits) %>% 
  unique 

#Join the counties in just one column 
ZIP3_to_Counties_df$countyGroup <- unlist(lapply(unlist(unique(ZIP3_to_Counties$FirstThreeDigits)), function(x){
  
  allCounties <- ZIP3_to_Counties$NAMELSAD_COUNTY_20[ZIP3_to_Counties$FirstThreeDigits == x]
  
  paste0(sort(allCounties), collapse = ",")
  
}))
```

Now, join with the other dataframe that contains the 5-digit ZIP codes. 


``` r
Suppl_Table_ZIP3_5_county <- ZipCode_info_2 %>% 
  left_join(ZIP3_to_Counties_df, by = "FirstThreeDigits")
```

Finally, join the info of the rural/urban classification from 2020. 


``` r
Suppl_Table_ZIP3_5_county_RUCA <- RUCA_Class_3DigitZIP_IN %>% 
  filter(year == 2019) %>% 
  select(Recipient_Addr_Zip5, group) %>% 
  unique %>% 
  sf::st_drop_geometry() %>% 
  mutate(FirstThreeDigits = as.character(Recipient_Addr_Zip5)) %>% 
  select(-Recipient_Addr_Zip5) %>% 
  left_join(Suppl_Table_ZIP3_5_county, by = "FirstThreeDigits") %>% 
  select(-ZIP_code_group)
```






