Applied Machine Learning and predictive Modelling

Authors:
Perez Olusese -email - perez.olusese@stud.hslu.ch
Vitalia Vedenikova - email-vitalia.vedenikova@stud.hslu.ch

Title: Crash course: Decoding Road accidents in the USA

# Important note regarding the group members
This project was completed by two students as opposed to three as it is stipulated in the module rules. The third student left the Master of Science in 
Applied information and Data Science.

# Contents:

1. ReadMe File: Vedenikova_Olusese_README.txt
2. "Data" Folder :  
    a. CSV-file with US car accidents data (US_Accidents_March23_sampled_500.csv)
    b. CSV-file with USA city population data (SUB-IP-EST2022-POP.csv)
    c. CSV-file with the clean merged data (accident_22_clean_final.csv)
2. "RCode" Folder:
    a. R-code corresponding to the report (Vedenikova_Olusese_ML_Group_Work.R)
3. "Report" Folder: 
    a. Group report in HTML (Vedenikova_Olusese_ML_Group_Work_13332531.html)
    b. Cache and files associated to the HTML document
4. "RMarkdown" Folder:
    a. Report and code in an .Rmd file (Vedenikova_Olusese_ML_Group_Work_13332531.Rmd)

# Data description:
We used two datasets:
1. US_Accidents_March23_sampled_500.csv
2. SUB-IP-EST2022-POP.csv 

These two primary datasets serve as the basis for our analysis. The US_Accidents_March23_sampled_500.csv file contains data collected from 2016 to 2023, we shall however focus on 
data from the year 2022 only. There are multiple continuous variables (temperature, precipitation level, visible distance, etc.) as well as categorical variables with more than 
two levels (weather conditions, timezone, etc.) which we made sure to keep for this analysis.
SUB-IP-EST2022-POP.csv was used to examine the population predictor in our models as well.

We applied the following packages:
require(dplyr)
require(tidyr)
require(tinytex)
library(lubridate)
library(ggplot2)
library(gt)
library(reshape2)
library(knitr)
library(kableExtra)
library(tidyverse)
library(summarytools)
library(tseries)
library(forecast)
library(broom)
library(leaflet)
library(sf)
library(maps)
library(cowplot)
library(sjPlot)
library(hms)
library(readxl)
library(RColorBrewer)
library(mgcv)
library(ellipse)
library(conflicted)
library(caret)
library(neuralnet)
library(parallel)
library(doParallel)
library(foreach)
library(data.table)
library(caret)
library(e1071)
library(pryr)

Important steps to replicate the analysis:
- Set the working directory
- Read the respective data sets

# For the USA accidents & USA population data set data preparations:
1. As a first step, we briefly examined the data set and filtered for the year 2022. 
2. We removed unnecessary variables that were also full of NA values
3. We had to transform all units in the Imperial system of measurement to the metric system and rename some columns
4. We used stratified random sampling per US county to only keep 84% of the data and bring it below the size of N = 10^5 
5. We categorized the streets into street types to be able to use them for our models
6. We calculated the duration of each accident, categorized the times of accidents into time intervals (morning, afternoon, etc.) and kept only the months of occurrence from the dates. 
7. We dropped all irrelevant variables
8. We augmented our data frame with cities' populations to take it into account during analysis
9. We handled missing data by replacing them by specifically defined means
10. We added the count of accidents per US state to be used in some models.

After joining the data sets we proceed with the EDA analysis which is documented in the R file.

Run the code on the EDA to display the plots. The plots will reveal several insights into the nature of the variables.

# Building models
The models built were Linear, GAM, Poisson, Binomial, SVM and ANN

Run the code for the various models to be able to display the EDA and the model results. 
Cross validation was performed for the SVM and ANN models, this can be viewed as one is running the file.

# Acknowledgements

Our data and sources are from the following sites.
1. https://www.kaggle.com/datasets/sobhanmoosavi/us-accidents
2. https://www.census.gov/data/tables/time-series/demo/popest/2020s-total-cities-and-towns.html










