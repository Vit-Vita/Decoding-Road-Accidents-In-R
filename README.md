# Crash course: Decoding Road accidents in the USA

## Introduction
In this report, we conduct a detailed statistical analysis of car accidents in the USA for the year 2022, focusing on the duration, frequency, and severity of accidents. 
By examining various variables, we aim to identify key predictors for the time it takes to resolve an accident, the occurrence of accidents, and their severity. 
Our findings reveal significant trends and correlations that can inform more effective road safety regulations and interventions. 
This analysis provides crucial insights that could help reduce the economic costs borne by taxpayers and enhance overall road safety.
We look forward to answering the following questions in our analysis:

- What variables are associated with longer times to resolve a car accident?

- What variables are associated with the occurrence an accident?

- What variables are the strongest predictors for the severity of an accident? Can it be accurately predicted?

## Data

Two primary datasets serve as the cornerstone for our analysis:

1. A dataset containing accident records in the USA for 2022: https://www.kaggle.com/datasets/sobhanmoosavi/us-accidents.
The author provided a sampled data set for easier handling which we will be using here.

2. Our second dataset is from US Census Bureau with estimated populations per city: https://www.census.gov/data/tables/time-series/demo/popest/2020s-total-cities-and-towns.html. We included this data set because we would like to examine the population predictor in our models as well.

## Data Preparation

This repository does not contain the data transformation process as it is not the subject of our work.
It involved the following steps: 
- As a first step, we briefly examined the data set and filtered for the year 2022.
- We removed unnecessary variables that were also full of NA values
- We converted all units to the Imperial system of measurement to the metric system and renamed some columns
- We used stratified random sampling per US county to only keep 84% of the data and bring it below the size of N = 10^5
- We categorized the streets into street types to be able to use them for our models
- We calculated the duration of each accident, categorized the times of accidents into time intervals (morning, afternoon, etc.) and kept only the months of occurrence from the dates.
- We dropped all irrelevant variables
- We augmented our data frame with cities’ populations to take it into account during analysis
- We handled missing data by replacing them by specifically defined means
- We added the count of accidents per US state to be used in some models.

## Structure of the repository 
tree -L 1
