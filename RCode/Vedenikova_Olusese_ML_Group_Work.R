# Set the working directory
setwd("C:/Users/Olusese/Documents/PAO MSCIDS/Machine Learning 1/ML_PROJECT")

# Load Libraries
require(dplyr)
require(tidyr)
require(tinytex)
library(lubridate)
library(ggplot2)
library(ggcorrplot)
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

############ 1. Load and read the data set. Only keep the data for the year 2022 ############

# Load and read the data
accident_raw <- read_csv("US_Accidents_March23_sampled_500k.csv")
str (accident_raw)
head (accident_raw)
colnames (accident_raw)

# create new variable year
accident_raw$year <-  (str_sub(accident_raw$Start_Time, 1, 4))
str (accident_raw)
unique(accident_raw$year)
class(accident_raw$year)

# filter only accidents in the year 2022
accident_2022 <- accident_raw %>% dplyr::filter(year == 2022)
str (accident_2022)
summary(accident_2022)
unique(accident_2022$year)

############ 2. Removing Unnecessary variables ############

names (accident_2022)
names(accident_2022) <- tolower(names(accident_2022)) 

head(accident_2022)
str (accident_2022)
summary(accident_2022)
# tibble [113,734 × 48]

# Looking for variables which are NAs
colSums(is.na(accident_2022))
max(colSums(is.na(accident_2022))) 

accident_2022$precipitation.in. %in% c(NA, "NA")
sum(accident_2022$temperature.f. %in% c(NA, "NA"))

# end_lat, end_lng indicate the position where the accident ended. We consider them not relevant for our analysis and they have the max amount of missing values at 15384 each = these variables are eliminated
# until the variable weather_timestamp all values are available, from weather down seems to have some missing values 

accident_2022_clean_01 <- subset(accident_2022, select = -c(end_lat, end_lng))
# new tibble [113,734 × 46]

############ 3. Fixing variable names and converting units to the metric system ############

#Fixing names
names(accident_2022_clean_01)

accident_2022_clean_02 <- rename(accident_2022_clean_01,c(
  "lat_deg" = "start_lat",
  "lng_deg" = "start_lng", 
  "dist_mi"="distance(mi)",
  "descript" = "description",
  "temp_f"  = "temperature(f)",
  "wind_chill_f" = "wind_chill(f)",
  "humid" = "humidity(%)",
  "press_in" = "pressure(in)",
  "visib_mi" = "visibility(mi)",
  "wind_speed_mph" = "wind_speed(mph)",
  "presip_in" = "precipitation(in)",
  "weather_cond" = "weather_condition",
  "nautic_twilight" = "nautical_twilight",   
  "astron_twilight" = "astronomical_twilight"))

names(accident_2022_clean_02)

#Converting to the metric system
accident_2022_clean_03 <- accident_2022_clean_02 %>%
  mutate(dist_km = dist_mi * 1.6093,
         temp_c = (temp_f - 32) * 5 / 9,
         wind_chill_c = (wind_chill_f - 32) * 5 / 9,
         press_hpa = press_in * 1.33322,
         visib_km = visib_mi * 1.6093,
         wind_speed_kph = wind_speed_mph * 1.6093,
         presip_mm = presip_in * 25.4)


#now removing the variables which are not in metric units and we drop all the 'twilight' variables since we already have the 'sunrise_sunset' variable telling us if it is day time or night time:
accident_2022_clean_03 <- accident_2022_clean_03 %>% 
  select(-ends_with('_mi')) %>%    # drop any column with a name that ends with '_mi' for 'miles'
  select(-ends_with('_f')) %>%     # drop any column with a name that ends with '_f' for 'farenheit'
  select(-ends_with('_in')) %>%    # drop any column with a name that ends with '_in' for 'inches'
  select(-ends_with('_mph')) %>%    # drop any column with a name that ends with '_mph' for 'miles per hour'
  select(-(civil_twilight:astron_twilight)) # drop 'twilight' variables

names(accident_2022_clean_03)
print (accident_2022_clean_03$presip_mm)
print (accident_2022_clean_03$press_in)
print (accident_2022_clean_03$press_hpa)
summary (accident_2022_clean_03$press_hpa)


############ 4. Reducing the size of the data frame with stratified sampling ############

names(accident_2022_clean_03)
colSums(is.na(accident_2022_clean_03))

# In order to keep the amount of data points below 10^5, we decided to reduce the amount of entries. The data set had 3 sources which were pulling information from the same places (US and state departments of transportation), we decided to randomly sample each county.

unique(accident_2022_clean_03$source)
summary (accident_2022_clean_03)  # 113734 entries, 13734 too many

accident_2022_clean_04 <- accident_2022_clean_03%>%
  group_by(county) %>%
  sample_frac(size=.84) %>%
  ungroup()

summary (accident_2022_clean_04) # now 95'619 with 84% of data sampled by county. 

accident_2022_clean_03 %>% 
  group_by(state,county) %>%
  do(data.frame(nrow=nrow(.)))

accident_2022_clean_04 %>% 
  group_by(state,county) %>%
  do(data.frame(nrow=nrow(.)))


############ 5. Creating a categorical variable for the streets ############

#We categorize the streets and make 'street' a categorical variable on this basis:

accident_2022_clean_05 <- accident_2022_clean_04 %>%
  mutate(street = case_when(
    grepl("\\bFwy\\b|\\bFWY\\b", street, ignore.case = TRUE) ~ "Freeway",
    grepl("\\bRd\\b|\\bRD\\b|\\bRoad\\b|\\bFL\\b|\\bFM\\b|\\bPl\\b|\\bPike\\b|\\bBdg\\b", street, ignore.case = TRUE) ~ "Road",
    grepl("\\bAve\\b|\\bAv\\b", street, ignore.case = TRUE) ~ "Avenue",
    grepl("\\bDr\\b|\\bdr\\b", street, ignore.case = TRUE) ~ "Drive",
    grepl("\\bPkwy\\b", street, ignore.case = TRUE) ~ "Parkway",
    grepl("\\bHwy\\b|\\bwy\\b", street, ignore.case = TRUE) ~ "Highway",
    grepl("\\bBlvd\\b|\\bblvd\\b", street, ignore.case = TRUE) ~ "Boulevard",
    grepl("\\bst\\b|\\bST\\b", street, ignore.case = TRUE) ~ "Street",
    grepl("\\bLn\\b|\\bln\\b", street, ignore.case = TRUE) ~ "Lane",
    grepl("\\bBrg\\b|\\bbrg\\b", street, ignore.case = TRUE) ~ "Bridge",
    grepl("\\bTpke\\b", street, ignore.case = TRUE) ~ "Turnpike",
    grepl("\\bRoute\\b|\\bEl\\ Camino\\b|\\bI-10\\b|\\bUS\\ 66\\b|\\bIL\\b|\\bIN\\b|\\bIA\\b|\\bUS\\b|\\bUT\\b", street, ignore.case = TRUE) ~ "Route",
    grepl("\\bTunl\\b|\\bTunnel\\b", street, ignore.case = TRUE) ~ "Tunnel",
    grepl("\\bTlwy\\b|\\bToll\\b", street, ignore.case = TRUE) ~ "Tollway",
    grepl("\\bWI\\b|\\bTX\\b", street, ignore.case = TRUE) ~ "State Highway",
    grepl("\\bBltwy\\b", street, ignore.case = TRUE) ~ "Beltway",
    grepl("\\bVA\\b|\\bW2100\\b|\\bW3300\\b|\\bExpy\\b", street, ignore.case = TRUE) ~ "Expressway",
    TRUE ~ "Others" # For any unmatched cases
  ))


############ 6. Handling the time variables ############

# We will add columns Hour and Month of the accident as well as time duration of the accident to our data set and remove "start_time" and "end_time" of the accidents:

accident_2022_clean_05$start_time<- ymd_hms(accident_2022_clean_05$start_time)
accident_2022_clean_05$end_time <- ymd_hms(accident_2022_clean_05$end_time)

accident_2022_clean_05 <- accident_2022_clean_05 %>% 
  mutate(duration = as.numeric(difftime(end_time, start_time, units = "mins")))

accident_2022_clean_05$hour_accident <- as_hms(ymd_hms(accident_2022_clean_05$start_time))
accident_2022_clean_05$hour_accident <- hour(accident_2022_clean_05$hour_accident)

accident_2022_clean_05$date <- as.Date(accident_2022_clean_05$start_time)
accident_2022_clean_05$month <- lubridate::month(ymd(accident_2022_clean_05$date))

accident_2022_clean_05 <- accident_2022_clean_05 %>% 
  select (-c(start_time))%>% 
  select (-c(end_time)) %>% 
  select (-c(date))

# We replace time with a categorical variable with levels by intervals of 6 hours: early morning (0h-6h), morning (6h-12h), afternoon (12h-18h), night(18h-0h). 

accident_2022_clean_05$time_interval <- case_when(
  accident_2022_clean_05$hour_accident >= 0 & accident_2022_clean_05$hour_accident < 6 ~ "Early morning",
  accident_2022_clean_05$hour_accident >= 6 & accident_2022_clean_05$hour_accident < 12 ~ "Morning",
  accident_2022_clean_05$hour_accident >= 12 & accident_2022_clean_05$hour_accident < 18 ~ "Afternoon",
  TRUE ~ "Night"
)

accident_2022_clean_06 <- accident_2022_clean_05 %>% 
  select (-c(hour_accident))



############ 7. Removing unnecessary variables ############

# We remove year, id, country, airport code, source of data, distance on which the accident took place, description of accidents, weather timestamp, lat_deg, ln_deg, zipcode. They are not relevant explanatory variables considering the focus of our work. 

drop.col <- c("id", "source", "country", "airport_code","year", "descript", "weather_timestamp", "dist_km", "lat_deg", "lng_deg", "zipcode", "timezone")
accident_2022_clean_06 <- accident_2022_clean_06 %>% 
  select (-one_of(drop.col))

#Here below, by looking at all the weather variables in a scatter plot, we can clearly see that there is a strong correlation between temperature and wind chill. Wind chill should be removed 
#(note : we commented out the code since it takes some time to run). 

#pairs( temp_c ~ wind_chill_c + presip_mm + humid + visib_km + wind_speed_kph + month, data = accident_2022_clean_06, upper.panel = panel.smooth)

# Some variables are removed in order to simplify our data set and future models. Since we already have the weather condition and the temperature, we decide to remove humidity, wind direction, press_hpa, wind_speed_kph as well as wind chill. 

drop.col2 <- c("humid", "wind_direction", "press_hpa", "wind_chill_c", "wind_speed_kph")
accident_2022_clean_06 <- accident_2022_clean_06 %>% 
  select (-one_of(drop.col2)) 

#The below boxplot also shows us that the visibility on the road is concentrated around the value of 16km, which is very good visibility. Also, visibility remains constant across the severity of the accidents as can be seen in the ggplot. We deduce that visibility is not correlated to car accidents and remove it from our data set. 

boxplot(accident_2022_clean_06$visib_km, main="Visibility in Km")

ggplot(data = accident_2022_clean_06, mapping = aes(y = visib_km, x = severity)) + geom_point() + geom_boxplot() + facet_wrap(.~ severity)

accident_2022_clean_06 <- accident_2022_clean_06 %>% 
  select (-visib_km) 

# We notice by looking at the boolean variables we have in our data set that there are quite a few that are 99%-100% false, therefore making them not interesting to model across states or road types. We decide to remove them for this reason.  
summary(accident_2022_clean_06$traffic_calming) # ~0.1% are TRUE
summary(accident_2022_clean_06$roundabout)      # ~0% are TRUE 
summary(accident_2022_clean_06$railway)         # ~0.1% are TRUE
summary(accident_2022_clean_06$turning_loop)    #  0% are TRUE 
summary(accident_2022_clean_06$give_way)        # ~0% are TRUE 
summary(accident_2022_clean_06$bump)            # ~0% are TRUE
summary(accident_2022_clean_06$no_exit)         # ~0% are TRUE

drop.col3 <- c("traffic_calming", "roundabout", "railway", "turning_loop", "give_way", "bump", "no_exit")

accident_2022_clean_07 <- accident_2022_clean_06 %>% 
  select (-one_of(drop.col3)) 

############ 8. Adding the population to our data set  ############
d.population <- read_excel("SUB-IP-EST2022-POP.xlsx")
names(d.population) <- d.population[2,]
d.population <- d.population[-(1:2),]
names(d.population)[4] <- "2021"
names(d.population)[5] <- "2022"
d.population <- d.population[-c(2:4)]
d.population <- d.population[-1,]
d.population[c('city', 'state')] <- str_split_fixed(d.population$`Geographic Area`, ',', 2)
d.population <- d.population[c('state', 'city', '2022')]

d.population <- d.population %>%
  mutate(state = str_replace(state,'Alabama', 'AL'))%>%
  mutate(state = str_replace(state,'Alaska', 'AK'))%>%
  mutate(state = str_replace(state,'Arkansas', 'AR'))%>%
  mutate(state = str_replace(state,'Arizona', 'AZ'))%>%
  mutate(state = str_replace(state,'California', 'CA'))%>%
  mutate(state = str_replace(state,'Colorado', 'CO'))%>%
  mutate(state = str_replace(state,'Connecticut', 'CT'))%>%
  mutate(state = str_replace(state,'Delaware', 'DE'))%>%
  mutate(state = str_replace(state,'District of Columbia', 'DC'))%>%
  mutate(state = str_replace(state,'Florida', 'FL'))%>%
  mutate(state = str_replace(state,'Georgia', 'GA'))%>%
  mutate(state = str_replace(state,'Hawaii', 'HI'))%>%
  mutate(state = str_replace(state,'Idaho', 'ID'))%>%
  mutate(state = str_replace(state,'Illinois', 'IL'))%>%
  mutate(state = str_replace(state,'Indiana', 'IN'))%>%
  mutate(state = str_replace(state,'Iowa', 'IA'))%>%
  mutate(state = str_replace(state,'Kansas', 'KS'))%>%
  mutate(state = str_replace(state,'Kentucky', 'KY'))%>%
  mutate(state = str_replace(state,'Louisiana', 'LA'))%>%
  mutate(state = str_replace(state,'Maine', 'ME'))%>%
  mutate(state = str_replace(state,'Maryland', 'MD'))%>%
  mutate(state = str_replace(state,'Massachusetts', 'MA'))%>%
  mutate(state = str_replace(state,'Michigan', 'MI'))%>%
  mutate(state = str_replace(state,'Minnesota', 'MN'))%>%
  mutate(state = str_replace(state,'Mississippi', 'MS'))%>%
  mutate(state = str_replace(state,'Missouri', 'MO'))%>%
  mutate(state = str_replace(state,'Montana', 'MT'))%>%
  mutate(state = str_replace(state,'Nebraska', 'NE'))%>%
  mutate(state = str_replace(state,'Nevada', 'NV'))%>%
  mutate(state = str_replace(state,'New Hampshire', 'NH'))%>%
  mutate(state = str_replace(state,'New Jersey', 'NJ'))%>%
  mutate(state = str_replace(state,'New Mexico', 'NM'))%>%
  mutate(state = str_replace(state,'New York', 'NY'))%>%
  mutate(state = str_replace(state,'North Carolina', 'NC'))%>%
  mutate(state = str_replace(state,'North Dakota', 'ND'))%>%
  mutate(state = str_replace(state,'Ohio', 'OH'))%>%
  mutate(state = str_replace(state,'Oklahoma', 'OK'))%>%
  mutate(state = str_replace(state,'Oregon', 'OR'))%>%
  mutate(state = str_replace(state,'Pennsylvania', 'PA'))%>%
  mutate(state = str_replace(state,'Rhode Island', 'RI'))%>%
  mutate(state = str_replace(state,'South Carolina', 'SC'))%>%
  mutate(state = str_replace(state,'South Dakota', 'SD'))%>%
  mutate(state = str_replace(state,'Tennessee', 'TN'))%>%
  mutate(state = str_replace(state,'Texas', 'TX'))%>%
  mutate(state = str_replace(state,'Utah', 'UT'))%>%
  mutate(state = str_replace(state,'Vermont', 'VT'))%>%
  mutate(state = str_replace(state,'Virginia', 'VA'))%>%
  mutate(state = str_replace(state,'Washington', 'WA'))%>%
  mutate(state = str_replace(state,'West VA', 'WV'))%>%
  mutate(state = str_replace(state,'Wisconsin', 'WI'))%>%
  mutate(state = str_replace(state,'Wyoming', 'WY'))

##rename the 3rd column to population
colnames(d.population)[3] = 'cities_population'

##to insure a proper merge we keep only the city names in the population data frame:
d.population <- d.population %>%
  mutate(city = str_replace(city,'city', ''))%>%
  mutate(city = str_replace(city,'town', ''))

##change the type of the 3rd column to character so the data types of key columns match for the merge
d.population <- d.population %>%
  mutate(cities_population = trimws(as.character(cities_population)))
d.population <- as.data.frame(d.population)
accident_2022_clean_08 <- as.data.frame(accident_2022_clean_07 )

##remove unnecessary rows
d.population <- d.population[-(19494:19498),]

##Make sure that there are no white spaces in the columns we will use as keys for the merge
d.population <- d.population%>%
  mutate(cities_population = trimws(cities_population))%>%
  mutate(city= trimws(city))%>%
  mutate(state= trimws(state))

accident_2022_clean_08  <- accident_2022_clean_08 %>%
  mutate(city= trimws(city))%>%
  mutate(state= trimws(state))

## We augment our accidents data set with the population information by making a left join:
accident_2022_clean_08 <- left_join(accident_2022_clean_08 , d.population, by = c("state","city"))
head(accident_2022_clean_08)

############ 9. Handle missing values ############

#first, we make sure that there is NA in every empty row: 
accident_2022_clean_09 <- accident_2022_clean_08 
accident_2022_clean_09[accident_2022_clean_09 == ''] <- NA

#We inspect which variables have missing values and see that there are missing values in : cities_population, presip_mm, temp_c, weather_cond, sunrise_sunset, city
colSums(is.na(accident_2022_clean_09))

#city: we take the most prevalent city for the relevant county and insert it in the empty rows. We also insert the relevant city's population. 

occurences_per_city <- accident_2022_clean_09 %>%
  group_by(county, city)%>%
  summarize(n=n())%>%
  ungroup()

most_prevalent_city <- occurences_per_city %>%
  group_by(county)%>%
  slice(which.max(n))%>%
  ungroup()

colnames(most_prevalent_city)[2] = 'prevalent_city'

accident_2022_clean_09 <- left_join(accident_2022_clean_09 , most_prevalent_city, by = "county")

for (x in 1:nrow(accident_2022_clean_09)){
  if (is.na(accident_2022_clean_09$city[x])){
    accident_2022_clean_09$city[x] <- accident_2022_clean_09$prevalent_city[x]
    accident_2022_clean_09$cities_population[x]<-accident_2022_clean_09$cities_population[accident_2022_clean_09$city==accident_2022_clean_09$city[x]]
  }
}

accident_2022_clean_09 <- accident_2022_clean_09 %>% 
  select (-prevalent_city)%>%
  select (-n) 

#cities_population: 25% of cities' populations were not in the census survey. We remedy this by taking the mean of each county's cities' population and replacing it into the rows with NA for population. 

accident_2022_clean_09$cities_population <- as.numeric(accident_2022_clean_09$cities_population)

accident_2022_clean_09 <- accident_2022_clean_09 %>%
  group_by(state, county) %>%
  mutate(cities_population = ifelse(is.na(cities_population), mean(cities_population, na.rm = TRUE),cities_population)) %>%
  ungroup()

#We take the population average on the county level otherwise for the 5520 still missing values
accident_2022_clean_09 <- accident_2022_clean_09 %>%
  group_by(state) %>%
  mutate(cities_population = ifelse(is.na(cities_population), mean(cities_population, na.rm = TRUE),cities_population)) %>%
  ungroup()


#We also see a lot of missing values for the temperature. We replace it with the mean temperature for the county in the relevant month when the accident took place. 

accident_2022_clean_09 <- accident_2022_clean_09 %>%
  group_by(state, county, month) %>%
  mutate(temp_c = ifelse(is.na(temp_c), mean(temp_c, na.rm = TRUE), temp_c)) %>%
  ungroup()

#if there are still temperatures missing, then we take the mean temperature at state level: 

accident_2022_clean_09 <- accident_2022_clean_09 %>%
  group_by(state, month) %>%
  mutate(temp_c = ifelse(is.na(temp_c), mean(temp_c, na.rm = TRUE), temp_c)) %>%
  ungroup()

#for this last missing temperature, we take the mean of the state
mean_temp_ME <- accident_2022_clean_09 %>%
  dplyr::filter(state == "ME") %>%
  summarize(mean_temperature = mean(temp_c, na.rm = TRUE))

accident_2022_clean_09$temp_c[84007] <- mean_temp_ME

#precipitation (mm): Most of the values are around 0, no matter the severity of the accident as shown by the bar graph and the boxplots below. We simply replace the NA with 0 and since it has a lot of outliers higher than 0, we keep it for now.
ggplot(accident_2022_clean_09, aes(x = presip_mm)) +
  geom_bar()

ggplot(accident_2022_clean_09, aes(x=severity, y=log(presip_mm), fill=severity)) + 
  geom_boxplot() +
  facet_wrap(~severity)

accident_2022_clean_09["presip_mm"][is.na(accident_2022_clean_09["presip_mm"])]<-0

#sunrise_sunset: we define sunrise or sunset based on the time interval

day_or_night <- function(time_interval) {
  if (time_interval == "Morning"|time_interval == "Afternoon") {
    return("Day")
  } else {
    return("Night")
  }
}

accident_2022_clean_09$sunrise_sunset <- ifelse(is.na(accident_2022_clean_09$sunrise_sunset), sapply(accident_2022_clean_09$time_interval, day_or_night), accident_2022_clean_09$sunrise_sunset)


#weather_cond : we take the most prevalent weather condition in each city

highest_fequency_weather_city <- accident_2022_clean_09 %>%
  group_by(state, county, city, weather_cond) %>%
  summarize(n=n())%>%
  slice(which.max(n))%>%
  ungroup()

colnames(highest_fequency_weather_city)[4] = 'most_common_weather_city'

accident_2022_clean_09 <- left_join(accident_2022_clean_09 , highest_fequency_weather_city, by = c("state","county", "city"))

for (x in 1:nrow(accident_2022_clean_09)){
  if (is.na(accident_2022_clean_09$weather_cond[x])){
    accident_2022_clean_09$weather_cond[x] <- accident_2022_clean_09$most_common_weather_city[x]
  }
}

accident_2022_clean_09 <- accident_2022_clean_09 %>% 
  select (-most_common_weather_city)%>%
  select (-n) 

#if we are still missing weather conditions, we take the ones of the state
highest_fequency_weather_state <- accident_2022_clean_09 %>%
  group_by(state, weather_cond) %>%
  summarize(n=n())%>%
  slice(which.max(n))%>%
  ungroup()

colnames(highest_fequency_weather_state)[2] = 'most_common_weather_state'

accident_2022_clean_09 <- left_join(accident_2022_clean_09 , highest_fequency_weather_state, by = "state")

for (x in 1:nrow(accident_2022_clean_09)){
  if (is.na(accident_2022_clean_09$weather_cond[x])){
    accident_2022_clean_09$weather_cond[x] <- accident_2022_clean_09$most_common_weather_state[x]
  }
}

accident_2022_clean_09 <- accident_2022_clean_09 %>% 
  select (-most_common_weather_state)%>%
  select (-n) 

print(accident_2022_clean_09[accident_2022_clean_09$state =="SD",])
accident_2022_clean_09$weather_cond[27426] <- "Fair" #we replace the missing values by "fair" since the dates of accidents at this location all match and the weather was recorded as fair for one of them. 
accident_2022_clean_09$weather_cond[27427] <- "Fair" 

colSums(is.na(accident_2022_clean_09))#we handled all the missing values that could be detected. 

############ 10. Create the variable "accidents_count_per_state ############

#We count the number of accidents per each US state and create a new column.
accident_2022_clean_09$state <- as.factor(accident_2022_clean_09$state)
state_counts <- as.data.frame(table(accident_2022_clean_09$state))
names(state_counts) <- c("state", "accidents_count_per_state")
accident_2022_clean_09 <- merge(accident_2022_clean_09, state_counts, by = "state", all.x = TRUE)

accident_2022_clean_10 <- apply(accident_2022_clean_09,2,as.character)

###############################################
## This is how we saved the combined datasets
#write.csv (accident_2022_clean_10, "../data/accident_22_clean_final.csv", row.names = TRUE)
#write.csv(accident_2022_clean_10, "D:/ML1_Project/data/accident_22_clean_final.csv", row.names = T)

#d.accidents <- read.csv("../data/accident_22_clean_final.csv", row.names = NULL)
#d.accidents <- read.csv("D:/ML1_Project/data/accident_22_clean_final.csv")

###############################################

###############################################
# BUILDING MACHINE LEARNING MODELS WITH THE TRANSFORMED DATA
################################################

# load the data and read it
d.accidents <- read_csv("accident_22_clean_final.csv")
head(d.accidents)

########## Linear Model#####################
### Vitalia Vedenikova took the lead on the Linear Model section

### Exploratory visual analysis
d.test <- d.accidents[, c("duration", "severity", "time_interval", "cities_population", "stop", "station", "junction", "traffic_signal", "sunrise_sunset", "crossing", "amenity", "temp_c", "presip_mm")]
d.test <- d.test %>%
  mutate(across(c("time_interval", "crossing", "stop", "station", "junction", "traffic_signal", "sunrise_sunset"), as.factor))

model.matrix(~0+., data=d.test) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(title = "Correlation Matrix", show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)

rm(d.test)

# EDA severity and duration
#Barplot showing the mean of duration per severity
duration.mean <- t(tapply(d.accidents$duration, list(d.accidents$severity), mean))
coul <- brewer.pal(4, "Set2")
barplot(duration.mean, border="#69b3a2", col=coul, xlab= "Severity", ylab="Duration (minutes)",main="Average Duration per Severity Level of Accident", ylim=c(0,2000))

#Boxplots showing a significantly higher level of duration of severity 4 compared to severity 1 and 3:
ggplot(data = d.accidents, mapping = aes(y = log(duration), x = severity)) + geom_point(aes(group = duration)) + geom_boxplot() + facet_grid(. ~ severity)

# Duration and the Locations of the Accidents
# US States
# US States and duration barplot
mean_times <- aggregate(duration~ state, data = d.accidents, FUN = mean)
mean_times <- mean_times[order(mean_times$duration),]
highest_times_states <- mean_times[49:38,]

ggplot(highest_times_states, aes(x = state, y = duration, fill = state, group = state)) + geom_bar(stat = "identity", color="black", position=position_dodge())+
  scale_x_discrete(limits=highest_times_states$state) + geom_errorbar(aes(ymin=duration, ymax=duration+sd(duration)), width=.2,
                                                                      position=position_dodge(.9)) + labs(x="States", y=" Average Duration (min)") + ggtitle("US States with Highest Average Durations") + theme(plot.title = element_text(hjust=0.5))
# Trimming outliers and fitting a barplot and a boxplot
# Data set trimmed of excessive outliers (only for visualizations and NOT for model fitting)
iqr <- IQR(d.accidents$duration)
lower <- quantile(d.accidents$duration, 0.25) - 1.5 * iqr
upper <- quantile(d.accidents$duration, 0.75) + 1.5 * iqr
d.duration_trimmed <- d.accidents[d.accidents$duration >= lower & d.accidents$duration <= upper, ]

# Street type Bar plot and Boxplots
# Box plots for street types using trimmed data
ggplot(data = d.duration_trimmed, mapping = aes(y = duration, x = street)) + geom_point() + geom_boxplot() +theme(axis.text.x = element_text(angle = 90)) + ggtitle("Duration against Street Types") + theme(plot.title = element_text(hjust=0.5))

## Bar plots of street type counts with severity groups
ggplot(d.accidents) + 
  geom_bar(aes(x = street, fill = factor(severity))) +theme(axis.text.x = element_text(angle = 90)) + ggtitle("Counts of Accidents per Street Type with Severity Factor") + theme(plot.title = element_text(hjust=0.5)) 

# When Accidents Took Place and Weather Conditions
# Box plots for each Month and duration
boxplot(duration ~ month, data = d.duration_trimmed,
        main = "Durations per Month",
        ylab = "Durations (min)")

#Bar plot with Weather Conditions with the longest duration on average
mean_times <- aggregate(duration~ weather_cond, data = d.duration_trimmed, FUN = mean)
mean_times <- mean_times[order(mean_times$duration),]
highest_times <- mean_times[65:60,]

ggplot(highest_times, aes(x = weather_cond, y = duration, fill = NULL)) + geom_bar(stat = "identity", color="black", position=position_dodge())+
  scale_x_discrete(limits=highest_times$weather_cond) + labs(x="Weather Conditions", y="Average Duration (min)") + ggtitle("Weather Conditions with Highest Average Durations") + theme(plot.title = element_text(hjust=0.5))+theme(axis.text.x = element_text(angle = 90))

rm(d.duration_trimmed)

########### Linear Model Fit###########
# We make sure that variables are correctly coded as categorical before fitting a model
d.accidents <- d.accidents %>%
  mutate(across(c("severity", "month", "state", "street", "weather_cond"), as.factor))

# We fit our model
lm.accidents.1 <-lm(duration~ severity + state + month + street + weather_cond, data=d.accidents)

# p-value  of weather_cond
summary_lm <- summary(lm.accidents.1)
summary_lm$coefficients["stateCO", "Pr(>|t|)"]

# We re-fit the model 
lm.accidents.2 <- update(lm.accidents.1, .~. - weather_cond)

# Adding an interaction term:
lm.accidents.3 <-update(lm.accidents.2, . ~ . + severity*street)

# Single term deletions
drop1(lm.accidents.3, test="F")

# residual plots
par(mfrow=c(1,2))
plot(lm.accidents.3, which = 2)
plot(lm.accidents.3, which = 5)

# Re-fit of the model with log transformed duration: 
lm.accidents.4 <- lm(log(duration)~ state + severity + street + month + severity*street, data=d.accidents)

# Q-Q Plot and Residuals vs Leverage
par(mfrow=c(1,2))
plot(lm.accidents.4, which = 2)
plot(lm.accidents.4, which = 5)

# r-squared and adjusted r-squared 
summary(lm.accidents.4)$r.squared
summary(lm.accidents.4)$adj.r.squared

# Single term deletions
drop1(lm.accidents.4, test="F")

# print relevant coefficients to comment
summarylm4 <- summary(lm.accidents.4)
coefficients <- summarylm4$coefficients
coeffs_toprint <- coefficients[c("severity2", "severity3", "severity4", "stateSD", "stateMA", "month7"), ]
coeffs_toprint 

################# Generalised Additive Model###############
### Vitalia Vedenikova took the lead on the Generalised Additive Model section

###### Generalized Additive Model Fit######
#GAM model fit
gam.accidents <-gam(log(duration)~ severity + state + month + street + severity*street + s(temp_c) + s(presip_mm) + s(cities_population), data=d.accidents)

#Approximative significance of smooth terms
gam.summary <- summary(gam.accidents)
gam.summary$s.table
gam.summary$r.sq

# GAM RESIDUALS
par(mfrow=c(1,2))
plot(gam.accidents, residuals = TRUE, select = 1)
plot(gam.accidents, residuals = TRUE, select = 3)

# gam.check output
set.seed(123)
gam_check_output <- capture.output(gam.check(gam.accidents))
cat(gam_check_output, sep = "\n")

########### GLM MODELS################
# Perez Olusese took the lead on the Poisson and Binomial models 

######## Poisson########
# Exploratory data analysis

# Boxplot for Severity vs. Accidents Count
ggplot(d.accidents, aes(x = factor(severity), y = accidents_count_per_state)) +
  geom_boxplot() +
  labs(title = "Severity vs. Accidents Count",
       x = "Severity",
       y = "Accidents Count")

# Sum the accident counts for each severity level
severity_summary <- d.accidents %>%
  group_by(severity) %>%
  summarise(total_accidents = sum(accidents_count_per_state, na.rm = TRUE))

# Create the bar plot
ggplot(severity_summary, aes(x = factor(severity), y = total_accidents)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Accidents Count by Severity",
       x = "Severity",
       y = "Total Accidents Count") +
  theme_minimal()

# Weather conditions vs. Accidents Count
# Boxplot for Weather vs. Accidents Count
# Convert weather_cond column to factor
d.accidents$weather_cond <- as.factor(d.accidents$weather_cond)

ggplot(d.accidents, aes(x = weather_cond, y = accidents_count_per_state)) +
  geom_boxplot() +
  labs(title = "Weather vs. Accidents Count",
       x = "Weather_cond",
       y = "Accidents Count")

# Sum the accident counts for each weather condition
weather_summary <- d.accidents %>%
  group_by(weather_cond) %>%
  summarise(total_accidents = sum(accidents_count_per_state, na.rm = TRUE))

# Create the bar plot
ggplot(weather_summary, aes(x = weather_cond, y = total_accidents)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Accidents Count by Weather Condition",
       x = "Weather Condition",
       y = "Total Accidents Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Sum the accident counts for each month
month_summary <- d.accidents %>%
  group_by(month) %>%
  summarise(total_accidents = sum(accidents_count_per_state, na.rm = TRUE))

# Create the bar plot
ggplot(month_summary, aes(x = factor(month), y = total_accidents)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Accidents Count by Month",
       x = "Month",
       y = "Total Accidents Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############## Poisson model fitting####################
glm_accident_1 <- glm(accidents_count_per_state ~ severity + weather_cond + month + time_interval,
                      data = d.accidents,
                      family = poisson)
summary(glm_accident_1)

# building quasi-poisson because of over dispersion
# Quasi-poisson fitting
quasi_accident_1 <- glm(accidents_count_per_state ~ severity + weather_cond + month + time_interval,
                        data = d.accidents,
                        family = quasipoisson)
summary(quasi_accident_1)

## interpreting weather conditions
exp(coef(glm_accident_1)["weather_condDrizzle and Fog"])

## interpreting weather conditions
exp(coef(glm_accident_1)["severity2"])

# coefficients comparisons
# poisson
coef(glm_accident_1)

# quasi poisson 
coef(quasi_accident_1)

######## GLM -Binomial#######

# Convert severity to binary
d.accidents_binary <- d.accidents %>%
  mutate(severity_binary = ifelse(severity %in% c(1, 2), 0, 1))

# Pairwise_Correlation_severity
d.test <- d.accidents[, c("severity", "stop", "junction", "traffic_signal")]
d.test <- d.test %>%
  mutate(across(c("stop", "junction", "traffic_signal"), as.factor))

model.matrix(~0+., data=d.test) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(title = "Correlation Matrix", show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)

rm(d.test)

# Build the GLM binomial model
glm_binomial_1 <- glm(severity_binary ~  junction + stop + traffic_signal,
                      family = binomial, data = d.accidents_binary)
summary(glm_binomial_1)

# intrepreting_junction
exp(coef(glm_binomial_1)["junctionTRUE"])

# Extract the residuals from the model
residuals <- residuals(glm_binomial_1, type = "deviance")

# Create the QQ plot for residuals
#qq_plot <- ggplot(data.frame(residuals = residuals), aes(sample = residuals)) +
#  stat_qq() +
#  stat_qq_line() +
#  labs(title = "QQ Plot of Deviance Residuals",
#       x = "Theoretical Quantiles",
#       y = "Sample Quantiles") +
#  theme_minimal()

# Display the QQ plot
print(qq_plot)

######### Support Vector Machine#########
### Vitalia Vedenikova and Perez Olusese worked on this model 
# Pairwise_Correlation_severity_more_variables
d.test <- d.accidents[, c("severity", "time_interval")]

d.test <- d.test %>%
  mutate(across(c("time_interval"), as.factor))

model.matrix(~0+., data=d.test) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(title = "Correlation Matrix", show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)


d.test <- d.accidents[, c("severity", "amenity", "crossing", "station", "cities_population", "month")]
d.test <- d.test %>%
  mutate(across(c("amenity", "crossing", "station"), as.factor))

model.matrix(~0+., data=d.test) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(title = "Correlation Matrix", show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)

rm(d.test)

# Ensure d.accidents_vector is a data.table
d.accidents_vector <- d.accidents
setDT(d.accidents_vector)

d.accidents_vector <- d.accidents_vector %>%
  mutate(severity = ifelse(severity %in% c(3, 4), 1, 0))

# Select variables 
bool_cols <- c("amenity", "crossing", "junction", "station", "stop", "traffic_signal", "cities_population", "month", "street", "time_interval")
d.accidents_vector<- d.accidents_vector[, c(bool_cols, "severity"), with = FALSE]

table(d.accidents_vector$severity)

#Initial proportions of severity 0 vs severity 1 (minor vs major accidents)
table(d.accidents_vector$severity)

set.seed(123)

# Balancing the data set 

# Define the sampling fraction for major severity
sampling_fraction <- 0.07506

# Sample data with different fractions for severity 2 and other severity levels
d.accidents_vector <- d.accidents_vector %>%
  group_by(severity) %>%
  do({
    if (.$severity[1] == 0) {
      sample_frac(., size = sampling_fraction)
    } else {
      .
    }
  }) %>%
  ungroup()

# Check the resulting sampled data
table(d.accidents_vector$severity)

setDT(d.accidents_vector)

# Initial SVM model fitting

# Convert boolean columns to factors
d.accidents_vector[, (bool_cols) := lapply(.SD, as.factor), .SDcols = bool_cols]

# Define the target variable and features
y <- d.accidents_vector$severity
X <- d.accidents_vector[, !("severity"), with = FALSE]

# Split the data into training and testing sets
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE, times = 1)
X_train <- X[trainIndex,]
X_test <- X[-trainIndex,]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# Set seed for reproducibility
set.seed(123)

# Train the SVM model with the RBF kernel-linear and cost set to 10
svm_model <- svm(
  as.factor(y_train) ~ ., 
  data = X_train, 
  kernel = "linear", 
  cost = 10, 
)

# Make predictions on the test set
predictions <- predict(svm_model, X_test)

# Evaluate the model 
conf_matrix <- confusionMatrix(predictions, as.factor(y_test))
print(conf_matrix)

# Plotting the Confusion matrix 
# Convert confusion matrix to a data frame for ggplot2
conf_df <- as.data.frame(conf_matrix$table)
colnames(conf_df) <- c("Prediction", "Reference", "Freq")

# Plot the confusion matrix
ggplot(conf_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  theme_minimal() +
  ggtitle("Confusion Matrix") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Train the SVM model with the RBF kernel-radial and cost set to 100
svm_model <- svm(
  as.factor(y_train) ~ ., 
  data = X_train, 
  kernel = "radial", 
  cost = 100, 
)

# Make predictions on the test set
predictions <- predict(svm_model, X_test)

# Evaluate the model
conf_matrix <- confusionMatrix(predictions, as.factor(y_test))
conf_matrix$overall["Accuracy"]
conf_matrix$byClass["Sensitivity"]

# Set seed for reproducibility
set.seed(123)

# Number of folds
k <- 5

# Create folds
folds <- createFolds(y, k = k, list = TRUE)

# Initialize a vector to store accuracy for each fold
accuracy <- numeric(k)

# Perform cross-validation
for (i in 1:k) {
  # Split the data into training and testing sets
  train_indices <- folds[[i]]
  X_train <- X[-train_indices,]
  y_train <- y[-train_indices]
  X_test <- X[train_indices,]
  y_test <- y[train_indices]
  
  # Train the SVM model with linear kernel
  svm_model <- svm(as.factor(y_train) ~ ., data = X_train, kernel = "linear", 
                   ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
  
  summary(svm_model)
  
  # Make predictions on the test set
  predictions <- predict(svm_model, X_test)
  table(predictions)
  
  # Calculate accuracy
  accuracy[i] <- mean(predictions == y_test)
}

# Print the average accuracy
max_accuracy <- max(accuracy)
print(max_accuracy) 

############## Artificial Neural Network ################
### Vitalia Vedenikova and Perez Olusese worked on this model

# Define d.accidents_neural
d.accidents_neural <- d.accidents[, c("severity", "crossing","traffic_signal", "temp_c", "month", "time_interval", "street", "junction")]

d.accidents_neural <- d.accidents_neural %>%
  mutate(severity = ifelse(severity %in% c(3, 4), 1, 0))

d.accidents_neural$severity <- factor(d.accidents_neural$severity)

set.seed(123)

#We use stratified sampling by severity
#d.accidents_neural <- d.accidents_neural %>%
#  group_by(severity) %>%
#  sample_frac(size = 0.8) %>%
#  ungroup()

table(d.accidents_neural$severity)

# Balancing the data set
# Define the sampling fraction for severity 2
sampling_fraction_2 <- 0.07506

# Sample data with different fractions for severity 2 and other severity levels
d.accidents_neural <- d.accidents_neural %>%
  group_by(severity) %>%
  do({
    if (.$severity[1] == 0) {
      sample_frac(., size = sampling_fraction_2)
    } else {
      .
    }
  }) %>%
  ungroup()

d.accidents_neural <- d.accidents_neural %>%
  mutate(across(c("severity", "time_interval","month", "street", "junction", "crossing", "traffic_signal"), as.factor))

#we encode dummy variables
model_matrix <- model.matrix(~ . - 1, data = d.accidents_neural)
d.accidents_neural <- data.frame(severity = d.accidents_neural$severity, model_matrix)

#normalize the numerical variables
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
vars_num <- c("temp_c")
d.accidents_neural[vars_num] <- as.data.frame(lapply(d.accidents_neural[vars_num], normalize))

#partition the data for training
indices <- createDataPartition(d.accidents_neural$severity, p=.85, list = F)

severity_mod <- d.accidents_neural %>% select(-severity)
train_mod <- severity_mod %>% slice(indices)

train <- d.accidents_neural %>%
  slice(indices)

test_in <- d.accidents_neural %>%
  slice(-indices) %>%
  select(-severity)

test_truth <- d.accidents_neural %>%
  slice(-indices) %>%
  pull(severity)

# Create the formula for the neural network (we make sure to remove severities as a predictors)
vars_exclude <- c("severity0", "severity1") 
predictors <- setdiff(names(train_mod), c("severity", vars_exclude)) 
formula <- as.formula(paste("severity0 + severity1 ~", paste(predictors, collapse = " + ")))

###################################
# Then, we look at the different models: 
########################################  
  
#We will implement parallel processing to make the process more efficient:
library(doParallel)
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

#Then we optimize the network structure
set.seed(142)

models <- train(formula, train_mod,
                method="neuralnet",   
                tuneGrid = expand.grid(.layer1=c(2:4), .layer2=c(0:2), .layer3=c(0)),               
                learningrate = 0.001,  
                threshold = 0.05,     
                stepmax = 70000
)

#stop the cluster
stopCluster(cl)
registerDoSEQ()

plot(models)

#The best model for this data appears to be one with 2 hidden units in layer 1 and 0 hidden units in layer 2. This is the one which we test out now: 
#We will implement parallel processing to make the process more efficient:
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

#We implement the best model:
set.seed(42)
best_model <- neuralnet(
  formula,
  train,
  hidden = c(2),
  learningrate = 0.001, 
  threshold = 0.05,   
  stepmax = 100000 
)

#stop the cluster
stopCluster(cl)
registerDoSEQ()

#We look at the confusion matrix and evaluate our model: 
test_results <- neuralnet::compute(best_model, test_in)
test_pred <- apply(test_results$net.result, 1, which.max)
test_pred <- factor(levels(test_truth)[test_pred], levels = levels(test_truth))
confusionMatrix(test_truth, test_pred)

# Best_Confusion_Matrix, cache=TRUE, include=TRUE, echo=FALSE}
conf_matrix <- confusionMatrix(test_truth, test_pred)
conf_df <- as.data.frame(conf_matrix$table)
colnames(conf_df) <- c("Prediction", "Reference", "Freq")

# Plot the confusion matrix
ggplot(conf_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  theme_minimal() +
  ggtitle("Confusion Matrix") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))