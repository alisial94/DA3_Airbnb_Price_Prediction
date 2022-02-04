#########################################
# Predicting AirBnB apartment prices    #
#      City: Melbourne, Australia       #
#           Data Preparation            #
#########################################


# SET UP ------------------------------------------------------------------
#
# CLEAR MEMORY
rm(list=ls())

library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(cowplot)
library(modelsummary)
library(fixest)
library(dplyr)
library(naniar)
library(stargazer)
library(Hmisc)
library(skimr)



# import data -------------------------------------------------------------
## clean data from last R script uploaded to git, calling it directly from there

data <- read_csv("https://raw.githubusercontent.com/alisial94/DA3_Airbnb_Price_Prediction/main/Data/clean/melborne_initial_clean.csv")


# FILTER DATA TO ACTUAL CASE ----------------------------------------------

# check for different property types
types <- data %>% group_by(property_type) %>% 
  summarise(number = n()) %>% 
  arrange(.,-number)
rm(types)

# keep if property type is Apartment
data <- data %>%
  filter(property_type %in% c("Entire serviced apartment", 
                              "Entire condominium (condo)", 
                              "Entire loft", 
                              "Entire home/apt")) 

# keep if accommodates 2-6 people
data <- data[data$accommodates >= 2 & data$accommodates <= 6,]

# CLEANE VARIABLES AND CREATE WORKFILE ------------------------------------------------
###############################
#### FACTORS
#
# Property type as factor and adjusting names
data %>% 
  group_by(property_type) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

data$property_type <- replace(data$property_type, data$property_type == "Entire condominium (condo)","Condo")
data$property_type <- replace(data$property_type, data$property_type == "Entire serviced apartment","Serviced_apartment")
data$property_type <- replace(data$property_type, data$property_type == "Entire loft","Loft")
data$property_type <- replace(data$property_type, data$property_type == "Entire home/apt","Home_Apt")


data <- data %>% 
  mutate( f_property_type = factor(property_type))


#Room type as factor
data %>% 
  group_by(room_type) %>% 
  summarise(cnt = n())

# Since the room type is only Entire home/apt I wont be using it 
# as it wont have an impact on the price for this analysis

# neighbourhood_cleansed as factors
unique(data$neighbourhood_cleansed)

data <- data %>%
  mutate(f_neighbourhood_cleansed = 
           factor(neighbourhood_cleansed, levels 
                  = c("Melbourne","Port Phillip","Yarra","Yarra Ranges","Moreland",
                      "Stonnington", "Wyndham","Whitehorse","Boroondara","Banyule",          
                      "Manningham", "Maribyrnong", "Darebin", "Kingston", "Glen Eira",        
                      "Greater Dandenong", "Hobsons Bay", "Monash", "Bayside", "Melton",           
                      "Knox","Maroondah", "Moonee Valley", "Frankston", "Whittlesea" )))


# get host_response_time as factors

data %>% 
  group_by(host_response_time) %>% 
  summarise(cnt = n())


data <- data %>% 
  mutate(f_host_response_time = factor(host_response_time, levels = c( "within an hour",  "within a few hours",
                                                                       "within a day", "a few days or more")))

# host listings as factor

###############################
#### NUMERIC VARIABLES
#
## Create Numerical variables
data <- data %>%
  mutate( usd_price = price*0.71) # AUD to USD conversion exchange rate on 
                                  # 4th Feb 2022 as per google

# clean number of bathrooms
data <- data %>% rename(bathrooms = bathrooms_text)

data %>% 
  group_by(bathrooms) %>% 
  summarise(cnt =n())

# get the number of baths from bathroom_text
data$bathrooms <- as.numeric(gsub("[^0-9.-]", "", data$bathrooms))

# add new numeric columns from certain columns
numericals <- c("host_response_rate","host_acceptance_rate","accommodates","bathrooms", "bedrooms", "beds", "review_scores_rating","number_of_reviews",
                "reviews_per_month","minimum_nights","calculated_host_listings_count")
data <- data %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))


# rename columns so they start with n_ as opposed to end with _n
nnames <- data %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(data))
colnames(data)[nnames_i] <- paste0("n_", numericals)


# create days since first review
data <- data %>%
  mutate(
    n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))


###############################
#### DUMMY VARIABLES
#
# create dummy vars 42 to 120

data <- data %>% 
  filter(host_has_profile_pic == TRUE)

# wont be using host_has_profile_pic for analysis since on 3 values with false 
# also removing observations with false value to maintain consistancey
# same is the case with variable has availability wont be using it for analysis

dummies <- c(names(data)[seq(42,120)],"host_is_superhost", "host_identity_verified", "instant_bookable" )
data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))

# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))

# fixing columns still not dummies
data$d_host_is_superhost <- ifelse(data$d_host_is_superhost == T, 1,0)
data$d_host_identity_verified <- ifelse(data$d_host_identity_verified == T, 1,0)
data$d_instant_bookable <- ifelse(data$d_instant_bookable == T, 1,0)


# CREATE WORK FILE --------------------------------------------------------

# keep columns if contain d_, n_, f_ usd_ and some others
data_clean <- data %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price, id, host_id,
         neighbourhood_cleansed,room_type,property_type)

# with price info only
data_clean <- data_clean %>%
  drop_na(price)  #  already dropped in the start of this exercise just double checking

# rename price in USD to price and price to local price for simplicity 
data_clean <- data_clean %>% 
  rename(price = usd_price,
         local_price = price)

write_csv(data_clean, "Data/clean/melborne_listing_data_final_variables.csv")



