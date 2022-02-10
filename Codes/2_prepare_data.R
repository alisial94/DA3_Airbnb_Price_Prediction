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
                                  # 4th Feb 2022 as per Google

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


# Investigating Variables  -------------------------------------------------------------------------

##################################
# DESCRIBE

# Property type
data_clean %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(num_values=n()) %>% 
  arrange(-num_values)

#####################
### look at price ###
#####################
datasummary_skim(data_clean$price)
describe(data_clean$price)

boxplot(data_clean$price)
g1<-ggplot(data_clean , aes(x = price)) +
  geom_histogram( fill='navyblue', color = 'white' ) +
  theme_classic()
g1

# filter out really high extreme values , price in USD (no need for ln price since the distribution isn't very right skewed)
data_clean <- data_clean %>%
  filter(price < 400)

# create ln price
g2<-ggplot(data_clean , aes(x = log(price))) +
  geom_histogram( fill='navyblue', color = 'white' ) +
  theme_classic()
g2

data_clean <- data_clean %>%
  mutate(ln_price = log(price))


################################################
# Look at some numeric key vars                #
################################################

############################
#### n_accommodates
datasummary_skim(data_clean$n_accommodates)

data_clean %>%
  group_by(n_accommodates) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())



ggplot(data_clean, aes(n_accommodates)) +
  geom_histogram(binwidth = 0.5, fill = "navyblue", color = "white", size = 0.25) +
  theme_classic()

g3 <- ggplot(data_clean, aes(x=n_accommodates, y=price)) +
  geom_point(size=1, colour="navyblue", shape=16, alpha = 0.6)+
  geom_smooth(method="loess", colour="red", se=FALSE)+
  labs(x= "Number of people accomodated",y="Price")+
  scale_x_discrete( limits = c("1","2","3","4","5","6"))+
  theme_classic()
g3 #price vs accommodates

#### no missing values

############################
## n_bathrooms
ggplot(data_clean, aes(n_bathrooms)) +
  geom_histogram(binwidth = 0.5, fill = "navyblue", color = "white") +
  xlab("N of bathrooms") +
  theme_classic()

data_clean %>%
  group_by(n_bathrooms) %>%
  summarise(mean_price = mean(price), n = n())

# check number of bathrooms for different number of accommodates
data_clean %>% 
  group_by(n_accommodates) %>% 
  summarise(num_baths = mean(n_bathrooms, na.rm = T), min_baths = min(n_bathrooms, na.rm = T), max_baths = max(n_bathrooms, na.rm = T))


#### I will create pooled categories -> 1, 2 and 3
#### there are no missing values for bathrooms


############################
## n_bedrooms
ggplot(data_clean, aes(n_bedrooms)) +
  geom_histogram(binwidth = 0.5, fill = "navyblue", color = "white", size = 0.25) +
  xlab("N of bedrooms") +
  theme_classic()

data_clean %>%
  group_by(n_bedrooms) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

# check number of bedrooms for different number of accommodates
data_clean %>% 
  group_by(n_accommodates) %>% 
  summarise(num_bedrooms = mean(n_bedrooms, na.rm = T), min_bedrooms = min(n_bedrooms, na.rm = T), max_bedrooms = max(n_bedrooms, na.rm = T))

#### I will create pooled categories -> 1, 2, and 3 
####  then impute accommodates/2 rounded to whole for missing



############################
#### n_beds
ggplot(data_clean, aes(n_beds)) +
  geom_histogram( fill = "navyblue", color = "white", size = 0.25) +
  xlab("N of beds") +
  scale_x_discrete( limits = c("1", "2","3","4","5","6") )+
  theme_classic()

data_clean %>%
  group_by(n_beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

# check number of beds for different number of accommodates
data_clean %>% 
  group_by(n_accommodates) %>% 
  summarise(num_beds = mean(n_beds, na.rm = T), min_beds = min(n_beds, na.rm = T), max_beds = max(n_beds, na.rm = T))


### i will also club the value to 1,2,3
#### then impute accommodates/2 rounded to whole for missing



############################
## n_review_scores_rating
data_clean %>%
  group_by(n_review_scores_rating) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

ggplot(data_clean, aes(n_review_scores_rating)) +
  geom_histogram( fill = "navyblue", color = "white", size = 0.25, bins = 50) +
  xlab("Review Score Rating") +
  theme_classic()

ggplot(data = data_clean, aes(x=n_review_scores_rating , y=price)) +
  geom_point(size=1.5, colour="red", shape=16, alpha=0.6) +
  geom_smooth(method="loess", colour="navyblue", se=F)+
  labs(x="Review score",y="Daily price (USD)")+
  theme_classic()

#### I will pool the values as 0-4.5,4.5-5
#### impute missing values with medians



############################
## n_number_of_reviews

data_clean %>%
  group_by(n_number_of_reviews) %>%
  summarise(cnt = n())

data_clean %>%
  filter(n_number_of_reviews <200) %>% 
  ggplot(aes(n_number_of_reviews)) +
  geom_histogram(binwidth = 5, fill = "navyblue", color = "white", size = 0.25) +
  ylab("") +
  xlab("N of reviews") +
  theme_classic()

ggplot(data = data_clean, aes(x=n_number_of_reviews , y=price)) +
  geom_point(size=1.5, colour="red", shape=16, alpha=0.6) +
  geom_smooth(method="loess", colour="navyblue", se=F)+
  labs(x="Number of Reviews",y="Daily price (USD)")+
  theme_classic()

ggplot(data = data_clean, aes(x=log(n_number_of_reviews) , y=price)) +
  geom_point(size=1.5, colour="red", shape=16, alpha=0.6) +
  geom_smooth(method="loess", colour="navyblue", se=F)+
  labs(x="log Number of Reviews",y="Daily price (USD)")+
  theme_classic()


#### I will take the log of it
#### I will also create pools for reviews ->  none, 1-51 and >51 



############################
## n_minimum_nights
data_clean %>% 
  group_by(n_minimum_nights) %>% 
  summarise(cnt = n())

# filtering the data since there is on observation with minimum nights 803, which is irregularly large and unrealistic

data_clean <- data_clean %>% 
  filter(n_minimum_nights <= 365)

ggplot(data_clean, aes(n_minimum_nights)) +
  geom_histogram( fill = "navyblue", color = "white", size = 0.25, binwidth = 1) +
  xlab("N of minimum nights") +
  xlim(0,50)+
  theme_classic()

#### I will create pooled categories -> 1, 2 and 3, 3+ , log didnt work so wont take it

############################
## n_days_since (first review)
data_clean %>% 
  group_by(n_days_since) %>% 
  summarise(cnt = n())

ggplot(data_clean, aes(n_days_since_first_review)) +
  geom_histogram( fill = "navyblue", color = "white", size = 0.25, binwidth = 200) +
  xlab("N of minimum nights") +
  theme_classic()

ggplot(data = data_clean, aes(x=log(n_days_since_first_review) , y=price))+
  geom_point(size=1.5, colour="red", alpha = 0.5) +
  geom_smooth(method="loess", colour="navyblue", se=F)+
  labs(x="Log number of days since first review",y="Log daily price")+
  theme_classic()

#### I will create ln
#### will pool in 3 categories 0-500, 500-1000, 1000+

############################
## n_calculated_host_listing_count

# rename the variable
data_clean <- data_clean %>% rename(n_host_listings_count = n_calculated_host_listings_count)

v<-data_clean %>% 
  group_by(n_host_listings_count) %>% 
  summarise(cnt = n())

ggplot(data_clean, aes(n_host_listings_count)) +
  geom_histogram( fill = "navyblue", color = "white", size = 0.25) +
  xlab("N of Host Listings") +
  theme_classic()

ggplot(data = data_clean, aes(x=log(n_days_since) , y=ln(price)))+
  geom_point(size=1.5, colour="red", alpha = 0.5) +
  geom_smooth(method="loess", colour="navyblue", se=F)+
  labs(x="Log number of days since first review",y="Log daily price")+
  theme_classic()

### after looking at this variable in detail, even though based on domain knowledge it is 
### a good indicator for price, but since it also possible that different number of the host listing
### are also present in our data set thus inflating the number of listing total.Therefore, reliability of this
### this variable is slightly compromised, and therefor i have decided to drop it from analysis.

data_clean$n_host_listings_count <- NULL

################################################
# Rename some dummy variables                  #
################################################

data_clean <- data_clean %>% rename(d_refrigerator = d_have_refrigeratorfridgefreezer,
                                    d_coffee_machine = d_have_coffeenespresso,
                                    d_bbq_equipment = d_have_bbq_grilbarbecue,
                                    d_free_parking = d_have_freeon_premisesfreestreet,
                                    d_paid_parking = d_have_paidparking,
                                    d_bath_tub = d_have_bathtubbath_tubhot_tub,
                                    d_linens = d_have_linenscomforts,
                                    d_wifi = d_have_wifiinternet,
                                    d_streaming_services = d_have_netflixamazoneapplerokuhbo,
                                    d_tv = d_have_tvhdtv,
                                    d_sound_system = d_have_speakerssound_system,
                                    d_shampoo_conditioner = d_have_shampooconditioner,
                                    d_body_wash = d_have_body_soapbody_washshower_gel,
                                    d_cooling = d_have_air_condfan,
                                    d_balcony = d_have_patio_or_balconyterrace,
                                    d_garden = d_have_backyardgarden,
                                    d_clothing_storage = d_have_closetwardrobeclothing_storage,
                                    d_cutlary_glasses = d_have_cooking_basicsdishesrice_makertoasterglasses,
                                    d_family_friendly = d_have_children2551010)



################################################
# Deal with missing values                     #
################################################

# where do we have missing variables now? (how much % of variables are missing)
to_filter <- sapply(data_clean, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

to_filter <- sort(sapply(data_clean, function(x) sum(is.na(x)/nrow(data_clean)*100)))
to_filter[to_filter > 0]



# 1. drop if no target (already did)
data_clean <- data_clean %>%
  drop_na(price)

## Imputing n_beds and n_bedrooms , I wont be adding flag variable for n_beds since the missing values are only 7

data_clean <- data_clean %>% 
  mutate(d_flag_bedrooms = ifelse(is.na(n_bedrooms), 1, 0))

data_clean <- data_clean %>%
  mutate(
    n_beds = ifelse(is.na(n_beds), round(n_accommodates / 2), n_beds), #assume that 1 bed corresponds to about 2 accommodates
    n_bedrooms = ifelse(is.na(n_bedrooms), round(n_accommodates / 2), n_bedrooms),) #assume that bedrooms correlate to around half the number of accommodates


# 4. Replace missing variables re reviews with zero, when no review + add flags
## also changed the name of variables that are percentages to start with 'p' instead of n
## also changing the name of variable n_days_since to n_days_since_last_review

data_clean <- data_clean %>%
  mutate(
    flag_review_scores_rating = ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    
    flag_host_acceptance_rate = ifelse(is.na(n_host_acceptance_rate),1, 0),
    p_host_acceptance_rate =  ifelse(is.na(n_host_acceptance_rate), median(n_host_acceptance_rate, na.rm = T), n_host_acceptance_rate),
    
    flag_host_response_rate = ifelse(is.na(n_host_response_rate),1, 0),
    p_host_response_rate =  ifelse(is.na(n_host_response_rate), median(n_host_response_rate, na.rm = T), n_host_response_rate),
    
    flag_host_response_time = ifelse(is.na(f_host_response_time),1, 0),
    f_host_response_time =  ifelse(is.na(f_host_response_time), "missing", f_host_response_time),
    
    flag_days_since_first_review = ifelse(is.na(n_days_since),1, 0),
    n_days_since_first_review =  ifelse(is.na(n_days_since), median(n_days_since, na.rm = T), n_days_since),
    
    flag_reviews_per_month = ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month)
  )


# removing extra columns 
## column existing because name changed 
## column d_reviews_per_month should have been removed earlier because it has the same value as n_reviews_per_month

to_drop <- c( "n_host_response_rate", "n_days_since", "n_host_acceptance_rate", "d_reviews_per_month")
data_clean <- data_clean %>%
  select(-one_of(to_drop))


# where do we have missing variables now?
to_filter <- sort(sapply(data_clean, function(x) sum(is.na(x)/nrow(data_clean)*100)))
to_filter[to_filter > 0]
#### there are no missing values left


################################################
# Create pooled categories for variables       #
################################################

# Pool accommodations with 1,1.5,2,2.5,3 bathrooms
data_clean <- data_clean %>%
  mutate(f_bathroom = cut(n_bathrooms, c(1,2,3,4), labels=c(1,2,3), right = F) )

# Pool accommodations with 1,2,3,4 bedrooms
data_clean <- data_clean %>%
  mutate(f_bedroom = cut(n_bedrooms, c(1,2,3,4), labels=c(1,2,3), right = F) )

# Pool accommodations with 1,2,3,4,5,6 beds
data_clean <- data_clean %>%
  mutate(f_beds = cut(n_beds, c(1,2,3,7), labels=c(1,2,3), right = F) )

# Pool review score rating to 2 categories: 0-4.5 and >4.5
data_clean <- data_clean %>%
  mutate(f_review_score_rating = cut(n_review_scores_rating, c(0,4.5,max(data_clean$n_number_of_reviews)), labels=c('0-4.5', '4.5+'), right = F))

# Pool num of reviews to 3 categories: none, 1-51 and >51
data_clean <- data_clean %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(data_clean$n_number_of_reviews)), labels=c('None','1-51','51+'), right = F))

# Pool and categorize the number of minimum nights: 1,2,3, 3+
data_clean <- data_clean %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,3,4,max(data_clean$n_minimum_nights)), labels=c('1','2','3','3+'), right = F))

# Pool and categorize the days since first review: 0-500, 500-1000, 1000+
data_clean <- data_clean %>%
  mutate(f_days_since_first_review= cut(n_days_since_first_review, c(0,501,1001,max(data_clean$n_days_since_first_review)), labels=c('<500', '<1000', '1000+'), right = F))


# Change Infinite values with NaNs
for (j in 1:ncol(data_clean) ) data.table::set(data_clean, which(is.infinite(data_clean[[j]])), j, NA)

to_filter <- sapply(data_clean, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# fill in missing values for pooled variables
data_clean <- data_clean %>%
  mutate(f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
         f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
         f_bedroom=ifelse(is.na(f_bedroom),1, f_bedroom),
         f_days_since_first_review=ifelse(is.na(f_days_since_first_review),1, f_days_since_first_review))


to_filter <- sapply(data_clean, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


column_names <- colnames(data_clean)


################################################
# Create new functional forms                  #
################################################

# Creating logs of these two variables
data_clean <- data_clean %>%
  mutate(
          ln_number_of_reviews = log(n_number_of_reviews+1),
          ln_days_since_first_review = log(n_days_since_first_review)
  )


# SAVE ADJUSTED WORKFILE --------------------------------------------------

write_csv(data_clean,"Data/clean/melborne_prepared.csv")
