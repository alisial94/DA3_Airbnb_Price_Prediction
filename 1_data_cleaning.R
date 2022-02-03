#########################################
# Predicting AirBnB apartment prices    #
#      City: Melbourne, Australia       #
#########################################



# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())


# Descriptive statistics and regressions
library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(cowplot)
library(modelsummary)
library(fixest)
library(dplyr)
install.packages('naniar')
library(naniar)


########################################
# PART I.
########################################
#####
# Load data

data_raw <- read_csv( "https://raw.githubusercontent.com/alisial94/DA3_Airbnb_Price_Prediction/main/Data/raw/raw_melb_listing.csv") 
data <- data_raw

# INITIAL STEPS -----------------------------------------------------------

# basic data checks
sort(unique(data$last_scraped)) # they were scraped between 2022 January 08 and 2022 January 09
sum(rowSums(is.na(data)) == ncol(data)) # no only NA rows
nrow(data[duplicated(data),]) #  no duplicates 
sum(colSums(is.na(data)) > 0.5*nrow(data)) # there are 4 columns with at least 50% of the values missing 
names(which(colSums(is.na(data)) > 0.5*nrow(data))) # names of columns with more than 50% vales missing


colnames(data)



to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# drop unnecessary columns
drops <- c("listing_url",
           "scrape_id",
           "last_scraped",
           "name",
           "description",
           "neighborhood_overview",
           "picture_url",
           "host_url",
           "host_name",
           "host_location",
           "host_about",
           "host_thumbnail_url",
           "host_picture_url",
           "host_neighbourhood",
           "host_total_listings_count",
           "neighbourhood", #neighbourhood_cleansed has better information
           "neighbourhood_group_cleansed",  # only NA values in column
           "bathrooms", # only NA values in column
           "minimum_minimum_nights", "minimum_maximum_nights", "minimum_nights_avg_ntm",
           "maximum_minimum_nights", "maximum_maximum_nights", "maximum_nights_avg_ntm", 
           "calendar_updated", # only NA values in column
           "number_of_reviews_ltm", "number_of_reviews_l30d",
           "license",
           "calculated_host_listings_count_entire_homes", 
           "calculated_host_listings_count_private_rooms", 
           "calculated_host_listings_count_shared_rooms")

data <-data [ , !(names(data) %in% drops)]



# drop broken lines - where the id is not a character of numbers
data$junk <- grepl("[[:alpha:]]", data$id)
data<-subset(data,junk==FALSE)
data$junk <- NULL

# display the class and type of each columns
sapply(data, class)
sapply(data, typeof)


# FORMATTING COLUMNS ------------------------------------------------------

# remove percentage signs 
for (perc in c("host_response_rate","host_acceptance_rate")){
  data[[perc]]<-as.numeric(gsub("%","",as.character(data[[perc]])))
}

# replacing 0 values introduced as part of previous command with NA
data <- data %>% replace_with_na(replace = list(host_response_rate = 0))
data <- data %>% replace_with_na(replace = list(host_acceptance_rate = 0))


# remove dollar signs from price variables
data$price <- as.numeric(gsub("\\$","",data$price))

# dropping NAs in price
data <- data %>%
  drop_na(price)


# AMENITIES ---------------------------------------------------------------
#amenities
data$amenities<-gsub("\\[","",data$amenities)
data$amenities<-gsub("\\]","",data$amenities)
data$amenities<-gsub('\\"',"",data$amenities)
data$amenities<-as.list(strsplit(data$amenities, ","))


#define levels and dummies 
levs <- levels(factor(unlist(data$amenities)))
data <- cbind(data,as.data.frame(do.call(rbind, lapply(lapply(data$amenities, factor, levs), table))))


drops <- c("amenities","translation missing: en.hosting_amenity_49",
           "translation missing: en.hosting_amenity_50")
data<-data[ , !(names(data) %in% drops)]

# create data frame of the amenities
amenities <- data %>% select(-(1:42))

# delete spaces in the beginning and end of the column names, and transfer all to lower case
names(amenities) <- tolower(trimws(names(amenities)))

# look at the column names we have
levs <- sort(names(amenities))

# merge all the columns with the same column name
amenities <- as.data.frame(do.call(cbind, by(t(amenities), INDICES= names(amenities),FUN=colSums)))

# replacing _ instead of space for variable names
names(amenities) <- gsub(" ","_", names(amenities))

# list of key words to merge together
cat <- c( "kitchen", "stove", "oven", "refrigerator|fridge|freezer","coffee|nespresso", "bbq_gril|barbecue",
          "free.*on_premises|free.*street", "paid.*parking", "bathtub|bath_tub|hot_tub","linens|comforts",
          "bikes","wifi|internet", "netflix|amazone|apple|roku|hbo", "tv|hdtv", "speakers|sound_system",
          "toiletries", "shampoo|conditioner", "body_soap|body_wash|shower_gel", "hair_dryer", "washer","paid_washer", 
          "dryer", "iron", "heating", "air_cond|fan", "patio_or_balcony|terrace", "backyard|garden",
          "board_game","closet|wardrobe|clothing_storage", "breakfast", "cleaning_before_checkout",
          "workspace", "cooking_basics|dishes|rice_maker|toaster|glasses", "dishwasher", "first_aid", 
          "sauna", "fitness|gym",  "children|2-5|5-10|10+", "pool", "crib","beachfront"
        )

# function to merge columns with the same key word in them
for (i in cat) {
  tdf <- amenities %>% select(matches(i))
  
  amenities$new_col <- ifelse(rowSums(tdf)>0, 1, 0)
  
  names(amenities)[names(amenities) == "new_col"] <- paste0("have_", i)
  
  amenities <- amenities %>% select(-colnames(tdf)) 
  
} 

# keep only columns where the percentage of 1s is at least 1% and at most 99%
selected <- sapply(names(amenities), function(x){
  ratio <- sum(amenities[[x]])/nrow(amenities)*100
  if (between(ratio, 1, 99)) {
    return(TRUE)
  } else { return(FALSE) }
})

amenities <- amenities[,selected]

# merging the cleaned amenities to other variables into a new dataframe
data_clean <- data %>% select((1:42))

data_clean <- cbind(data_clean, amenities)

data_clean <- data_clean[, !duplicated(colnames(data_clean))]

#write csv
write_csv(data_clean,"Data/clean/melborne_initial_clean.csv")
