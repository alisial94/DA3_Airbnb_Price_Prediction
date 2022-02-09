#########################################
# Predicting AirBnB apartment prices    #
#      City: Melbourne, Australia       #
#             Prediction                #
#########################################


# SET UP ------------------------------------------------------------------
#
# CLEAR MEMORY
rm(list=ls())


# Descriptive statistics and regressions
library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(stargazer)
library(xtable)
library(directlabels)
library(knitr)
library(cowplot)
library(rattle)
library(Hmisc)
library(kableExtra)
library(ggcorrplot)
library(modelsummary)
library(ranger)
library(pdp)
library(gbm)



# import data -------------------------------------------------------------
## clean data from last R script uploaded to git, calling it directly from there

data <- read_csv("https://raw.githubusercontent.com/alisial94/DA3_Airbnb_Price_Prediction/main/Data/clean/melborne_prepared.csv")

source("da_helper_function.R")
source("theme_bg.R")

# where do we have missing values now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]
# No columns with missing observations


data %>%
  group_by(f_property_type, room_type) %>%
  summarise(mean_price = mean(price))



# Creating LOWESS plots to identify the association petween price and the rest of teh variables
 for (i in colnames(data)) {
   ggplot(data,aes_string(x=i, y="price"))+
   geom_smooth(method = "loess", formula = y~x, se=FALSE)+ 
   geom_point()
   ggsave(paste0("graphs/association/",i,".png"))
 }


# Removing columns based on above created graphs
#drop <- c("d_fire_pit", "d_lake_access","d_ping_pong_table", "d_private_hot_tub","d_private_outdoor_heated_pool","d_private_outdoor_pool", "d_private_pool", "f_room_type", "n_number_of_reviews","d_bikes","d_board_games","d_game_console","d_have_fitnessgym","d_have_body_soapgel","d_have_sound_system","d_hot_tub","d_lock_on_bedroom_door","d_lockbox","d_shared_outdoor_pool","n_days_since_last_review","n_minimum_nights")
#data <- data %>%
#  select(-one_of(drop))



# Grouping variables
# Basic Variables
basic_lev  <- c("f_property_type","f_neighbourhood_cleansed","n_accommodates","f_bathroom","f_bedroom","f_beds",
                "price","d_flag_bedrooms","f_minimum_nights")
reviews <- c("f_review_score_rating","n_reviews_per_month","flag_review_scores_rating",
             "flag_days_since_first_review","ln_days_since_first_review","ln_number_of_reviews")
host <- c("f_host_response_time","p_host_response_rate","p_host_acceptance_rate","d_host_greets_you",
            "d_host_is_superhost","d_host_identity_verified","flag_host_acceptance_rate",
            "flag_host_response_rate","flag_host_response_time")
ammenities <- c("d_bath_tub","d_building_staff","d_carbon_monoxide_alarm","d_cleaning_products","d_dining_table",
                "d_drying_rack_for_clothing","d_elevator","d_essentials","d_extra_pillows_and_blankets",
                "d_fire_extinguisher","d_have_first_aid","d_hangers","d_hot_water","d_hot_water_kettle",
                "d_laundromat_nearby","d_microwave","d_outdoor_dining_area","d_outdoor_furniture",
                "d_private_entrance","d_roomdarkening_shades","d_smart_lock","d_security_cameras_on_property",
                "d_have_pool","d_smoke_alarm","d_have_kitchen","d_have_stove","d_have_oven","d_refrigerator",
                "d_coffee_machine","d_wifi","d_bbq_equipment","d_tv","d_have_iron","d_have_heating","d_cooling",
                "d_balcony","d_garden","d_have_breakfast","d_have_workspace","d_family_friendly",
                "d_luggage_dropoff_allowed","d_single_level_home","d_bathroom_essentials","d_free_parking",
                "d_paid_parking", "d_linens","d_streaming_services","d_shampoo_conditioner", "d_body_wash",
                "d_have_washer","d_have_dryer","d_clothing_storage","d_cutlary_glasses")

df <- data
# Checking interactions
price_diff_by_variables4 <- function(df, factor_var, dummy_var, factor_lab, dummy_lab){ 
  # Looking for interactions.
  # It is a function it takes 3 arguments: 1) Your dataframe,
  # 2) the factor variable (like room_type)
  # 3)the dummy variable you are interested in (like TV)
  
  # Process your data frame and make a new dataframe which contains the stats
  factor_var <- as.name(factor_var)
  dummy_var <- as.name(dummy_var)
  
  stats <- df %>%
    group_by(!!factor_var, !!dummy_var) %>%
    dplyr::summarize(Mean = mean(price, na.rm=TRUE),
                     se = sd(price)/sqrt(n()))
  
  stats[,2] <- lapply(stats[,2], factor)
  
  ggplot(stats, aes_string(colnames(stats)[1], colnames(stats)[3], fill = colnames(stats)[2]))+
    geom_bar(stat='identity', position = position_dodge(width=0.9), alpha=0.8)+
    geom_errorbar(aes(ymin=Mean-(1.96*se),ymax=Mean+(1.96*se)),
                  position=position_dodge(width = 0.9), width = 0.25)+
    scale_color_manual(name=dummy_lab,
                       values=c(color[2],color[1],color[3],color[4])) +
    scale_fill_manual(name=dummy_lab,
                      values=c(color[2],color[1],color[3],color[4])) +
    ylab('Mean Price')+
    xlab(factor_lab) +
    theme_bg()+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          axis.line=element_line(),
          legend.position = "top",
          #legend.position = c(0.7, 0.9),
          legend.box = "vertical",
          legend.text = element_text(size = 5),
          legend.title = element_text(size = 5, face = "bold"),
          legend.key.size = unit(x = 0.4, units = "cm")
    )
}
# Plot interactions between room type/property type and all dummies 
sapply(ammenities, function(x){
  p <- price_diff_by_variables4(df, "f_property_type", x, "property_type", x)
  print(p)
})


# Based on individual box plot for each amenity with property type, following will be interacted with property type
interactions <- c("f_property_type*d_bath_tub",
                  "f_property_type*d_building_staff",
                  "f_property_type*d_elevator",
                  "f_property_type*d_extra_pillows_and_blankets",
                  "f_property_type*d_fire_extinguisher",
                  "f_property_type*d_hangers",
                  "f_property_type*d_hot_water_kettle",
                  "f_property_type*d_laundromat_nearby",
                  "f_property_type*d_outdoor_dining_area",
                  "f_property_type*d_outdoor_furniture",
                  "f_property_type*d_private_entrance",
                  "f_property_type*d_smart_lock",
                  "f_property_type*d_security_cameras_on_property",
                  "f_property_type*d_have_kitchen",
                  "f_property_type*d_refrigerator",
                  "f_property_type*d_coffee_machine",
                  "f_property_type*d_wifi",
                  "f_property_type*d_bbq_equipment",
                  "f_property_type*d_tv",
                  "f_property_type*d_have_iron",
                  "f_property_type*d_have_heating",
                  "f_property_type*d_cooling",
                  "f_property_type*d_balcony",
                  "f_property_type*d_garden",
                  "f_property_type*d_have_workspace",
                  "f_property_type*d_family_friendly",
                  "f_property_type*d_bathroom_essentials",
                  "f_property_type*d_linens",
                  "f_property_type*d_body_wash")



#################################
# Create test and train samples #
#################################
# now all stuff runs on training vs test (holdout), alternative: 4-fold CV
# create test and train samples (70% of observations in train sample)
smp_size <- floor(0.7 * nrow(data))
## K = 5
k_folds <- 5
# Define seed value
seed_val <- 200
train_ids <- sample(seq_len(nrow(data)), size = smp_size)
data$train <- 0
data$train[train_ids] <- 1
# Create train and test sample variables
data_train <- data %>% filter(train == 1)
data_test <- data %>% filter(train == 0)                  
                  
                  
  
#Bulding the most complex model to use in LASSO
model4 <- paste0(" ~ ",paste(c(basic_lev, reviews, host, ammenities, interactions),collapse = " + "))                
                  
# Creating the most complex OLS model to run a LASSO. Here LASSO is being used as a tool to choose predictors
# Set lasso tuning parameters:
# a) basic setup
train_control <- trainControl( method = "cv", number = k_folds)
# b) tell the actual lambda (penalty parameter) to use for lasso
tune_grid     <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))
# c) create a formula
formula <- formula(paste0("price ", paste(setdiff(model4, "price"), collapse = " + ")))
# Run LASSO
set.seed(seed_val)
lasso_model <- caret::train(formula,
                            data = data_train,
                            method = "glmnet",
                            preProcess = c("center", "scale"),
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            na.action=na.exclude)
# Check the output                   
lasso_model
# Penalty parameters
lasso_model$bestTune
# Check th optimal lambda parameter
lasso_model$bestTune$lambda
# Check the RMSE curve
plot(lasso_model)


# One can get the coefficients as well
lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `s1`)  # the column has a name "1", to be renamed
print(lasso_coeffs)


# Check the number of variables which actually has coefficients other than 0
lasso_coeffs_nz<-lasso_coeffs %>%
  filter(coefficient!=0)
print(nrow(lasso_coeffs_nz))
print(lasso_coeffs_nz)


#write_csv(lasso_coeffs_nz,"NonZeroCoefficients.csv")

# Get the RMSE of the Lasso model 
#   Note you should compare this to the test RMSE
lasso_fitstats <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) 
lasso_fitstats

# Create an auxilary tibble
lasso_add <- tibble(Model='LASSO', Coefficients=nrow(lasso_coeffs_nz),
                    R_squared=lasso_fitstats$Rsquared, BIC = NA, 
                    Training_RMSE = NA, Test_RMSE = lasso_fitstats$RMSE )


# modifying the list of variables to be used based on LASSO results 
basic_lev <- c("f_property_type","n_accommodates","f_neighbourhood_cleansed","f_bathroom",
               "f_bedroom","d_flag_bedrooms","f_minimum_nights")
host <- c("f_host_response_time","p_host_response_rate","p_host_acceptance_rate","d_host_greets_you", #"d_host_identity_verified",
          "d_host_is_superhost","flag_host_acceptance_rate","flag_host_response_rate","flag_host_response_time")
reviews <- c("f_review_score_rating","flag_review_scores_rating","ln_days_since_first_review","ln_number_of_reviews",
             "flag_days_since_first_review")
ammenities <- c("d_carbon_monoxide_alarm","d_elevator","d_fire_extinguisher","d_hangers",
                "d_hot_water_kettle","d_microwave","d_outdoor_furniture","d_private_entrance",
                "d_smart_lock","d_security_cameras_on_property","d_have_pool","d_smoke_alarm","d_have_oven",
                "d_coffee_machine","d_wifi","d_tv","d_have_iron","d_have_heating", "d_cooling",  
                "d_have_breakfast",  "d_free_parking","d_family_friendly", 
                "d_shampoo_conditioner","d_have_washer","d_have_dryer", "d_clothing_storage",
                "d_cutlary_glasses", "d_laundromat_nearby","d_have_kitchen","d_refrigerator","d_garden",
                "d_bathroom_essentials")
interactions <- c("f_property_type*d_bath_tub",
                  "f_property_type*d_building_staff",
                  "f_property_type*d_elevator",
                  "f_property_type*d_extra_pillows_and_blankets",
                  "f_property_type*d_fire_extinguisher",
                  "f_property_type*d_hangers",
                  "f_property_type*d_hot_water_kettle",
                  "f_property_type*d_laundromat_nearby",
                  "f_property_type*d_outdoor_dining_area",
                  "f_property_type*d_outdoor_furniture",
                  "f_property_type*d_private_entrance",
                  "f_property_type*d_security_cameras_on_property",
                  "f_property_type*d_have_kitchen",
                  "f_property_type*d_coffee_machine",
                  "f_property_type*d_bbq_equipment",
                  "f_property_type*d_balcony",
                  "f_property_type*d_garden",
                  "f_property_type*d_family_friendly",
                  "f_property_type*d_bathroom_essentials",
                  "f_property_type*d_body_wash",
                  "f_property_type*d_have_iron",
                  "f_property_type*d_cooling",
                  "f_property_type*d_have_workspace",
                  "f_property_type*d_linens")


# Building OLS models
model1 <- " ~ n_accommodates"
model2 <- paste0(" ~ ",paste(basic_lev,collapse = " + "))
model3 <- paste0(" ~ ",paste(c(basic_lev, reviews, host, ammenities),collapse = " + "))



##############################
#   cross validation OLS    #
##############################


# Do the iteration
library(fixest)
for ( i in 1:4 ){
  print(paste0( "Estimating model: " ,i ))
  # Get the model name
  model_name <-  paste0("model",i)
  model_pretty_name <- paste0("M",i,"")
  # Specify the formula
  yvar <- "price"
  xvars <- eval(parse(text = model_name))
  formula <- formula(paste0(yvar,xvars))
  
  # Estimate model on the whole sample
  model_work_data <- feols( formula , data = data_train , vcov='hetero' )
  #  and get the summary statistics
  fs  <- fitstat(model_work_data,c('rmse','r2','bic'))
  BIC <- fs$bic
  r2  <- fs$r2
  rmse_train <- fs$rmse
  ncoeff <- length( model_work_data$coefficients )
  
  # Do the k-fold estimation
  set.seed(seed_val)
  cv_i <- train( formula, data_train, method = "lm", 
                 trControl = trainControl(method = "cv", number = k_folds))
  rmse_test <- mean( cv_i$resample$RMSE )
  
  # Save the results
  model_add <- tibble(Model=model_pretty_name, Coefficients=ncoeff,
                      R_squared=r2, BIC = BIC, 
                      Training_RMSE = rmse_train, Test_RMSE = rmse_test )
  if ( i == 1 ){
    model_results <- model_add
  } else{
    model_results <- rbind( model_results , model_add )
  }
}

# Check summary table
# Add it to final results

model_results <- rbind( model_results , lasso_add )
model_results

# Based on the results, model4 is clearly over fitted. The table shows that  R-squared comes out to be 1 with a 
# negative BIC. Model4 was primarily used as a model to be used in LASSO to identify 
# predictors with non-zero coefficients. Therefor, I decided to add all relevant variables.

#predictors_model3 <- c(basic_lev, reviews, host, ammenities)
predictors_model2 <- c(basic_lev)


set.seed(200)
system.time({
  ols_model <- train(
    formula(paste0("price ~ ", paste0(predictors_model2, collapse = " + "))),
    data = data_train,
    method = "lm",
    trControl = train_control
  )
})
ols_model_coeffs <-  ols_model$finalModel$coefficients
ols_model_coeffs_df <- data.frame(
  "variable" = names(ols_model_coeffs),
  "ols_coefficient" = ols_model_coeffs
) %>%
  mutate(variable = gsub("`","",variable))



##################
## Random Forest##
##################

predictors <- c(basic_lev, host, reviews, ammenities, interactions)
# set tuning 
tune_grid <- expand.grid(
  .mtry = c(8, 10, 12),
  .splitrule = "variance",
  .min.node.size = c(5, 10, 15)
)
set.seed(1200)
system.time({
  rf_model <- train(
    formula(paste0(" price ~ ", paste0(predictors, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity",
    .num.trees=500
  )
})
rf_model
# auto tuning random forest 
set.seed(1200)
system.time({
  rf_model_auto <- train(
    formula(paste0("price ~", paste0(predictors, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    importance = "impurity",
    .num.trees=500
  )
})
rf_model_auto

##############################
# Variable Importance Plots  #
##############################


rf_model_var_imp <- ranger::importance(rf_model$finalModel)/1000
rf_model_var_imp_df <-
  data.frame(varname = names(rf_model_var_imp),imp = rf_model_var_imp) %>%
  mutate(varname = gsub("f_neighbourhood_cleansed", "Borough:", varname) ) %>%
  mutate(varname = gsub("f_room_type", "Room type:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))
rf_model_var_imp_df

# to have a quick look
plot(varImp(rf_model))

# have a version with top 10 vars only
ggplot(rf_model_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color='red', size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color='red', size=0.75) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()


##############################
# 2) varimp plot grouped
##############################
# grouped variable importance - keep binaries created off factors together

varnames <- rf_model$finalModel$xNames


f_neighbourhood_cleansed <- grep("f_neighbourhood_cleansed",varnames, value = TRUE)
f_host_varnames <- grep(("host"),varnames, value = TRUE)
#f_host_varnames <- f_host_varnames[-(9:10)] #(removing the host related flag variables)
f_property_type_varnames <- grep("f_property_type",varnames, value = TRUE)
f_reviews_varnames <- grep("review",varnames, value = TRUE)

#amenities_varnames <- c("d_bath_tub", "d_cleaning_products","d_elevator", "d_fire_extinguisher","d_hangers",
 #                       "d_hot_water", "d_hot_water_kettle", "d_microwave","d_outdoor_furniture", 
  #                      "d_private_entrance","d_smart_lock","d_security_cameras_on_property","d_have_pool", 
   #                     "d_smoke_alarm", "d_have_oven", "d_coffee_machine","d_wifi" ,"d_tv","d_have_iron",
    #                    "d_have_heating", "d_cooling", "d_balcony", "d_have_breakfast", "d_have_workspace",                                                
     #                   "d_luggage_dropoff_allowed", "d_free_parking", "d_paid_parking" , "d_streaming_services",                                            
      #                  "d_shampoo_conditioner", "d_have_washer" ,"d_have_dryer" ,"d_cutlary_glasses",
       #                 "d_building_staff", "d_laundromat_nearby", "d_outdoor_dining_area","d_have_kitchen",                                                  
        #                "d_refrigerator","d_bbq_equipment", "d_garden", "d_family_friendly","d_bathroom_essentials",                                           
         #               "d_body_wash")

amenities_varnames <- c("d_carbon_monoxide_alarm","d_elevator","d_fire_extinguisher","d_hangers", "d_hot_water_kettle",                                              
                        "d_microwave", "d_outdoor_furniture", "d_private_entrance", "d_smart_lock", "d_security_cameras_on_property",                                  
                        "d_have_pool", "d_smoke_alarm", "d_have_oven", "d_coffee_machine", "d_wifi", "d_tv", "d_have_iron",                                                     
                        "d_have_heating", "d_cooling", "d_have_breakfast", "d_free_parking" , "d_family_friendly" ,                                              
                        "d_shampoo_conditioner", "d_have_washer", "d_have_dryer", "d_clothing_storage","d_cutlary_glasses",                                               
                        "d_laundromat_nearby","d_have_kitchen","d_refrigerator","d_garden", "d_bathroom_essentials" ,                                          
                        "d_bath_tub", "d_building_staff", "d_extra_pillows_and_blankets", "d_outdoor_dining_area",                                           
                        "d_bbq_equipment", "d_balcony" , "d_body_wash", "d_have_workspace", "d_linens" )


groups <- list(Neighbourhood = f_neighbourhood_cleansed,
               Host_Related=f_host_varnames,
               Property_Type = f_property_type_varnames,
               Reviews = f_reviews_varnames,
               Amenities = amenities_varnames,
               Bathrooms = "f_bathroom",
               Bedrooms = "f_bedroom",
               Minimum_Nights = "f_minimum_nights",
               Number_Accommodates = "n_accommodates")



# Need a function to calculate grouped varimp
group.importance <- function(rf.obj, groups) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(ranger::importance(rf.obj)[g], na.rm = TRUE)
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}

rf_model_var_imp_grouped <- group.importance(rf_model$finalModel, groups)
rf_model_var_imp_grouped_df <- data.frame(varname = rownames(rf_model_var_imp_grouped),
                                          imp = rf_model_var_imp_grouped[,1]) %>% 
                                      mutate(imp_percentage = imp/sum(imp))


ggplot(rf_model_var_imp_grouped_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color='red', size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color='red', size=0.7) +
  ylab("Importance (Percent)") +   xlab("Variable Name") +
  coord_flip() +
  # expand=c(0,0),
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()


##Variable Importance Plots rf_model_auto

rf_model_auto_var_imp <- ranger::importance(rf_model_auto$finalModel)/1000
rf_model_auto_var_imp_df <-
  data.frame(varname = names(rf_model_auto_var_imp),imp = rf_model_var_imp) %>%
  mutate(varname = gsub("f_neighbourhood_cleansed", "Neighbourhood:", varname) ) %>%
  mutate(varname = gsub("f_property_type", "Property type:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))
rf_model_auto_var_imp_df

# to have a quick look

plot(varImp(rf_model_auto))

# have a version with top 10 vars only

ggplot(rf_model_auto_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color='red', size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color='red', size=0.75) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()



##############################
# 2) varimp plot grouped
##############################
# grouped variable importance - keep binaries created off factors together
varnames_auto <- rf_model_auto$finalModel$xNames


f_neighbourhood_cleansed_auto <- grep("f_neighbourhood_cleansed",varnames, value = TRUE)
f_host_varnames_auto <- grep(("host"),varnames, value = TRUE)
f_property_type_varnames_auto <- grep("f_property_type",varnames, value = TRUE)
f_reviews_varnames_auto <- grep("review",varnames, value = TRUE)
amenities_varnames_auto <- c("d_carbon_monoxide_alarm","d_elevator","d_fire_extinguisher","d_hangers", "d_hot_water_kettle",                                              
                             "d_microwave", "d_outdoor_furniture", "d_private_entrance", "d_smart_lock", "d_security_cameras_on_property",                                  
                             "d_have_pool", "d_smoke_alarm", "d_have_oven", "d_coffee_machine", "d_wifi", "d_tv", "d_have_iron",                                                     
                             "d_have_heating", "d_cooling", "d_have_breakfast", "d_free_parking" , "d_family_friendly" ,                                              
                             "d_shampoo_conditioner", "d_have_washer", "d_have_dryer", "d_clothing_storage","d_cutlary_glasses",                                               
                             "d_laundromat_nearby","d_have_kitchen","d_refrigerator","d_garden", "d_bathroom_essentials" ,                                          
                             "d_bath_tub", "d_building_staff", "d_extra_pillows_and_blankets", "d_outdoor_dining_area",                                           
                             "d_bbq_equipment", "d_balcony" , "d_body_wash", "d_have_workspace", "d_linens")

groups_auto <- list(Neighbourhood = f_neighbourhood_cleansed,
               Host_Related=f_host_varnames,
               Property_Type = f_property_type_varnames,
               Reviews = f_reviews_varnames,
               Amenities = amenities_varnames,
               Bathrooms = "f_bathroom",
               Bedrooms = "f_bedroom",
               Minimum_Nights = "f_minimum_nights",
               Number_Accommodates = "n_accommodates")



# Need a function to calculate grouped varimp
group.importance <- function(rf.obj, groups_auto) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(ranger::importance(rf.obj)[g], na.rm = TRUE)
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}
rf_model_auto_var_imp_grouped <- group.importance(rf_model_auto$finalModel, groups)
rf_model_auto_var_imp_grouped_df <- data.frame(varname = rownames(rf_model_auto_var_imp_grouped),
                                               imp = rf_model_auto_var_imp_grouped[,1])  %>%
                                                mutate(imp_percentage = imp/sum(imp))
ggplot(rf_model_auto_var_imp_grouped_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color='red', size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color='red', size=0.7) +
  ylab("Importance (Percent)") +   xlab("Variable Name") +
  coord_flip() +
  # expand=c(0,0),
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()


# evaluate random forests 
results <- resamples(
  list(
    model_1  = rf_model,
    model_auto  = rf_model_auto
  )
)
summary(results)


# CART with pruning
# CART with built-in pruning
set.seed(1335)
system.time({
  cart_model <- train(
    formula(paste0("price ~", paste0(predictors, collapse = " + "))),
    data = data_train,
    method = "rpart",
    tuneLength = 10,
    trControl = train_control
  )
})
cart_model

library(rpart)
library(rpart.plot)

# Tree graph
rpart.plot(cart_model$finalModel, tweak=1.2, digits=-1, extra=1)


# GBM
gbm_grid <-  expand.grid(interaction.depth = 5, # complexity of the tree
                         n.trees = 250, # number of iterations, i.e. trees
                         shrinkage = 0.1, # learning rate: how quickly the algorithm adapts
                         n.minobsinnode = 20 # the minimum number of training set samples in a node to commence splitting
)
set.seed(109)
system.time({
  gbm_model <- train(formula(paste0("price ~", paste0(predictors, collapse = " + "))),
                     data = data_train,
                     method = "gbm",
                     trControl = train_control,
                     verbose = FALSE,
                     tuneGrid = gbm_grid)
})
gbm_model
gbm_model$finalModel



# get prediction rmse and add to next summary table
# ---- compare these models

final_models <-
  list("OLS" = ols_model,
       "CART" = cart_model,
       "Random forest 1: Tuning provided" = rf_model,
       "Random forest 2: Auto Tuning" = rf_model_auto,
       "GBM"  = gbm_model)
results <- resamples(final_models) %>% summary()
results



# Model selection is carried out on this CV RMSE

result <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")
result


# evaluate preferred model on the holdout set -----------------------------
result_2 <- map(final_models, ~{
  RMSE(predict(.x, newdata = data_test), data_test[["price"]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("Holdout RMSE" = ".")
result_2



#########################################################################################
# Partial Dependence Plots for the best model; random forest  with auto tuning parameters
#########################################################################################

# 1) Property Type
pdp_f_property_type <- pdp::partial(rf_model_auto, pred.var = "f_property_type", 
                                    pred.grid = distinct_(data_test, "f_property_type"), 
                                    train = data_train)
pdp_f_property_type %>%
  autoplot( ) +
  geom_point(color='red', size=2) +
  geom_line(color='red', size=1) +
  ylab("Predicted price") +
  xlab("Property Type") +
  theme_bw()


# 2) Number of accommodates
pdp_n_accommodates <- pdp::partial(rf_model_auto, pred.var = "n_accommodates", 
                                   pred.grid = distinct_(data_test, "n_accommodates"), 
                                   train = data_train)
pdp_n_accommodates %>%
  autoplot( ) +
  geom_point(color='red', size=4) +
  ylab("Predicted price") +
  xlab("Accommodates (persons)") +
  scale_y_continuous(limits=c(125,150), breaks=seq(125,150, by=10)) +
  theme_bw()

# 3) Neighborhood Cleansed
pdp_f_neighbourhood_cleansed <- pdp::partial(rf_model_auto, pred.var = "f_neighbourhood_cleansed", 
                                   pred.grid = distinct_(data_test, "f_neighbourhood_cleansed"), 
                                   train = data_train)
pdp_f_neighbourhood_cleansed %>%
  autoplot( ) +
  geom_point(color='red', size=4) +
  ylab("Predicted price") +
  xlab("Neighbourhoods") +
  theme_bw()



####
# Subsample performance: RMSE / mean(y) ---------------------------------------
# NOTE  we do this on the holdout set.
# 
data_holdout_w_prediction <- data_test %>%
  mutate(predicted_price = predict(rf_model_auto, newdata = data_test))


######### create nice summary table of heterogeneity
a <- data_holdout_w_prediction %>%
  mutate(is_low_size = ifelse(n_accommodates <= 3, "small apt", "large apt")) %>%
  group_by(is_low_size) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )

data_holdout_w_prediction %>% 
  group_by(f_neighbourhood_cleansed) %>% 
  summarise(cnt = n())

b <- data_holdout_w_prediction %>%
  filter(f_municipality %in% c("Heraklion", "Khania",
                               "Lasithi", "Rethymnon")) %>%
  group_by(f_municipality) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )
c <- data_holdout_w_prediction %>%
  filter(n_beds %in% c("2","3", "4","5","6","7","8")) %>%
  group_by(n_beds) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )
d <- data_holdout_w_prediction %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )
e <- data_holdout_w_prediction %>%
  filter(f_property_type %in% c("Entire loft", "Entire serviced apartment")) %>%
  group_by(f_property_type) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )

