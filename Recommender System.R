setwd("~/Downloads")

library(tidyverse)
library(readr)
library(readxl)
library(recommenderlab)
library(reshape2)

data = read_xlsx("EXAMPLE_BEVERAGE_DATA_THAILAND.xlsx")
unique(data$BRAND_E1)
unique(data$FLAVOUR)
# since both are the most abundant. we'll try with both.

length(unique(data$HOUSEID)) # number of unique House ID's

clean_data = data %>% filter(channel == 'Hypermarkets') %>% select(HOUSEID, BRAND_E1, FLAVOUR)

# Recommending BRANDS
# what and how many of each product do each household buy
brand_data = clean_data %>% 
  group_by(HOUSEID, BRAND_E1) %>% 
  summarise(b = n()) %>% 
  spread(key = BRAND_E1, value = b) %>% 
  as.matrix() %>%
  as("realRatingMatrix")

# using User Based Collaborative Filtering, we can find recommendations for each HouseID
brand_reco_model = Recommender(brand_data, method = "UBCF")

# churn out a top 5 brand list for each HouseID
brand_top5 = predict(brand_reco_model, brand_data, type = "topNList", n = 5)
brand_top5_list = as(brand_top5, "list")

# running and evaluation on the recommender system
brand_evalscheme = evaluationScheme(brand_data, method = "split", train = 0.9, given = -1)
brand_reco_model_test = Recommender(getData(brand_evalscheme, "train"), "UBCF") 
predict_test_brand = predict(brand_reco_model_test, getData(brand_evalscheme, "known"), type = "ratings")
calcPredictionAccuracy(predict_test_brand, getData(brand_evalscheme , "unknown"))

houseid_brand_acc = calcPredictionAccuracy(predict_test_brand, getData(brand_evalscheme, "unknown"), byUser = TRUE)
# We can also see the metrics for each individual user that was in the test set:
houseid_brand_acc

# Recommending FLAVOUR
flavour_data = clean_data %>% 
  group_by(HOUSEID, FLAVOUR) %>% 
  summarise(f = n()) %>% 
  spread(key = FLAVOUR, value = f) %>% 
  as.matrix() %>%
  as("realRatingMatrix")

# using User Based Collaborative Filtering, we can find recommendations for each HouseID
flav_reco_model = Recommender(flavour_data, method = "UBCF")

# churn out a top 5 flavour list for each HouseID
flav_top5 = predict(flav_reco_model, flavour_data, type = "topNList", n = 5)
flav_top5_list = as(flav_top5, "list")

# evaluation of the recommender model
flav_evalscheme = evaluationScheme(flavour_data, method = "split", train = 0.9, given = -1)
flav_reco_model_test = Recommender(getData(flav_evalscheme, "train"), "UBCF") 
predict_test_flav = predict(flav_reco_model_test, getData(flav_evalscheme, "known"), type = "ratings")
calcPredictionAccuracy(predict_test_flav, getData(flav_evalscheme , "unknown"))

houseid_flav_acc = calcPredictionAccuracy(predict_test_flav, getData(flav_evalscheme, "unknown"), byUser = TRUE)
# We can also see the metrics for each individual user that was in the test set:
houseid_flav_acc


# recommendations for BRAND/FLAVOUR for new users based on the popular choice based on multiple variables
clean_data_2 = data %>% filter(channel == 'Hypermarkets') %>% select(HOUSEID, BRAND_E1, FLAVOUR, DEMOG_AREA:DEMOG_WORKINGSTATUS)

demo_income_flav = clean_data_2 %>% group_by(DEMOG_HHINCOME, FLAVOUR) %>% summarise(f = n()) %>% spread(key = FLAVOUR, value = f) %>% select(-'N/A')

popularchoice = function(dat, n, product_feature, demog_feature) { # change this, demog and product
  for (i in 1:n) {
    m = max(as.numeric(dat[i,]), na.rm = TRUE)
    w = which(as.numeric(dat[i,]) == m)
    choice = colnames(dat[, w])
    print(paste("For", as.character(demog_feature), "the popular choice for", as.character(product_feature), "under", as.character(dat[[1]][i]), "is", as.character(choice)))
  }
}


##################################### Flavour Recommendations based on ...
# Area
demog_product_data = clean_data_2 %>% 
  group_by(DEMOG_AREA, FLAVOUR) %>% # change this, demog and product
  summarise(x = n()) %>% 
  spread(key = FLAVOUR, value = x) %>%  # change this, product
  select(-'N/A')
n = length(unique(clean_data_2$DEMOG_AREA)) # change this, demog
popularchoice(demog_product_data, n, 'flavour', 'demog_area')

# Household Income
demog_product_data = clean_data_2 %>% 
  group_by(DEMOG_HHINCOME, FLAVOUR) %>% # change this, demog and product
  summarise(x = n()) %>% 
  spread(key = FLAVOUR, value = x) %>%  # change this, product
  select(-'N/A')
n = length(unique(clean_data_2$DEMOG_HHINCOME)) # change this, demog
popularchoice(demog_product_data, n, 'flavour', 'demog_hhincome') # change this, demog and product

# Household Size
demog_product_data = clean_data_2 %>% 
  group_by(DEMOG_HHSIZE, FLAVOUR) %>% # change this, demog and product
  summarise(x = n()) %>% 
  spread(key = FLAVOUR, value = x) %>%  # change this, product
  select(-'N/A')
n = length(unique(clean_data_2$DEMOG_HHSIZE)) # change this, demog
popularchoice(demog_product_data, n, 'flavour', 'demog_hhsize') # change this, demog and product

# Life Stage
demog_product_data = clean_data_2 %>% 
  group_by(DEMOG_LIFESTAGE, FLAVOUR) %>% # change this, demog and product
  summarise(x = n()) %>% 
  spread(key = FLAVOUR, value = x) %>%  # change this, product
  select(-'N/A')
n = length(unique(clean_data_2$DEMOG_LIFESTAGE)) # change this, demog
popularchoice(demog_product_data, n, 'flavour', 'demog_lifestage') # change this, demog and product

# Housewife Age
demog_product_data = clean_data_2 %>% 
  group_by(DEMOG_HWAGE, FLAVOUR) %>% # change this, demog and product
  summarise(x = n()) %>% 
  spread(key = FLAVOUR, value = x) %>%  # change this, product
  select(-'N/A')
n = length(unique(clean_data_2$DEMOG_HWAGE)) # change this, demog
popularchoice(demog_product_data, n, 'flavour', 'demog_housewifeage') # change this, demog and product

# Region
demog_product_data = clean_data_2 %>% 
  group_by(DEMOG_REGION, FLAVOUR) %>% # change this, demog and product
  summarise(x = n()) %>% 
  spread(key = FLAVOUR, value = x) %>%  # change this, product
  select(-'N/A')
n = length(unique(clean_data_2$DEMOG_REGION)) # change this, demog
popularchoice(demog_product_data, n, 'flavour', 'demog_region') # change this, demog and product

# Working Status
demog_product_data = clean_data_2 %>% 
  group_by(DEMOG_WORKINGSTATUS, FLAVOUR) %>% # change this, demog and product
  summarise(x = n()) %>% 
  spread(key = FLAVOUR, value = x) %>%  # change this, product
  select(-'N/A')
n = length(unique(clean_data_2$DEMOG_WORKINGSTATUS)) # change this, demog
popularchoice(demog_product_data, n, 'flavour', 'demog_workingstatus') # change this, demog and product

####################################### Brands recommendations based on ...
# Area
demog_product_data = clean_data_2 %>% 
  group_by(DEMOG_AREA, BRAND_E1) %>% # change this, demog and product
  summarise(x = n()) %>% 
  spread(key = BRAND_E1, value = x) %>%  # change this, product
  select(-'OTHERS BRAND')
n = length(unique(clean_data_2$DEMOG_AREA)) # change this, demog
popularchoice(demog_product_data, n, 'brand', 'demog_area')

# Household Income
demog_product_data = clean_data_2 %>% 
  group_by(DEMOG_HHINCOME, BRAND_E1) %>% # change this, demog and product
  summarise(x = n()) %>% 
  spread(key = BRAND_E1, value = x) %>%  # change this, product
  select(-'OTHERS BRAND')
n = length(unique(clean_data_2$DEMOG_HHINCOME)) # change this, demog
popularchoice(demog_product_data, n, 'brand', 'demog_hhincome') # change this, demog and product

# Household Size
demog_product_data = clean_data_2 %>% 
  group_by(DEMOG_HHSIZE, BRAND_E1) %>% # change this, demog and product
  summarise(x = n()) %>% 
  spread(key = BRAND_E1, value = x) %>%  # change this, product
  select(-'OTHERS BRAND')
n = length(unique(clean_data_2$DEMOG_HHSIZE)) # change this, demog
popularchoice(demog_product_data, n, 'brand', 'demog_hhsize') # change this, demog and product

# Life Stage
demog_product_data = clean_data_2 %>% 
  group_by(DEMOG_LIFESTAGE, BRAND_E1) %>% # change this, demog and product
  summarise(x = n()) %>% 
  spread(key = BRAND_E1, value = x) %>%  # change this, product
  select(-'OTHERS BRAND')
n = length(unique(clean_data_2$DEMOG_LIFESTAGE)) # change this, demog
popularchoice(demog_product_data, n, 'brand', 'demog_lifestage') # change this, demog and product

# Housewife Age
demog_product_data = clean_data_2 %>% 
  group_by(DEMOG_HWAGE, BRAND_E1) %>% # change this, demog and product
  summarise(x = n()) %>% 
  spread(key = BRAND_E1, value = x) %>%  # change this, product
  select(-'OTHERS BRAND')
n = length(unique(clean_data_2$DEMOG_HWAGE)) # change this, demog
popularchoice(demog_product_data, n, 'brand', 'demog_housewifeage') # change this, demog and product

# Region
demog_product_data = clean_data_2 %>% 
  group_by(DEMOG_REGION, BRAND_E1) %>% # change this, demog and product
  summarise(x = n()) %>% 
  spread(key = BRAND_E1, value = x) %>%  # change this, product
  select(-'OTHERS BRAND')
n = length(unique(clean_data_2$DEMOG_REGION)) # change this, demog
popularchoice(demog_product_data, n, 'brand', 'demog_region') # change this, demog and product

# Working Status
demog_product_data = clean_data_2 %>% 
  group_by(DEMOG_WORKINGSTATUS, BRAND_E1) %>% # change this, demog and product
  summarise(x = n()) %>% 
  spread(key = BRAND_E1, value = x) %>%  # change this, product
  select(-'OTHERS BRAND')
n = length(unique(clean_data_2$DEMOG_WORKINGSTATUS)) # change this, demog
popularchoice(demog_product_data, n, 'brand', 'demog_workingstatus') # change this, demog and product


