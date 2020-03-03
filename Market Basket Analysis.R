# Market Basket Analysis

library(tidyverse) # already includes tidyr, readr, stringr
library(readxl)
library(arules)
library(arulesViz)

data = read_xlsx("EXAMPLE_BEVERAGE_DATA_THAILAND.xlsx")

summary(data)

shopping_cart = cleaner_data %>% 
  filter(channel == "Hypermarkets") %>% 
  filter(DEMOG_REGION == "Greater Bangkok") %>% 
  group_by(HOUSEID, realdate) %>% 
  summarise(cart = list(unique(BRAND_E1)))

rules = apriori(shopping_cart$cart, parameter = list(support = 0.005, confidence = 0.005, minlen = 2))
x = inspect(sort(rules, by= "lift"))
x[x$lift > 1,]
plot(rules, method = 'graph')
