setwd("/Users/jaoming/Downloads")
library(tidyverse) # already includes tidyr, readr, stringr
library(readxl)

data = read_xlsx("EXAMPLE_BEVERAGE_DATA_THAILAND.xlsx")

# number of unique values in each column
data %>% 
  filter(channel == "Hypermarkets") %>% 
  summarise_all(n_distinct)
# number of unique storenames
data %>% 
  filter(channel == "Hypermarkets") %>% 
  select(STORENAME) %>% 
  distinct()

# cleaning of data
clean_data = data %>% 
  select(HOUSEID, 
         date1, 
         date2, 
         Date3, 
         weekday, 
         channel, 
         category, 
         STORENAME, 
         spend, 
         DEMOG_REGION, 
         BRAND_E1, 
         packdesc, 
         PACKAGE)

# data with the demograghics
demo_data = data %>% 
  select(HOUSEID, 
         date1, 
         date2, 
         Date3, 
         weekday, 
         channel, 
         DEMOG_HHINCOME:DEMOG_WORKINGSTATUS)

clean_data$spend = as.double(clean_data$spend)
cleaner_data = clean_data %>% 
  mutate(Date_1 = as.Date(sapply(str_split(clean_data$date1, pattern = " w/e "), FUN = function(x) {x[2]})), 
         Date_2 = as.Date(sapply(str_split(clean_data$date2, pattern = " w/e "), FUN = function(x) {x[2]})), 
         Date_3 = as.Date(Date3)) %>%
  select(HOUSEID, Date_1, Date_2, Date_3, spend, weekday, DEMOG_REGION, BRAND_E1, channel, category)

cleaner_data$weekday = factor(cleaner_data$weekday, 
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 
                              ordered = TRUE)

cleaner_data = cleaner_data %>% 
  mutate(realdate = Date_1 + as.integer(weekday) - 7) %>%  # getting the real date
  select(HOUSEID, realdate, spend, DEMOG_REGION, BRAND_E1, channel, category)

# RFM (Recency Frequency Monetary)
# Finding out how many transactions were made per House ID
total_visits = cleaner_data %>% 
  group_by(HOUSEID) %>%
  summarise(TotalVisits = n()) %>%
  arrange(desc(TotalVisits)) # this will show the top 10 House ID's that purchase this product the most

ggplot(total_visits, aes(TotalVisits)) +
  geom_histogram(binwidth = 100)

# Finding out how much each House ID spends
total_spending = cleaner_data %>% 
  group_by(HOUSEID) %>% 
  summarise(TotalSpending = sum(spend)) %>% 
  arrange(desc(TotalSpending))

# plot
ggplot(total_spending, aes(TotalSpending)) +
  geom_histogram(binwidth = 2500)

# Finding out how long it has been since the family bought the product (taking reference from the start of 2019)
ReferenceDate = as.Date("2019/01/01") # since this date is far later than the ones shown in the file
last_visit = cleaner_data %>%
  mutate(recency = abs(realdate - ReferenceDate)) %>%
  group_by(HOUSEID) %>%
  summarise(Recency = min(recency)) %>% 
  arrange(Recency)

# plot
ggplot(last_visit, aes(Recency)) +
  geom_histogram(binwidth = 50)

## Ranking for RFM
number_of_houses = length(unique(cleaner_data$HOUSEID))

last_visit = last_visit %>% arrange(Recency)
total_spending = total_spending %>% arrange(desc(TotalSpending))
total_visits = total_visits %>% arrange(desc(TotalVisits))

# map qunatiles splits all the values into 5 groups based on the order it was arranged in
map_quantiles <- function(vect, num_groups=5) {
  ranks <- order(vect)
  result <- rep(0,length(vect))
  one_unit <- floor(length(vect) / num_groups)
  for (index in 1:num_groups) {
    if (index == num_groups) {
      result[(index - 1) * one_unit < ranks] <- index
    } else {
      result[(index - 1) * one_unit < ranks & ranks <= index * one_unit] <- index
    }
  }
  return(result)
}

total_visits$rfm2 = map_quantiles(total_visits$TotalVisits)
total_spending$rfm3 = map_quantiles(total_spending$TotalSpending)
last_visit$rfm1 = 6 - map_quantiles(last_visit$Recency) # because lower the value means more recent

# creating the rfm dataframe
final_rfm = as.data.frame(unique(cleaner_data$HOUSEID))
colnames(final_rfm) = "HOUSEID"

final_rfm = final_rfm %>%
  left_join(last_visit, by = "HOUSEID") %>%
  left_join(total_visits, by = "HOUSEID") %>%
  left_join(total_spending, by = "HOUSEID") %>%
  mutate(rfmscore = (rfm1*100)+(rfm2*10)+(rfm3))

# adding the area the family shops at into the dataframe
data %>% group_by(HOUSEID, DEMOG_AREA) %>% summarise(n = n())
# shows that each HOUSEID only goes to one DEMOG_AREA

house_to_area = data %>% 
  group_by(HOUSEID, DEMOG_AREA) %>%
  summarise(n = n()) %>%
  select(HOUSEID, DEMOG_AREA)
  
demog_rfm = final_rfm %>% left_join(house_to_area, by = "HOUSEID") 

# Others
# Finding out the spread of sources (where do people usually go to purchase this product)
channel_spread = data %>%
  group_by(channel) %>%
  summarise(n = n()) %>% 
  arrange(desc(n))

ggplot(channel_spread, aes(x = reorder(channel, n), y = n)) +
  geom_col() +
  labs(x = 'Channel', 
       y = 'Count') +
  coord_flip()

# Finding out which store people go to the most
store_spread = data %>%
  group_by(STORENAME) %>%
  summarise(n = n()) %>% 
  arrange(desc(n))

# show the top 10 
ggplot(store_spread[1:10,], aes(x = reorder(STORENAME, n), y = n)) +
  geom_col() +
  labs(x = 'Store', 
       y = 'Count') +
  coord_flip()

