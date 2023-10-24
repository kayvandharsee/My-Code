library(tidyverse)
### 1
my_furniture_list <- tibble(
  Item = c("bed mattress", "office desk", "night stand", "lamp", "kitchen chair"),
  Amount = c(1,1,2,2,4), 
  Price = c(1000, 500, 100, 50, 50),
  Location = c("bedroom","office","bedroom","bedroom", "kitchen"))


added_furniture_list <- tibble(
  Item = c("towel", "soap", "wardrobe", "paper towel"),
  Amount = c(6,3,1,10),
  Price = c(10, 5, 200, 1),
  Location = c("bathroom","bathroom","bedroom", "bathroom"))

both_list <- list(store_A = my_furniture_list, 
                  store_B = added_furniture_list) 

# concatenate two tibble objects by columns
both_tibble <- rbind(my_furniture_list, added_furniture_list)


# a i
both_list[[c(2,3)]] #[1]  10   5 200   1

# ii
both_list[[1]][3] 
#A tibble: 5 × 1
#Price
#<dbl>
#1  1000
#2   500
#3   100
#4    50
#5    50

# iii
both_list["store_B"][[2]]
#Error in both_list["store_B"][[2]] : subscript out of bounds


# b i
both_list$store_A$Item[3]
typeof(both_list$store_A$Item[3])
both_tibble[[1]][3]
typeof(both_tibble[[1]][3])
#B and C

# ii
arrange(both_tibble, desc(Amount))
# D

# iii
class(both_list["store_B"])
# C


# c
both_tibble <- both_tibble %>%
  mutate(Location = fct_reorder(Location, Item,
                                .fun = function(x) length(unique(x)),
                                .desc = TRUE))
summary_table1 <- both_tibble %>%
  group_by(Location) %>%
  summarise(n_distinct_items = n_distinct(Item)) %>%
  arrange(desc(n_distinct_items))
summary_table1

# d
both_tibble <- both_tibble %>%
  mutate(Location = as.factor(as.character(Location)))
summary_table2 <- both_tibble %>%
  group_by(Location) %>%
  summarise(total_price = sum(Price * Amount)) %>%
  arrange(Location)
summary_table2


### 2
view(diamonds)
diamonds_500 <- diamonds %>% slice(1:500)

# a
table(diamonds_500$cut)

# b
diamonds_price_summary <- diamonds_500 %>% group_by(cut) %>%
  summarize(mean_price = mean(price), sd_price = sd(price))
diamonds_price_summary

# c
diamonds_price_summary <- diamonds_price_summary %>% 
  mutate(cut, mean_price, .desc = TRUE)
diamonds_price_summary <- diamonds_price_summary %>%
  select(-.desc) %>%
  arrange(desc(mean_price))
diamonds_price_summary

# d
ggplot(diamonds_500, aes(x = cut, y = price)) + 
  geom_boxplot() + labs(x = "Cut", y = "Price") + 
  ggtitle("Price for Different Cuts")
# Price isn't a good indicator, as for all 5 cuts the median price is relatively
# indistinguishable between the different cuts. This makes it hard to tell the
# visible difference without the labels telling us.

# e
diamonds_filtered <- diamonds_500 %>% filter(cut == "Fair" | cut == "Premium")
ggplot(diamonds_filtered, aes(x = carat)) +
  geom_density(aes(fill = cut)) +
  facet_wrap(~ cut, nrow = 2) + labs(x = "Carat", y = "Density") +
  ggtitle("Density of Carat for Fair, Premium Cuts")
# The density plot for Fair cut diamonds is much more concentrated, as it slowly increases
# in area until jsut before the 1 carat mark where it slowly decreases. The Premium
# cut diamonds are far from consistent, and contain sharp upwards bursts at many values
# such 0.75 carat, and is otherwse close to 0 in terms of density.
# overall there is a significant difference


### 3
storms_cut <- storms %>% slice(1:84) %>% unite("date", c("year", "month", "day"), sep = "-")
storms_cut

# a
storms_cut <- storms_cut %>%
  select(name, date, hour, lat, long, status, wind, pressure)

# b
storms_summary <- storms_cut %>% group_by(name, status) %>%
  summarize(n = n()) %>% mutate(prop = n/sum(n))
storms_summary

# c
storms_summary_wide <- storms_summary %>%
  pivot_wider(id_cols = name, names_from = status, values_from = prop)
storms_summary_wide

# d
amy_storm <- storms_cut %>%
  filter(name == "Amy")
ggplot(amy_storm, aes(x = long, y = lat, color = status)) +
  geom_point() +
  labs(x = "Longitude", y = "Latitude", color = "Status") +
  ggtitle("Trajectory of Storm Amy")

# e
summary_table <- storms_cut %>%
  group_by(name, date) %>%
  summarise(mean_pressure = mean(pressure, na.rm = TRUE))
ggplot(summary_table, aes(x = date, y = mean_pressure, color = name, group = name)) +
  geom_line() + geom_point() + labs(x = "date", y= "mean pressure") + 
  ggtitle("mean pressure of storms per day")
