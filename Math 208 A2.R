library(tidyverse)
heart_tbl <- read_csv("heart.csv")\
heart_tbl

# 1a
ggplot(heart_tbl, aes(x=RestingBP, fill=Sex)) +
  geom_histogram(binwidth=10, color="black", position="identity", alpha=0.5) +
  labs(title="Histogram of Heart Rates by Sex", x="Heart Rate", y="Count") +
  scale_fill_discrete(name="Sex")
  # as the female population usually is around one third of the male population
  # for each heart rate bar, I would say that there is no meaningful
  # difference, and that there is simply just more males in the dataset than
  # females

# 1b
ggplot(heart_tbl, aes(x=RestingECG, fill=ChestPainType)) +
  geom_bar(position="stack") +
  labs(title="Distribution of Chest Pain Type by Resting ECG", x="Resting ECG", 
       y="Count") +
  scale_fill_discrete(name="Chest Pain Type")

# 1c
ggplot(heart_tbl, aes(x=RestingECG, fill=ChestPainType)) +
  geom_bar(position="fill") +
  labs(title="Relative Distribution of Chest Pain Type by Resting ECG",
       x="Resting ECG", y="Proportion") +
  scale_fill_discrete(name="Chest Pain Type")

# 1d
summary_table <- heart_tbl %>%
  group_by(Sex, ChestPainType) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n))
summary_table

# 1e
wider_table <- summary_table %>%
  pivot_wider(names_from = Sex, values_from = prop) %>%
  select(-n)
wider_table

# 1f
final_table <- heart_tbl %>%
  group_by(ChestPainType) %>%
  summarise(
    RestingBP_mean = mean(RestingBP, na.rm = TRUE),
    Cholesterol_mean = mean(Cholesterol, na.rm = TRUE),
    FastingBS_mean = mean(FastingBS, na.rm = TRUE),
    MaxHR_mean = mean(MaxHR, na.rm = TRUE),
    RestingBP_median = median(RestingBP, na.rm = TRUE),
    Cholesterol_median = median(Cholesterol, na.rm = TRUE),
    FastingBS_median = median(FastingBS, na.rm = TRUE),
    MaxHR_median = median(MaxHR, na.rm = TRUE),
    RestingBP_IQR = IQR(RestingBP),
    Cholesterol_IQR = IQR(Cholesterol),
    FastingBS_IQR = IQR(FastingBS),
    MaxHR_IQR = IQR(MaxHR)
  )
final_table

# 1g
longer_table <- final_table %>%
  pivot_longer(cols = -ChestPainType, names_to = "name",
               values_to = "value") %>%
  pivot_wider(names_from = ChestPainType, values_from = value)
longer_table

# 1h
heart_tbl_modified<- heart_tbl %>% 
  select(HeartDisease,RestingBP,Cholesterol,MaxHR) %>% 
  mutate(HeartDisease=ifelse(HeartDisease==1,"Yes","No"))
long_data <- heart_tbl_modified %>%
  pivot_longer(cols = c(RestingBP, Cholesterol, MaxHR),
               names_to = "Measurement", values_to = "Value")
long_data
ggplot(long_data, aes(x=HeartDisease, y=Value, fill=HeartDisease)) +
  geom_boxplot() +
  facet_wrap(~ Measurement, scales = "free") +
  labs(title="Boxplots of Measurements by Heart Disease Status",
       x="Heart Disease Status", y="Value") +
  scale_fill_discrete(name="Heart Disease Status")


mig <- read_csv("migration.csv")

# 2a
mig_sep <- mig %>%
  separate(Ref_Date, into = c("year", "month"), sep = "/")
mig_sep

# 2b
mig_sep <- mig_sep %>%
  mutate(year = as.integer(year), month = as.integer(month))
mig_sep

# 2c
mig_sep <- mig_sep %>% select(-Vector, -Coordinate) %>% filter(GEO != "Canada")
mig_summary_geo_int <- mig_sep %>%
  group_by(GEO, INT) %>%
  summarise(sum = sum(Value, na.rm = TRUE)) %>%
  arrange(desc(sum))
mig_summary_geo_int

# 2d
mig_summary_prov_total <- mig_summary_geo_int %>%
  pivot_wider(names_from = INT, values_from = sum) %>%
  mutate(migrants_by_province = `In-migrants` + `Out-migrants`)
mig_summary_prov_total

# 2e
mig_sep_2002 <- mig_sep %>%
  filter(year == 2002)
mig_summary <- mig_sep_2002 %>%
  group_by(GEO) %>%
  summarise(total_migrants = sum(Value))
ggplot(mig_summary,
       aes(x = fct_reorder(GEO,-total_migrants), y = total_migrants,
           fill = fct_reorder(GEO,-total_migrants))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::scientific) +
  theme(axis.text.x = element_text(angle=90 ,hjust=1)) +
  labs(x="Province", y="Migrants",
       title="Total Number of Migrants by Province in 2002",
       fill="GEO")

# 2f
mig_sep_2012_quarter <- mig_sep %>% filter(year == 2002) %>% mutate( 
  quarter = case_when ( month == 3 ~ "1st",
                        month == 6 ~ "2nd",
                        month == 9 ~ "3rd",
                        month == 12 ~ "4th")
) %>% select(-month)
ggplot(mig_summary,
       aes(x = quarter, y = total_migrants,
           fill = fct_reorder(GEO,-total_migrants))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::scientific) +
  theme(axis.text.x = element_text(angle =90,hjust=1)) +
  labs(x="Quarter", y="Migrants",
       title="Total Number of Migrants by Quarter in 2002", 
       fill="GEO")
