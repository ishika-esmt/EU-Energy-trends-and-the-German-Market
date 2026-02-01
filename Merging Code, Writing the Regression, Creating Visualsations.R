install.packages("tidyverse")

library(tidyverse)
library(readr)
years1 <- read_csv("C:/Users/georg/Desktop/ESMT/Data Visualisation/Data installed generation capacity/Installed_generation_capacity_201510270000_201710280000_Day.csv")
print(years1)
head(years1)

library(readr)
library(dplyr)
library(janitor)

## Merging the Generation Capacity files
file_path1 <- "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data installed generation capacity/Installed_generation_capacity_201510270000_201710280000_Day.csv"
file_path2 <- "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data installed generation capacity/Installed_generation_capacity_201710270000_201910280000_Day.csv"
file_path3 <- "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data installed generation capacity/Installed_generation_capacity_201910270000_202110280000_Day.csv"
file_path4 <- "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data installed generation capacity/Installed_generation_capacity_202110270000_202310280000_Day.csv"
file_path5 <- "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data installed generation capacity/Installed_generation_capacity_202310270000_202510280000_Day.csv"


year1 <- read_delim(
  file = file_path1,
  delim = ";",
  na = c("-", "", "NA"),
  locale = locale(decimal_mark = ".", grouping_mark = ","),  
  col_types = cols(
    `Start date` = col_date(format = "%b %d, %Y"),
    `End date`   = col_date(format = "%b %d, %Y"),
  ),
  show_col_types = FALSE
) 
year2 <- read_delim(
  file = file_path2,
  delim = ";",
  na = c("-", "", "NA"),
  locale = locale(decimal_mark = ".", grouping_mark = ","),  
  col_types = cols(
    `Start date` = col_date(format = "%b %d, %Y"),
    `End date`   = col_date(format = "%b %d, %Y"),
  ),
  show_col_types = FALSE
) 
year3 <- read_delim(
  file = file_path3,
  delim = ";",
  na = c("-", "", "NA"),
  locale = locale(decimal_mark = ".", grouping_mark = ","),  
  col_types = cols(
    `Start date` = col_date(format = "%b %d, %Y"),
    `End date`   = col_date(format = "%b %d, %Y"),
  ),
  show_col_types = FALSE
) 
year4 <- read_delim(
  file = file_path4,
  delim = ";",
  na = c("-", "", "NA"),
  locale = locale(decimal_mark = ".", grouping_mark = ","),  
  col_types = cols(
    `Start date` = col_date(format = "%b %d, %Y"),
    `End date`   = col_date(format = "%b %d, %Y"),
  ),
  show_col_types = FALSE
) 
year5 <- read_delim(
  file = file_path5,
  delim = ";",
  na = c("-", "", "NA"),
  locale = locale(decimal_mark = ".", grouping_mark = ","), 
  col_types = cols(
    `Start date` = col_date(format = "%b %d, %Y"),
    `End date`   = col_date(format = "%b %d, %Y"),
  ),
  show_col_types = FALSE
) 

all_merged <- bind_rows(year1, year2, year3, year4, year5)
write_csv(
  all_merged,
  "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data installed generation capacity/merged_installed_generation_capacity.csv"
)

## merging the Market Data
file_path_market1 <- "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data Market/Day-ahead_prices_201510010000_202011010000_Day.csv"
file_path_market2 <- "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data Market/Day-ahead_prices_202011010000_202210020000_Day.csv"
file_path_market3 <- "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data Market/Day-ahead_prices_202210020000_202410020000_Day.csv"
file_path_market4 <- "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data Market/Day-ahead_prices_202410010000_202510290000_Day.csv"
market_year1  <- read_delim(
  file = file_path_market1,
  delim = ";",
  na = c("-", "", "NA"),
  locale = locale(decimal_mark = ".", grouping_mark = ","),  
  col_types = cols(
    `Start date` = col_date(format = "%b %d, %Y"),
    `End date`   = col_date(format = "%b %d, %Y"),
  ),
  show_col_types = FALSE
)
market_year2  <- read_delim(
  file = file_path_market2,
  delim = ";",
  na = c("-", "", "NA"),
  locale = locale(decimal_mark = ".", grouping_mark = ","),  
  col_types = cols(
    `Start date` = col_date(format = "%b %d, %Y"),
    `End date`   = col_date(format = "%b %d, %Y"),
  ),
  show_col_types = FALSE
)
market_year3  <- read_delim(
  file = file_path_market3,
  delim = ";",
  na = c("-", "", "NA"),
  locale = locale(decimal_mark = ".", grouping_mark = ","),  
  col_types = cols(
    `Start date` = col_date(format = "%b %d, %Y"),
    `End date`   = col_date(format = "%b %d, %Y"),
  ),
  show_col_types = FALSE
)
market_year4  <- read_delim(
  file = file_path_market4,
  delim = ";",
  na = c("-", "", "NA"),
  locale = locale(decimal_mark = ".", grouping_mark = ","),  
  col_types = cols(
    `Start date` = col_date(format = "%b %d, %Y"),
    `End date`   = col_date(format = "%b %d, %Y"),
  ),
  show_col_types = FALSE
)

all_market_merged <- bind_rows(market_year1, market_year2, market_year3, market_year4)
all_marget_merged2<- clean_names(all_market_merged)
write_csv(
  all_market_merged,
  "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data market/merged_market_dayahead_price.csv"
)

file_forecasted1 <- "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data Forecasted Generation/Forecasted_generation_Day-Ahead_201501100000_201510250000_Day.csv"
file_forecasted2 <- "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data Forecasted Generation/Forecasted_generation_Day-Ahead_201510250000_201710250000_Day.csv"
file_forecasted3 <- "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data Forecasted Generation/Forecasted_generation_Day-Ahead_201710250000_201910250000_Day.csv"
file_forecasted4 <- "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data Forecasted Generation/Forecasted_generation_Day-Ahead_201910250000_202110260000_Day.csv"
file_forecasted5 <- "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data Forecasted Generation/Forecasted_generation_Day-Ahead_202110260000_202310280000_Day.csv"
file_forecasted6 <- "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data Forecasted Generation/Forecasted_generation_Day-Ahead_202310280000_202510290000_Day.csv"
forecastedyear1  <- read_delim(
  file = file_forecasted1,
  delim = ";",
  na = c("-", "", "NA"),
  locale = locale(decimal_mark = ".", grouping_mark = ","),  
  col_types = cols(
    `Start date` = col_date(format = "%b %d, %Y"),
    `End date`   = col_date(format = "%b %d, %Y"),
  ),
  show_col_types = FALSE
)
forecastedyear2  <- read_delim(
  file = file_forecasted2,
  delim = ";",
  na = c("-", "", "NA"),
  locale = locale(decimal_mark = ".", grouping_mark = ","),  
  col_types = cols(
    `Start date` = col_date(format = "%b %d, %Y"),
    `End date`   = col_date(format = "%b %d, %Y"),
  ),
  show_col_types = FALSE
)
forecastedyear3  <- read_delim(
  file = file_forecasted3,
  delim = ";",
  na = c("-", "", "NA"),
  locale = locale(decimal_mark = ".", grouping_mark = ","),  
  col_types = cols(
    `Start date` = col_date(format = "%b %d, %Y"),
    `End date`   = col_date(format = "%b %d, %Y"),
  ),
  show_col_types = FALSE
)
forecastedyear4  <- read_delim(
  file = file_forecasted4,
  delim = ";",
  na = c("-", "", "NA"),
  locale = locale(decimal_mark = ".", grouping_mark = ","),  
  col_types = cols(
    `Start date` = col_date(format = "%b %d, %Y"),
    `End date`   = col_date(format = "%b %d, %Y"),
  ),
  show_col_types = FALSE
)
forecastedyear5  <- read_delim(
  file = file_forecasted5,
  delim = ";",
  na = c("-", "", "NA"),
  locale = locale(decimal_mark = ".", grouping_mark = ","),  
  col_types = cols(
    `Start date` = col_date(format = "%b %d, %Y"),
    `End date`   = col_date(format = "%b %d, %Y"),
  ),
  show_col_types = FALSE
)
forecastedyear6  <- read_delim(
  file = file_forecasted6,
  delim = ";",
  na = c("-", "", "NA"),
  locale = locale(decimal_mark = ".", grouping_mark = ","),  
  col_types = cols(
    `Start date` = col_date(format = "%b %d, %Y"),
    `End date`   = col_date(format = "%b %d, %Y"),
  ),
  show_col_types = FALSE
)
all_forecast_merged <- bind_rows(forecastedyear1, forecastedyear2, forecastedyear3, forecastedyear4,forecastedyear5,forecastedyear6)
write_csv(
  all_forecast_merged,
  "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data Forecasted Generation/merged_dayahead_generation_forecast.csv"
)

# Now I want to merge the forecast dataset with the dayahead price, I will clean the names of all
forecast_merge_prep <- all_forecast_merged %>% 
  clean_names() %>% 
  select(-total_m_wh_calculated_resolutions,-photovoltaics_and_wind_m_wh_calculated_resolutions)
  
# I only talk about the market price for germany/luxembourg. I only account for the Prodution in Germany since I consider it negligible for Luxmbourg
#market_merge_prep <- clean_names(all_market_merged[c("Start date", "End date", "Germany/Luxembourg [€/MWh] calculated resolutions")])
market_merge_prep <- all_market_merged %>%
  clean_names() %>%
  filter(start_date > as.Date("2018-10-01")
         ) %>% 
  select(start_date, end_date, germany_luxembourg_m_wh_calculated_resolutions)
merge_test_inshallah <- inner_join(forecast_merge_prep, market_merge_prep, by = c("start_date","end_date"))
merge_test_inshallah2 <- na.omit(merge_test_inshallah)

file_consumption_forecasted <- read.csv("C:/Users/georg/Desktop/ESMT/Data Visualisation/Merged Consumption Forecasted/merged_electricity consumption_forecasted.csv")
names(file_consumption_forecasted)
consumption_merge_prep <- file_consumption_forecasted %>% 
  select(-date) %>% 
  mutate(start_date = as.Date(start_date)) %>% 
  mutate(end_date = as.Date(end_date))

all_stuff_merged_regression <- inner_join(merge_test_inshallah2, consumption_merge_prep, by = c("start_date","end_date"))
dim(all_stuff_merged_regression)
names(all_stuff_merged_regression)

regression_analysis <- lm(
  germany_luxembourg_m_wh_calculated_resolutions ~ grid_load_mwh + 
    wind_onshore_m_wh_calculated_resolutions + 
    wind_offshore_m_wh_calculated_resolutions + 
    photovoltaics_m_wh_calculated_resolutions + 
    other_m_wh_calculated_resolutions,
  data = all_stuff_merged_regression 
)


print(summary(regression_analysis))
#install.packages("car")
library(car)
print(vif(regression_analysis))

write_csv(
  all_stuff_merged_regression,
  "C:/Users/georg/Desktop/ESMT/Data Visualisation/all_stuff_merged.csv"
)

# Some nice Plots to aid this Analysis!, Maybe something like looking at the Analytics
# I can test it on the older Data (from the Time it was Germany, Luxembourg, Austria)
df_plot <- all_stuff_merged_regression %>%
  mutate(predicted = predict(regression_analysis, newdata = all_stuff_merged_regression)) %>%
  filter(!is.na(germany_luxembourg_m_wh_calculated_resolutions), !is.na(predicted))

ggplot(df_plot, aes(x = germany_luxembourg_m_wh_calculated_resolutions, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Actual price (€/MWh)", y = "Predicted price (€/MWh)")

ggplot(all_stuff_merged_regression, aes(x = start_date)) +
  geom_line(aes(y = germany_luxembourg_m_wh_calculated_resolutions, color = "Price (€/MWh)")) +
  geom_line(aes(y = photovoltaics_m_wh_calculated_resolutions / 1000, color = "Solar (×1000 MWh)")) +
  labs(title = "Solar Generation vs. Day-Ahead Prices",
       x = "Date", y = "Price / Generation") +
  theme_minimal()

gasmarketforecastfile <- "C:/Users/georg/Desktop/ESMT/Data Visualisation/Data Gas Market/Gas Market Data Bundesnetzagentur.csv"
gasmarketforecastyear <- read_delim(
  file = gasmarketforecastfile,
  delim = ";",                        
  show_col_types = FALSE# surpressing annoying column messsafge
) %>%
  clean_names() %>% #just in case so i can remove extra spaces, for some reason didn't work before when I was trying to read the file        
  mutate(
    start_date = as.Date(start_date, format = "%d.%m.%Y"),
    price_gas_mwh = as.numeric(price_gas_mwh)
  ) %>% 
drop_na(price_gas_mwh)

# This only keeps all rows with values
final_merged_gas_electricity = inner_join(
  all_stuff_merged_regression,
  gasmarketforecastyear,
  by = "start_date"
)


# regression Analysis explaining the rise in in electricity during the years in which 
regression_analysis_gas_data <- lm(
  germany_luxembourg_m_wh_calculated_resolutions ~
    wind_onshore_m_wh_calculated_resolutions + 
    wind_offshore_m_wh_calculated_resolutions + 
    photovoltaics_m_wh_calculated_resolutions + 
    other_m_wh_calculated_resolutions + price_gas_mwh,
  data = final_merged_gas_electricity 
)
print(summary(regression_analysis_gas_data))
print(vif(regression_analysis_gas_data))

# How well does my model perform without the gas data post energy crisis
#How well does my model perform pre energy crisis
# How well does my model perform post energy crisis

#Split Datafiles precrisis and postcrisis
# I decide that the crisis period is from January 2022 to december 2023
# I train my model 

#Do the same for the data merged with the data that has all the gas prices, I only want the data after June 2025

crisis_years <- final_merged_gas_electricity %>%
  filter(start_date >= as.Date("2022-01-01") & 
           start_date <= as.Date("2023-12-31"))
training_data_model_without_gas <-all_stuff_merged_regression %>% 
  filter(start_date < as.Date("2022-01-01") | 
           start_date > as.Date("2018-10-02"))
training_data_model_with_gas <- final_merged_gas_electricity %>% 
  filter(start_date >= as.Date("2023-12-31") & 
           start_date <= as.Date("2025-10-24"))

regression_analysis_model_no_gas<- lm(
  germany_luxembourg_m_wh_calculated_resolutions ~ grid_load_mwh + 
    wind_onshore_m_wh_calculated_resolutions + 
    wind_offshore_m_wh_calculated_resolutions + 
    photovoltaics_m_wh_calculated_resolutions + 
    other_m_wh_calculated_resolutions,
  data = training_data_model_without_gas 
)


print(summary(regression_analysis_model_no_gas))
print(vif(regression_analysis_model_no_gas))

regression_analysis_model_with_gas<- lm(
  germany_luxembourg_m_wh_calculated_resolutions ~ grid_load_mwh + 
    wind_onshore_m_wh_calculated_resolutions + 
    wind_offshore_m_wh_calculated_resolutions + 
    photovoltaics_m_wh_calculated_resolutions + 
    other_m_wh_calculated_resolutions + price_gas_mwh,
  data = training_data_model_with_gas 
)


print(summary(regression_analysis_model_with_gas))
print(vif(regression_analysis_model_with_gas))

# Predicting the crisis period with both models
crisis_years_with_predictions <- crisis_years %>%
  mutate(
    pred_without_gas    = predict(regression_analysis_model_no_gas,  newdata = .),
    pred_with_gas = predict(regression_analysis_model_with_gas, newdata = .)
  )
# Evaluating the models performaces
ggplot(crisis_years_with_predictions, aes(x = germany_luxembourg_m_wh_calculated_resolutions)) +
  geom_point(aes(y = pred_with_gas, color = "With Gas"), alpha = 0.6) +
  geom_point(aes(y = pred_without_gas, color = "Without Gas"), alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Predicted vs Actual Electricity Prices (Crisis 2022–2023)",
       x = "Actual Price (€/MWh)",
       y = "Predicted Price (€/MWh)",
       color = "Model") +
  theme_minimal()

ggplot(crisis_years_with_predictions, aes(x = start_date)) +
  geom_line(aes(y = germany_luxembourg_m_wh_calculated_resolutions, color = "Actual"), size = 0.8) +
  geom_line(aes(y = pred_with_gas, color = "Predicted (with gas)"), size = 0.8, linetype = "solid") +
  labs(title = "Model trained with gas data vs Actual: Crisis Period (2022–2023)",
       y = "Day-ahead Price (€/MWh)",
       x = "Date") +
  theme_minimal()

ggplot(crisis_years_with_predictions, aes(x = start_date)) +
  geom_line(aes(y = germany_luxembourg_m_wh_calculated_resolutions, color = "Actual"), size = 0.8) +
  geom_line(aes(y = pred_without_gas, color = "Predicted (without gas)"), size = 0.8, linetype = "solid") +
  labs(title = "Model trained without gas data vs Actual: Crisis Period (2022–2023)",
       y = "Day-ahead Price (€/MWh)",
       x = "Date") +
  theme_minimal()


### How do they each respectively predict the high energy prices in the crisis period
regression_analysis_model_no_gas_crisis_years<- lm(
  germany_luxembourg_m_wh_calculated_resolutions ~ grid_load_mwh + 
    wind_onshore_m_wh_calculated_resolutions + 
    wind_offshore_m_wh_calculated_resolutions + 
    photovoltaics_m_wh_calculated_resolutions + 
    other_m_wh_calculated_resolutions,
  data = crisis_years 
)


print(summary(regression_analysis_model_no_gas_crisis_years))
print(vif(regression_analysis_model_no_gas_crisis_years))

regression_analysis_model_with_gas_crisis_years<- lm(
  germany_luxembourg_m_wh_calculated_resolutions ~ grid_load_mwh + 
    wind_onshore_m_wh_calculated_resolutions + 
    wind_offshore_m_wh_calculated_resolutions + 
    photovoltaics_m_wh_calculated_resolutions + 
    other_m_wh_calculated_resolutions,
  data = crisis_years 
)


print(summary(regression_analysis_model_with_gas_crisis_years))
print(vif(regression_analysis_model_with_gas_crisis_years))

# now off to plotting the results
library(tidyr)
library(lubridate)
library(ggplot2)

#First I wanna take the weekly averages, at first my graph looked way too messy
pdat <- crisis_years_with_predictions %>%
  transmute(
    date = start_date,
    actual    = germany_luxembourg_m_wh_calculated_resolutions,
    pred_post = pred_with_gas,        # post-crisis model (with gas)
    pred_pre  = pred_without_gas      # pre-crisis model (no gas)
  ) %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(
    Actual                         = mean(actual,    na.rm = TRUE),
    `Post-crisis model (with gas)` = mean(pred_post, na.rm = TRUE),
    `Pre-crisis model (no gas)`    = mean(pred_pre,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-week, names_to = "Series", values_to = "price")

#Now I plot, don't foget the background!
ggplot(pdat, aes(x = week, y = price,
                 color = Series, linetype = Series)) +
  geom_line(size = 0.9) +
  scale_color_manual(values = c(
    "Actual"                         = "pink",
    "Post-crisis model (with gas)"   = "darkgreen",
    "Pre-crisis model (no gas)"      = "darkred"
  )) +
  scale_linetype_manual(values = c(
    "Actual"                         = "solid",
    "Post-crisis model (with gas)"   = "solid",
    "Pre-crisis model (no gas)"      = "solid"
  )) +
  labs(title = "Electricity Prices During the Crisis (2022–2023)",
       subtitle = "smoothened weekly averages of actual vs. model predictions",
       x = "Date", y = "Price (€/MWh)", color = "Series", linetype = "Series") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))+
theme(
  panel.grid = element_line(color = "transparent", linewidth = 0.2),
  plot.background = element_rect(fill = "transparent", color = NA),
  panel.background = element_rect(fill = "transparent", color = NA)
)
ggsave(
  filename = "crisis_prices_comparison.png",   
  plot = last_plot(),                          
  width = 10, height = 6,                   
  dpi = 300,                               
  bg = "transparent"                       
)

library(lubridate)

# Reading the dataset
data2 <- read.csv("C:/Users/georg/Desktop/ESMT/Data Visualisation/Merged Consumption Acutal/merged_electricity generation_actual.csv")

# Filling the missing values with 0
data2[is.na(data2)] <- 0

class(data2$start_date)


data3 <- data2 %>%
  mutate(
    start_date = ymd(start_date),   # or as.Date(start_date)
    end_date   = ymd(end_date),     # or as.Date(end_date)
    year       = year(start_date)
  )

# Calculating the sum of each generation type per years
yearly_generation_by_type <- data3 %>%
  group_by(year) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

# Plotting the developing of each generation type by year
develope_generation_yearly <- yearly_generation_by_type %>%
  pivot_longer(
    cols = -c(year),
    names_to = "generation_type",
    values_to = "value"
  )
names(data3)
ggplot(develope_generation_yearly, aes(x = year, y = value, color = generation_type)) +
  geom_line(size = .8) +
  scale_color_manual(
    values = c(
      "biomass_mwh"              = "#1b9e77",
      "hydropower_mwh"           = "#66c2a5",
      "wind_offshore_mwh"        = "#3288bd",
      "wind_onshore_mwh"         = "#4575b4",
      "photovoltaics_mwh"        = "#fee08b",
      "other_renewable_mwh"      = "#91cf60",
      "nuclear_mwh"              = "#984ea3",
      "lignite_mwh"              = "#d95f02",
      "hard_coal_mwh"            = "#a6761d",
      "fossil_gas_mwh"           = "#e41a1c",
      "hydro_pumped_storage_mwh" = "#80b1d3",
      "other_conventional_mwh"   = "#bdbdbd"
    ),
    labels = c(
      "biomass_mwh"              = "Biomass",
      "hydropower_mwh"           = "Hydropower",
      "wind_offshore_mwh"        = "Wind Offshore",
      "wind_onshore_mwh"         = "Wind Onshore",
      "photovoltaics_mwh"        = "Solar (PV)",
      "other_renewable_mwh"      = "Other Renewables",
      "nuclear_mwh"              = "Nuclear",
      "lignite_mwh"              = "Lignite",
      "hard_coal_mwh"            = "Hard Coal",
      "fossil_gas_mwh"           = "Fossil Gas",
      "hydro_pumped_storage_mwh" = "Pumped Storage",
      "other_conventional_mwh"   = "Other Conventional"
    ),
    name = "Generation Type"
  ) +
  labs(
    title = "Electricity Generation by Energy Type in Germany",
    x = "Year",
    y = "Electricity Generation (TWh)",
    color = "Generation Type"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13))+
  theme(legend.position = "bottom")+
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = " "))+
  theme(
    panel.grid = element_line(color = "transparent", linewidth = 0.2),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )
ggsave(
  filename = "electricity_gen_by_energy_type.png",  
  plot = last_plot(),                         
  width = 10, height = 6,                   
  dpi = 300,                               
  bg = "transparent"                       
)



# Reading the dataset
data5 <- read.csv("C:/Users/georg/Desktop/ESMT/Data Visualisation/Europe Yearly/europe_yearly_full_release_reshaped.csv")

data_long <- data5 %>%
  pivot_longer(
    cols = matches("^Variable|Value|Unit"),
    names_to = c(".value", "set"),
    names_pattern = "(Variable|Value|Unit)\\.?([0-9]*)"
  )

# Select the year (1990-2024) and the relevant variables ("Wind", "Wind and solar", "Onshore wind")
wind_country_1990_2024_twh <- data_long %>%
  filter(
    Year_ >= 1990 & Year_ <= 2024,
    Variable %in% c(
      "Wind", 
      "Wind and solar",
      "Onshore wind"
    ),
    Unit == "TWh"
  )

wind_country_1990_2024_percent <- data_long %>%
  filter(
    Year_ >= 1990 & Year_ <= 2024,
    Variable %in% c(
      "Wind", 
      "Wind and solar",
      "Onshore wind"
    ),
    Unit == "%"
  )

# Convert Value to numeric
wind_country_1990_2024_twh$Value <- as.numeric(wind_country_1990_2024_twh$Value)
wind_country_1990_2024_percent$Value <- as.numeric(wind_country_1990_2024_percent$Value)

# ---- TOP 10 Countries by increasing the Wind (TWh) in 1990 - 2024 ---- #
# Defining the TOP 10 countries to 2024
top10_wind_country_2024_TWh <- wind_country_1990_2024_twh %>%
  filter(Year_ == 2024, Area_ != "EU") %>%
  group_by(Area_) %>%
  summarise(Total_Wind_TWh = sum(Value, na.rm = TRUE)) %>%
  arrange(desc(Total_Wind_TWh)) %>%
  slice_head(n = 10) %>%
  pull(Area_)


# The data for the TOP 10 country for each year
top10_wind_country_each_year_twh <- wind_country_1990_2024_twh %>%
  filter(Area_ %in% top10_wind_country_2024_TWh) %>%
  group_by(Area_, Year_) %>%
  summarise(Total_Wind_TWh = sum(Value, na.rm = TRUE), .groups = "drop")


# Line chart about the increase between 1990 - 2024
ggplot(
  top10_wind_country_each_year_twh, aes(x = Year_, y = Total_Wind_TWh, color = Area_)
) +
  geom_line(size = 0.8) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Finland"         = "#1F77B4",
      "France"          = "#2CA02C",
      "Germany"         = "#FF7F0E",
      "Italy"           = "#9467BD",
      "Netherlands"     = "#17BECF",
      "Poland"          = "#E377C2",
      "Spain"           = "#7F7F7F",
      "Sweden"          = "#8C564B",
      "Türkiye"         = "#D62728",
      "United Kingdom"  = "#AEC7E8"
    )) +
  theme(plot.title = element_text(face = "bold", size = 13))+
  labs(
    title = "Wind Energy Generation (TWh) Over the Years (1990 - 2024)",
    x = NULL,
    y = "Wind Energy Generation (TWh)",
    color = NULL
  )+theme_minimal()+theme(legend.position = "bottom")+ 
  theme(
    legend.position = "bottom",
    panel.grid = element_line(color = "transparent", linewidth = 0.2),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(face = "bold", size = 13)
  )
ggsave(
  filename = "wind_energy_generation_over_the_years.png",   
  plot = last_plot(),                          
  width = 10, height = 6,                   
  dpi = 300,                               
  bg = "transparent"                       
)


# ---- TOP 10 Countries by increasing the Wind Energy Shares (%) in 1990 - 2024 ---- #

# Defining the TOP 10 countries to 2024
top10_wind_country_2024_percent <- wind_country_1990_2024_percent %>%
  filter(Year_ == 2024) %>%
  group_by(Area_) %>%
  summarise(Total_Wind_TWh = median(Value, na.rm = TRUE)) %>%
  arrange(desc(Total_Wind_TWh)) %>%
  slice_head(n = 10) %>%
  pull(Area_)
# The data for the TOP 10 country for each year
top10_wind_country_each_year_percent <- wind_country_1990_2024_percent %>%
  filter(Area_ %in% top10_wind_country_2024_percent) %>%
  group_by(Area_, Year_) %>%
  summarise(Total_Wind_Percent = median(Value, na.rm = TRUE), .groups = "drop")

# Line chart about the increase between 1990 - 2024
ggplot(
  top10_wind_country_each_year_percent, aes(x = Year_, y = Total_Wind_Percent, color = Area_)
) +
  geom_line(size = 0.8) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Wind Energy Shares (%) Over the Years (1990 - 2024)",
    x = NULL,
    y = "wind Energy Shares (%)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 13)
  )+ 
  theme(
    legend.position = "bottom",
    panel.grid = element_line(color = "transparent", linewidth = 0.2),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )
ggsave(
  filename = "wind_energy_percentage_over_the_years.png",   
  plot = last_plot(),                          
  width = 10, height = 6,                   
  dpi = 300,                               
  bg = "transparent"                       
)


# Reading the dataset
data99 <- read.csv("C:/Users/georg/Desktop/ESMT/Data Visualisation/Europe Yearly/europe_yearly_full_release_reshaped.csv")

data_long2 <- data99 %>%
  pivot_longer(
    cols = matches("^Variable|Value|Unit"),
    names_to = c(".value", "set"),
    names_pattern = "(Variable|Value|Unit)\\.?([0-9]*)"
  )

# Select the year (2024) and the relevant variables ("Solar", "Wind and solar")
solar_country_2024 <- data_long %>%
  filter(
    Year_ == 2024,
    Variable %in% c(
      "Solar",
      "wind and solar"
    )
  )

# Convert Value to numeric
solar_country_2024$Value <- as.numeric(solar_country_2024$Value)

# ---- TOP 10 Countries by Solar (TWh) ---- #
top10_solar_country_TWh <- solar_country_2024 %>%
  filter(Unit == "TWh") %>%
  group_by(Area_) %>%
  summarise(Total_Solar_TWh = sum(Value, na.rm = TRUE)) %>%
  arrange(desc(Total_Solar_TWh)) %>%
  slice_head(n = 10)

ggplot(
  top10_solar_country_TWh, 
  aes(x = reorder(Area, Total_Solar_TWh), 
      y = Total_Solar_TWh, fill = Area_)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "TOP 10 Countries by Solar Energy Generation (TWh, 2024)",
    x = NULL,
    y = "Solar Energy Generation (TWH)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# ---- TOP 10 Countries by Solar (%) ---- #
top10_solar_country_percent <- solar_country_2024 %>%
  filter(Unit == "%") %>%
  group_by(Area_) %>%
  summarise(Total_Solar_Percent = median(Value, na.rm = TRUE)) %>%
  arrange(desc(Total_Solar_Percent)) %>%
  slice_head(n = 10)

ggplot(
  top10_solar_country_percent, 
  aes(x = reorder(Area_, Total_Solar_Percent), 
      y = Total_Solar_Percent, fill = Area_)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "TOP 10 Countries by Solar Energy Share (% of total, 2024)",
    x = NULL,
    y = "Solar Energy Share (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

