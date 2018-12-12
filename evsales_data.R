# gathering evsales data from yearly sources on inside EVs website
library(tidyverse)
library(tesseract)
library(lubridate)

# example of tesseract package usage
data_2010_raw <- tesseract::ocr_data(image = "2010-2011_sales.png") %>%
  select(word) %>%
  mutate(
    word = as.character(word),
    word = str_remove(word, ","),
    word = str_to_upper(word)
  )

# to convert date, use ymd(, truncated = 1)
data_2010 <- data_2010_raw[[1]] %>%
  t() %>%
  matrix(nrow = 4, ncol = 13) %>%
  t() %>%
  as.tibble() %>%
  rename(
    Month = V1,
    "Chevrolet Volt" = V2,
    "Nissan Leaf" = V3,
    "Mitsubishi iMiEV" = V4
  ) %>%
  mutate(
    Year = c(2010, rep(2011, 12)),
    Date = ymd(paste(Year, Month, sep = "-"), truncated = 1)
  ) %>%
  gather(key = Model, value = Sales, 2:4) %>%
  select(Model, Month, Sales, Year, Date) %>%
  mutate(Sales = as.integer(Sales))

str(data_2010)
str(data_2012)


# collect data from csvs
data_2012 <- read_csv("sales_2012.csv") %>%
  gather(key = Month, value = Sales, 2:13) %>%
  mutate(
    Year = 2012,
    Date = ymd(paste(Year, Month, sep = "-"), truncated = 1)
  )
data_2013 <- read_csv("sales_2013.csv") %>%
  gather(key = Month, value = Sales, 2:13) %>%
  mutate(
    Year = 2013,
    Date = ymd(paste(Year, Month, sep = "-"), truncated = 1)
  )
data_2014 <- read_csv("sales_2014.csv") %>%
  gather(key = Month, value = Sales, 2:13) %>%
  mutate(
    Year = 2014,
    Date = ymd(paste(Year, Month, sep = "-"), truncated = 1)
  )
data_2015 <- read_csv("sales_2015.csv") %>%
  gather(key = Month, value = Sales, 2:13) %>%
  mutate(
    Year = 2015,
    Date = ymd(paste(Year, Month, sep = "-"), truncated = 1)
  )
data_2016 <- read_csv("sales_2016.csv") %>%
  gather(key = Month, value = Sales, 2:13) %>%
  mutate(
    Year = 2016,
    Date = ymd(paste(Year, Month, sep = "-"), truncated = 1)
  )
data_2017 <- read_csv("sales_2017.csv") %>%
  gather(key = Month, value = Sales, 2:13) %>%
  mutate(
    Year = 2017,
    Date = ymd(paste(Year, Month, sep = "-"), truncated = 1)
  )
data_2018 <- read_csv("sales_2018.csv") %>%
  gather(key = Month, value = Sales, 2:12) %>%
  mutate(
    Year = 2018,
    Date = ymd(paste(Year, Month, sep = "-"), truncated = 1)
  )


# combine data
ev_data_raw_combined <- bind_rows(list(
  data_2012,
  data_2013,
  data_2014,
  data_2015,
  data_2016,
  data_2017,
  data_2018
))


# homogenize model names
ev_data_homogenized_model_names <- ev_data_raw_combined %>% mutate(
  Model = ifelse(Model == "Audi A3 Sprtbk e-tron",
    "Audi A3 Sportback e-tron", Model
  ),
  Model = ifelse(Model == "BMWX5 xDrive 40e",
    "BMWX 5 xDrive 40e", Model
  ),
  Model = ifelse(Model == "BMW i3 (BEV + REx)",
    "BMW i3", Model
  ),
  Model = ifelse(Model == "Porsche Panamera E-Hybrid",
    "Porsche Panamera S-E", Model
  ),
  Model = ifelse(Model == "Honda fit EV",
    "Honda Fit EV", Model
  )
)

# Find which models are hybrid and which are battery electric
EV_models <- unique(ev_data$Model) %>% as.tibble()

# create dataframe of models and their fuel source
fuel <- read_csv("fuel.csv") %>% mutate(
  Fuel = ifelse(Fuel == 1, "BEV", "PHEV"),
  Model = str_replace(Model, pattern = "\\*", replacement = ""),
  Model = str_trim(string = Model, side = c("right"))
)

# check if any model is missing fuel type
#missing_fuel <- ev_data %>% anti_join(y = fuel, by = "Model") # should be empty

# export csv of models missing fuel
# other_models <- unique(other_fuels$Model) %>%
#   as.tibble() %>%
#   write_csv(path = file.path(getwd(), "remaining_fuel.csv"))

# add fuel type, and brand/model name columns
ev_data <- ev_data_homogenized_model_names %>%
  left_join(y = fuel, by = "Model") %>%
  mutate(
    Name = Model,
    Brand = stringr::word(Model, sep = " "),
    Model = stringr::word(Name, 2, -1, sep = " ")
  ) %>%
  select(Brand, Model, Name, Fuel, Year, Month, Date, Sales )


evsales_path <- getwd()

readr::write_csv(
  x = ev_data,
  path = file.path(evsales_path, "evsales_data.csv")
)
