# data_cleaning.R
library(tidyverse)
library(lubridate)
library(forecast)

# Load raw data
df <- read.csv("drug_deaths.csv")

# ==========================================
# A. DEMOGRAPHIC DATA PREP
# ==========================================
drug_columns <- c("Heroin", "Cocaine", "Fentanyl", "Fentanyl_Analogue", 
                  "Oxycodone", "Oxymorphone", "Ethanol", "Hydrocodone", 
                  "Benzodiazepine", "Methadone", "Amphet", "Tramad", "Hydromorphone")
missing_flags <- c("", " ", "N/A", "NA", "Unknown", "NULL")

df_demo <- df %>%
    mutate(across(c(Sex, Race), ~ ifelse(. %in% missing_flags, NA, .))) %>%
    select(ID, Age, Sex, Race, all_of(drug_columns)) %>%
    mutate(Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age)) %>%
    drop_na(Sex, Race) %>%
    mutate(Age_Group = cut(Age, 
                           breaks = c(0, 24, 34, 44, 54, 64, 100), 
                           labels = c("Under 25", "25-34", "35-44", "45-54", "55-64", "65+"))) %>%
    mutate(across(all_of(drug_columns), 
                  ~ as.numeric(ifelse(grepl("Y|1", toupper(as.character(.))), 1, 0))))

significant_drugs <- c()
for (drug in drug_columns) {
    test_data <- df_demo %>% drop_na(Race, all_of(drug))
    test_result <- suppressWarnings(chisq.test(table(test_data$Race, test_data[[drug]])))
    if(test_result$p.value < 0.05) {
        significant_drugs <- c(significant_drugs, drug)
    }
}

# ==========================================
# B. TIME-SERIES DATA PREP
# ==========================================
df_time <- df %>%
    mutate(Date = mdy(word(Date, 1))) %>%
    filter(!is.na(Date))

yearly_deaths <- df_time %>%
    mutate(Year = year(Date)) %>%
    group_by(Year) %>%
    summarise(Total_Deaths = n(), .groups = 'drop') %>%
    arrange(Year)

monthly_deaths <- df_time %>%
    mutate(YearMonth = floor_date(Date, "month")) %>%
    group_by(YearMonth) %>%
    summarise(Total_Deaths = n(), .groups = 'drop') %>%
    arrange(YearMonth)

ts_deaths <- ts(monthly_deaths$Total_Deaths, start = c(2012, 1), frequency = 12)
fit <- auto.arima(ts_deaths)
forecast_12_months <- forecast(fit, h = 12)

# ==========================================
# C. CALCULATE KPIs
# ==========================================
total_analyzed <- nrow(df_demo)
pct_male <- round((sum(df_demo$Sex == "Male", na.rm = TRUE) / total_analyzed) * 100)
total_historical_deaths <- sum(yearly_deaths$Total_Deaths)
forecasted_next_year <- round(sum(forecast_12_months$mean))
peak_year <- yearly_deaths$Year[which.max(yearly_deaths$Total_Deaths)]