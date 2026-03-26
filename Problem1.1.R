library(lubridate)
library(tidyverse)
library(forecast)
library(prophet) # Added for the Prophet model

data = read.csv("./Project Assignment/drug_deaths.csv")

# ---------------------------------------------------------
# SEGMENT 1 & 2: DATA PREP & HISTORICAL PLOTS (Your original code)
# ---------------------------------------------------------

# Clean the date format
data <- data %>%
  mutate(Date = mdy(word(Date, 1))) %>%
  filter(!is.na(Date))

# Grouping by Year (Barplot)
yearly_deaths <- data %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(Total_Deaths = n(), .groups = 'drop') %>%
  arrange(Year)

barplot(yearly_deaths$Total_Deaths, 
        names.arg = yearly_deaths$Year, 
        main = "Historical Annual Overdose Deaths (2012-2018)",
        ylab = "Number of Deaths", xlab = "Year",
        col = "steelblue",
        ylim = c(0, max(yearly_deaths$Total_Deaths) * 1.1))

# Grouping by Month (Time Series Prep)
monthly_deaths <- data %>% 
  mutate(YearMonth = floor_date(Date, "month")) %>% 
  group_by(YearMonth) %>% 
  summarise(Total_Deaths = n(), .groups = 'drop') %>%  
  arrange(YearMonth) 

ts_deaths <- ts(monthly_deaths$Total_Deaths, 
                start = c(2012, 1), frequency = 12) 

plot(ts_deaths, 
     main = "Historical Monthly Overdose Deaths (2012-2018)", 
     xlab = "Year", ylab = "Number of Deaths",
     col = "darkblue", lwd = 2)            


# ---------------------------------------------------------
# SEGMENT 3: FORECASTING COMPARISON (ARIMA vs ETS vs Prophet)
# ---------------------------------------------------------

# --- MODEL 1: ARIMA (Your original model) ---
fit_arima <- auto.arima(ts_deaths)
forecast_arima <- forecast(fit_arima, h = 12)

plot_arima <- autoplot(forecast_arima) +
  ggtitle("Model 1: ARIMA 12-Month Forecast") +
  xlab("Year") + ylab("Number of Deaths") +
  theme_minimal()
print(plot_arima)


# --- MODEL 2: ETS (Exponential Smoothing) ---
# ETS adapts better to recent volatility and changing trends
fit_ets <- ets(ts_deaths)
forecast_ets <- forecast(fit_ets, h = 12)

plot_ets <- autoplot(forecast_ets) +
  ggtitle("Model 2: ETS 12-Month Forecast") +
  xlab("Year") + ylab("Number of Deaths") +
  theme_minimal()
print(plot_ets)


# --- MODEL 3: PROPHET (Meta's Forecasting Tool) ---
# Prophet requires a dataframe with columns exactly named 'ds' (datestamp) and 'y' (value)
df_prophet <- monthly_deaths %>%
  rename(ds = YearMonth, y = Total_Deaths)

# Fit the Prophet model
m_prophet <- prophet(df_prophet, yearly.seasonality = TRUE)

# Create a dataframe for the next 12 months
future_prophet <- make_future_dataframe(m_prophet, periods = 12, freq = "month")

# Generate the forecast
forecast_prophet <- predict(m_prophet, future_prophet)

# Plot the Prophet forecast (Prophet has its own built-in ggplot integration)
plot_prophet <- plot(m_prophet, forecast_prophet) +
  ggtitle("Model 3: Prophet 12-Month Forecast") +
  xlab("Year") + ylab("Number of Deaths") +
  theme_minimal()
print(plot_prophet)