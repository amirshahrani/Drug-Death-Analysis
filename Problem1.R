library(lubridate)
library(tidyverse)
library(forecast)

data = read.csv("./Project Assignment/drug_deaths.csv")

# Clean the date format
data <- data %>%
  mutate(Date = mdy(word(Date, 1))) %>%
  filter(!is.na(Date))


# The first segment is to plot the data into yearly overdose deaths
# Starts with group the data by year by creating new dataframe count death for each year
# After that, we plot it using barplot for easier reading
# Observation: The number of deaths increase from 2012 until 2017 and drop at 2018


# Grouping by Year
yearly_deaths <- data %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(Total_Deaths = n(), .groups = 'drop') %>%
  arrange(Year)

head(yearly_deaths)

barplot(yearly_deaths$Total_Deaths, 
        names.arg = yearly_deaths$Year, 
        main = "Historical Annual Overdose Deaths (2012-2018)",
        ylab = "Number of Deaths",
        xlab = "Year",
        col = "steelblue",
        ylim = c(0, max(yearly_deaths$Total_Deaths) * 1.1)) # Gives a little headroom for the tallest bar


# The second segment is to plot the data in time series graph
# Starts with group the data by month by creating dataframe to get more precise insight
# After that, using ts() function to convert it to time series format starting from Jan 2012
# Then, we plot it in time series plot format
# Observation: Consistent increase until 2018 but accelerate during late 2015


#Grouping by Month
#Create new dataframe called monthly deaths with column called YearMonth and Total_Deaths

monthly_deaths <- data %>% #takes data to use
  mutate(YearMonth = floor_date(Date, "month")) %>% #create new col called "YearMonth" and using floor_date to change every month to YYYY-MM-01
  group_by(YearMonth) %>% # group by month 
  summarise(Total_Deaths = n(), .groups = 'drop') %>%  #count every death per month
  arrange(YearMonth) #sort from oldest to newest

head(monthly_deaths)

ts_deaths <- ts(monthly_deaths$Total_Deaths, 
                start = c(2012, 1), # starts in January 2012
                frequency = 12) # For 12 months

plot(ts_deaths, 
     main = "Historical Monthly Overdose Deaths (2012-2018)", 
     xlab = "Year", 
     ylab = "Number of Deaths",
     col = "darkblue",
     lwd = 2)            

# The third segment is to forecast next 12 months overdose deaths
# Starts with using ARIMA model to learn the time series data
# After that, plot it using time series plot
# Observation: The shaded area continue the upward trend, it suggests that there will be more higher spikes because of its high unpredictability. 
# The dark blue region (80% confidence interval) and the light blue is the 95% confidence interval).

# Fit the ARIMA model
fit <- auto.arima(ts_deaths)

# View the model summary (AIC, BIC, and error metrics)
summary(fit)

# Forecast the next 12 periods (months)
forecast_12_months <- forecast(fit, h = 12)

# Plot the forecast
forecast_plot <- autoplot(forecast_12_months) +
  ggtitle("12-Month Forecast of Monthly Overdose Deaths") +
  xlab("Year") +
  ylab("Number of Deaths") +
  theme_minimal()

print(forecast_plot)