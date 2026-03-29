# forecast.R

# --- UI COMPONENT ---
forecast_ui <- nav_panel("Predictive: Time-Series Forecast",
                         layout_columns(
                             fill = FALSE,
                             value_box(
                                 title = "Historical Fatalities", value = format(total_historical_deaths, big.mark = ","),
                                 showcase = bs_icon("clipboard-data"), theme = "secondary"
                             ),
                             value_box(
                                 title = "Historical Peak Year", value = peak_year,
                                 showcase = bs_icon("graph-up-arrow"), theme = "warning"
                             ),
                             value_box(
                                 title = "Forecasted (Next 12 Months)", value = format(forecasted_next_year, big.mark = ","),
                                 showcase = bs_icon("calendar2-plus"), theme = "danger"
                             )
                         ),
                         br(),
                         fluidRow(
                             column(6, card(full_screen = TRUE, card_body(plotlyOutput("plot_yearly", height = "350px")))),
                             column(6, card(full_screen = TRUE, card_body(plotlyOutput("plot_monthly", height = "350px"))))
                         ),
                         br(),
                         fluidRow(
                             column(12, card(full_screen = TRUE, card_body(plotlyOutput("plot_forecast", height = "450px"))))
                         )
)

# --- SERVER COMPONENT ---
forecast_server <- function(input, output, session) {
    
    output$plot_yearly <- renderPlotly({
        p <- ggplot(yearly_deaths, aes(x = factor(Year), y = Total_Deaths)) +
            geom_bar(stat = "identity", fill = "steelblue", color = "black", linewidth = 0.2) +
            theme_minimal() +
            labs(title = "Fatalities Peaked in 2017 Before a Slight Drop in 2018", x = "Year", y = "Number of Deaths")
        
        ggplotly(p, tooltip = c("x", "y"))
    })
    
    output$plot_monthly <- renderPlotly({
        p <- ggplot(monthly_deaths, aes(x = YearMonth, y = Total_Deaths)) +
            geom_line(color = "darkblue", linewidth = 1) +
            # I added formula = y ~ x here to silence the console message
            geom_smooth(method = "loess", formula = y ~ x, color = "firebrick", se = FALSE, linetype = "dashed", linewidth = 0.5) +
            theme_minimal() +
            labs(title = "Monthly Overdoses Accelerated Sharply Starting in Late 2015", x = "Year", y = "Number of Deaths")
        
        ggplotly(p, tooltip = c("x", "y"))
    })
    
    output$plot_forecast <- renderPlotly({
        
        # 1. Prepare historical data — explicitly coerce YearMonth to Date
        df_hist <- data.frame(
            Date    = as.Date(monthly_deaths$YearMonth),
            Deaths  = as.numeric(monthly_deaths$Total_Deaths),
            Type    = "Historical",
            Lower80 = NA_real_, Upper80 = NA_real_,
            Lower95 = NA_real_, Upper95 = NA_real_
        )
        
        # 2. Generate forecast dates anchored on the last historical Date
        last_date  <- max(df_hist$Date)
        # Use lubridate to reliably step by month
        fore_dates <- last_date %m+% months(1:12)   # <-- KEY FIX
        
        # 3. Prepare forecast data
        df_fore <- data.frame(
            Date    = fore_dates,
            Deaths  = as.numeric(forecast_12_months$mean),
            Type    = "Forecast",
            Lower80 = as.numeric(forecast_12_months$lower[, 1]),
            Upper80 = as.numeric(forecast_12_months$upper[, 1]),
            Lower95 = as.numeric(forecast_12_months$lower[, 2]),
            Upper95 = as.numeric(forecast_12_months$upper[, 2])
        )
        
        # 4. Connection point so the line doesn't gap
        connection_point <- df_hist[nrow(df_hist), ]
        connection_point$Type    <- "Forecast"
        connection_point$Lower80 <- connection_point$Deaths
        connection_point$Upper80 <- connection_point$Deaths
        connection_point$Lower95 <- connection_point$Deaths
        connection_point$Upper95 <- connection_point$Deaths
        
        # 5. Combine
        df_all <- rbind(df_hist, connection_point, df_fore)
        
        # 6. Plot
        p <- ggplot(df_all, aes(x = Date, y = Deaths)) +
            geom_ribbon(aes(ymin = Lower95, ymax = Upper95),
                        fill = "steelblue", alpha = 0.2, na.rm = TRUE) +
            geom_ribbon(aes(ymin = Lower80, ymax = Upper80),
                        fill = "steelblue", alpha = 0.4, na.rm = TRUE) +
            geom_line(aes(color = Type, linetype = Type), linewidth = 1) +
            scale_color_manual(values    = c("Historical" = "black", "Forecast" = "firebrick")) +
            scale_linetype_manual(values = c("Historical" = "solid", "Forecast" = "dashed")) +
            theme_minimal() +
            labs(
                title   = "12-Month ARIMA Forecast Predicts Continued Volatility",
                x       = "Date", y = "Monthly Deaths",
                color   = "Data Type", linetype = "Data Type"
            )
        
        ggplotly(p, tooltip = c("x", "y"))
    })
}