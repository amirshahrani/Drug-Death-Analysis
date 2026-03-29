# geo.R
library(ggplot2)
library(maps)

# Build city-level growth trends from the globally prepared raw dataset `df`.
build_geo_hotspots <- function(df_input) {
    df_spatial <- df_input %>%
        mutate(
            coords = stringr::str_extract(DeathCityGeo, "\\(([^)]+)\\)"),
            coords = stringr::str_remove_all(coords, "\\(|\\)"),
            Lat = as.numeric(stringr::str_split_i(coords, ",", 1)),
            Lon = as.numeric(stringr::str_split_i(coords, ",", 2)),
            Year = lubridate::year(lubridate::mdy(stringr::word(Date, 1)))
        ) %>%
        filter(!is.na(Lat), !is.na(Lon), !is.na(Year), !is.na(DeathCity), DeathCity != "")

    city_trends <- df_spatial %>%
        mutate(Period = if_else(Year <= 2015, "Baseline_12_15", "Recent_16_18")) %>%
        count(DeathCity, Period) %>%
        tidyr::pivot_wider(names_from = Period, values_from = n, values_fill = list(n = 0)) %>%
        mutate(
            Growth_Volume = Recent_16_18 - Baseline_12_15,
            Growth_Rate = if_else(Baseline_12_15 > 0, (Growth_Volume / Baseline_12_15) * 100, Inf)
        ) %>%
        filter(Growth_Volume > 0) %>%
        arrange(desc(Growth_Volume))

    city_coords <- df_spatial %>%
        group_by(DeathCity) %>%
        summarise(
            Lat = mean(Lat, na.rm = TRUE),
            Lon = mean(Lon, na.rm = TRUE),
            .groups = "drop"
        )

    emerging_hotspots <- city_trends %>%
        inner_join(city_coords, by = "DeathCity")

    list(
        emerging_hotspots = emerging_hotspots,
        top_hotspots = head(emerging_hotspots, 20)
    )
}

# --- UI COMPONENT ---
geo_ui <- nav_panel(
    "Prescriptive: Geographic Hotspots",
    layout_columns(
        fill = FALSE,
        value_box(
            title = "Emerging Hotspot Cities",
            value = textOutput("geo_total_hotspots", inline = TRUE),
            showcase = bs_icon("geo-alt-fill"),
            theme = "danger"
        ),
        value_box(
            title = "Largest Net Increase",
            value = textOutput("geo_largest_growth", inline = TRUE),
            showcase = bs_icon("graph-up"),
            theme = "warning"
        ),
        value_box(
            title = "Highest Growth City",
            value = textOutput("geo_top_city", inline = TRUE),
            showcase = bs_icon("pin-map-fill"),
            theme = "primary"
        )
    ),
    br(),
    fluidRow(
        column(6, card(full_screen = TRUE, card_body(plotlyOutput("geo_plot_bar", height = "420px")))),
        column(6, card(full_screen = TRUE, card_body(plotlyOutput("geo_plot_map", height = "420px"))))
    )
)

# --- SERVER COMPONENT ---
geo_server <- function(input, output, session) {
    geo_data <- build_geo_hotspots(df)

    output$geo_total_hotspots <- renderText({
        format(nrow(geo_data$emerging_hotspots), big.mark = ",")
    })

    output$geo_largest_growth <- renderText({
        if (nrow(geo_data$emerging_hotspots) == 0) {
            return("0")
        }
        format(max(geo_data$emerging_hotspots$Growth_Volume, na.rm = TRUE), big.mark = ",")
    })

    output$geo_top_city <- renderText({
        if (nrow(geo_data$emerging_hotspots) == 0) {
            return("N/A")
        }
        geo_data$emerging_hotspots$DeathCity[1]
    })

    output$geo_plot_bar <- renderPlotly({
        if (nrow(geo_data$top_hotspots) == 0) {
            empty <- ggplot() +
                annotate("text", x = 1, y = 1, label = "No hotspot data available") +
                theme_void()
            return(ggplotly(empty))
        }

        p_bar <- ggplot(geo_data$top_hotspots, aes(x = reorder(DeathCity, Growth_Volume), y = Growth_Volume)) +
            geom_col(fill = "darkred", alpha = 0.85) +
            coord_flip() +
            geom_text(aes(label = paste0("+", Growth_Volume)), hjust = -0.2, size = 3.5, color = "black") +
            scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
            theme_minimal() +
            labs(
                title = "Top 20 Emerging Overdose Hotspots",
                subtitle = "Net increase in fatal overdoses (2016-2018 vs 2012-2015)",
                x = "City",
                y = "Net Increase in Deaths"
            )

        ggplotly(p_bar, tooltip = c("x", "y"))
    })

    output$geo_plot_map <- renderPlotly({
        if (nrow(geo_data$emerging_hotspots) == 0) {
            empty <- ggplot() +
                annotate("text", x = 1, y = 1, label = "No hotspot data available") +
                theme_void()
            return(ggplotly(empty))
        }

        ct_county_map <- ggplot2::map_data("county", region = "connecticut")
        top_labels <- head(geo_data$emerging_hotspots, 10)

        p_map <- ggplot() +
            geom_polygon(
                data = ct_county_map,
                aes(x = long, y = lat, group = group),
                fill = "grey90",
                color = "white",
                linewidth = 0.8
            ) +
            geom_point(
                data = geo_data$emerging_hotspots,
                aes(
                    x = Lon, y = Lat, size = Growth_Volume,
                    text = paste0(
                        DeathCity, "<br>Growth: +", Growth_Volume,
                        "<br>Growth Rate: ", round(Growth_Rate, 1), "%"
                    )
                ),
                color = "darkred",
                alpha = 0.65
            ) +
            geom_text(
                data = top_labels,
                aes(x = Lon, y = Lat, label = DeathCity),
                size = 3,
                vjust = -0.7,
                fontface = "bold"
            ) +
            scale_size_continuous(range = c(2, 14), name = "Net Increase") +
            coord_quickmap() +
            theme_void() +
            labs(
                title = "Suggested Zones for Narcan and Resource Allocation",
                subtitle = "Bubble size scales with emerging overdose volume"
            )

        ggplotly(p_map, tooltip = "text")
    })
}
