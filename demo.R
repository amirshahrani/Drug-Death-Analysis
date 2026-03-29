# demo.R

# --- UI COMPONENT ---
demo_ui <- nav_panel("Diagnostic: Demographics",
                     layout_columns(
                         fill = FALSE,
                         value_box(
                             title = "Total Cases Analyzed", value = format(total_analyzed, big.mark = ","),
                             showcase = bs_icon("people-fill"), theme = "primary"
                         ),
                         value_box(
                             title = "Male Victim Rate", value = paste0(pct_male, "%"),
                             showcase = bs_icon("gender-male"), theme = "info"
                         ),
                         value_box(
                             title = "Most Lethal Cocktail", value = "Heroin + Fentanyl",
                             showcase = bs_icon("bandaid"), theme = "danger"
                         )
                     ),
                     br(),
                     fluidRow(
                         column(6, card(full_screen = TRUE, card_body(plotlyOutput("plot_race_sex", height = "380px")))),
                         column(6, card(full_screen = TRUE, card_body(plotlyOutput("plot_age_risk", height = "380px"))))
                     ),
                     br(),
                     fluidRow(
                         column(6, card(full_screen = TRUE, card_body(plotlyOutput("plot_combos", height = "380px")))),
                         column(6, card(full_screen = TRUE, card_body(plotlyOutput("plot_heatmap", height = "380px"))))
                     )
)

# --- SERVER COMPONENT ---
demo_server <- function(input, output, session) {
    output$plot_race_sex <- renderPlotly({
        race_summary <- df_demo %>%
            add_count(Race, name = "Race_Total") %>%
            mutate(Race = ifelse(Race_Total > 50, Race, "Other")) %>%
            group_by(Race, Sex) %>% summarise(Total_Deaths = n(), .groups = "drop")
        
        p <- ggplot(race_summary, aes(x = Race, y = Total_Deaths, fill = Sex)) +
            geom_bar(stat = "identity", position = "dodge", color = "black", linewidth = 0.2) +
            theme_minimal() +
            labs(title = "White Demographic Represents the Highest Overdose Volume", x = "Race", y = "Total Deaths", fill = "Sex") +
            scale_fill_manual(values = c("Male" = "steelblue", "Female" = "orchid3"))
        ggplotly(p, tooltip = c("x", "fill", "y"))
    })
    
    output$plot_age_risk <- renderPlotly({
        age_risk_summary <- df_demo %>% group_by(Age_Group, Sex) %>% summarise(Total_Deaths = n(), .groups = "drop")
        p <- ggplot(age_risk_summary, aes(x = Age_Group, y = Total_Deaths, fill = Sex)) +
            geom_bar(stat = "identity", position = "dodge", color = "black", linewidth = 0.2) +
            theme_minimal() +
            labs(title = "Males Aged 25-54 Carry the Highest Fatality Risk", x = "Age Group", y = "Total Deaths") +
            scale_fill_manual(values = c("Male" = "steelblue", "Female" = "orchid3"))
        ggplotly(p, tooltip = c("x", "fill", "y"))
    })
    
    output$plot_combos <- renderPlotly({
        combo_summary <- df_demo %>%
            rowwise() %>%
            mutate(Combination = paste(significant_drugs[c_across(all_of(significant_drugs)) == 1], collapse = " + ")) %>%
            ungroup() %>% filter(grepl("\\+", Combination)) %>% group_by(Combination) %>%
            summarise(Total_Deaths = n(), .groups = "drop") %>% arrange(desc(Total_Deaths)) %>% slice_head(n = 10)
        
        p <- ggplot(combo_summary, aes(x = reorder(Combination, Total_Deaths), y = Total_Deaths)) +
            geom_bar(stat = "identity", fill = "firebrick", color = "black", linewidth = 0.2) +
            coord_flip() + theme_minimal() +
            labs(title = "Heroin + Fentanyl is the Most Lethal Polysubstance Cocktail", x = "", y = "Total Overdose Deaths")
        ggplotly(p, tooltip = c("y"))
    })
    
    output$plot_heatmap <- renderPlotly({
        pattern_summary_all <- df_demo %>%
            group_by(Race) %>% mutate(Total_Cases = n()) %>% filter(Total_Cases > 50) %>%
            summarise(across(all_of(significant_drugs), ~ mean(., na.rm = TRUE) * 100), .groups = "drop") %>%
            pivot_longer(cols = all_of(significant_drugs), names_to = "Drug", values_to = "Percentage")
        
        p <- ggplot(pattern_summary_all, aes(x = Drug, y = Race, fill = Percentage, 
                                             text = paste0(Race, " - ", Drug, ": ", round(Percentage, 1), "%"))) +
            geom_tile(color = "white", linewidth = 0.5) + scale_fill_gradient(low = "#f7fbff", high = "#08519c") +
            theme_minimal() +
            labs(title = "Distinct Drug Usage Patterns Exist Across Races", x = "Substance", y = "Race", fill = "Usage %") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggplotly(p, tooltip = "text")
    })
}