library("shiny")
library("ggplot2")
server <- function(input, output) {
    co2_data <- read.csv("owid-co2-data.csv")
    co2_data1 <- co2_data
    co2_data <- filter(co2_data, iso_code != "")
    co2_data <- filter(co2_data, iso_code != "OWID_WRL")

    
    # page2 col1
    output$global_co2_value <- renderText({
      global_country_data <- co2_data %>%
        filter(year == input$page2_col1_slider_year)
      global_co2_value <- sum(global_country_data$co2, na.rm = TRUE)
      global_co2_value
    })
    output$global_pop_value <- renderText({
      global_country_data <- co2_data %>%
        filter(year == input$page2_col1_slider_year)
      global_pop_value <- sum(global_country_data$population,
                              na.rm = TRUE
      ) / 100000
      global_pop_value
    })
    output$global_gdp_value <- renderText({
      global_country_data <- co2_data %>%
        filter(year == input$page2_col1_slider_year)
      global_gdp_value <- sum(global_country_data$gdp,
                              na.rm = TRUE
      ) / 1000000000
      global_gdp_value
    })
    
    #page2 col2
    output$Continental_co2_value <- renderText({
      Continental_co2_value <- co2_data1 %>%
        filter(country == input$page2_col2_Continent_name) %>%
        filter(year == input$page2_col2_slider_year)
      Continental_co2_value <- Continental_co2_value$co2
      Continental_co2_value
    })
    output$Continental_pop_value <- renderText({
      Continental_pop_value <- co2_data1 %>%
        filter(country == input$page2_col2_Continent_name) %>%
        filter(year == input$page2_col2_slider_year)
      Continental_pop_value <- Continental_pop_value$population / 100000
      Continental_pop_value
    })
    output$Continental_gdp_value <- renderText({
      Continental_gdp_value <- co2_data1 %>%
        filter(country == input$page2_col2_Continent_name) %>%
        filter(year == input$page2_col2_slider_year)
      Continental_gdp_value <- Continental_gdp_value$gdp/ 1000000000
      Continental_gdp_value
    })
    
    #page2 col3
    output$National_co2_value <- renderText({
      National_co2_value <- co2_data %>%
        filter(country == input$page2_col3_country_name) %>%
        filter(year == input$page2_col3_slider_year)
      National_co2_value <- National_co2_value$co2
      National_co2_value
    })
    output$National_pop_value <- renderText({
      National_pop_value <- co2_data %>%
        filter(country == input$page2_col3_country_name) %>%
        filter(year == input$page2_col3_slider_year)
      National_pop_value <- National_pop_value$population / 100000
      National_pop_value
    })
    output$National_gdp_value <- renderText({
      National_gdp_value <- co2_data %>%
        filter(country == input$page2_col3_country_name) %>%
        filter(year == input$page2_col3_slider_year)
      National_gdp_value <- National_gdp_value$gdp/ 1000000000
      National_gdp_value
    })
    
    #page2 chart

    world_continent_names = c("World","Africa","Asia","Europe","North America","Oceania","South America")  

    output$page2_co2_chart <- renderPlot({
      page2_co2_chart_data <- co2_data1 %>%
        filter(country == world_continent_names) %>%
        filter(year >= input$page2_chart_start_year &
                 year <= input$page2_chart_end_year)
        
        page2_co2_chart <- ggplot(data = page2_co2_chart_data) +
        geom_smooth(mapping = aes(x = year, y = co2)) +
        facet_wrap(~ country,scales= "free") +
        labs(
          title = "Growth curves of Co2 emissions",
          x = "Year", y = "Co2 emissions (Unit: million tons)"
        )
        page2_co2_chart
    })
    output$page2_pop_chart <- renderPlot({
      page2_pop_chart_data <- co2_data1 %>%
        filter(country == world_continent_names) %>%
        filter(year >= input$page2_chart_start_year &
                 year <= input$page2_chart_end_year)
      
      page2_pop_chart <- ggplot(data = page2_pop_chart_data) +
        geom_smooth(mapping = aes(x = year, y = population/100000)) +
        facet_wrap(~ country,scales= "free") +
        labs(
          title = "Growth curves of Population",
          x = "Year", y = "Population (Unit: 100000 person)"
        )
      page2_pop_chart
    })
    
    output$page2_gdp_chart <- renderPlot({
      page2_gdp_chart_data <- co2_data1 %>%
        filter(country == "World") %>%
        filter(year >= input$page2_chart_start_year &
                 year <= input$page2_chart_end_year)
      
      page2_gdp_chart <- ggplot(data = page2_gdp_chart_data) +
        geom_smooth(mapping = aes(x = year, y = gdp/ 1000000000)) +
        labs(
          title = "Growth curves of global GDP",
          x = "Year", y = "GDP (Unit: billion US dollars)"
        )
      page2_gdp_chart
    })
    
    #######page3 code here!####### 
    
    #######page4 code here!####### 
    
    #######page5 code here!####### 
}
