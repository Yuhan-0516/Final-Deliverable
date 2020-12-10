library("shiny")
library("ggplot2")
library("tidyverse")
library("maps")
library("countrycode")
library("rworldmap")
library("plotly")
gc()
server <- function(input, output) {
  co2_data <- read.csv("data/owid-co2-data.csv")
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
  co2_updated <- read.csv("data/owid-co2-data-updated.csv")
  
  # Cleaning up data #
  co2_without_groups <- co2_updated %>%
    filter(iso_code != "") %>%
    filter(country != "World")
  
  # Change Column Names for appropriate labeling #
  df <- co2_without_groups
  names(df)[names(df) == "co2"] <- "Annual Production Based CO2 Emissions" #omit
  names(df)[names(df) == "co2_growth_prct"] <- "Percentage change in CO2 emissions"
  names(df)[names(df) == "co2_growth_abs"] <- "Annual change in CO2 emissions measured in million tonnes"
  names(df)[names(df) == "consumption_co2"] <- "Annual consumption-based CO2 emissions"
  names(df)[names(df) == "trade_co2"] <- "Net CO2 emissions embedded in trade"
  names(df)[names(df) == "trade_co2_share"] <- "CO2 emissions embedded in trade"
  names(df)[names(df) == "co2_per_capita"] <- "Average per capita CO2 emissions"
  names(df)[names(df) == "consumption_co2_per_capita"] <- "Per capita consumption-based CO2 emissions"
  names(df)[names(df) == "share_global_co2"] <- "National or regional annual CO2 emissions"
  names(df)[names(df) == "cumulative_co2"] <- "Cumulative emissions of CO2 from 1751 through to the given year"
  names(df)[names(df) == "share_global_cumulative_co2"] <- "National or regional annual cumulative CO2 emissions"
  names(df)[names(df) == "co2_per_gdp"] <- "CO2 emissions measured per unit of gross domestic product"
  names(df)[names(df) == "consumption_co2_per_gdp"] <- "Consumption-based CO2 emissions measured per unit of gross domestic product"
  names(df)[names(df) == "co2_per_unit_energy"] <- "CO2 emissions measured per unit of energy consumed"
  names(df)[names(df) == "cement_co2"] <- "CO2 emissions from cement production"
  names(df)[names(df) == "coal_co2"] <- "CO2 emissions from coal production" # inspect
  names(df)[names(df) == "flaring_co2"] <- "CO2 emissions from gas flaring" # inspect
  names(df)[names(df) == "gas_co2"] <- "CO2 emissions from gas production" # inspect
  names(df)[names(df) == "oil_co2"] <- "CO2 emissions from oil production" # inspect
  names(df)[names(df) == "cement_co2_per_capita"] <- "Per capita CO2 emissions from cement production"
  names(df)[names(df) == "coal_co2_per_capita"] <- "Per capita CO2 emissions from coal production"
  names(df)[names(df) == "flaring_co2_per_capita"] <- "Per capita CO2 emissions from flaring"
  names(df)[names(df) == "gas_co2_per_capita"] <- "Per capita CO2 emissions from gas production"
  names(df)[names(df) == "oil_co2_per_capita"] <- "Per capita CO2 emissions from oil production"
  names(df)[names(df) == "total_ghg"] <- "Annual greenhouse gas emissions, measured"
  names(df)[names(df) == "ghg_per_capita"] <- "Greenhouse gas emissions per capita"
  names(df)[names(df) == "methane"] <- "Annual methane emissions"
  names(df)[names(df) == "methane_per_capita"] <- "Methane emissions per capita"
  names(df)[names(df) == "nitrous_oxide"] <- "Annual nitrous oxide emissions"
  names(df)[names(df) == "nitrous_oxide_per_capita"] <- "Nitrous oxide emissions per capita"
  names(df)[names(df) == "primary_energy_consumption"] <- "Primary energy consumption"
  names(df)[names(df) == "energy_per_capita"] <- "Primary energy consumption per capita" # inspect
  names(df)[names(df) == "energy_per_gdp"] <- "Primary energy consumption per unit of gross domestic product" # inspect
  names(df)[names(df) == "population"] <- "Total population"
  names(df)[names(df) == "gdp"] <- "Total real gross domestic product, inflation-adjusted"
  
  col_names_df <- colnames(co2_without_groups)
  col_names_df_2 <- colnames(df)
  
  output$page3_heatmap <- renderPlotly({
    world_shape <- map_data("world2")
    
    iso_code <- countrycode(sourcevar = world_shape$region,
                            origin = "country.name",
                            destination = "iso3c")
    
    world_shape_iso <- cbind(world_shape, iso_code) %>%
      left_join(co2_without_groups, by = "iso_code")
    
    index <- match(input$metric_input, col_names_df)
    
    heatmap <- ggplot(world_shape_iso %>% filter(year == input$time_input)) +
      geom_polygon(
        mapping = aes_string(x = "long", y = "lat", group = "group", fill = input$metric_input, label = "country"),
        color = "white",
        size = .1
      ) +
      labs(fill = col_names_df_2[index]) +
      xlab("Latitude") +
      ylab("Longitude") +
      coord_map() +
      scale_fill_continuous(low = "Green", high = "Red") 
    theme_gray()
    
    return(ggplotly(heatmap))
  })
  
  output$page3_charts <- renderPlotly({
    
    continents_list <- c("Asia", "Europe", "Africa", "South America", "North America", "Oceania")
    
    colors <- c("#003f5c", "#444e86", "#955196", "#dd5182", "#ff6e54", "#ffa600")
    
    data_by_continent <- co2_updated %>%
      filter(country %in% continents_list) %>%
      filter(year == input$time_input)
    
    
    bar_plot <- ggplot(data_by_continent) +
      geom_bar(aes_string(x = "country", y = "share_global_co2"),
               stat = "identity",
               fill = colors,
               width = 0.5,
               na.rm = T) +
      xlab("Continent") +
      ylab("Continental annual CO2 emissions") +
      theme_gray()
    
    return(ggplotly(bar_plot))
  })
  
  #######page4 code here!#######
  
  output$page4_co2_growth_prct_chart <- renderPlot({
    page4_chart_data <- co2_without_groups %>%
      filter(country == input$page4_country_name) %>%
      filter(year >= input$page4_chart_start_year & year <= input$page4_chart_end_year) %>% 
      summarise(country, year, co2_growth_prct, co2_per_capita, co2_per_gdp)
    
    page4_co2_growth_prct_chart <- ggplot(data = page4_chart_data) +
      geom_smooth(mapping = aes(x = year, y = co2_growth_prct)) +
      labs(
        title = "Percentage change in CO2 emissions",
        x = "Year", y = "Change in %"
      )
    return(page4_co2_growth_prct_chart)
  })
  
  output$page4_co2_per_capita_chart <- renderPlot({
    page4_chart_data <- co2_without_groups %>%
      filter(country == input$page4_country_name) %>%
      filter(year >= input$page4_chart_start_year & year <= input$page4_chart_end_year) %>% 
      summarise(country, year, co2_growth_prct, co2_per_capita, co2_per_gdp)
    
    page4_co2_per_capita_chart <- ggplot(data = page4_chart_data) +
      geom_smooth(mapping = aes(x = year, y = co2_per_capita)) +
      labs(
        title = "Average per capita CO2 emissions",
        x = "Year", y = "Co2 in tons"
      )
    return(page4_co2_per_capita_chart)
  })
  
  output$page4_co2_per_gdp_chart <- renderPlot({
    page4_chart_data <- co2_without_groups %>%
      filter(country == input$page4_country_name) %>%
      filter(year >= input$page4_chart_start_year & year <= input$page4_chart_end_year) %>% 
      summarise(country, year, co2_growth_prct, co2_per_capita, co2_per_gdp)
    
    page4_co2_per_gdp_chart <- ggplot(data = page4_chart_data) +
      geom_smooth(mapping = aes(x = year, y = co2_per_gdp)) +
      labs(
        title = "CO2 emissions measured per unit of gross domestic product",
        x = "Year", y = "Co2 in tons"
      )
    return(page4_co2_per_gdp_chart)
    
  })
  
}