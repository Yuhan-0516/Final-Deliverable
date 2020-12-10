library("shiny")
library("dplyr")
library("plotly")

co2_data <- read.csv("data/owid-co2-data.csv")
co2_data <- filter(co2_data, iso_code != "")
co2_data <- filter(co2_data, iso_code != "OWID_WRL")

country_names <- co2_data %>%
    filter(year == 2018) %>%
    select(country)

continent_names <- c("Africa","Asia","Europe","North America",
                     "Oceania","South America")

includeCSS("style.css")

page_one <- tabPanel(

    "Introduction",
    includeCSS("style.css"),
    tags$body(
        tags$div(
        tags$h2("Greenhouse effect and CO2 emissions"),
        tags$hr(),
        tags$p("CO2 has the effect of hindering
               the spread of infrared rays. Therefore, when the content of CO2
               in the atmosphere increases, the heat transmitted by the earth
               to outer space will decrease, and the temperature will rise.
               This is known as the Greenhouse effect and CO2 is a major greenhouse gas. The greenhouse effect will have
               significant impacts on environment. So for our project we want to address the severity 
               of these issues on our environment through the use of data. In doing so, we would also like
               to identify potential underlying trends between different CO2 emission data with economic factors and other natural factors such as population growth and energy consumption. By doing I hope we could educate people more on climate change and raise awareness about this global issue."),
        tags$ul( tags$li("# Increase of insect pests"),
                 tags$image( src ="http://www.fuannuo.cn/upload/202002/1581933767722096.jpg",
                             width="500", height="350"),
                 tags$li("# Glaciers melting and sea level rising"),
                 tags$image( src ="https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1607266830585&di=1db9f60f49a3e646acff139e930bf9a9&imgtype=0&src=http%3A%2F%2Fp.ssl.qhimg.com%2Ft01a26246fb31e8052e.jpg",
                             width="500", height="350"),
                 tags$li("# Increase of extreme weather and storms"),
                 tags$image( src ="https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1607266977395&di=de01d3148420da5b2a60844a41d7b27c&imgtype=0&src=http%3A%2F%2Fe.hiphotos.baidu.com%2Fbaike%2Fpic%2Fitem%2Fe4dde71190ef76c6737f622a9616fdfaaf516706.jpg",
                             width="500", height="280"),
                 tags$li("# Increase of droughts"),
                 tags$image( src ="https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1607267156300&di=7e8311ae93b19031c0fcf638e396f725&imgtype=0&src=http%3A%2F%2Fn.sinaimg.cn%2Fsinacn15%2F400%2Fw1200h800%2F20180322%2F05df-fysnevk6209859.jpg",
                             width="500", height="350")
                 ),
        ),

        tags$h2("Development vs. Environmental protection"),
        tags$hr(),
        tags$p("The global population is increasing and the economy is
               developing, which will lead to the continuous production of Co2.
               From this point, environmental protection and human
               development seem to be a pair of contradictory subjects.
               But we can't stop development just because of environmental
               protection, nor can we just consider development
               and ignore environmental protection."),
        tags$h3(tags$strong("Green development is our solution!")),

        tags$h2("Main research issues"),
        tags$hr(),
        tags$ul( tags$li("What are the real datasets on CO2 emissions
                    (including global, continental and national)?"),
                 tags$li("What is the relationship between Co2 emissions
                         and population, GDP?"),
                 tags$li("What are the main sources of Co2 emissions?"),
                 tags$li("What countries are doing well in green development?")
        ),
        tags$p("It is hoped that through the study of these issues,
               people can be better familiar with the Co2 emissions!"),

        tags$br(),
        tags$h3("*Declaration on datasets"),
        tags$p("The dataset used by our group does not have data of every year,
               and the time span is short. HW4 provides a better dataset,
               so we will use the same dataset in our final proposal."),

    ),
) # end of page one

page_two <- tabPanel(
    "CO2 emissions",
    includeCSS("style.css"),

    tags$p("On this page, you can get CO2 emissions, population and
            GDP data for any year (1950-2018), anywhere!"),
    column(4,
           offset = 0.5,
           tags$h3("Global Data"),
           sliderInput(
               "To select a year",
               inputId = "page2_col1_slider_year",
               min = 1950,
               max = 2018,
               value = 2000,
           ),

           strong("Co2 emissions"),
           p("(Unit: million tons)"),
           em(textOutput(outputId = "global_co2_value")),
           strong("Population "),
           p("(Unit: 100000 person)"),
           em(textOutput(outputId = "global_pop_value")),
           strong("GDP"),
           p("(Unit: billion US dollars)"),
           em(textOutput(outputId = "global_gdp_value"))
    ),

    column(4,
           offset = 0.5,
           tags$h3("Continental Data"),

           selectInput(
               inputId = "page2_col2_Continent_name",
               label = "To select a continent",
               choices = continent_names
           ),

           sliderInput(
               "To select a year",
               inputId = "page2_col2_slider_year",
               min = 1950,
               max = 2018,
               value = 2000,
           ),

           strong("Co2 emissions"),
           p("(Unit: million tons)"),
           em(textOutput(outputId = "Continental_co2_value")),
           strong("Population "),
           p("(Unit: 100000 person)"),
           em(textOutput(outputId = "Continental_pop_value")),
           strong("GDP"),
           p("(Unit: billion US dollars)"),
           em(textOutput(outputId = "Continental_gdp_value"))
    ),
    column(4,
           offset = 0.5,
           tags$h3("National Data"),

           selectInput(
               inputId = "page2_col3_country_name",
               label = "To select a country",
               choices = country_names$country
           ),
           sliderInput(
               "To select a year",
               inputId = "page2_col3_slider_year",
               min = 1950,
               max = 2018,
               value = 2000,
           ),

           strong("Co2 emissions"),
           p("(Unit: million tons)"),
           em(textOutput(outputId = "National_co2_value")),
           strong("Population "),
           p("(Unit: 100000 person)"),
           em(textOutput(outputId = "National_pop_value")),
           strong("GDP"),
           p("(Unit: billion US dollars)"),
           em(textOutput(outputId = "National_gdp_value"))
    ),


    tags$h3("Global and Continental Data Curves"),
    tags$hr(),
    sliderInput(
        "To select a beginning year",
        inputId = "page2_chart_start_year",
        min = 1950,
        max = 2018,
        value = 1995,
    ),
    sliderInput(
        "To select a ending year",
        inputId = "page2_chart_end_year",
        min = 1960,
        max = 2014,
        value = 2018,
    ),
    plotOutput(outputId = "page2_co2_chart"),
    tags$p(tags$strong("# The world's Co2 emissions are still increasing.")),
    tags$p(tags$strong("# North America and Europe have achieved
           year by year reductions in Co2 emissions, while Oceania has
           remained relative stable.")),
    tags$p(tags$strong("# Co2 emissions in Asia, South America and Africa are increasing,
           with Asia contributing the most to global Co2 emissions.")),
    plotOutput(outputId = "page2_pop_chart"),
    tags$p(tags$strong("# The population is growing in all countries,
                       though at different rates.")),
    plotOutput(outputId = "page2_gdp_chart"),
    tags$p(tags$strong("# Due to the lack of data, we can only plot
                       the curve of global GDP."))
)

col_names <- colnames(co2_without_groups)

page_three <- tabPanel(

    "Energy and CO2 Emissions",
    
    sidebarLayout(
        
        sidebarPanel(
            width = 2,
            time_input <- sliderInput(
                inputId = "time_input",
                label = "Choose a year",
                min = 1960,
                max = 2014,
                value = 1967,
                step = 1
            ),
            metric_input <- selectInput(
                inputId = "metric_input",
                label = "Select a metric",
                choices = col_names[c(19, 5, 20, 21, 22, 35, 36)],
            ),
            h3("Explanation"),
            p("For the heatmap on the left it shows us a visualization in terms of each metrics' intensity within each country of the world (Grey color means there's no data available for the given year)"),
            p("For the Bar Chart to the right it shows us the continental data for each continent's CO2 emission data in relation to global trade. These two plots put together are trying to show the correlation between energy sector CO2 emissions and CO2 emissions related to global trade.")
        ),
        
        mainPanel(
            column(width = 7, h3("Heatmap By Country of Different Metrics of Interest By Year"), plotlyOutput("page3_heatmap")),
            column(width = 5, h3("Bar Chart of Each Contintent's CO2 emissions as a part of global trade by Year"), plotlyOutput("page3_charts")),
            
        )
    )
)

page_four <- tabPanel(
    "Green development",
    #######page4 code here!#######
)

page_five <- tabPanel(
    "Conclusion",
    h2("Summary"),
    h3("Takeaway from CO2 emissions data"),
    p("On the CO2 emissions tab, we can see there are three different plots. One being the plot of CO2 Emissions Growth Curve, another being the plot of Population Growth, and the last one being that of Global GDP Growth. From these three main plots we could say that these three factors represent the natural growth within climate change. This is becuase all three plots are directly correlated in the way that as an uptrend in population occurs there is a major growth in CO2 emissions by that country, and also a major increase in GDP globaly also signifies that both population and GDP are major indicators and benefactors that also follows the growth momentum of CO2 emissions. This shows that they are directly correlated with one another and a change in either GDP or population could mean significant change in CO2 emissions globally and within a country."),
    h3("Takeaway from Energy and CO2 data"),
    p("On the Energy and CO2 emissions tab you can see from the bar plot to the right and the heat map to the left there are a few obvious patterns that could be seen. Firstly you could see that as you increase the slider for year there is a significant color change for all recorded CO2 Emissions by the energy sector (coal, gas, flaring, oil). A prominent example of this is coal. You could see that as we increase the year the colour change in the Asia continent signifies a large increase in CO2 emissions from coal production. As an effect the gloabl shares of CO2 emissions by Asia also increased as shown in the Bar Chart to the right. This shows that energy sector CO2 emissions is a large benefactor towards a continent's CO2 emissions in terms of trade. Secondly, a noticeable pattern is of countries within the North American continent and Asia. For instance countries like the United States has their energy sector CO2 emissions reduced as time progressed, however countries like China within Asia has a significant increase in their energy sector CO2 emissions as time progressed. In summary, this is also concurrent with the GDP data shown in the page before. Taking into consideration the fast rates of economic growth developing countries like China is facing it also catalyzed a significant increase in CO2 emissions by their energy sectors and also their share of CO2 emissions in global trade as labor costs are lower within developing nations (India, China, Vietnam, etc.), which also displays the higher tendency for developing nations to rely more on energy sector production that signifcantly increase CO2 emissions globaly."),
    h3("Takeaway from Green Development data"),
    p("On the Green Development tab you can see that there are"),
    h3("How does this relate to our objectives?"),
    p("Tying back to our introduction our intention is to raise awareness about the severity of climate change through data visualizations. In doing so we explored the correlations between different groups of data that showed a clear relationship and identified benefactors towards Carbon Dioxide Emissions as a major proponent of the worsening Climate Change situation worldwide. We identified several important sub-trends within climate change that being Energy production and global trade as a major contributor as a source of climate change. Through this analysis we also showed nations that are performing well in terms of green development and seek to look into solutions which worked for those countries that could be applied to other countries worldwide to combat climate change globaly.")
)




ui <- navbarPage(
    "Development and Co2 emission reduction", # application title
    page_one,
    page_two,
    page_three,
    page_four,
    page_five
)
