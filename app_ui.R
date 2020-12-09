library("shiny")
library("dplyr")

co2_data <- read.csv("owid-co2-data.csv")
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
        tags$h2("Greenhouse effect and Co2 emissions"),
        tags$hr(),
        tags$p("Co2 has the effect of hindering
               the spread of infrared rays. Therefore, when the content of Co2
               in the atmosphere increases, the heat transmitted by the earth
               to outer space will decrease, and the temperature will rise.
               This is the greenhouse effect caused by Co2."),
        tags$p("The greenhouse effect will have
               significant impacts on environment."),
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
    "Co2 emissions",
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
        min = 1950,
        max = 2018,
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

col_names <- c("consumption_co2", "co2_per_capita")

page_three <- tabPanel(

    "Climate Change Heat Map Visualization",

    plotOutput("page3_heatmap"),

    time_input <- sliderInput(
        inputId = "time_input",
        label = "Choose a year",
        min = 1751,
        max = 2018,
        value = 2000,
        step = 1
    ),
    metric_input <- selectInput(
        inputId = "metric_input",
        label = "Select a metric",
        choices = col_names
    )
)

page_four <- tabPanel(
    "Green development",
    #######page4 code here!#######
)

page_five <- tabPanel(
    "Conclusion",
    #######page5 code here!#######
)




ui <- navbarPage(
    "Development and Co2 emission reduction", # application title
    page_one,
    page_two,
    page_three,
    page_four,
    page_five
)
