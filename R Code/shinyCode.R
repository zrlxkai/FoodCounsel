#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("devtools") #install this to use library(bubbles)
#devtools::install_github("jcheng5/bubbles") #install this to use library(bubbles)
#install.packages("plotly")
#install.packages("shinythemes")

library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(plotly)
library(rsconnect)
library(shinydashboardPlus)
library(shinythemes)
library(bslib)
library(tidyr)
library(tidyverse) #for bubble graph
library(shinydashboard) #for bubble graph
library(bubbles) #for bubble graph

#import data
setwd("C:/Users/User/OneDrive/Documents/FoodCounsel") #change to your local file
getwd()
stageData <- read.csv("Stage.csv")
HouseholdData <- read.csv("Household.csv")
FSData <- read.csv("Food Service.csv")
RetailData <- read.csv("Retail.csv")
foodwastemap <- read.csv("food_waste_map.csv")

#initialize data
#data used for stage tab
Loss <- stageData$loss_percentage
Stage <- stageData$food_supply_stage
Country <- stageData$country
df <- data.frame(Country, Stage, Loss)

#data used for percentage tab
cyl_data <- select(stageData, year, country, loss_percentage, commodity, food_supply_stage, activity)
cyl_data[cyl_data == ""] <- NA
cyl_data[is.na(cyl_data)] <- "General"

#data used for region tab
ANZHousehold <- select(filter(HouseholdData,Region == "Australia and New Zealand"),Country,Estimate)
CASHousehold <- select(filter(HouseholdData,Region == "Central Asia"),Country,Estimate)
EASHousehold <- select(filter(HouseholdData,Region == "Eastern Asia"),Country,Estimate)
EEUHousehold <- select(filter(HouseholdData,Region == "Eastern Europe"),Country,Estimate)
LACHousehold <- select(filter(HouseholdData,Region == "Latin America and the Caribbean"),Country,Estimate)
MELHousehold <- select(filter(HouseholdData,Region == "Melanesia"),Country,Estimate)
MICHousehold <- select(filter(HouseholdData,Region == "Micronesia"),Country,Estimate)
NAFHousehold <- select(filter(HouseholdData,Region == "Northern Africa"),Country,Estimate)
NAMHousehold <- select(filter(HouseholdData,Region == "Northern America"),Country,Estimate)
NEUHousehold <- select(filter(HouseholdData,Region == "Northern Europe"),Country,Estimate)
POLHousehold <- select(filter(HouseholdData,Region == "Polynesia"),Country,Estimate)
SEAHousehold <- select(filter(HouseholdData,Region == "South-eastern Asia"),Country,Estimate)
SASHousehold <- select(filter(HouseholdData,Region == "Southern Asia"),Country,Estimate)
SEUHousehold <- select(filter(HouseholdData,Region == "Southern Europe"),Country,Estimate)
SSAHousehold <- select(filter(HouseholdData,Region == "Sub-Saharan Africa"),Country,Estimate)
WASHousehold <- select(filter(HouseholdData,Region == "Western Asia"),Country,Estimate)
WEUHousehold <- select(filter(HouseholdData,Region == "Western Europe"),Country,Estimate)
ANZfs <- select(filter(FSData,Region == "Australia and New Zealand"),Country,Estimate)
CASfs <- select(filter(FSData,Region == "Central Asia"),Country,Estimate)
EASfs <- select(filter(FSData,Region == "Eastern Asia"),Country,Estimate)
EEUfs <- select(filter(FSData,Region == "Eastern Europe"),Country,Estimate)
LACfs <- select(filter(FSData,Region == "Latin America and the Caribbean"),Country,Estimate)
MELfs <- select(filter(FSData,Region == "Melanesia"),Country,Estimate)
MICfs <- select(filter(FSData,Region == "Micronesia"),Country,Estimate)
NAFfs <- select(filter(FSData,Region == "Northern Africa"),Country,Estimate)
NAMfs <- select(filter(FSData,Region == "Northern America"),Country,Estimate)
NEUfs <- select(filter(FSData,Region == "Northern Europe"),Country,Estimate)
POLfs <- select(filter(FSData,Region == "Polynesia"),Country,Estimate)
SEAfs <- select(filter(FSData,Region == "South-eastern Asia"),Country,Estimate)
SASfs <- select(filter(FSData,Region == "Southern Asia"),Country,Estimate)
SEUfs <- select(filter(FSData,Region == "Southern Europe"),Country,Estimate)
SSAfs <- select(filter(FSData,Region == "Sub-Saharan Africa"),Country,Estimate)
WASfs <- select(filter(FSData,Region == "Western Asia"),Country,Estimate)
WEUfs <- select(filter(FSData,Region == "Western Europe"),Country,Estimate)
ANZRetail <- select(filter(RetailData,Region == "Australia and New Zealand"),Country,Estimate)
CASRetail <- select(filter(RetailData,Region == "Central Asia"),Country,Estimate)
EASRetail <- select(filter(RetailData,Region == "Eastern Asia"),Country,Estimate)
EEURetail <- select(filter(RetailData,Region == "Eastern Europe"),Country,Estimate)
LACRetail <- select(filter(RetailData,Region == "Latin America and the Caribbean"),Country,Estimate)
MELRetail <- select(filter(RetailData,Region == "Melanesia"),Country,Estimate)
MICRetail <- select(filter(RetailData,Region == "Micronesia"),Country,Estimate)
NAFRetail <- select(filter(RetailData,Region == "Northern Africa"),Country,Estimate)
NAMRetail <- select(filter(RetailData,Region == "Northern America"),Country,Estimate)
NEURetail <- select(filter(RetailData,Region == "Northern Europe"),Country,Estimate)
POLRetail <- select(filter(RetailData,Region == "Polynesia"),Country,Estimate)
SEARetail <- select(filter(RetailData,Region == "South-eastern Asia"),Country,Estimate)
SASRetail <- select(filter(RetailData,Region == "Southern Asia"),Country,Estimate)
SEURetail <- select(filter(RetailData,Region == "Southern Europe"),Country,Estimate)
SSARetail <- select(filter(RetailData,Region == "Sub-Saharan Africa"),Country,Estimate)
WASRetail <- select(filter(RetailData,Region == "Western Asia"),Country,Estimate)
WEURetail <- select(filter(RetailData,Region == "Western Europe"),Country,Estimate)

#data used for map tab
mapdata <- map_data("world")
mapdata <- left_join(mapdata, foodwastemap, by = "region")
mapdata1 <- mapdata %>% 
  filter(!is.na(Household_tonnes)) %>%
  filter(!is.na(Retail_tonnes)) %>%
  filter(!is.na(Food_service_tonnes))


# Define UI for application
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  shinythemes::themeSelector(),
  tags$head(
    tags$style(HTML("
                      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
                      /* Change font of header text */
                      p {
                          color: black;
                          font-family: sans-serif;
                          font-size: 130%;
                        }
                      ul {
                          color: black;
                          font-family: sans-serif;
                          font-size: 130%;
                        }
                      /* Make text visible on inputs */
                      .shiny-input-container {
                        color: #474747;
                      }"))
  ),
  
  navbarPage( 
    "Food Counsel", #app title
    
    #HomePage
    tabPanel("Home", 
             HTML('<center><img src="https://www.foreverknowledge.info/uploads/tx_pspblog/banners/prevebt-food-waste-blog-banner.jpg", height = 200, width = 1000></center>'),
             br(),
             br(),
             sidebarPanel(
               h2("FOOD COUNSEL"),
               p("Food Counsel is an application that visualizes the graphical representation of food waste-related information and data."),
               br(),
               br(),
               p("This website consists of 4 main tabs including: "),
               h3("Food Waste World Map"),
               p("This tab visualizes the amount of food waste from all countries in the world by source categories (Household, Food Service, Retail). 
                 The food waste is visualised in terms of annual food waste and food waste per capita."),
               br(),
               h3("Loss Percentage"),
               p("This tab will show the total food loss percentage globally that occurs in various activities that may arise in the food supply chain 
                 stage over a range of years."),
               br(),
               h3("By Region"),
               p("This tab will display a bubble chart and the dataset of the total food waste in tones/year based on the input from the user which 
                 includes region and industry "),
               br(),
               h3("Stage"),
               p("This tab will show the breakdown of the dataset to identify at which stage does food waste occur the most."),
               br(),
               
               
             ),
             mainPanel(
               h1("Hello there"),
               p("Food loss and waste refer to food that is either spilled, spoiled, lost, poor in quality or even food that got thrown away despite still 
                 in a good condition. It could occur at any stage in the food supply chain including production, post-harvest, processing, distribution, 
                 retail, and consumption stage."),
               p("The fact that substantial amounts of food are produced but not eaten by humans has substantial negative impacts: environmentally, 
                 socially and economically. Estimates suggest that 8-10% of global greenhouse gas emissions are associated with food that is not consumed."),
               br(),
               p("This issue is directly linked to Sustainable Development Goal 12: Ensure sustainable consumption and production patterns."),
               p("- Target 12.3: halve global per capita food waste: This target focuses on reducing the food loss along production and supply chains, including post-harvest losses and halving per 
                 capita global food waste at the retail and consumer levels by 2033."),
               br(),
               br(),
               strong(h3("How do we come to Food Waste Data Visualization Project?")),
               strong(h4("Problem statement")),
               p("Food wastage has become a huge issue due to the concerning amount of its increasing rate each year. 
                 Therefore, it's important to analyse this issue."),
               br(),
               br(),
               strong(h4("Solution")),
               p("Food Wastage is a complex issue and has been a serious problem globally. Hence, this project will help to 
                 visualize the data regarding food wastage to raise awareness regarding the gravity it holds."),
               br(),
               strong(h3("Questions")),
               p("- What is the estimated value of food waste caused by households, food services, and retails?"),
               p("- What are the trends of the change in the food loss percentage from the year 2000 until 2021?"),
               p("- What is the total food wastage caused by households, food services, and retails in a region or country?"),
               p("- At which stage does the food waste occur the most?"),
               br(),
               strong(h3("Objectives")),
               p("- To identify patterns of food waste trends from 2000 to 2021 globally."),
               p("- To visualize the data of food waste according to the food chain stages."),
               p("- To display the estimation of food waste according to household, food service and retail categories."),
               p("- To display the total amount of food waste in a region and country."),
               br(),
               strong(h3("Datasets Sources")),
               p("We use datasets from the official website of the United Nations Environment Programme and the website of 
                 the Food and Agriculture Organization of the United Nations."),
               verbatimTextOutput("txtout"),
             ) # mainPanel
             
    ),
    
    #map tab
    tabPanel("Food waste world map", 
             h1("Food waste world map"),
             fluidRow(
               sidebarPanel(radioButtons("category1",
                                         "Choose category:",
                                         c("Household" = "household",
                                           "Food service" = "foodservice",
                                           "Retail" = "retail")),
                            radioButtons("category2",
                                         "",
                                         c("kg/capita/year" = "capita",
                                           "1000 tonnes/year" = "tonnes"))
               ),
               box(width = 8, status = "info", solidHeader = TRUE,
                   title = "Food Waste by Countries and Categories",
                   imageOutput("map", width = "100%")
               )
             ),
             fluidRow(
               sidebarPanel(
                 tableOutput("mapdataset")
               )
             )
             
    ),
    
    #Loss Percentage tab
    tabPanel( "Loss Percentage",
              h2("Total Food Loss Percentage Over the Years"),
              br(),
              p("This tab will show the total food loss percentage globally that occurs in various 
                activities that may arise in the food supply chain stage over a range of years."),
              br(),
              
              sidebarPanel(
                
                #Input year for loss percentage
                sliderInput("input_year", 
                            label = h5("Year:"),
                            min = 2000, max = 2020,
                            value = c(2010,2015)),
                
                #Display table for loss percentage
                #fluidRow(column(12, dataTableOutput("CYLtable"), align = "center")),
                
              ), #sidebar panel
              
              mainPanel(
                
                #Display scatterplot for loss percentage
                plotlyOutput("scatterplot")
              ), #main panel
              
              column(
                8, offset=2, br(),br(),br(), 
                h3("Dataset Table"),
                dataTableOutput("CYLtable"), align = "center"
              ) #column
              
              
    ), 
    
    #By Region tab
    tabPanel("By Region",
             h2("Statistics of Estimated Food Waste (tones/year) by Region and Industry"),
             br(),
             p("This tab will display a bubble chart and the dataset of the total food waste in tones/year based
               on the input from user which includes region and industry"),
             br(),
             fluidRow(
               #choose the type of dataset
               selectInput(inputId = "channel1", label = "Choose Region",
                           choices = c("Australia and New Zealand"="ANZ",
                                       "Central Asia"="CAS",
                                       "Eastern Asia"="EAS",
                                       "Eastern Europe"="EEU",
                                       "Latin America and the Caribbean"="LAC",
                                       "Melanesia"="MEL",
                                       "Micronesia"="MIC",
                                       "Northern Africa"="NAF",
                                       "Northern America"="NAM",
                                       "Northern Europe"="NEU",
                                       "Polynesia"="POL",
                                       "South-eastern Asia"="SEA",
                                       "Southern Asia"="SAS",
                                       "Southern Europe"="SEU",
                                       "Sub-Saharan Africa"="SSA",
                                       "Western Asia"="WAS",
                                       "Western Europe"="WEU")),
               h6(),
               radioButtons("ind",
                            "Choose the Industry:",
                            c("Household"="household",
                              "Food Service"="foodservice",
                              "Retail"="retail"))
               
             ),
             fluidRow(
               sidebarPanel(
                 tableOutput("datasetss"), width = 2
               ),
               mainPanel(
                 box(
                   width = 8, status = "info", solidHeader = TRUE,
                   title = "Food Waste by Countries and Industries",
                   bubblesOutput("bubbleRegion", width = "100%", height = 600)
                 )
               )
             )
    ), 
    
    #stage tab
    tabPanel( "Stage",
              h2("Stage of Food Waste"),
              br(),
              p("This tab will show the break down of dataset to identify at which stage does food waste occur the most."),
              br(),
              
              sidebarPanel(          
                h3("Tick for categorized figure"),
                
                #checkbox
                checkboxInput("cbCategorized", "Categorized", FALSE),
                
                h3("Dataset Table"),
                br(),
                
                #display table
                fluidRow(column(12, dataTableOutput("table"), align = "center")),
                
              ), #sidebar panel end
              
              mainPanel( 
                
                h2("Figure of percentage of food waste (%) sorted by stages of food waste for all country"),
                br(),
                
                #display graph
                plotOutput("plot")
                
              ) #main panel end
              
    ),
    
    #About us Panel
    tabPanel("About Us",
             h1("Group Members", align = "center"),
             HTML('<center><img src="https://scontent-kut2-2.xx.fbcdn.net/v/t39.30808-6/287686175_5189400484478801_6261460154473537345_n.jpg?_nc_cat=104&ccb=1-7&_nc_sid=730e14&_nc_eui2=AeFmu7tnisy6YIiQ6pcGCr5QHyH2iO5b8gQfIfaI7lvyBLLtmibQUZm4n6HfOcymsf_Aif2mzjENl_hjzZAnGjZW&_nc_ohc=OB_ydnd9dR4AX9L164s&_nc_ht=scontent-kut2-2.xx&oh=00_AT9WZfUuFviUtnCNDKvIv2sqlusXvRrqvRY3H9ta9IiXLw&oe=62B2F772", width = 75%></center>'),
             
             
    ),
    
    #footer
    footer = dashboardFooter(
      left = "By Makan Makan",
      right = "Malaysia, 2021"
    )
    
  ) #Navbar Page end
) #fluid Page end

# Define server 
server <- function(input, output) {
  
  #server for stage tab
  #print table
  output$table <- renderDataTable(df)
  
  #print graph
  output$plot <- renderPlot({
    #if check box ticked
    if(input$cbCategorized == TRUE){
      ggplot(df, aes(x = Country, y = (Loss)), height = 2000) +
        geom_boxplot(aes(colour = Stage))+
        facet_wrap(~Stage) +
        ggtitle("Food Waste Stage") +
        xlab("Country") +
        ylab("Food Waste Percentage (%)") +
        scale_fill_gradient("Average", low="darkolivegreen1",high="deepskyblue3") +
        theme_bw(base_size = 15) +
        theme(axis.text.x = element_blank(),
              axis.title.x=element_blank())
      
    } #if end
    #check box didnt ticked
    else{
      ggplot(df, aes(x = Country, y = (Loss)), height = 2000) +
        geom_boxplot(aes(colour = Stage))+
        ggtitle("Food Waste Stage") +
        xlab("Country") +
        ylab("Food Waste Percentage (%)") +
        scale_fill_gradient("Average", low="darkolivegreen1",high="deepskyblue3") +
        theme_bw(base_size = 15) +
        theme(axis.text.x = element_blank(),
              axis.title.x=element_blank())
    } #else end
    
  }) #server for stage tab end
  
  
  #server for loss percentage tab
  #Table for loss percentage
  output$CYLtable <- renderDataTable({
    #filter_cyl <- filter(cyl_data, year==input$input_year)
    filter_cyl <- filter(cyl_data, year>=input$input_year[1] & year<=input$input_year[2])
  }) # end table loss percentage
  
  #Scatterplot for loss percentage
  output$scatterplot <- renderPlotly({
    data <- cyl_data
    
    scatterPlot <- data %>%
      ggplot(gapminder, mapping = aes(x = year,
                                      y = loss_percentage,
                                      color = country,
                                      text = paste("commodity:", commodity, "<br>",
                                                   "stage:", food_supply_stage ))) +
      geom_point(alpha = (1/3)) +
      coord_cartesian(xlim = c(input$input_year[1],input$input_year[2]),
                      ylim = c(min(data$loss_percentage),max(data$loss_percentage))) + 
      theme(legend.position="none") +
      scale_x_continuous(breaks = seq(input$input_year[1],input$input_year[2], by = 1)) +
      labs(title <- "Food Loss Percentage Over the Years")
    
    scatterPlot <- ggplotly(scatterPlot)
    
  }) #server for loss percentage tab end
  
  
  #server for region tab
  output$bubbleRegion <- renderBubbles({
    if(input$channel1 == "ANZ"){
      if(input$ind == "household"){
        bubbles(value = ANZHousehold$Estimate,
                label = ANZHousehold$Country,
                color = rainbow(nrow(ANZHousehold), alpha=NULL)[sample(nrow(ANZHousehold))])
      }
      else if(input$ind == "foodservice"){
        bubbles(value = ANZfs$Estimate,
                label = ANZfs$Country,
                color = rainbow(nrow(ANZfs), alpha=NULL)[sample(nrow(ANZfs))])
      }
      else if(input$ind == "retail"){
        bubbles(value = ANZRetail$Estimate,
                label = ANZRetail$Country,
                color = rainbow(nrow(ANZRetail), alpha=NULL)[sample(nrow(ANZRetail))])
      }
    }
    else if(input$channel1 == "CAS"){
      if(input$ind == "household"){
        bubbles(value = CASHousehold$Estimate,
                label = CASHousehold$Country,
                color = rainbow(nrow(CASHousehold), alpha=NULL)[sample(nrow(CASHousehold))])
      }
      else if(input$ind == "foodservice"){
        bubbles(value = CASfs$Estimate,
                label = CASfs$Country,
                color = rainbow(nrow(CASfs), alpha=NULL)[sample(nrow(CASfs))])
      }
      else if(input$ind == "retail"){
        bubbles(value = CASRetail$Estimate,
                label = CASRetail$Country,
                color = rainbow(nrow(CASRetail), alpha=NULL)[sample(nrow(CASRetail))])
      }
    }
    else if(input$channel1 == "EAS"){
      if(input$ind == "household"){
        bubbles(value = EASHousehold$Estimate,
                label = EASHousehold$Country,
                color = rainbow(nrow(EASHousehold), alpha=NULL)[sample(nrow(EASHousehold))])
      }
      else if(input$ind == "foodservice"){
        bubbles(value = EASfs$Estimate,
                label = EASfs$Country,
                color = rainbow(nrow(EASfs), alpha=NULL)[sample(nrow(EASfs))])
      }
      else if(input$ind == "retail"){
        bubbles(value = EASRetail$Estimate,
                label = EASRetail$Country,
                color = rainbow(nrow(EASRetail), alpha=NULL)[sample(nrow(EASRetail))])
      }
    }
    else if(input$channel1 == "EEU"){
      if(input$ind == "household"){
        bubbles(value = EEUHousehold$Estimate,
                label = EEUHousehold$Country,
                color = rainbow(nrow(EEUHousehold), alpha=NULL)[sample(nrow(EEUHousehold))])
      }
      else if(input$ind == "foodservice"){
        bubbles(value = EEUfs$Estimate,
                label = EEUfs$Country,
                color = rainbow(nrow(EEUfs), alpha=NULL)[sample(nrow(EEUfs))])
      }
      else if(input$ind == "retail"){
        bubbles(value = EEURetail$Estimate,
                label = EEURetail$Country,
                color = rainbow(nrow(EEURetail), alpha=NULL)[sample(nrow(EEURetail))])
      }
    }
    else if(input$channel1 == "LAC"){
      if(input$ind == "household"){
        bubbles(value = LACHousehold$Estimate,
                label = LACHousehold$Country,
                color = rainbow(nrow(LACHousehold), alpha=NULL)[sample(nrow(LACHousehold))])
      }
      else if(input$ind == "foodservice"){
        bubbles(value = LACfs$Estimate,
                label = LACfs$Country,
                color = rainbow(nrow(LACfs), alpha=NULL)[sample(nrow(LACfs))])
      }
      else if(input$ind == "retail"){
        bubbles(value = LACRetail$Estimate,
                label = LACRetail$Country,
                color = rainbow(nrow(LACRetail), alpha=NULL)[sample(nrow(LACRetail))])
      }
    }
    else if(input$channel1 == "MEL"){
      if(input$ind == "household"){
        bubbles(value = MELHousehold$Estimate,
                label = MELHousehold$Country,
                color = rainbow(nrow(MELHousehold), alpha=NULL)[sample(nrow(MELHousehold))])
      }
      else if(input$ind == "foodservice"){
        bubbles(value = MELfs$Estimate,
                label = MELfs$Country,
                color = rainbow(nrow(MELfs), alpha=NULL)[sample(nrow(MELfs))])
      }
      else if(input$ind == "retail"){
        bubbles(value = MELRetail$Estimate,
                label = MELRetail$Country,
                color = rainbow(nrow(MELRetail), alpha=NULL)[sample(nrow(MELRetail))])
      }
    }
    else if(input$channel1 == "MIC"){
      if(input$ind == "household"){
        bubbles(value = MICHousehold$Estimate,
                label = MICHousehold$Country,
                color = rainbow(nrow(MICHousehold), alpha=NULL)[sample(nrow(MICHousehold))])
      }
      else if(input$ind == "foodservice"){
        bubbles(value = MICfs$Estimate,
                label = MICfs$Country,
                color = rainbow(nrow(MICfs), alpha=NULL)[sample(nrow(MICfs))])
      }
      else if(input$ind == "retail"){
        bubbles(value = MICRetail$Estimate,
                label = MICRetail$Country,
                color = rainbow(nrow(MICRetail), alpha=NULL)[sample(nrow(MICRetail))])
      }
    }
    else if(input$channel1 == "NAF"){
      if(input$ind == "household"){
        bubbles(value = NAFHousehold$Estimate,
                label = NAFHousehold$Country,
                color = rainbow(nrow(NAFHousehold), alpha=NULL)[sample(nrow(NAFHousehold))])
      }
      else if(input$ind == "foodservice"){
        bubbles(value = NAFfs$Estimate,
                label = NAFfs$Country,
                color = rainbow(nrow(NAFfs), alpha=NULL)[sample(nrow(NAFfs))])
      }
      else if(input$ind == "retail"){
        bubbles(value = NAFRetail$Estimate,
                label = NAFRetail$Country,
                color = rainbow(nrow(NAFRetail), alpha=NULL)[sample(nrow(NAFRetail))])
      }
    }
    else if(input$channel1 == "NAM"){
      if(input$ind == "household"){
        bubbles(value = NAMHousehold$Estimate,
                label = NAMHousehold$Country,
                color = rainbow(nrow(NAMHousehold), alpha=NULL)[sample(nrow(NAMHousehold))])
      }
      else if(input$ind == "foodservice"){
        bubbles(value = NAMfs$Estimate,
                label = NAMfs$Country,
                color = rainbow(nrow(NAMfs), alpha=NULL)[sample(nrow(NAMfs))])
      }
      else if(input$ind == "retail"){
        bubbles(value = NAMRetail$Estimate,
                label = NAMRetail$Country,
                color = rainbow(nrow(NAMRetail), alpha=NULL)[sample(nrow(NAMRetail))])
      }
    }
    else if(input$channel1 == "NEU"){
      if(input$ind == "household"){
        bubbles(value = NEUHousehold$Estimate,
                label = NEUHousehold$Country,
                color = rainbow(nrow(NEUHousehold), alpha=NULL)[sample(nrow(NEUHousehold))])
      }
      else if(input$ind == "foodservice"){
        bubbles(value = NEUfs$Estimate,
                label = NEUfs$Country,
                color = rainbow(nrow(NEUfs), alpha=NULL)[sample(nrow(NEUfs))])
      }
      else if(input$ind == "retail"){
        bubbles(value = NEURetail$Estimate,
                label = NEURetail$Country,
                color = rainbow(nrow(NEURetail), alpha=NULL)[sample(nrow(NEURetail))])
      }
    }
    else if(input$channel1 == "POL"){
      if(input$ind == "household"){
        bubbles(value = POLHousehold$Estimate,
                label = POLHousehold$Country,
                color = rainbow(nrow(POLHousehold), alpha=NULL)[sample(nrow(POLHousehold))])
      }
      else if(input$ind == "foodservice"){
        bubbles(value = POLfs$Estimate,
                label = POLfs$Country,
                color = rainbow(nrow(POLfs), alpha=NULL)[sample(nrow(POLfs))])
      }
      else if(input$ind == "retail"){
        bubbles(value = POLRetail$Estimate,
                label = POLRetail$Country,
                color = rainbow(nrow(POLRetail), alpha=NULL)[sample(nrow(POLRetail))])
      }
    }
    else if(input$channel1 == "SEA"){
      if(input$ind == "household"){
        bubbles(value = SEAHousehold$Estimate,
                label = SEAHousehold$Country,
                color = rainbow(nrow(SEAHousehold), alpha=NULL)[sample(nrow(SEAHousehold))])
      }
      else if(input$ind == "foodservice"){
        bubbles(value = SEAfs$Estimate,
                label = SEAfs$Country,
                color = rainbow(nrow(SEAfs), alpha=NULL)[sample(nrow(SEAfs))])
      }
      else if(input$ind == "retail"){
        bubbles(value = SEARetail$Estimate,
                label = SEARetail$Country,
                color = rainbow(nrow(SEARetail), alpha=NULL)[sample(nrow(SEARetail))])
      }
    }
    else if(input$channel1 == "SAS"){
      if(input$ind == "household"){
        bubbles(value = SASHousehold$Estimate,
                label = SASHousehold$Country,
                color = rainbow(nrow(SASHousehold), alpha=NULL)[sample(nrow(SASHousehold))])
      }
      else if(input$ind == "foodservice"){
        bubbles(value = SASfs$Estimate,
                label = SASfs$Country,
                color = rainbow(nrow(SASfs), alpha=NULL)[sample(nrow(SASfs))])
      }
      else if(input$ind == "retail"){
        bubbles(value = SASRetail$Estimate,
                label = SASRetail$Country,
                color = rainbow(nrow(SASRetail), alpha=NULL)[sample(nrow(SASRetail))])
      }
    }
    else if(input$channel1 == "SEU"){
      if(input$ind == "household"){
        bubbles(value = SEUHousehold$Estimate,
                label = SEUHousehold$Country,
                color = rainbow(nrow(SEUHousehold), alpha=NULL)[sample(nrow(SEUHousehold))])
      }
      else if(input$ind == "foodservice"){
        bubbles(value = SEUfs$Estimate,
                label = SEUfs$Country,
                color = rainbow(nrow(SEUfs), alpha=NULL)[sample(nrow(SEUfs))])
      }
      else if(input$ind == "retail"){
        bubbles(value = SEURetail$Estimate,
                label = SEURetail$Country,
                color = rainbow(nrow(SEURetail), alpha=NULL)[sample(nrow(SEURetail))])
      }
    }
    else if(input$channel1 == "SSA"){
      if(input$ind == "household"){
        bubbles(value = SSAHousehold$Estimate,
                label = SSAHousehold$Country,
                color = rainbow(nrow(SSAHousehold), alpha=NULL)[sample(nrow(SSAHousehold))])
      }
      else if(input$ind == "foodservice"){
        bubbles(value = SSAfs$Estimate,
                label = SSAfs$Country,
                color = rainbow(nrow(SSAfs), alpha=NULL)[sample(nrow(SSAfs))])
      }
      else if(input$ind == "retail"){
        bubbles(value = SSARetail$Estimate,
                label = SSARetail$Country,
                color = rainbow(nrow(SSARetail), alpha=NULL)[sample(nrow(SSARetail))])
      }
    }
    else if(input$channel1 == "WAS"){
      if(input$ind == "household"){
        bubbles(value = WASHousehold$Estimate,
                label = WASHousehold$Country,
                color = rainbow(nrow(WASHousehold), alpha=NULL)[sample(nrow(WASHousehold))])
      }
      else if(input$ind == "foodservice"){
        bubbles(value = WASfs$Estimate,
                label = WASfs$Country,
                color = rainbow(nrow(WASfs), alpha=NULL)[sample(nrow(WASfs))])
      }
      else if(input$ind == "retail"){
        bubbles(value = WASRetail$Estimate,
                label = WASRetail$Country,
                color = rainbow(nrow(WASRetail), alpha=NULL)[sample(nrow(WASRetail))])
      }
    }
    else if(input$channel1 == "WEU"){
      if(input$ind == "household"){
        bubbles(value = WEUHousehold$Estimate,
                label = WEUHousehold$Country,
                color = rainbow(nrow(WEUHousehold), alpha=NULL)[sample(nrow(WEUHousehold))])
      }
      else if(input$ind == "foodservice"){
        bubbles(value = WEUfs$Estimate,
                label = WEUfs$Country,
                color = rainbow(nrow(WEUfs), alpha=NULL)[sample(nrow(WEUfs))])
      }
      else if(input$ind == "retail"){
        bubbles(value = WEURetail$Estimate,
                label = WEURetail$Country,
                color = rainbow(nrow(WEURetail), alpha=NULL)[sample(nrow(WEURetail))])
      }
    }
  })
  output$datasetss <- renderTable({
    if(input$channel1 == "ANZ"){
      if(input$ind == "household"){
        ANZHousehold
      }
      else if(input$ind == "foodservice"){
        ANZfs
      }
      else if(input$ind == "retail"){
        ANZRetail
      }
    }
    else if(input$channel1 == "CAS"){
      if(input$ind == "household"){
        CASHousehold
      }
      else if(input$ind == "foodservice"){
        CASfs
      }
      else if(input$ind == "retail"){
        CASRetail
      }
    }
    else if(input$channel1 == "EAS"){
      if(input$ind == "household"){
        EASHousehold
      }
      else if(input$ind == "foodservice"){
        EASfs
      }
      else if(input$ind == "retail"){
        EASRetail
      }
    }
    else if(input$channel1 == "EEU"){
      if(input$ind == "household"){
        EEUHousehold
      }
      else if(input$ind == "foodservice"){
        EEUfs
      }
      else if(input$ind == "retail"){
        EEURetail
      }
    }
    else if(input$channel1 == "LAC"){
      if(input$ind == "household"){
        LACHousehold
      }
      else if(input$ind == "foodservice"){
        LACfs
      }
      else if(input$ind == "retail"){
        LACRetail
      }
    }
    else if(input$channel1 == "MEL"){
      if(input$ind == "household"){
        MELHousehold
      }
      else if(input$ind == "foodservice"){
        MELfs
      }
      else if(input$ind == "retail"){
        MELRetail
      }
    }
    else if(input$channel1 == "MIC"){
      if(input$ind == "household"){
        MICHousehold
      }
      else if(input$ind == "foodservice"){
        MICfs
      }
      else if(input$ind == "retail"){
        MICRetail
      }
    }
    else if(input$channel1 == "NAF"){
      if(input$ind == "household"){
        NAFHousehold
      }
      else if(input$ind == "foodservice"){
        NAFfs
      }
      else if(input$ind == "retail"){
        NAFRetail
      }
    }
    else if(input$channel1 == "NAM"){
      if(input$ind == "household"){
        NAMHousehold
      }
      else if(input$ind == "foodservice"){
        NAMfs
      }
      else if(input$ind == "retail"){
        NAMRetail
      }
    }
    else if(input$channel1 == "NEU"){
      if(input$ind == "household"){
        NEUHousehold
      }
      else if(input$ind == "foodservice"){
        NEUfs
      }
      else if(input$ind == "retail"){
        NEURetail
      }
    }
    else if(input$channel1 == "POL"){
      if(input$ind == "household"){
        POLHousehold
      }
      else if(input$ind == "foodservice"){
        POLfs
      }
      else if(input$ind == "retail"){
        POLRetail
      }
    }
    else if(input$channel1 == "SEA"){
      if(input$ind == "household"){
        SEAHousehold
      }
      else if(input$ind == "foodservice"){
        SEAfs
      }
      else if(input$ind == "retail"){
        SEARetail
      }
    }
    else if(input$channel1 == "SAS"){
      if(input$ind == "household"){
        SASHousehold
      }
      else if(input$ind == "foodservice"){
        SASfs
      }
      else if(input$ind == "retail"){
        SASRetail
      }
    }
    else if(input$channel1 == "SEU"){
      if(input$ind == "household"){
        SEUHousehold
      }
      else if(input$ind == "foodservice"){
        SEUfs
      }
      else if(input$ind == "retail"){
        SEURetail
      }
    }
    else if(input$channel1 == "SSA"){
      if(input$ind == "household"){
        SSAHousehold
      }
      else if(input$ind == "foodservice"){
        SSAfs
      }
      else if(input$ind == "retail"){
        SSARetail
      }
    }
    else if(input$channel1 == "WAS"){
      if(input$ind == "household"){
        WASHousehold
      }
      else if(input$ind == "foodservice"){
        WASfs
      }
      else if(input$ind == "retail"){
        WASRetail
      }
    }
    else if(input$channel1 == "WEU"){
      if(input$ind == "household"){
        WEUHousehold
      }
      else if(input$ind == "foodservice"){
        WEUfs
      }
      else if(input$ind == "retail"){
        WEURetail
      }
    }
  }) #server for region tab end
  
  
  #server for map tab
  output$map <- renderPlot(
    if(input$category1 == "household" & input$category2 == "capita"){
      map1 <- ggplot(mapdata1, aes( x = long, y = lat, group = group)) + 
        geom_polygon(aes(fill = Household_capita), color = "black")
      
      map2 <- map1 + scale_fill_gradient(name = "Household Waste\nkg/capita/year", low = "yellow", high = "red", na.value = "grey50", ) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              rect = element_blank())
      
      map2
    }
    else if(input$category1 == "household" & input$category2 == "tonnes"){
      map1 <- ggplot(mapdata1, aes( x = long, y = lat, group = group)) + 
        geom_polygon(aes(fill = Household_tonnes), color = "black")
      
      map2 <- map1 + scale_fill_gradient(name = "Household Waste\n1000 tonnes/year", low = "yellow", high = "red", na.value = "grey50", ) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              rect = element_blank())
      
      map2
    }
    else if(input$category1 == "foodservice" & input$category2 == "capita"){
      map1 <- ggplot(mapdata1, aes( x = long, y = lat, group = group)) + 
        geom_polygon(aes(fill = Food_service_capita), color = "black")
      
      map2 <- map1 + scale_fill_gradient(name = "Food Service Waste\nkg/capita/year", low = "yellow", high = "red", na.value = "grey50", ) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              rect = element_blank())
      
      map2
    }
    else if(input$category1 == "foodservice" & input$category2 == "tonnes"){
      map1 <- ggplot(mapdata1, aes( x = long, y = lat, group = group)) + 
        geom_polygon(aes(fill = Food_service_tonnes), color = "black")
      
      map2 <- map1 + scale_fill_gradient(name = "Food Service Waste\n1000 tonnes/year", low = "yellow", high = "red", na.value = "grey50", ) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              rect = element_blank())
      
      map2
    }
    else if(input$category1 == "retail" & input$category2 == "capita"){
      map1 <- ggplot(mapdata1, aes( x = long, y = lat, group = group)) + 
        geom_polygon(aes(fill = Retail_capita), color = "black")
      
      map2 <- map1 + scale_fill_gradient(name = "Retail Waste\nkg/capita/year", low = "yellow", high = "red", na.value = "grey50", ) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              rect = element_blank())
      
      map2
    }
    else{
      map1 <- ggplot(mapdata1, aes( x = long, y = lat, group = group)) + 
        geom_polygon(aes(fill = Retail_tonnes), color = "black")
      
      map2 <- map1 + scale_fill_gradient(name = "Retail Waste\nx1000 tonnes/year", low = "yellow", high = "red", na.value = "grey50", ) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              rect = element_blank())
      
      map2
    }
  )
  output$mapdataset <- renderTable(
    if(input$category1 == "household" & input$category2 == "capita"){
      select(foodwastemap, region, Household_capita)
    }
    else if(input$category1 == "household" & input$category2 == "tonnes"){
      select(foodwastemap, region, Household_tonnes)
    }
    else if(input$category1 == "foodservice" & input$category2 == "capita"){
      select(foodwastemap, region, Food_service_capita)
    }
    else if(input$category1 == "foodservice" & input$category2 == "tonnes"){
      select(foodwastemap, region, Food_service_tonnes)
    }
    else if(input$category1 == "retail" & input$category2 == "capita"){
      select(foodwastemap, region, Retail_capita)
    }
    else{
      select(foodwastemap, region, Retail_tonnes)
    }
  ) #server for map tab end
  
} #server end

# Run the application 
shinyApp(ui = ui, server = server)
