
       #---------------------- Load Libraries ----------------------
rm(list = ls())
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)      
library(plotly)
library(dplyr)
library(tidyverse)
library(DT)
library(hrbrthemes)       
#library(shidashi)

       #-------------------------- Load Data --------------------------

load("OECD skill job level 1.RData")
load("OECD skill job level 2.RData")

        #-------------------------------------------------------------

sjd1 <- sjd1 %>%
  mutate(skill_imbalance_indicator = ifelse(value > 0, "Shortage", "surplus"))

#Color 0 surplus 1 shortage
sjd2$color <- ifelse(sjd2$value <= 0, 0, 1)

sjd1$color <- ifelse(sjd1$value <= 0, 0, 1)

tbl <- sjd1  %>%
  group_by(skill1) %>%
  summarise(mean = round(mean(value),2),
            median = round(median(value),2),
            sd = round(sd(value),2),
            min =round( min(value),2),
            max = max(value))

      #-------- Filter the data fortop 5 shortage skills -----------------

shortage_data <- subset(sjd1, skill_imbalance_indicator == "Shortage")
shortage_data <- shortage_data[order(shortage_data$value, decreasing = TRUE), ]
shortage_data <- head(shortage_data,5)
shortage_data <- shortage_data[-c(4)]

      #--------- Filter the data for top 5 surplus skills------------------

surplus_data <- subset(sjd1, skill_imbalance_indicator == "surplus")
surplus_data <- surplus_data[order(surplus_data$value, decreasing = TRUE), ]
surplus_data <- head(surplus_data,5)
surplus_data <- surplus_data[-c(4)]


      #---------------------------------------------------------------

country_1 <- unique(sjd2$country)
country_2 <- unique(sjd2$country)

      #--------------------------filtering by region -----------------

region_country <- subset(sjd1, !country %in% c("OECD", "European Union"))
region_country <- mutate(region_country, region = ifelse(region_country$country %in% c("Canada", "Mexico", "United States"), "Americas",
                                                         ifelse(region_country$country %in% c("Turkey", "Switzerland", "Norway", "Iceland"), "Non-EU",
                                                                ifelse(region_country$country %in% c("Australia","New Zealand"),"Oceania","EU"))))
oecd <- filter(sjd1,country!="OECD")
oecd$region = "OECD"

region_country <- rbind(oecd,region_country)

sjd1 <- merge(sjd1,region_country)


region_country1 <- subset(sjd2, !country %in% c("OECD", "European Union"))
region_country1 <- mutate(region_country1, region = ifelse(region_country1$country %in% c("Canada", "Mexico", "United States"), "Americas",
                                                         ifelse(region_country1$country %in% c("Turkey", "Switzerland", "Norway", "Iceland"), "Non-EU",
                                                                ifelse(region_country1$country %in% c("Australia","New Zealand"),"Oceania","EU"))))

oecd1 <- filter(sjd2,country=="OECD")
oecd1$region = "OECD"
region_country1 <- rbind(oecd1,region_country1)
sjd2 <- merge(sjd2,region_country1)

      #-------------------------- Shiny App --------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Skill Imbalance"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",tabName = "Overview",icon = icon("dashboard")),
      menuItem("Country",tabName = "Country", icon = icon("globe")),
      menuItem("Skills",tabName = "Skills", icon = icon("chart-bar"))
      
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('.box {margin: 10px;} .value {font-size: 20px; font-weight: bold; text-align: center;}'))),
    tabItems(
      tabItem(tabName = "Overview",
              fluidRow(
                column(width = 4, valueBox(33,"Countries",icon = icon("globe"), color = "blue", width = 10)),
                column(width = 4, valueBox(7,"Skills",icon("chart-bar"),color = "aqua", width = 10)),
                column(width = 4, valueBox(35,"Sub-Skills", icon = icon("chart-line"),color = "light-blue", width = 10))
              ),
              fluidRow(
               column(width = 4, selectInput("region","Select a region:",selected = "EU",
                          choices = unique(sjd1$region))),
              column(width = 4,selectInput("country","Select a country:",selected = "None",
                          choices = unique(sjd2$country))),
              column(width = 4, selectInput("skill","Select a Top-skill:",selected = "None",
                                            choices = unique(sjd1$skill1)))
              ),
             
               
     fluidRow(
       plotlyOutput("plot1",height = "400px",width = "1000px")
              ),
              fluidRow(
                box(width = 6,title = "Top 5 Shoratge Skills and the Countries",status =  "primary", solidHeader = TRUE,
                    collapsible = TRUE, height = 700,
                    column(width = 6,dataTableOutput("shortage_data"))),
                box(width = 6,title = "Top 5 Surplus Skills and the Countries",status =  "primary", solidHeader = TRUE,
                    collapsible = TRUE, height = 700,
                    column(width = 6,dataTableOutput("surplus_data")))
                )
      ), 
      
      tabItem(tabName = "Country",
              fluidRow(
                column(width = 4, valueBox(33,"Countries",icon = icon("globe"), color = "blue", width = 10)),
                column(width = 4, valueBox(7,"Skills",icon("chart-bar"),color = "aqua", width = 10)),
                column(width = 4, valueBox(35,"Sub-Skills", icon = icon("chart-line"),color = "light-blue", width = 10))
              ),
              fluidRow(
                column(width = 6, selectInput("region1","Select a region:",selected = "EU",
                                              choices = unique(sjd1$region))),
                column(width = 6,selectInput("country1","Select a country:",selected = "None",
                                             choices = unique(sjd2$country)))
               
                
              ),
              fluidRow(
             plotlyOutput("plot2",height = "500px", width = "900px"),
             column(width = 4,selectInput("countryA","Select  country1:",selected = "Austria",
                                          choices = unique(sjd2$country))),
             column(width = 4,selectInput("countryB","Select  country2:",selected = "None",
                                          choices = unique(sjd2$country))),
             column(width = 4, selectInput("skillX","Select Major-skill:",selected = "None",
                                           choices = unique(sjd1$skill1)))
              ),
           fluidRow(
          column(width = 6,plotlyOutput("graph1")),
           column(width = 6,plotlyOutput("graph2"))
              
              
       )),
      
      tabItem(tabName = "Skills",
              fluidRow(
                column(width = 4, valueBox(33,"Countries",icon = icon("globe"), color = "blue", width = 10)),
                column(width = 4, valueBox(7,"Skills",icon("chart-bar"),color = "aqua", width = 10)),
                column(width = 4, valueBox(35,"Sub-Skills", icon = icon("chart-line"),color = "light-blue", width = 10))
              ),
              
              fluidRow(
                column(width = 4, selectInput("region2","Select a region:",selected = "EU",
                                              choices = unique(sjd1$region))),
                column(width = 4, selectInput("skillA","Select a Top-skill:",selected = "None",
                                              choices = unique(sjd1$skill1))),
                column(width = 4, selectInput("skillB","Select a Top-skill:",selected = "None",
                                              choices = unique(sjd2$skill2)))
              ),
                  plotlyOutput("plot3", height = "500px", width = "1000px"),
              
              fluidRow(
                box(width = 12,title = "Statistics table of Skills",status =  "primary", solidHeader = TRUE,
                           collapsible = TRUE, height = 500,
                dataTableOutput("tbl"))
                )
      )
              
    
   )
    
  )
)
    #---------------------- Server code ------------------------------

server <- function(input, output,session) {
  
   #----------------------Reactive for plot 1 -----------------------------------
  
  # reactive expression for filtering based on region selection in Overview tab
  region_filter_overview <- reactive({
    filter(sjd1, region == input$region)
  })
  
  # observeevent for updating country choices based on selected region in Overview tab
  observeEvent(region_filter_overview(), {
    country <- unique(region_filter_overview()$country)
    updateSelectInput(session, inputId = "country", choices = country)
  })
  
  #-----------------------plot 1 -------------------------------------------------
  
  output$plot1 <- renderPlotly({
    g <- ggplot(region_filter_overview(), aes(x = country, y = skill1)) + 
      geom_tile(aes(fill = value)) +
      scale_fill_gradient2(low = "white", mid = "lightblue", high = "blue",
                           limits = c(-1, 1), 
                           name = "Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle =45 , vjust = 0.5, hjust = 1),
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_blank(),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(color = "black", size = 0.5))+
      geom_text(aes(label = round(value, 2)), size = 2, color = "black")+
      labs(title = paste0(" Overview of Major-skills in ",input$region))
    
    ggplotly(g, tooltip = c("country", "skill1", "value"), 
             text = paste("Country: ", region_filter_overview()$country, "<br>",
                          "Skill: ", region_filter_overview()$skill1, "<br>",
                          "Value: ", region_filter_overview()$value))
    
    ggplotly(g) %>%
      layout(annotations = list(x = 1, y = -0.4, text = "Source: OECD,2015", 
                                                                      showarrow = F, xref='paper', yref='paper', 
                                                                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                                                      font=list(size=12, color="black"))
      )
    
  })
  
  output$shortage_data <- renderDataTable(datatable(
    shortage_data))
  
  output$surplus_data <- renderDataTable(datatable(
    surplus_data))
  
  
  
  
  #----------------------------Reactive for plot 2 -----------------------------------
  
  # reactive expression for filtering based on region selection in Country tab
  region_filter_country <- reactive({
    filter(sjd2, region == input$region1)
  })
  
  # observeevent for updating country choices based on selected region in Overview tab
  observeEvent(region_filter_country(), {
    country1 <- unique(region_filter_country()$country)
    updateSelectInput(session, inputId = "country1", choices = country1)
    
  })
  
  plotdata1 <- reactive({
    imbalance <- merge(sjd1 %>%
                         filter(country == input$countryA)%>%
                         select(2, 3) %>%
                         `colnames<-`(c("Skill", "country1")),
                       sjd1 %>%
                         filter(country == input$countryB)%>%
                         select(2, 3) %>%
                         `colnames<-`(c("Skill", "country2")))
    imbalance
  })
  
  
  
  #----------------------------- plot 2------------------------------------
  
  output$plot2 <-  renderPlotly({
    filtered_data <- sjd2 %>% filter( country == input$country1)
  g2 <- ggplot(filtered_data, aes(x = reorder(skill2, value), y = value, fill = factor(color))) +
    geom_col() +
    scale_fill_manual(values = c("light blue", "Dark blue")) +
    geom_point(data= sjd2 %>%
                 filter(country == "OECD"),
               aes(x = skill2, y = value,)) +
    theme(
      axis.text.x = element_text(color = "black", angle = 45, hjust = 0.9),
      axis.title.x = element_blank(),
      panel.background = element_rect(fill = NA),
      panel.grid.major = element_line(colour = "#00000010"),
      panel.grid.major.x = element_blank(),
      legend.position = "none"
    ) +
    ylab("value") +
    xlab("skill2")+
    scale_y_continuous(
      limits = c(-0.25, 0.7),
      breaks = seq(-0.25, 0.7, 0.05),
      expand = c(0, 0)
    ) 
  
  ggplotly(g2, tooltip = c("country", "skill2", "value"), 
           text = paste("Country: ", filtered_data $country, "<br>",
                        "Skill: ", filtered_data $skill2, "<br>",
                        "Value: ",filtered_data $value))
  
  g2 <- g2 +
    ggtitle(paste("Skills shortage and surplus,", input$country1, ", 2015")) 
    
  
  g2_plotly <- ggplotly(g2)
  g2_plotly %>%    layout(annotations = list(x = 1, y = -0.7, text = "Source: OECD,2015", 
                                             showarrow = F, xref='paper', yref='paper', 
                                             xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                             font=list(size=12, color="black")))
  
})
  output$graph1 <- renderPlotly({
       ggplotly(ggplot(plotdata1(), aes(x = Skill, y = country1,
                                       text = paste("Country: ", {country1}, "<br>",
                                                    "Skill: ", plotdata1()$countryB, "<br>",
                                                    "Value: ", plotdata1()$skillX)
       ))+
                                  
                                 # text = paste("Country: ", plotdata1()$countryA, "<br>",
                                 #              "Skill: ", plotdata1()$countryB, "<br>",
                                 #              "Value: ", plotdata1()$skillX)) +
        geom_segment(aes(x = Skill, xend = Skill, y = country1, yend = country2), color = "grey") +
        geom_point(aes(x = Skill, y = country1, color = "country1"), size = 1.5) +
        geom_point(aes(x = Skill, y = country2, color = "country2"), size = 1.5) +
        coord_flip() +
        scale_color_manual(labels = c(input$countryA, input$countryB), values = c("blue", "#00449450")) +
        theme(panel.background = element_blank(),
              axis.text.x = element_text(size = 7),
              axis.text.y = element_text(size = 7),
              axis.title.x = element_blank(),#(face="talic", margin = margin(8, 00, 8, 00)),
              axis.title.y = element_blank(),#(face="italic", margin = margin(0, 08, 0, 08)),
              legend.position = "top",
              legend.background = element_blank(),
              legend.key = element_rect(color = "white", fill = NA),
              axis.line = element_line(size = 1),
              axis.title = element_text(size = 9),
              axis.text = element_text(size = 9),
              legend.text = element_text(size = 7),
              legend.title = element_text(size = 7),
              plot.title = element_text(size = 11, hjust = 0.5, face = 'bold')) +
        labs(title = paste0 ("Major skills imbalance between ", input$countryA, " and ", input$countryB),
             caption = paste0 ("Source: EWCS, 2015"))
        #xlab("Major Skills") 
        #ylab("Imbalance")
        
    ,tooltip = "text")
       
        })
       
      
  
  plotdata2 <- reactive({
    imbalance <- merge(sjd2 %>%
                         filter(country == input$countryA, skill1 == input$skillX)%>%
                         select(3, 4) %>%
                         `colnames<-`(c("Skill", "country1")),
                       sjd2 %>%
                         filter(country == input$countryB, skill1 == input$skillX)%>%
                         select(3, 4) %>%
                         `colnames<-`(c("Skill", "country2")))
    #imbalance
  })
       
    output$graph2 <- renderPlotly({
      
       gr2 <- ggplot(plotdata2(), aes(x = Skill, y = country1 ,
                                )) +
                                
        geom_segment(aes(x = Skill, xend = Skill, y = country1, yend = country2), color = "grey") +
        geom_point(aes(x = Skill, y = country1, ))+ #size = 1.5) + #color = "country1"
        geom_point(aes(x = Skill, y = country2, ))+ #size = 1.5) + #color = "country2"
        coord_flip() +
        scale_color_manual(labels = c(input$countryA, input$countryB))+ #, values = c("blue", "#00449450")) +
        theme(panel.background = element_blank(),
              axis.text.x = element_text(size = 9),
              axis.text.y = element_text(size = 9),
              axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)),
              axis.title.y = element_text(face="italic", margin = margin(0, 08, 0, 08)),
              legend.position = "top",
              legend.background = element_blank(),
              legend.key = element_rect(color = "white", fill = NA),
              axis.line = element_line(size = 1),
              axis.title = element_text(size = 9),
              axis.text = element_text(size = 9),
              legend.text = element_text(size = 7),
              legend.title = element_text(size = 7),
              plot.title = element_text(size = 11, hjust = 0.5, face = 'bold')) +
        labs(title = paste0 ("sub-skills imbalance between ", input$countryA, " and ", input$countryB),
             caption = paste0 ("Source: EWCS, 2015"))
       
        
        
        ggplotly(gr1 ,tooltip = tooltip = c("country", "skill2", "value"),
                 text = paste("Country: ", plotdata2()$countryA, "<br>",
                                 "Skill: ", plotdata2()$countryB, "<br>",
                              "Value: ", plotdata2()$skillX))
        # xlab("") +
        # ylab("Imbalance")
    }) 
    
   
 #----------------------- Reactive for plot 3-----------------------------------
  
  # reactive expression for filtering based on region selection in skills tab
  region_filter_skill <- reactive({
    filter(sjd2, skill1 == input$skillA)
  })

 # observeevent for updating country choices based on selected region in Overview tab
  observeEvent(region_filter_skill(), {
   skill2 <- unique(region_filter_skill()$skill2)
    updateSelectInput(session, inputId = "skillB", choices = skill2)
  })
  
 #------------------------------plot 3 -----------------------------------------------  
  
  output$plot3 <- renderPlotly({
    filtered_data2 <- sjd2 %>% filter(skill2 == input$skillB & region == input$region2)
    
    g3 <- ggplot(filtered_data2, aes(x = reorder(country, value), y = value, fill = factor(color))) +
      geom_col() +
      scale_fill_manual(values = c("light blue", "Dark blue")) +
      theme(
        axis.text.x = element_text(color = "black", angle = 45, hjust = 0.9),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "#00000010"),
        panel.grid.major.x = element_blank(),
        legend.position = "none"
      ) +
      ylab("Value") +
      xlab("Country") +
      labs(title =paste0( filtered_data2[which.max(filtered_data2$value),1]," has the highest shortage,2015"))+
      scale_y_continuous(
        limits = c(-0.25, 0.7),
        breaks = seq(-0.25, 0.7, 0.05),
        expand = c(0, 0)
      ) 
    
    ggplotly(g3, tooltip = c("country", "skill2", "value"), 
             text = paste("Country: ", filtered_data2$country, "<br>",
                          "Skill: ", filtered_data2$skill2, "<br>",
                          "Value: ", filtered_data2$value))
    ggplotly(g3) %>%
      layout(annotations = list(x = 1, y = -0.3, text = "Source: OECD,2015", 
                                                                      showarrow = F, xref='paper', yref='paper', 
                                                                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                                                      font=list(size=12, color="black"))
      )
    
  })
    
    
    output$tbl <- renderDataTable(datatable(
      tbl))
    
  
}  

shinyApp(ui = ui, server = server) 

