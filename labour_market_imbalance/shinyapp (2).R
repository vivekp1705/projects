#Kindly install, before loading the libraries

library(readr)
library(tidyverse)
library(countrycode) #for matching countries with continents
library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr)
library(rstudioapi)
library(scales)
library(DT)
library(rsconnect)
library(plotly)
library(stats)
library(remotes)
library(d3heatmap)
library(shinythemes)
library(RColorBrewer)
library(htmlwidgets)


#if (!require("devtools")) install.packages("devtools")
#devtools::install_github("talgalili/d3heatmap")

rm(list = ls())
# set working directory and load data
#setwd(dirname(getActiveDocumentContext()$path)) 

load("OECD skill job level 1.rdata")
load("OECD skill job level 2.rdata")

sjd1  <- sjd1 %>% 
  mutate(region = case_when(country %in% c('Australia', 'New Zealand') ~ "Oceania", 
                            country %in% c('Canada', 'Mexico', 'United States', 'Chile') ~ "Americas", 
                            country %in% c('European Union', 'OECD') ~ '',
                            country %in% c('Iceland', 'Norway', 'Switzerland', 'Turkey') ~ "Europe - non-EU",
                            TRUE ~ 'Europe - EU'))

sjd2  <- sjd2 %>% 
  mutate(region = case_when(country %in% c('Australia', 'New Zealand') ~ "Oceania", 
                            country %in% c('Canada', 'Mexico', 'United States', 'Chile') ~ "Americas", 
                            country %in% c('European Union', 'OECD') ~ '',
                            country %in% c('Iceland', 'Norway', 'Switzerland', 'Turkey') ~ "Europe - non-EU",
                            TRUE ~ 'Europe - EU'))

sjd1_all <-  sjd1 %>% 
  mutate(region = "OECD")

sjd2_all <-  sjd2 %>% 
  mutate(region = "OECD")

#skill1_all <- sjd2 %>% 
#             filter(region != "All") %>% 

sjd1 <-  rbind(sjd1,
               sjd1_all)

sjd2 <-  rbind(sjd2,
               sjd2_all)


sjd1_overall = sjd1 %>% # calculate average of all skills per country
  group_by(region, country) %>%
  summarize(value = mean(value)) %>% 
  mutate('skill1' = 'Overall')

# insert skill average per country into table
sjd1_reordered = rbind(sjd1[,c('skill1', 'region', 'country', 'value')],
                       sjd1_overall[,c('skill1', 'region', 'country', 'value')])


sjd2_overall = sjd2 %>% # calculate average of all skills per country and top-level skill
  group_by(region, country, skill1) %>%
  summarize(value = mean(value)) %>% 
  mutate('skill2' = 'Overall')

# insert skill2 average per skill1 into table
sjd2_reordered = rbind(sjd2[,c('skill1', 'skill2', 'region', 'country', 'value')],
                       sjd2_overall[,c('skill1', 'skill2', 'region', 'country', 'value')])

sjd2_oecd <- sjd2_reordered  %>% filter(country == "OECD" & region == "") %>%  select(-region)
sjd2_eu <- sjd2_reordered  %>% filter(country == "European Union" & region == "") %>%  select(-region)

sjd2_oecd <- sjd2_oecd %>% 
  mutate(region = 'Oceania')
sjd2_eu <- sjd2_eu %>% 
  mutate(region = 'Oceania')

sjd2_1 <- sjd2_oecd %>% 
  mutate(region = 'Americas')
sjd2_2 <- sjd2_oecd %>% 
  mutate(region = 'Europe - EU')
sjd2_3 <- sjd2_oecd %>% 
  mutate(region = 'Europe - non-EU')

sjd2_oecd <- rbind(sjd2_oecd, 
                   sjd2_1,
                   sjd2_2,
                   sjd2_3)

sjd2eu1 <- sjd2_eu %>% 
  mutate(region = 'Americas')
sjd2eu2 <- sjd2_eu %>% 
  mutate(region = 'Europe - EU')
sjd2eu3 <- sjd2_eu %>% 
  mutate(region = 'Europe - non-EU')

sjd2_eu <- rbind(sjd2_eu, 
                 sjd2eu1,
                 sjd2eu2,
                 sjd2eu3)

sjd2_oecd <- rbind(sjd2_reordered, sjd2_oecd, sjd2_eu)


sjd2_oecd  <- sjd2_oecd %>% 
  mutate(color = case_when(country %in% c('Australia', 'New Zealand') ~ "0", 
                           country %in% c('Canada', 'Mexico', 'United States', 'Chile') ~ "1", 
                           country %in% c('European Union', 'OECD') ~ "2",
                           country %in% c('Iceland', 'Norway', 'Switzerland', 'Turkey') ~ "3",
                           TRUE ~ '4'))

sjd2_reordered  <- sjd2_reordered %>% 
  mutate(color = case_when(country %in% c('Australia', 'New Zealand') ~ "Oceania", 
                           country %in% c('Canada', 'Mexico', 'United States', 'Chile') ~ "Americas", 
                           country %in% c('European Union') ~ "EU",
                           country %in% c('OECD') ~ "OECD",
                           country %in% c('Iceland', 'Norway', 'Switzerland', 'Turkey') ~ "Europe (non-EU)",
                           TRUE ~ 'European Union'))


#-------------------------------------- Defining UI ----------------------------------------

ui_tabs <- 
  fluidPage (titlePanel(title = div(img(src = "dirname(getActiveDocumentContext()$path)/OECD-social-sharex.jpg"), 
                                    "Skills Imbalance on the Labour Market in OECD Countries")), 
             theme = shinytheme("cerulean"),
             
             #----------- tab1 -----------
             fluidRow(navbarPage(title = "Select the type of analysis",
                                 fluidRow(column(2, selectInput("region", 
                                                                "Region", 
                                                                choices = unique(sjd1[sjd1$region != '', ]$region), 
                                                                selected = "OECD", multiple = F), #a region filter at the page level to synchronize it with all the tabs
                                 ),
                                 column(2,selectInput ("level1",
                                                       "Top-Level Skill",
                                                       choices = unique(sjd2$skill1),
                                                       selected = "Basic Skills (Content)")
                                                       ) #a level filter at the page level to syncronise it with all the tabs
                                 ),
                                 #----------------- Top Level Skill Analysis -----------------
                                 navbarMenu("Skill Analysis", 
                                            
                                            #---------- heat map 1 -----------
                                            tabPanel(title = "Top Level Skills",
                                                     fluidRow (
                                                       column (8, 
                                                               div(d3heatmapOutput("heatmap1", 
                                                                               height="350px", # changes the height of the table of data (350 seems good)
                                                                               width="1350px" # changes the width of the table of data (1900 seems good)
                                                               ), caption = "jhbjhb" #still struggling to add a title for the heatmaps
                                                               ),
                                                               helpText("The OECD skills imbalances indicator helps identify mismatches between the supply \
                      and demand of skills in a country. The value ranges from -1 (a surplus skill that \
                      is easy to find) to 1 (a shortage skill that is hard to find). In the above chart, \
                      orange cells identify surplus skills and purple cells identify shortage skills. \
                     Cells with dark shading indicate greater imbalance, while cells with light shading \
                     indicate greater balance."  ),
                                                               #--------table 1----------
                                                                  
                                                       ),
                                                       column (4, 
                                                        div(dataTableOutput('stats1'), style = "font-size:80%")  #adjustment for accomodating table and heat map in the same row without overlap
                                                       )
                                                     ),
                                                     #----------------- Box plot and column plot top level -------------
                                                     fluidRow(column(8, plotlyOutput("column_plot")),
                                                              column(4, plotlyOutput("boxplot")))           
                                            ),
                                #--------------- Sub Category Level Skill ----------------
                                            tabPanel(title = "Sub Categories of Top Level Skills",
                                                     fluidRow(column(2, selectInput ("level2", "Sub-Category Level", choices = sjd2$skill2, selected
                                                                              = "All", multiple = F),
                                                              )
                                                     ),
                                                     fluidRow(column(8, d3heatmapOutput("heatmap2", 
                                                                                        height="350px", # changes the height of the table of data (350 seems good)
                                                                                        width="1400px")),
                                                              column(4, div(dataTableOutput('stats2'), style = "font-size:80%")# changes the width of the table of data (1400 seems good)
                                                              ) 
                                                     ),
                                                     fluidRow(column(8, helpText("The OECD skills imbalances indicator helps identify mismatches between the supply \
                      and demand of skills in a country. The value ranges from -1 (a surplus skill that \
                      is easy to find) to 1 (a shortage skill that is hard to find). In the above chart, \
                      orange cells identify surplus skills and purple cells identify shortage skills. \
                      Cells with dark shading indicate greater imbalance, while cells with light shading \
                      indicate greater balance."))),
                                                     fluidRow(column(8, plotlyOutput("plot4")),
                                                              column(4, plotlyOutput("boxplot2"))
                                                     )
                                            )),
                                 #--------- tab 2---------
                                 navbarMenu(title = "Regional Analysis",
                                            
                                            #---------- Skills by countries ----------
                                            tabPanel(title = "By Country",
                                                     fluidRow(column(2, selectInput("country",
                                                                                    "Country", 
                                                                                    choices = unique(sjd1$country))
                                                     )),
                                                     fluidRow(column(5, plotlyOutput("plot1",
                                                                                     height = "400px"),
                                                                     fluidRow(dataTableOutput("stats3"))),
                                                              column(7, plotlyOutput("plot2",
                                                                                     height = "400px"))
                                                     ),
                                                     fluidRow()
                                            )
                                 ),
                                            
                                 )
                                 
             )
  )

# ------------------------------------- Define server ------------------------------

server  <- 
  function(input, output) {
    #--------------------------------------------- SKILL ANALYSIS (Tab 1) ----------------------------------------------#

    #-------------------------------- Heat Map 1 -----------------------------------
    # calculate reactive data table of statistics to display
    sjd1_stats = reactive({
      sjd1 %>% 
        filter(region == input$region) %>% 
        group_by(skill1) %>% 
        summarize(skill1_max = sprintf("%.2f", round(max(value, na.rm = TRUE), 2)), # changes display formatting so all values show two decimal places
                  skill1_min = sprintf("%.2f", round(min(value, na.rm = TRUE), 2)),
                  skill1_median = sprintf("%.2f", round(median(value, na.rm = TRUE), 2)),
                  skill1_mean = sprintf("%.2f", round(mean(value, na.rm = TRUE), 2)),
                  skill1_stdev = sprintf("%.2f", round(sd(value, na.rm = TRUE), 2))
        )
    })
    
    # render data table
    output$stats1 = DT::renderDT(
      datatable(sjd1_stats()[ , c('skill1', 
                                  'skill1_max', 
                                  'skill1_min', 
                                  'skill1_median', 
                                  'skill1_mean', 
                                  'skill1_stdev')],
                colnames = c('Level 1 skill', 
                             'Max', 
                             'Min', 
                             'Median', 
                             'Mean', 
                             'Std Dev'), 
                options = list(paging = FALSE, # short table so no need for paging or searching
                               searching = FALSE),
                caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  
                                                  font-size:200% ;',paste0('Table - 1: ', input$level1, ' statistics in ', input$region))
      )
    )
    
    # pivot table so it becomes wide
    pivot_sjd1 = reactive({
      data.frame(sjd1_reordered %>% 
                   filter(region == input$region) %>% 
                   pivot_wider(names_from = country,
                               names_prefix = "",
                               values_from = "value"))
    }) 
    # set rownames to Skill1 value so first column can be removed to form matrix as required for d3heatmap function
    reactive({
      rownames(pivot_sjd1()) = pivot_sjd1()$skill1
    })
    
    # create vector with skill1 values to be used in d3heatmap function as row labels
    skills = reactive({
      pivot_sjd1()$skill1
    })
    
    # remove first two columns containing text values so data frame can be converted to matrix
    pivot_sjd1_numeric = reactive({
      pivot_sjd1()[, 3:length(pivot_sjd1()[1,])]
    })
    
    # convert data frame (numbers only) to matrix for use in d3heatmap function
    sjd1_matrix = reactive({
      data.matrix(pivot_sjd1_numeric())
    })
    
    # render heatmap with options (colors, legend)
    output$heatmap1 = renderD3heatmap({
      d3heatmap(sjd1_matrix(),
                col = brewer.pal(9, "PuOr"), # color palette (purple -> white -> orange, avoided red / green)
                rng = c(-.2, 0.6), # range of color scale based on observed extremes in data
                breaks = c(-.7, -.5, -.3, -.1, 0, .1, .3, .5, .7), # break color scale into 9 equally size ranges between the min and max values of rng
                density.info = "histogram", # shows histogram of observed values
                symm = TRUE, # make colors symmetrical around 0
                dendrogram = "none", # no dendrogram, which visually groups similar rows / columns together with "forks"
                sideRow = 1, # row labels on left
                key = TRUE, # include a legend
                key.location = "bl", # legend in bottom left
                keysize = .45, # make legend 45% of the row height x column width
                labRow = skills(), # each row label is a skill
                cellnote_row = "Level 1 skill", # label for row names when hovering
                cellnote_col = "Country", # label for column names when hovering
                cellnote_val = "Skill imbalance", # label for cell values when hovering
                print.values = TRUE, # show values inside cells
                digits = 2, # round cell values to two decimal places
      ) %>%
        hmAxis( # set size and font size of x-axis
          axis = "x",
          size = 100, 
          font.size = 11,
        ) %>%
        hmAxis( # set size and font size of y-axis
          axis = "y",
          size = 300, 
          font.size = 12 
        )
    })
    
    #-------------------------------- Table 1 -----------------------------------
    
    # calculate reactive data table of statistics to display
    sjd2_stats = reactive({
      sjd2_reordered %>% 
        #select(-region) %>% 
        filter(skill1 == input$level1 & skill2 != "Overall" & region == input$region) %>%
        group_by(skill2) %>% 
        summarize(skill2_max = sprintf("%.2f", round(max(value, na.rm = TRUE), 2)), # changes display formatting so all values show two decimal places
                  skill2_min = sprintf("%.2f", round(min(value, na.rm = TRUE), 2)),
                  skill2_median = sprintf("%.2f", round(median(value, na.rm = TRUE), 2)),
                  skill2_mean = sprintf("%.2f", round(mean(value, na.rm = TRUE), 2)),
                  skill2_stdev = sprintf("%.2f", round(sd(value, na.rm = TRUE), 2))
        )
    })
    
    
    #-------------------------------- Boxplot 1 -----------------------------------
    
    output$boxplot <- renderPlotly({
      ggplotly(sjd2_reordered %>%
                 filter(skill1 == input$level1 & skill2 == "Overall" & region != "") %>% 
                 ggplot(aes(x = region, y = value, fill = region)) +
                 geom_boxplot()+
                 # stat_summary(fun = "mean", geom = "point", shape = 10, size = 2, color = "white")+
                 theme_light()+
                 labs(title = paste0("Range and average of ", input$level1, 
                                     '<br>',
                                     '<sup>',
                                     "skill mismatch level across all regions"),
                      x = 'Regions',
                      y = "Skill Mismatch Indicator")+
                 theme(panel.background = element_blank(),
                       axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)),
                       axis.title.y = element_text(face="italic", margin = margin(00, 8, 00, 8)),
                       legend.position = "none", 
                       legend.background = element_blank(),
                       #legend.title = element_text(size = 10),
                       axis.line = element_line(size = 1),
                       axis.title = element_text(size = 11),
                       axis.text = element_text(size = 11),
                       #legend.text = element_text(size = 8),
                       #legend.spacing = unit(2, 'cm'),
                       plot.title = element_text(size = 16, hjust = 0.5, face = 'bold'),
                       plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"))+
                 scale_fill_manual(values = c("#0077B6", "#ADD8E6", "#82EEFD", "#87CEEB", "#90E0EF", "#0077B6"))) %>%  #"#90E0EF", "#0077B6", "#61A5C2"))+
        layout(annotations = list(x = 1, y = 1, text = "Source - OECD:2015", showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=8)))
    })
    
    #-------------------------------- Column plot 1 -----------------------------------
    
    titleData <- reactive({
      sjd2_reordered %>% 
        filter(skill1 == input$level1 & skill2 == "Overall") %>% 
        #filter(skill2 == input$level2) %>% 
        #filter(region!= "All")
        filter(region == input$region & country != "OECD" & country != "European Union")
    })
    
    output$column_plot = renderPlotly(({
      ggplotly(sjd2_reordered %>% 
                 filter(skill1 == input$level1 & skill2 == "Overall" & region == input$region & country != "OECD" & country != "European Union") %>% #forcing reaction to the plot
                 ggplot2::ggplot(aes(x = reorder(country, -value), y = value, fill = color,
                                     text = paste0("Country: ", {country}, '<br>', #hoverinfo
                                                   "Region: ", {color}, '<br>',
                                                   "Level of Mismatch: ", {value})))+ #text for configuring hover info
                 geom_col()+
                 geom_hline(yintercept = as.numeric(sjd2_reordered %>% #getting the value of OECD for every skill
                                                      filter(skill1 == input$level1 & skill2 == "Overall" & country == "OECD" & region != "OECD") %>% select(value)), 
                            linetype = "dashed", color="#30D5C8")+ #displaying horizontal line for OECD value 
                 geom_text(aes(1,as.numeric(sjd2_reordered %>% #displaying the value of OECD on the y intercept
                                              filter(skill1 == input$level1 & skill2 == "Overall" & country == "OECD" & region != "OECD") %>% select(value)),
                               label = paste0("OECD: ", as.numeric(sjd2_reordered %>% 
                                                                     filter(skill1 == input$level1 & skill2 == "Overall" & country == "OECD" & region != "OECD") %>% 
                                                                     select(value))), 
                               vjust = 5))+
                 labs(title = paste0("Labour market top level skill comparison of ", input$level1, " in ", input$region, #reactive skill and region names
                                     '<br>',
                                     '<sup>',
                                     #for reactive name of region based on highest skill value by region
                                     titleData()[which.max(titleData()$value),4], " has the highest shortage of ", input$level1, " in ", input$region), #reactive name of the country having highest shortage
                      x = 'Countries',
                      y = "Skill Mismatch Indicator",
                      caption = "Source: OECD, 2015") + 
                 scale_fill_manual(values = c("#0077B6", "#ADD8E6", "#82EEFD", "#87CEEB", "#90E0EF", "#0077B6"), name = "Region")+ #naming legend title and adding colors 
                 theme(panel.background = element_blank(), 
                       axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)), #adjusting margins of x and y axis titles
                       axis.title.y = element_text(face="italic", margin = margin(0, 08, 0, 08)),
                       axis.text.x = element_text(hjust=0.9, angle = 45), #text aligned at an angle
                       legend.position = "top", 
                       legend.background = element_blank(),
                       legend.title = element_blank(),
                       #legend.title = element_text(size = 10),
                       axis.line = element_line(size = 1),
                       axis.title = element_text(size = 11),
                       axis.text = element_text(size = 11),
                       legend.text = element_blank(),
                       #legend.spacing = unit(2, 'cm'),
                       plot.title = element_text(size = 16, hjust = 0.5, face = 'bold'),
                       plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic")),
               tooltip = "text") %>% #tooltip for displaying desired information on the graph
        layout(annotations = list(x = 1, y = 1, text = "Source - OECD:2015", showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=9))) #for displaying caption
    }))
    
    
    
    
    #----------------------------------- SKILL ANALYSIS (tab 2) --------------------------
    
    #-------------------------------- Table 2 -----------------------------------
    
    # render data table
    output$stats2 = renderDataTable(
      datatable(sjd2_stats()[ , c('skill2', 
                                  'skill2_max', 
                                  'skill2_min', 
                                  'skill2_median', 
                                  'skill2_mean', 
                                  'skill2_stdev')],
                colnames = c('Level 2 skill', 
                             'Max', 
                             'Min', 
                             'Median', 
                             'Mean', 
                             'Std Dev'), 
                options = list(paging = FALSE, # short table (max 11 rows) so no need for paging or searching
                               searching = FALSE),
                caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  font-size:200% ;',
                                                  paste0('Table - 2: Sub Categorical statistics for ', input$level1, ' in ', input$region))
      )
    )
    
    
    #-------------------------------- Heat map 2 ----------------------------------- 
    
    # pivot table so it becomes wide
    pivot_sjd2 = reactive({
      data.frame(sjd2_reordered %>% 
                   filter(region == input$region) %>% 
                   select(-region, -color) %>% 
                   filter(skill1 == input$level1) %>% 
                   pivot_wider(names_from = country,
                               names_prefix = "",
                               names_sort = TRUE,
                               values_from = "value"))
    }) 
    # set rownames to skill2 value so first column can be removed to form matrix as required for d3heatmap function
    reactive({
      rownames(pivot_sjd2()) = pivot_sjd2()$skill2
    })
    
    # create vector with skill2 values to be used in d3heatmap function as row labels
    skillsv2 = reactive({
      pivot_sjd2()$skill2
    })
    
    # remove first two columns containing text values so data frame can be converted to matrix
    pivot_sjd2_numeric = reactive({
      pivot_sjd2()[, 3:length(pivot_sjd2()[1,])]
    })
    
    # convert data frame (numbers only) to matrix for use in d3heatmap function
    sjd2_matrix = reactive({
      data.matrix(pivot_sjd2_numeric())
    })
    
    # render heatmap with options (colors, legend)
    output$heatmap2 = renderD3heatmap({
      d3heatmap(sjd2_matrix(),
                col = brewer.pal(9, "PuOr"), # color palette (purple -> white -> orange, avoided red / green)
                rng = c(-.2, 0.6), # range of color scale based on observed extremes in data
                breaks = c(-.7, -.5, -.3, -.1, 0, .1, .3, .5, .7), # break color scale into 9 equally size ranges between the min and max values of rng
                density.info = "histogram", # shows histogram of observed values
                symm = TRUE, # make colors symmetrical around 0
                revC = FALSE, # reverse column order to show countries alphabetically (symm = TRUE reverses them by default!)
                dendrogram = "none", # no dendrogram, which visually groups similar rows / columns together with "forks"
                sideRow = 4, # row labels on left
                key = TRUE, # include a legend
                key.location = "bl", # legend in bottom left
                keysize = .45, # make legend 45% of the row height x column width
                labRow = skillsv2(), # each row label is a skill
                cellnote_row = "Level 2 skill", # label for row names when hovering
                cellnote_col = "Country", # label for column names when hovering
                cellnote_val = "Skill imbalance", # label for cell values when hovering
                print.values = TRUE, # show values inside cells
                digits = 2, # round cell values to two decimal places
                caption = "Heatmap 2 - Snapshot of sub category level skills in OECD Countries"
      ) %>%
        hmAxis( # set size and font size of x-axis
          axis = "x",
          size = 100, 
          font.size = 11,
        ) %>%
        hmAxis( # set size and font size of y-axis
          axis = "y",
          size = 300, 
          font.size = 12 
        ) %>% 
        hmLegend(show = T, title = "Title", location = "tl")
    })
     
    # ----------------------------------------- Column plot 2 -------------------------------------
    
    skill1 <- reactive({
      filter(sjd2_reordered, skill1 == input$level1 & skill2 != "Overall")
    })
    
    observeEvent(skill1(),{
      skill2 <- unique(skill1()$skill2)
      updateSelectInput(inputId = "level2", choices = skill2)
    })
    
    plotData <- reactive({
      sjd2_oecd %>% 
        #filter(skill1 == input$level) %>% 
        filter(skill2 == input$level2) %>% 
        #filter(region!= "All")
        filter(region == input$region & country != "OECD" & country != "European Union")
    })
    
    
    
    output$plot4 <- renderPlotly({
      ggplotly(sjd2_reordered %>%
                 filter(skill2 == input$level2 & region == input$region & country != "OECD" & country != "European Union") %>% 
                 ggplot2::ggplot(aes(x = reorder(country, -value), y = value, fill = color,
                                     text = paste0("Country: ", {country}, '<br>', #hoverinfo
                                                   "Region: ", {color}, '<br>',
                                                   "Level of Mismatch: ", {value})))+ #text for configuring hover info))+
                 geom_col()+
                 scale_fill_manual(values = c("#0077B6", "#ADD8E6", "#82EEFD", "#87CEEB", "#90E0EF", "#0077B6"), name = "Regions")+ 
                 geom_hline(yintercept = as.numeric(sjd2_reordered %>% #getting the value of OECD for every skill
                                                      filter(skill1 == input$level1 & skill2 == input$level2 & country == "OECD" & region != "OECD") %>% select(value)), 
                            linetype = "dashed", color="#30D5C8")+ #displaying horizontal line for OECD value 
                 geom_text(aes(4,as.numeric(sjd2_reordered %>% #displaying the value of OECD on the y intercept
                                              filter(skill1 == input$level1 & skill2 == input$level2 & country == "OECD" & region != "OECD") %>% select(value)),
                               label = paste0("OECD: ", as.numeric(sjd2_reordered %>% 
                                                                     filter(skill1 == input$level1 & skill2 == input$level2 & country == "OECD" & region != "OECD") %>% 
                                                                     select(value))), 
                               vjust = 11, hjust = 1))+
                 labs(title = paste0("Labour market comparison at the sub category level of ", input$level2, " in ", input$region, 
                                     '<br>',
                                     '<sup>',
                                     plotData()[which.max(plotData()$value),4], " has the highest shortage of ", input$level1, " in ", input$region),  
                      x = 'Countries',
                      y = "Skill Mismatch Indicator",
                      caption = "Source: OECD, 2015") + 
                 #scale_y_continuous(breaks = seq(0,35,5)) +
                 theme(panel.background = element_blank(),
                       axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)),
                       axis.title.y = element_text(face="italic", margin = margin(0, 08, 0, 08)),
                       axis.text.x = element_text(hjust=0.9, angle = 45),
                       legend.position = "top", 
                       legend.background = element_blank(),
                       legend.title = element_blank(),
                       axis.line = element_line(size = 1),
                       axis.title = element_text(size = 11),
                       axis.text = element_text(size = 11),
                       legend.text = element_blank(),
                       #legend.spacing = unit(2, 'cm'),
                       plot.title = element_text(size = 16, hjust = 0.5, face = 'bold'),
                       plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic")), 
               tooltip = "text") %>% 
        layout(annotations = list(x = 1, y = 1, text = "Source - OECD:2015", showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=8)))
    })
    
    # ----------------------------------------- Box plot 2 -------------------------------------
    
    output$boxplot2 <- renderPlotly({
      ggplotly(sjd2_reordered %>%
                 filter(skill1 == input$level1 & skill2 == input$level2 & region != "") %>% 
                 ggplot(aes(x = region, y = value, fill = region)) +
                 geom_boxplot()+
                 #stat_summary(fun = "mean", geom = "point", shape = 10, size = 2, color = "white")+
                 theme_light()+
                 labs(title = paste0("Range and average of ", input$level2, 
                                     '<br>',
                                     '<sup>',
                                     "skill mismatch level across all regions"),
                      x = 'Regions',
                      y = "Skill Mismatch Indicator")+
                 theme(panel.background = element_blank(),
                       axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)),
                       axis.title.y = element_text(face="italic", margin = margin(00, 8, 00, 8)),
                       legend.position = "none", 
                       legend.background = element_blank(),
                       #legend.title = element_text(size = 10),
                       axis.line = element_line(size = 1),
                       axis.title = element_text(size = 10),
                       axis.text = element_text(size = 10),
                       #legend.text = element_text(size = 8),
                       #legend.spacing = unit(2, 'cm'),
                       plot.title = element_text(size = 16, hjust = 0.5, face = 'bold'),
                       plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"))+
                 scale_fill_manual(values = c("#0077B6", "#ADD8E6", "#82EEFD", "#87CEEB", "#90E0EF", "#0077B6"))) %>%  #"#90E0EF", "#0077B6", "#61A5C2"))+
        layout(annotations = list(x = 1, y = 1, text = "Source - OECD:2015", showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=8)))
    })
    
    # ------------------------------- REGIONAL ANALYSIS  -------------------------------
    
    #reactive filter of country
    region <- reactive({
      filter(sjd1, region == input$region)
    })
    observeEvent(region(),{
      country <- unique(region()$country)
      updateSelectInput(inputId = "country", choices = country)
    })
    
    #skill 1&2 reactive df
    plot2_df <- reactive({
      sjd2_reordered %>%
        filter(region == input$region & skill1 == input$level1 & skill2 != "Overall") %>% 
        filter(country == input$country)
    })
    
    plot1_df <- reactive({
      sjd1 %>% 
        filter(region == input$region) %>% 
        filter(country == input$country)
    })
    
    
    #plot1 -- bar chart skill 1
    output$plot1 <- renderPlotly({
      ggplotly(plot1_df() %>% 
                 #group_by(country, skill1) %>% 
                 #summarise(mismatch = mean(value)) %>% 
                 ggplot2::ggplot(aes(x = reorder(skill1, -value), y = value, fill = skill1,
                                     text = paste0("Type of Skill: ", {skill1}, '<br>',
                                                   "Level of Mismatch: ", {value})))+
                 geom_col()+
                 coord_flip()+
                 scale_fill_manual(values = c("#D1EAF0", "#ADD8E6", "#82EEFD", "#87CEEB", "#90E0EF", "#0077B6", "#61A5C2"))+
                 labs(title = paste0("Labour market at the top level in ",input$country,
                                     '<br>',
                                     '<sup>',
                                     'has the highest shortage of ', plot1_df()[which.max(plot1_df()$value),2], 
                                     '<br>'),
                      x = '',
                      y = "OECD Skill Mismatch Indicator",
                      caption = "Source: OECD, 2015") + 
                 #scale_y_continuous(breaks = seq(0,35,5)) +
                 theme(panel.background = element_blank(),
                       axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)),
                       axis.title.y = element_text(face="italic", margin = margin(0, 08, 0, 08)),
                       axis.text.x = element_text(angle=45, hjust=0.9),
                       legend.position = "none", 
                       legend.background = element_blank(),
                       legend.title = element_blank(),
                       axis.line = element_line(size = 1),
                       axis.title = element_text(size = 12),
                       axis.text = element_text(size = 11),
                       legend.text = element_blank(),
                       #legend.spacing = unit(2, 'cm'),
                       plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
                       plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic")), 
               tooltip = "text", ) %>% 
        layout(annotations = list(x = 1, y = 1, text = "Source - OECD:2015", showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=9)))
    }) 
    
    #plot2 -- bar chart skill 2
    
    
    output$plot2 <- renderPlotly({
      ggplotly(plot2_df() %>% 
                 ggplot2::ggplot(aes(x = reorder(skill2, value), y = value, fill = skill1,
                                     text = paste0("Top Level Skill:", {skill1}, '<br>',
                                                   "Sub Category Skill: ", {skill2}, '<br>',
                                                   "Level of Mismatch: ", {value})))+
                 geom_col()+
                 coord_flip()+
                 #  facet_wrap(~skill1, )+
                 labs(title = paste0("Labour market at the ", input$level1, " in ", input$country,
                                     '<br>',
                                     '<sup>',
                                     'has the highest shortage of ', plot2_df()[which.max(plot2_df()$value),2], 
                                     " and highest supply of ", plot2_df()[which.min(plot2_df()$value),2], ' skills', '<br>'),  
                      x = 'Sub categories of top level skills',
                      y = "OECD Skill Mismatch Indicator",
                      caption = "Source: OECD, 2015") + 
                 scale_fill_manual(values = c("#D1EAF0", "#ADD8E6", "#82EEFD", "#87CEEB", "#90E0EF", "#0077B6", "#61A5C2"))+
                 #scale_y_continuous(breaks = seq(0,35,5)) +
                 theme(panel.background = element_blank(),
                       axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)),
                       axis.title.y = element_text(face="italic", margin = margin(0, 08, 0, 08)),
                       axis.text.x = element_text(hjust=0.9),
                       legend.position = "none", 
                       legend.background = element_blank(),
                       legend.title = element_blank(),
                       #legend.title = element_text(size = 10),
                       axis.line = element_line(size = 1),
                       axis.title = element_text(size = 11),
                       axis.text = element_text(size = 11),
                       legend.text = element_text(size = 10),
                       #legend.spacing = unit(2, 'cm'),
                       plot.title = element_text(size = 16, hjust = 0.5, face = 'bold'),
                       plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic")),
               tooltip = "text") %>% 
        layout(annotations = list(x = 1, y = -0.05, text = "Source - OECD:2015", showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=9)))
    })
    
    
    #table 1 - top level skills by country
    
    sjd1_table1 <-  reactive({
      sjd2_reordered %>% 
        filter(region == input$region & country == input$country) %>% 
        group_by(skill1) %>% 
        summarize(skill2_max = sprintf("%.2f", round(max(value, na.rm = TRUE), 2)), # changes display formatting so all values show two decimal places
                  skill2_min = sprintf("%.2f", round(min(value, na.rm = TRUE), 2)),
                  skill2_median = sprintf("%.2f", round(median(value, na.rm = TRUE), 2)),
                  skill2_mean = sprintf("%.2f", round(mean(value, na.rm = TRUE), 2)),
                  skill2_stdev = sprintf("%.2f", round(sd(value, na.rm = TRUE), 2))
        )
    })
    
    
    
    sjd2_stats3 = reactive({
      sjd2_reordered %>% 
        #select(-region) %>% 
        filter(region == input$region & country == input$country) %>% 
        group_by(skill1) %>% 
        summarize(skill2_max = sprintf("%.2f", round(max(value, na.rm = TRUE), 2)), # changes display formatting so all values show two decimal places
                  skill2_min = sprintf("%.2f", round(min(value, na.rm = TRUE), 2)),
                  skill2_median = sprintf("%.2f", round(median(value, na.rm = TRUE), 2)),
                  skill2_mean = sprintf("%.2f", round(mean(value, na.rm = TRUE), 2)),
                  skill2_stdev = sprintf("%.2f", round(sd(value, na.rm = TRUE), 2))
        )
    })
    
    # render data table
    output$stats3 = renderDataTable(
      datatable(sjd2_stats3()[ , c('skill1', 
                                   'skill2_max', 
                                   'skill2_min', 
                                   'skill2_median', 
                                   'skill2_mean', 
                                   'skill2_stdev')],
                colnames = c('Top level skill', 
                             'Max', 
                             'Min', 
                             'Median', 
                             'Mean', 
                             'Std Dev'), 
                options = list(paging = FALSE, # short table (max 11 rows) so no need for paging or searching
                               searching = FALSE),
                caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  
                                                  font-size:200% ;',paste0('Table 3 - Ranges of Sub Category Level Skills in ', input$country, 
                                                                           ' within all the top level skills'))
      )
    )
    
    
  }



# Run the application
shinyApp(ui_tabs, server)



