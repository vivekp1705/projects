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



# if (!require("devtools")) install.packages("devtools")
# devtools::install_github("talgalili/d3heatmap")

rm(list = ls())
# set working directory and load data
#setwd(dirname(getActiveDocumentContext()$path)) 

load("OECD skill job level 1.rdata")
load("OECD skill job level 2.rdata")

# add region to top-level skill table
sjd1  <- sjd1 %>% 
  mutate(region = case_when(country %in% c('Australia', 'New Zealand') ~ "Oceania", 
                            country %in% c('Canada', 'Mexico', 'United States', 'Chile') ~ "Americas", 
                            country %in% c('European Union', 'OECD') ~ '',
                            country %in% c('Iceland', 'Norway', 'Switzerland', 'Turkey') ~ "Europe - non-EU",
                            TRUE ~ 'Europe - EU'))

# add region to secondary-level skill table
sjd2  <- sjd2 %>% 
  mutate(region = case_when(country %in% c('Australia', 'New Zealand') ~ "Oceania", 
                            country %in% c('Canada', 'Mexico', 'United States', 'Chile') ~ "Americas", 
                            country %in% c('European Union', 'OECD') ~ '',
                            country %in% c('Iceland', 'Norway', 'Switzerland', 'Turkey') ~ "Europe - non-EU",
                            TRUE ~ 'Europe - EU'))

# duplicate rows in top-level skill table with region set to OECD to facilitate filtering
sjd1_all <-  sjd1 %>% 
  filter(region != '') %>% 
  mutate(region = "OECD")

sjd1 <-  rbind(sjd1,
               sjd1_all)

# duplicate rows in secondary-level skill table with region set to OECD to facilitate filtering
sjd2_all <-  sjd2 %>% 
  filter(region != '') %>% 
  mutate(region = "OECD")

sjd2 <-  rbind(sjd2,
               sjd2_all)

# calculate average of all skills per country
sjd1_overall = sjd1 %>% 
  group_by(region, country) %>%
  summarize(value = mean(value),
            .groups = "keep") %>% 
  mutate('skill1' = 'Overall')


# insert skill average per country into table and re-order columns
sjd1_reordered = rbind(sjd1[,c('skill1', 'region', 'country', 'value')],
                       sjd1_overall[,c('skill1', 'region', 'country', 'value')])

# calculate average of all skills per country and top-level skill 
sjd2_overall = sjd2 %>% 
  group_by(region, country, skill1) %>%
  summarize(value = mean(value),
            .groups = "keep") %>% 
  mutate('skill2' = 'Overall')

# insert skill2 average per skill1 into table into table and re-order columns
sjd2_reordered = rbind(sjd2[,c('skill1', 'skill2', 'region', 'country', 'value')],
                       sjd2_overall[,c('skill1', 'skill2', 'region', 'country', 'value')])

# creating a column for adding distinct colors across regions

sjd2_reordered  <- sjd2_reordered %>% 
  mutate(color = case_when(country %in% c('Australia', 'New Zealand') ~ "Oceania", 
                           country %in% c('Canada', 'Mexico', 'United States', 'Chile') ~ "Americas", 
                           country %in% c('European Union') ~ "EU",
                           country %in% c('OECD') ~ "OECD",
                           country %in% c('Iceland', 'Norway', 'Switzerland', 'Turkey') ~ "Europe (non-EU)",
                           TRUE ~ 'European Union'))


# -------------------------------------- Defining UI ----------------------------------------

ui_tabs <- 
  fluidPage(  
   titlePanel(title=div(img(src="OECD-social-sharex.jpg", height=50, width=100), 
                        "Skills Imbalance on the Labour Market in OECD Countries")),
    theme = shinytheme("cerulean"), #theme for the dashboard
             
             #----------- tab1 -----------
             fluidRow(navbarPage(title = "Select the type of analysis",
                                 fluidRow(column(2, selectInput("region", 
                                                                "Region", 
                                                                choices = unique(sjd1[sjd1$region != '', ]$region), 
                                                                selected = "OECD", 
                                                                multiple = F), #a region filter at the page level to synchronize it with all the tabs
                                 ),
                                 column(2,selectInput ("level1",
                                                       "Top-Level Skill",
                                                       choices = unique(sjd2$skill1),
                                                       selected = "Basic Skills (Content)")
                                                       ), # a top level filter at the page level to synchronise it with all the tabs
                                 column(2, ),
                                 column(2, box(title = "Countries", uiOutput("txt1"), height = 50, width= 100, background = "navy"), # KPI indicating number of countries in the region
                                        ),
                                 column(2, box(title = "Top-Level Skills", uiOutput("txt2"), height = 50, width= 100), # KPI indicating total number of top level skills
                                        ),
                                 column(2, box(title = "Sub-Category Level Skills", uiOutput("txt3"), height = 50, width = 100), # KPI indicating number of sub categorical skills in the top level skill
                                 ),
                                 ),
                                 #----------------- Top-Level Skill Analysis -----------------
                                 navbarMenu("Skill Analysis", 
                                            
                                            #---------- heat map 1 -----------
                                            tabPanel(title = "Top-Level Skills",
                                                     fluidRow (
                                                       column (8, fluidRow(column(4),
                                                                           uiOutput("txt4",), # heatmap1 title
                                                                           div(style = "height:8px") # adjusting the distance between title and the heeat map
                                                       ),
                                                               d3heatmapOutput("heatmap1",
                                                                               # , 
                                                                                    height="400px", # changes the height of the table of data (350px)
                                                                               #     width="1350px" # changes the width of the table of data (1350px)
                                                               
                                                               ),
                                                               helpText("The OECD skills imbalance indicator helps identify mismatches between the supply \
                      and demand of skills in a country. The value ranges from -1 (a surplus skill that \
                      is easy to find) to 1 (a shortage skill that is hard to find). In the above chart, \
                      orange cells identify surplus skills and purple cells identify shortage skills. \
                     Cells with dark shading indicate greater imbalance, while cells with light shading \
                     indicate greater balance."  ),
                                                               #--------table 1----------
                                                                  
                                                       ),
#                                                       column (4, div(dataTableOutput('stats1'), style = "font-size:80%")  #adjustment for accommodating table and heat map in the same row without overlap
                                                       column (4, dataTableOutput('stats1')  #adjustment for accommodating table and heat map in the same row without overlap
                                                       )
                                                     ),
                                                     #----------------- Box plot and column plot top-level -------------
                                                     fluidRow(column(8, plotlyOutput("column_plot")),
                                                              column(4, plotlyOutput("boxplot")))           
                                            ),
                                #--------------- Subcategory Skill ----------------
                                            tabPanel(title = "Subcategories of Top-Level Skills",
                                                     fluidRow(column(2, selectInput ("level2", "Subcategory Skill", 
                                                                                     choices = unique(sjd2$skill2), 
#                                                                                     selected = "All", 
                                                                              multiple = F),
                                                              )
                                                     ),
                                                  #---------- heat map 2 -----------
                                                     fluidRow(column(8,fluidRow(column(2),
                                                                                uiOutput("txt5",), #heatmap2 title
                                                                                div(style = "height:8px") # adjusting the distance between heatmap 2 and its title
                                                     ), d3heatmapOutput("heatmap2",
                                                                        height="400px", # changes the height of the table of data
                                                                      )),
                                                              #---------- table 2 -----------
                                                              column(4, div(dataTableOutput('stats2')) # style = "font-size:80%"
                                                              ) 
                                                     ),
                                                     fluidRow(column(8, helpText("The OECD skills imbalance indicator helps identify mismatches between the supply \
                      and demand of skills in a country. The value ranges from -1 (a surplus skill that \
                      is easy to find) to 1 (a shortage skill that is hard to find). In the above chart, \
                      orange cells identify surplus skills and purple cells identify shortage skills. \
                      Cells with dark shading indicate greater imbalance, while cells with light shading \
                      indicate greater balance."))),
                                              #----------------- Box plot and column plot sub category-level -------------
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
                                                     #---------- top level skills by countries ----------
                                                     fluidRow(column(6, plotlyOutput("plot1",
                                                                                     height = "400px",
                                                                                     ),
                                                                     div(style = "height:68px"), #to align all the 3 tables with each other
                                                                     fluidRow(dataTableOutput("stats3"))),
                                                              #---------- Statastics of top level skills by countries ----------
                                                              column(6, plotlyOutput("plot2",
                                                                                     height = "400px",
                                                                                     ),
                                                                     div(style = "height:8px"), #to align all the 3 tables with each other
                                                                     #---------- top 5 demand and surplus tables ----------
                                                                     fluidRow(column(6, dataTableOutput("demand")),
                                                                                     
                                                                              column(6, dataTableOutput("surplus"))
                                                                     ))
                                                     ),
                                                     fluidRow()
                                            )
                                 ),
                                            
                                 )
                                 
             )
  
)

# ------------------------------------- Define server ------------------------------

server  <- 
  function(input, output, session) {
    # --------------------------------------------- SKILL ANALYSIS (Tab 1) ----------------------------------------------#
    
    # KPI indicators 
    
    # countries
    txt1 <- reactive({
      sjd2_reordered %>% 
        filter(region == input$region & country != "OECD" & country != "European Union" & skill1 == input$level1)
    })
    
    output$txt1 <- renderUI({
      HTML(paste0("<b><strong>", "<font face = Verdana; size = 3; font-weight = bold; color = navy>",
        n_distinct(txt1()$country),
        "</b></strong></font>", "<sup>", "<br>")) # adding html tags for adjusting font size, type and color
    })
    
    # skill1
    txt2 <- reactive({
      sjd2_reordered %>% 
        filter(region == input$region & country != "OECD" & country != "European Union")
    })
    
    output$txt2 <- renderUI({
      HTML(paste0("<b><strong>", "<font face = Verdana; size = 3; font-weight = bold; color = navy>",
        n_distinct(txt2()$skill1),
        "</b></strong></font>", "<sup>", "<br>")) # adding html tags for adjusting font size, type and color
    })
    
    # skill2
    output$txt3 <- renderUI({
      HTML(paste0("<b><strong>", "<font face = Verdana; size = 3; font-weight = bold; color = navy>",
        n_distinct(txt1()$skill2),
        "</b></strong></font>", "<sup>", "<br>")) # adding html tags for adjusting font size, type and color
    })
    
    # title for heatmap 1
    
    output$txt4 <- renderUI({
      HTML(paste0("<b><strong>", "<font face = Verdana; size = +2; font-weight = bold; color = #000000>", 
                  "Snapshot of top-level skills in ",  
                  input$region, "</b></strong></font>", "<sup>", "<br>")) # adding html tags for adjusting font size, type and color
    })
    
    # -------------------------------- Heat Map 1 -----------------------------------
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
                colnames = c('Top-level skill', 
                             'Max', 
                             'Min', 
                             'Median', 
                             'Mean', 
                             'Std Dev'), 
                options = list(paging = FALSE, # short table so no need for paging or searching
                               searching = FALSE),
                caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  
                                                  font-size:150% ;font-family: verdana ; font-weight: bold'
                                                  ,paste0(input$level1, ' stats in ', input$region)) # adding html tags for adjusting font size, type and color
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
                breaks = c(-.7, -.5, -.3, -.1, 0, .1, .3, .5, .7), # break color scale into 9 symmetric size ranges
                density.info = "histogram", # shows histogram of observed values
                symm = TRUE, # make colors symmetrical around 0
                revC = FALSE, # reverse column order to show countries alphabetically (symm = TRUE reverses them by default!)
                dendrogram = "none", # no dendrogram, which visually groups similar rows / columns together with "forks"
                sideRow = 1, # row labels on left
                key = TRUE, # include a legend
                key.location = "bl", # legend in bottom left
                keysize = .45, # make legend 45% of the row height x column width
                labRow = skills(), # each row label is a skill
                cellnote_row = "Top-level skill", # label for row names when hovering
                cellnote_col = "Country", # label for column names when hovering
                cellnote_val = "Skill imbalance", # label for cell values when hovering
                print.values = FALSE, # show values inside cells, change to TRUE if change mind
                digits = 2, # round cell values to two decimal places
              #  main = paste0("Snapshot of top-level skills in ",  input$region)
            
      ) %>%
        hmAxis( # set size and font size of x-axis
          axis = "x",
          size = 100, 
          font.size = 11,
        ) %>%
        hmAxis( # set size and font size of y-axis
          axis = "y",
          size = 200, 
          font.size = 12 
        )
    })
    
    # -------------------------------- Table 1 -----------------------------------
    
    # calculate reactive data table of statistics to display
    sjd2_stats = reactive({
      sjd2_reordered %>% 
        filter(skill1 == input$level1 & skill2 != "Overall" & region == input$region) %>%
        group_by(skill2) %>% 
        summarize(skill2_max = sprintf("%.2f", round(max(value, na.rm = TRUE), 2)), # changes display formatting so all values show two decimal places
                  skill2_min = sprintf("%.2f", round(min(value, na.rm = TRUE), 2)),
                  skill2_median = sprintf("%.2f", round(median(value, na.rm = TRUE), 2)),
                  skill2_mean = sprintf("%.2f", round(mean(value, na.rm = TRUE), 2)),
                  skill2_stdev = sprintf("%.2f", round(sd(value, na.rm = TRUE), 2))
        )
    })
    
    
    # -------------------------------- Boxplot 1 -----------------------------------
    
    output$boxplot <- renderPlotly({
      ggplotly(sjd2_reordered %>%
                 filter(skill1 == input$level1 & skill2 == "Overall" & region != "") %>% 
                 ggplot(aes(x = region, y = value, fill = region)) +
                 geom_boxplot()+
                 theme_light()+
                 labs(title = paste0("Range and average of ", input$level1, #reactive title
                                     '<br>',
                                     '<sup>',
                                     "skills imbalance level across all regions",
                                     '<br>',
                                     '<sup>'), 
                      x = '',
                      y = "OECD Skills Imbalance Indicator")+
                 theme(panel.background = element_blank(),
                       axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)), # adjusting margin of axis label from the axis values
                       axis.title.y = element_text(face="italic", margin = margin(00, 8, 00, 8)),
                       legend.position = "none", 
                       legend.background = element_blank(),
                       #legend.title = element_text(size = 10),
                       axis.line = element_line(linewidth = 1),
                       axis.title = element_text(size = 11),
                       axis.text = element_text(size = 11),
                       #legend.text = element_text(size = 8),
                       #legend.spacing = unit(2, 'cm'),
                       plot.title = element_text(size = 16, hjust = 0.5, face = 'bold', family = "Verdana"),
                       plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic", family = "Verdana"))+
                 scale_fill_manual(values = c("#0077B6", "#ADD8E6", "#82EEFD", "#87CEEB", "#90E0EF", "#0077B6"))) %>%  #"#90E0EF", "#0077B6", "#61A5C2"))+
        layout(annotations = list(x = 0.22, y = 1, text = "Source: OECD, 2015", showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=11)))
    })
    
    # -------------------------------- Column plot 1 -----------------------------------
    
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
                                                   "Skills Imbalance Indicator: ", {round(value,2)})))+ #text for configuring hover info
                 geom_col()+
                 geom_hline(yintercept = as.numeric(sjd2_reordered %>% #getting the value of OECD for every skill
                                                      filter(skill1 == input$level1 & skill2 == "Overall" & country == "OECD" & region != "OECD") %>% select(value)), 
                            linetype = "dashed", color="black")+ #displaying horizontal line for OECD value (was #30D5C8)
                 annotate("text", 
                          x = max(2, nrow(unique((sjd2_reordered %>%
                                                    filter(skill1 == input$level1 & skill2 == "Overall" & region == input$region & country != "OECD" & country != "European Union")))) - 2), 
                          y = 1.15 * as.numeric(sjd2_reordered %>% #getting the value of OECD for every skill
                                                 filter(skill1 == input$level1 & skill2 == "Overall" & country == "OECD" & region != "OECD") %>% select(value)), 
                          label=paste0("OECD: ", sprintf("%.2f", round(as.numeric(sjd2_reordered %>% 
                                                              filter(skill1 == input$level1 & skill2 == "Overall" & country == "OECD" & region != "OECD") %>% 
                                                              select(value)), 2))),
                          size=4, 
                          color="black") +  # add label to horizontal line to show OECD average
                 labs(title = paste0("Labour market top-level skill comparison of ", input$level1, " in ", input$region, #reactive skill and region names
                                     '<br>',
                                     '<sup>',
                                     #for reactive name of region based on highest skill value by region
                                     titleData()[which.max(titleData()$value),4], " has the highest shortage of ", input$level1, " in ", input$region), #reactive name of the country having highest shortage
                      x = '',
                      y = "OECD Skills Imbalance Indicator",
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
                       axis.line = element_line(linewidth = 1),
                       axis.title = element_text(size = 11),
                       axis.text = element_text(size = 11),
                       legend.text = element_blank(),
                       #legend.spacing = unit(2, 'cm'),
                       plot.title = element_text(size = 16, hjust = 0.5, face = 'bold', family = "Verdana"),
                       plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic", family = "Verdana")),
               tooltip = "text") %>% #tooltip for displaying desired information on the graph
        layout(annotations = list(x = 1, y = -0.5, text = "Source: OECD, 2015", showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=11))) #for displaying caption
    }))
    
    
    
    
    # ----------------------------------- SKILL ANALYSIS (tab 2) --------------------------
    
    # -------------------------------- Table 2 -----------------------------------
    
    # render data table
    output$stats2 = renderDataTable(
      datatable(sjd2_stats()[ , c('skill2', 
                                  'skill2_max', 
                                  'skill2_min', 
                                  'skill2_median', 
                                  'skill2_mean', 
                                  'skill2_stdev')],
                colnames = c('Subcategory skill', 
                             'Max', 
                             'Min', 
                             'Median', 
                             'Mean', 
                             'Std Dev'), 
                options = list(paging = FALSE, # short table (max 11 rows) so no need for paging or searching
                               searching = FALSE),
                caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  font-size:150% ;
                                                  font-family: verdana ; font-weight: bold',
                                                  paste0('Subcategory stats for ', input$level1, ' in ', input$region)) # adding html tags for adjusting font size, type and color
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
    
    # title for heat map 2
    
    output$txt5 <- renderUI({
      HTML(paste0("<b><strong>", "<font face = Verdana; size = +2; font-weight = bold; color = #000000>", 
                  "Snapshot of subcategory skills for ", input$level1, " in " ,
                  input$region, "</b></strong></font>", "<sup>", "<br>")) #adding html tags for adjusting font size, type and color
    })
    
    # render heatmap with options (colors, legend)
    output$heatmap2 = renderD3heatmap({
      d3heatmap(sjd2_matrix(),
                col = brewer.pal(9, "PuOr"), # color palette (purple -> white -> orange, avoided red / green)
                rng = c(-.2, 0.6), # range of color scale based on observed extremes in data
                breaks = c(-.7, -.5, -.3, -.1, 0, .1, .3, .5, .7), # break color scale into 9 symmetric size ranges                
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
                print.values = FALSE, # show values inside cells (change to TRUE if change mind)
                digits = 2, # round cell values to two decimal places
               # main = paste0("Snapshot of subcategory level skills for ", input$level1, " in " , input$region)
      ) %>%
        hmAxis( # set size and font size of x-axis
          axis = "x",
          size = 100, 
          font.size = 11,
        ) %>%
        hmAxis( # set size and font size of y-axis
          axis = "y",
          size = 200, 
          font.size = 12 
        ) 
    })
     
    # ----------------------------------------- Column plot 2 -------------------------------------
    
    #reactive function for updating sub category skills (skill 2) after selecting top level skill (skill 1)
    
    skill1 <- reactive({
      filter(sjd2_reordered, skill1 == input$level1 & skill2 != "Overall")
    })
    
    #observing skill1 to update skill2 choices
    observeEvent(skill1(),{
      skill2 <- unique(skill1()$skill2)
      updateSelectInput(session, inputId = "level2", choices = skill2)
    })
    
    # reactive data to only show specific country names
    
    plotData <- reactive({
      sjd2_reordered %>% 
        filter(skill2 == input$level2) %>% 
        filter(region == input$region & country != "OECD" & country != "European Union")
    })
    
  # column plot for comparing countries at sub category level 
    output$plot4 <- renderPlotly({
      ggplotly(sjd2_reordered %>%
         filter(skill2 == input$level2 & region == input$region & country != "OECD" & country != "European Union") %>% 
         ggplot2::ggplot(aes(x = reorder(country, -value), y = value, fill = color,
                             text = paste0("Country: ", {country}, '<br>', #hoverinfo
                                           "Region: ", {color}, '<br>',
                                           "Skills Imbalance Indicator: ", {round(value,2)})))+ #text for configuring hover info))+
         geom_col()+
         scale_fill_manual(values = c("#0077B6", "#ADD8E6", "#82EEFD", "#87CEEB", "#90E0EF", "#0077B6"), name = "Regions")+ 
         geom_hline(yintercept = as.numeric(sjd2_reordered %>% #getting the value of OECD for every skill
                                              filter(skill1 == input$level1 & skill2 == input$level2 & country == "OECD" & region != "OECD") %>% select(value)), 
                    linetype = "dashed", color="black")+ #displaying horizontal line for OECD value 
         annotate("text", 
                  x = max(2, nrow(unique((sjd2_reordered %>%
                                   filter(skill2 == input$level2 & region == input$region & country != "OECD" & country != "European Union")))) - 2), 
                  y = 1.15 * as.numeric(sjd2_reordered %>% #getting the value of OECD for every skill
                                          filter(skill1 == input$level1 & skill2 == input$level2 & country == "OECD" & region != "OECD") %>% select(value)), 
                  label=paste0("OECD: ", sprintf("%.2f", round(as.numeric(sjd2_reordered %>% 
                                                      filter(skill1 == input$level1 & skill2 == input$level2 & country == "OECD" & region != "OECD") %>% 
                                                      select(value)), 2))), 
                  size=4, 
                  color="black") +  # add label to horizontal line to show OECD average
         labs(title = paste0("Labour market comparison at the subcategory level of ", input$level2, " in ", input$region, 
                             '<br>',
                             '<sup>',
                             plotData()[which.max(plotData()$value),4], # getting the name of skill from column 4 on the basis of the value of skill mismatch 
                                                                         # (deriving value from another column in the same row) 
                             " has the highest shortage of ", input$level1, " in ", input$region),  
              x = '',
              y = "OECD Skills Imbalance Indicator",
              caption = "Source: OECD, 2015") + 
         theme(panel.background = element_blank(),
              axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)),
              axis.title.y = element_text(face="italic", margin = margin(0, 08, 0, 08)),
              axis.text.x = element_text(hjust=0.9, angle = 45),
              legend.position = "top", 
              legend.background = element_blank(),
              legend.title = element_blank(),
              axis.line = element_line(linewidth = 1),
              axis.title = element_text(size = 11),
              axis.text = element_text(size = 11),
              legend.text = element_blank(),
              plot.title = element_text(size = 16, hjust = 0.5, face = 'bold', family = "Verdana"),
              plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic", family = "Verdana")), 
              tooltip = "text") %>% 
      layout(annotations = list(x = 1, y = -0.5, text = "Source: OECD, 2015", showarrow = F, xref='paper', yref='paper', 
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=11))) # for adding and adjusting caption
    })
    
    # ----------------------------------------- Box plot 2 -------------------------------------
    
    output$boxplot2 <- renderPlotly({
      ggplotly(sjd2_reordered %>%
                 filter(skill1 == input$level1 & skill2 == input$level2 & region != "") %>% 
                 ggplot(aes(x = region, y = value, fill = region)) +
                 geom_boxplot()+
                 theme_light()+
                 labs(title = paste0("Range and average of ", input$level2, # reactive title for the plot
                                     '<br>',
                                     '<sup>',
                                     "skills imbalance level across all regions"),
                      x = '',
                      y = "OECD Skills Imbalance Indicator")+
                 theme(panel.background = element_blank(),
                       axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)),
                       axis.title.y = element_text(face="italic", margin = margin(00, 8, 00, 8)),
                       legend.position = "none", 
                       legend.background = element_blank(),
                       axis.line = element_line(linewidth = 1),
                       axis.title = element_text(size = 10),
                       axis.text = element_text(size = 10),
                       plot.title = element_text(size = 16, hjust = 0.5, face = 'bold', family = "Verdana"),
                       plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic", family = "Verdana"))+ #adjusting font size and type for the plot 
                 scale_fill_manual(values = c("#0077B6", "#ADD8E6", "#82EEFD", "#87CEEB", "#90E0EF", "#0077B6"))) %>%  # adding specific colors
        layout(annotations = list(x = 0.22, y = 1, text = "Source: OECD, 2015", showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=11))) # adjusting caption size and location on the plot
    })
    
    # ------------------------------- REGIONAL ANALYSIS  -------------------------------
    
    # reactive filter of country
    region <- reactive({
      filter(sjd1, region == input$region)
    })
    observeEvent(region(),{
      country <- unique(region()$country)
      updateSelectInput(session, inputId = "country", choices = country)
    })
    
    # skill 1&2 reactive df
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
    
    
    # plot1 -- bar chart skill 1
    output$plot1 <- renderPlotly({
      ggplotly(plot1_df() %>% 
                ggplot2::ggplot(aes(x = reorder(skill1, -value), y = value, fill = skill1,
                                     text = paste0("Type of Skill: ", {skill1}, '<br>', # hover info
                                                   "Skills Imbalance Indicator: ", {round(value,2)})))+
                 geom_col()+
                 coord_flip()+
                 scale_fill_manual(values = c("#D1EAF0", "#ADD8E6", "#82EEFD", "#87CEEB", "#90E0EF", "#0077B6", "#61A5C2"))+ # adding colors
                 labs(title = paste0("Labour market at the top-level in ",input$country,
                                     '<br>', # reactive title
                                     '<sup>',
                                     'has the highest shortage of ', plot1_df()[which.max(plot1_df()$value),2], 
                                     '<br>'),# for extracting the info from another column in the same row as the highest value of skill mismatch
                      x = '',
                      y = "OECD Skills Imbalance Indicator",
                      caption = "Source: OECD, 2015") + 
                 theme(panel.background = element_blank(),
                       axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)), # adjusting margins
                       axis.title.y = element_text(face="italic", margin = margin(0, 08, 0, 08)),
                       axis.text.x = element_text(angle=45, hjust=0.9), # aligning text of the plot at an angle
                       legend.position = "none", 
                       legend.background = element_blank(),
                       legend.title = element_blank(),
                       axis.line = element_line(linewidth = 1),
                       axis.title = element_text(size = 12),
                       axis.text = element_text(size = 11),
                       legend.text = element_blank(),
                       plot.title = element_text(size = 16, hjust = 0.5, face = "bold", family = "Verdana"), # font size location and type adjustment 
                       plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic")), 
               tooltip = "text", ) %>% 
        layout(annotations = list(x = 1, y = 1, text = "Source: OECD, 2015", showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=9)))
    }) 
    
    #plot2 -- bar chart skill 2
    
    output$plot2 <- renderPlotly({
      ggplotly(plot2_df() %>% 
                 ggplot2::ggplot(aes(x = reorder(skill2, value), y = value, fill = skill1,
                                     text = paste0("Top-Level Skill:", {skill1}, '<br>', # reactive hover info
                                                   "Subcategory Skill: ", {skill2}, '<br>',
                                                   "Skills Imbalance Indicator: ", {round(value,2)})))+ 
                 geom_col()+
                 coord_flip()+
                 labs(title = paste0("Labour market at the ", input$level1, " in ", input$country,
                                     '<br>', # reactive plot title
                                     '<sup>',
                                     'has the highest shortage of ', plot2_df()[which.max(plot2_df()$value),2], # deriving the name of skills with highest shortage
                                     " and highest supply of ", plot2_df()[which.min(plot2_df()$value),2], ' skills', '<br>'),  # deriving the name of skills with highest supply
                      x = 'Subcategories of top-level skills',
                      y = "OECD Skills Imbalance Indicator",
                      caption = "Source: OECD, 2015") + 
                 scale_fill_manual(values = c("#D1EAF0", "#ADD8E6", "#82EEFD", "#87CEEB", "#90E0EF", "#0077B6", "#61A5C2"))+ #adding colors
                 theme(panel.background = element_blank(),
                       axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)), #adjusting margins
                       axis.title.y = element_text(face="italic", margin = margin(0, 08, 0, 08)),
                       axis.text.x = element_text(hjust=0.9), # adjustments in theme layout
                       legend.position = "none", 
                       legend.background = element_blank(),
                       legend.title = element_blank(),
                       axis.line = element_line(linewidth = 1),
                       axis.title = element_text(size = 11),
                       axis.text = element_text(size = 11),
                       legend.text = element_text(size = 10),
                       plot.title = element_text(size = 16, hjust = 0.5, face = 'bold', family = "Verdana"), #adjusting font size, type and location
                       plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic", family = "Verdana")),
               tooltip = "text") %>% 
        layout(annotations = list(x = 1, y = -0.05, text = "Source: OECD, 2015", showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=9)))
    })
    
    
    # table 3 - top-level skills by country
    
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
                colnames = c('Top-level skill', 
                             'Max', 
                             'Min', 
                             'Median', 
                             'Mean', 
                             'Std Dev'), 
                options = list(paging = FALSE, # short table (max 11 rows) so no need for paging or searching
                               searching = FALSE),
                caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  
                                                  font-size:150% ; font-family: verdana ; font-weight: bold', #adjusting font size, type for title 
                                                  paste0('Range of Subcategory Skills in ', input$country,  #reactive title
                                                                           ' for top-level skills'))
      )
    )
    
    
    # reactive dataset for displaying skill demand
    demand <- reactive({
      sjd2_reordered %>% 
        filter(region == input$region & skill1 == input$level1 & skill2 != "Overall") %>% 
        select(country, skill2, value) %>% 
      arrange(desc(value)) %>% 
      top_n(5) %>% 
      mutate(value = sprintf("%.2f", round(value, 2)))
    })
    
    
    
    output$demand = renderDataTable(
      datatable(demand(),
                colnames = c('Country', 
                             'Subcategory skill', 
                             'Value'), 
                options = list(paging = FALSE, # short table (max 11 rows) so no need for paging or searching
                               searching = FALSE),
                caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  
                                                  font-size:150% ; font-family: verdana ; font-weight: bold'  #formating the caption to be displayed as the title with a selected font style 
                                                  ,paste0('Top Countries with Demand surplus for subcategory skills in ', input$region, ' for ', input$level1 
                                                  )) # for reacting to the selected region and skill in the title
        
      )
    )
    
    # reactive dataset for displaying skill surplus
    surplus <- reactive({
      sjd2_reordered %>% 
        filter(region == input$region & skill1 == input$level1 & skill2 != "Overall" & value < 0) %>% # to get only negative values
        select(country, skill2, value) %>% 
        arrange(value) %>% 
        top_n(-5) %>% 
        mutate(value = sprintf("%.2f", round(value, 2)))
    })
    output$surplus = renderDataTable(
      datatable(surplus(),
                colnames = c('Country', 
                             'Subcategory skill', 
                             'Value'), 
                options = list(paging = FALSE, # short table (max 11 rows) so no need for paging or searching
                               searching = FALSE),
                caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  
                                                  font-size:150% ;font-family: verdana ; font-weight: bold',
                                                  paste0('Top Countries with Supply surplus for subcategory skills in ', input$region, ' for ', input$level1 
                                                  )) # for reacting to the selected region and skill in the title
                
      )
    )
    
    
  }

# Run the application
shinyApp(ui_tabs, server)




