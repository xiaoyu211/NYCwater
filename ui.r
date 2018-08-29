shinyUI(navbarPage("Exploring NYC's Water", theme = "style.css",
  #tags$body(

  
  
#################### Start of a Menu Item ####################  
tabPanel("Main",
         sidebarLayout(
           div(class="side", sidebarPanel(width=0)),
           mainPanel(width=12,
            img(src="waterstuff6.png", style="width:100%")
           )
        )
      
    ),
#################### End of the Menu Item ####################


###############Start Richard's Menu Item################### 
tabPanel("Overview",
         sidebarLayout(position="right",                            
                       sidebarPanel(
                         conditionalPanel(condition="input.ccpanel==1",
                                          helpText("We display the proportion of the Complaints type and Year it happened"),
                                          selectInput("year","Choose a Year to display",
                                                      choices = list("2013"=1,"2014"=2,"2015"=3),
                                                      selected = 1),br(),
                                          selectInput("borough","Choose a Borough to display",
                                                      choices = list("BRONX"=1,"BROOKLYN"=2,"MANHATTAN"=3,"QUEENS"=4,"STATEN ISLAND"=5),
                                                      selected = 1)),
                         
                         conditionalPanel(condition="input.ccpanel==2",
                                          selectInput("mapyear","Choose a Year to display",
                                                      choices = list("2013"=1,"2014"=2,"2015"=3),
                                                      selected = 3))
                         
                       ),
                       mainPanel(
                         tabsetPanel(type="pill",id="ccpanel",
                                     tabPanel("Basic Information",br(),tags$div(class="descrip_text", 
                                                                                textOutput("pie_text")), br(),
                                              plotlyOutput('piechart',height="600px" ),value=1),
                                     
                                     tabPanel("Basic Information",br(),tags$div(class="descrip_text",
                                                                                textOutput("heat_text")), br(),
                                              chartOutput("baseMap", "leaflet"),
                                              #leafletOutput("baseMap"),
                                              tags$style('.leaflet {width: 930px;height:580px}'),
                                              tags$head(tags$script(src="https://leaflet.github.io/Leaflet.heat/dist/leaflet-heat.js")),
                                              uiOutput('heatMap'),value=2)
                         )))),


#################### End of Richard's Menu Item ####################



#################### Start Josh's Menu Item ####################
    tabPanel("Water Quality",  
      # Sidebar with a selector input for neighborhood
      sidebarLayout(position="right",
        sidebarPanel(
          conditionalPanel(condition="input.conditionedPanels==1",
            helpText("Explore this wordcloud that shows the various descriptors used by NYC residents to describe their water"),
            br(),
            sliderInput("desc_range", 
              label = "Number of Descriptors",
              min = 1, max = 13, value = 13)
          ),
          conditionalPanel(condition="input.conditionedPanels==2",
            helpText("Try selecting different descriptions of water to see if it correlates with water sampled from NYC's upstate reservoirs"),
            hr(),
            selectInput("complaint_desc", 
              label = "Choose a description",
              choices = list("All", "Chemical Taste", "Clear with Insects/Worms", "Clear w/ Particles", "Cloudy Water", "Metallic Taste/Odor", "Musty Taste/Odor", "Milky Water", "Oil in Water", "Sewer Taste/Odor", "Unknown Taste"),
              selected = "All complaints")
          ),
          conditionalPanel(condition="input.conditionedPanels==3",
            helpText("Discover the locations where NYC residents claim the drinking water has made them sick"),
            hr(),
            selectInput("ill_year", 
              label = "Choose a year",
              choices = list("All", "2015", "2014", "2013"))
          )
        ),
        # Show main panel
        mainPanel(
          tabsetPanel(type="pill",
            # Panel 1 is a bubble chart showing descriptors
            tabPanel("Reported Water Quality", br(), tags$div(class="descrip_text", textOutput("descrip_text")), br(), tags$div(class="descrip_plot", plotOutput("descrip_plot")), value=1), 
            # Panel 2 is a line chart comparing water quality
            tabPanel("Sampled Water Quality", br(), tags$div(class="descrip_text", textOutput("sample_text")), br(), plotlyOutput("sample_plot"), value=2),
            # Panel 3 is a map showing illness
            #tabPanel("Illness", br(), tags$div(class="descrip_text", textOutput("ill_text")), br(), leafletOutput("ill_map"), value=3),
            tabPanel("Illness", br(), tags$div(class="descrip_text", textOutput("ill_text")), br(), showOutput("ill_map", "leaflet"),tags$style('.leaflet {width: 930px;height:580px}'), value=3),
            id = "conditionedPanels"
          ) 
        )
      )
    ),
#################### End of Josh's Menu Item ####################


###############Start Schinria's Menu Item################### 
tabPanel("Duplicates",
#tabPanel("Duplicate Complaints",
         # Sidebar with a selector input for neighborhood
         sidebarLayout(position="right",
                       sidebarPanel(
                         conditionalPanel(condition="input.cPanels==4",
                                          helpText("Check out what NYC boroughs had duplicate complaints filed"), hr(),
                                          selectInput("burr", "Choose a borough:", 
                                                      choices=c("Bronx","Brooklyn","Manhattan","Queens","Staten Island"))
                                        
                         ),
                         conditionalPanel(condition="input.cPanels==5",
                                          helpText("Now compare the ratio of duplicate to non-duplicate complaints"), hr(),
                                          selectInput(inputId = "type",
                                                      label = "Choose a chart type",
                                                      choices = c("multiBarChart", "multiBarHorizontalChart"),
                                                      selected = "multiBarChart"),
                                          checkboxInput(inputId = "stack",
                                                        label = strong("Want to stack the bars?"),
                                                        value = FALSE)
                         )   
                       ),
                       # Show main panel
                       mainPanel(
                         tabsetPanel(type="pill",
                                     #Panel 1 is a bar chart of cuplicates by year            
                                     tabPanel("Complaints By Year", br(), tags$div(class="descrip_text", textOutput("barplot_text")), br(), plotOutput("duplicatePlot"), value=4),
                                     #Panel 2 is a stacked bar chart of duplicates vs. non-duplicates
                                     tabPanel("Complaints by Borough", br(), tags$div(class="descrip_text", textOutput("barplot2_text")), br(), showOutput("myChart", "nvd3"), value=5),
                                     id = "cPanels"
                         )
                       )
         )
),

# End ui
#################### xiaoyu s Menu Item ####################

tabPanel("Resolution Time",
         sidebarLayout(position="right",
                       #h3("All about Time"),
                       sidebarPanel(
                         
                         conditionalPanel(condition = "input.condPanels == 1",
                                          selectInput("cases", "Case Status:", 
                                                      choices = list( "Open" = 1, "Closed" = 2), selected = 1)),
                         
                         conditionalPanel(condition = "input.condPanels == 2 ",
                                          selectInput("time", "Timeline by Boroughs:", 
                                                      choices = list("All Boroughs" = 1,
                                                                     "BRONX " = 2,
                                                                     "BROOKLYN" = 3,
                                                                     "MANHATTAN" = 4,
                                                                     "QUEENS" = 5,
                                                                     "STATEN ISLAND" = 6),selected = 1),
                                          selectInput("compare", "Between-Boroughs Comparison:", 
                                                      choices = list("NONE" = 1,
                                                                     "BRONX " = 2, 
                                                                     "BROOKLYN" = 3,
                                                                     "MANHATTAN" = 4,
                                                                     "QUEENS" = 5,
                                                                     "STATEN ISLAND" = 6),selected = 1),
                                          
                                          selectInput("type_2", "Complaint.Type Timeline:", 
                                                      choices = list("DrinkingWater" = 1,
                                                                     "HEAT/HOT WATER" = 2, 
                                                                     "Water System" = 3,
                                                                     "Water Leak" = 4), selected = 1),
                                          
                                          selectInput("com", "Between-Boroughs Comparison:", 
                                                      choices = list("NONE" = 0,
                                                                     "DrinkingWater" = 1,
                                                                     "HEAT/HOT WATER" = 2, 
                                                                     "Water System" = 3,
                                                                     "Water Leak" = 4), selected = 0)),
                         
                         conditionalPanel(condition = "input.condPanels == 3",
                                          selectInput("status", "Time Spent:", 
                                                      choices = list("By Boroughs" = 1,
                                                                     "By ComplaintType" = 2), selected = 1))
                         
                         
                       ),
                       
                       mainPanel(tabsetPanel(id = "condPanels",type="pill",
                                             
                                             tabPanel("Summary",br(),
                                                      fluidRow(column(6,plotlyOutput("case2", width="350px",height="350px")),
                                                               column(6,plotlyOutput("case3", width="350px",height="350px"))),value = 1),
                                             
                                             tabPanel("Timeline",br(),
                                                      dygraphOutput("dygraph", width="800px",height="300px"),
                                                      dygraphOutput("dygraph2", width="800px",height="300px"), value = 2), 
                                             
                                             tabPanel("Time Spent",br(),
                                                      plotlyOutput("plot", width="700px",height="400px"),
                                                      tableOutput("view"), value = 3)
                                             
                                             
                                             
                       ))))

#################### end of  xiaoyu s Menu Item ####################





))
