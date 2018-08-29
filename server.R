
library(shiny)

# library(dplyr)
# library(data.table)
# library(wordcloud)
# library(plotly)
# library(leaflet)
# library(rCharts)
# 
# library(ggplot2)
# require(lubridate)
# library(dygraphs)
# library(xts)
# 
# 
# #xiaoyu data
# A <- readRDS("../data/ONE.Rds")
# FIVE <- readRDS("../data/FIVE.Rds")
# THREE <- readRDS("../data/THREE.Rds")
# 
# C <- data.frame(Borough = A$Borough[A$Status == "Closed"], 
#                 Complaint.Type = A$Complaint.Type[A$Status == "Closed"],Days = A$Days[A$Status == "Closed"])
# 
# ## error 
# final_shiny <- readRDS("../data/final_shiny.rds")
# shiny2_stacked <- readRDS("../data/shiny2_stacked.rds")
# 
# 
# # Read in data
# water <- readRDS("../data/data_4.Rds")
# water$Created.Date <- NULL
# water$Resolution.Action.Updated.Date <- NULL
# 
# #################### Data ####################
# water_qual_Turbid <- readRDS("../data/turbid.RDS")
# water_qual_Chlorine <- readRDS("../data/chlorine.RDS")
# # https://data.cityofnewyork.us/Environment/Drinking-Water-Quality-Distribution-Monitoring-Dat/bkwf-xfky
# #water_qual_Turbid <- aggregate(water_qual$Turbidity, list(water_qual$Date), mean)
# #water_qual_Turbid <- plyr::rename(water_qual_Turbid, c("Group.1"="Date", "x"="Turbidity"))
# #water_qual_Turbid$Date <- format(as.yearmon(water_qual_Turbid$Date, "%m/%d/%Y"), "%m")
# #water_qual_Turbid <- aggregate(water_qual_Turbid$Turbidity, list(water_qual_Turbid$Date), mean)
# #water_qual_Turbid <- plyr::rename(water_qual_Turbid, c("Group.1"="Date", "x"="Turbidity"))
# #water_qual_Turbid$Date <- mapvalues(water_qual_Turbid$Date, from = water_qual_Turbid$Date, c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec"))
# 
# #water_qual_Chlorine <- aggregate(water_qual$Chlorine, list(water_qual$Date), mean)
# #water_qual_Chlorine <- plyr::rename(water_qual_Chlorine, c("Group.1"="Date", "x"="Chlorine"))
# #water_qual_Chlorine$Date <- format(as.yearmon(water_qual_Chlorine$Date, "%m/%d/%Y"), "%m")
# #water_qual_Chlorine <- aggregate(water_qual_Chlorine$Chlorine, list(water_qual_Chlorine$Date), mean)
# #water_qual_Chlorine <- plyr::rename(water_qual_Chlorine, c("Group.1"="Date", "x"="Chlorine"))
# #water_qual_Chlorine$Date <- mapvalues(water_qual_Chlorine$Date, from = water_qual_Chlorine$Date, c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec"))
# 
# 
# drink_water <- filter(water, water$Complaint.Type == "Drinking Water")
# quality_water <- filter(water, water$Complaint.Type == "Water Quality")
# 
# quality_water$Descriptor <- mapvalues(quality_water$Descriptor, from = c(unique(quality_water$Descriptor)), to=c("Chlorine Taste/Odor", "Other", "Cloudy Water", "Other Water Problem", "Milky Water", "Unknown Taste", "Metallic Taste/Odor", "Musty Taste/Odor", "Clear w/ Particles", "Chemical Taste", "Sewer Taste/Odor", "Oil in Water", "Other", "Clear with Insects/Worms"))
# 
# rep_q_water <- quality_water
# rep_q_water <- rep_q_water[grepl("2015", rep_q_water$Date),]
# rep_q_water$Date <- (format(as.yearmon(rep_q_water$Date, "%Y-%m-%d"), "%m"))
# 
# # Main data table
# rep_q_water_table <- as.data.frame(table(rep_q_water$Date, rep_q_water$Descriptor))
# rep_q_water_table <- plyr::rename(rep_q_water_table, c("Var1"="Date", "Var2"="Descriptor", "Freq"="Number"))
# 
# final_shiny_1 <- readRDS("../data/final_shiny.rds")
# 
# shiny2_stacked_1 <- readRDS("../data/shiny2_stacked.rds")
# 
# source(file = "Global.R")

shinyServer(function(input, output) {

#################### Josh's Output ####################
  output$ill_map <- renderMap({ #renderLeaflet
    if(input$ill_year == "All"){
      drink_water2 <- drink_water
    } else {
      drink_water2 <- drink_water[grepl(input$ill_year, drink_water$Date),]
    }
    drink_water2 <- na.omit(drink_water2)
    
    leaf_map <- Leaflet$new()
    leaf_map$setView(c(40.7577,-73.9857), 10)
    leaf_map$tileLayer(provider = "Stamen.TonerLite")
    for (i in 1:nrow(drink_water2)) {leaf_map$marker(c(drink_water2$Latitude[i], drink_water2$Longitude[i]), bindPopup = paste("Location: ", drink_water2$Incident.Address[i])) }
    #leaf_map$marker(c(drink_water2$Latitude[1], drink_water2$Longitude[1]), bindPopup = paste("Location: ", drink_water2$Incident.Address[1]))
    leaf_map
    #leaflet(na.omit(drink_water2)) %>% addTiles() %>% addProviderTiles("CartoDB.DarkMatter") %>%setView(lng = -73.9857, lat = 40.7577, zoom = 12) %>% addCircleMarkers(radius=6, fillOpacity = 0.5, popup = paste("Location: ", drink_water2$Incident.Address), clusterOptions = markerClusterOptions())
    #m <- leaflet(na.omit(drink_water2))
    #m <- addProviderTiles(m, "CartoDB.DarkMatter")
    #m <- setView(m, lng = -73.9857, lat = 40.7577, zoom = 12)
    #m <- addCircleMarkers(m, radius=6, fillOpacity = 0.5, popup = paste("Location: ", drink_water2$Incident.Address), clusterOptions = markerClusterOptions())
    #m
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)

  output$descrip_plot <- renderPlot({
    # min.freq = input$freq, max.words=input$max,
    df <- as.data.frame(table(quality_water$Descriptor))
    par(bg="#f5f5f5")
    wordcloud_rep(df$Var1, df$Freq, min.freq = 1, max.words=input$desc_range[1], scale=c(3,1), random.order = TRUE, random.color=TRUE, rot.per=.3,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  output$descrip_text = renderText({
      paste("Viewing ", input$desc_range[1], " descriptors")
  })

  output$sample_text = renderText({
      paste("Graph of ", input$complaint_desc, " Complaints in 2015")
  })

  output$ill_text = renderText({
      paste(input$ill_year, "Cluster Graph of Reported Illness")
  })

  output$sample_plot = renderPlotly({
    x_axis <- list(
      title = "Months in 2015"
    )
    y_axis <- list(
      title = "Turbidity in NTU"
    )
    ay <- list(
      title="Number of Complaints",
      overlaying = "y",
      side = "right"
    )
    
    if(input$complaint_desc == "All"){
      rep_q_water_table2 <- rep_q_water_table
    } else {
      rep_q_water_table2 <- rep_q_water_table[rep_q_water_table$Descriptor== input$complaint_desc,]
    }
    #rep_q_water_table2
    # Total data table
    rep_q_water_table_monthly <- aggregate(rep_q_water_table2$Number, list(rep_q_water_table2$Date), sum)
    rep_q_water_table_monthly$Group.1 <- mapvalues(rep_q_water_table_monthly$Group.1, from = rep_q_water_table_monthly$Group.1, c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec"))  
    
    
    p<- plot_ly(water_qual_Turbid, x =Date, y =Turbidity, name = "Turbidity Level", colors=brewer.pal(3, "BrBG"), text=paste("Turbidity:", Turbidity, " (NTU)")) %>%
    add_trace(rep_q_water_table_monthly, x=rep_q_water_table_monthly$Group.1, y = rep_q_water_table_monthly$x, name = "NYC Resident Complaints", yaxis = "y2", text=paste("Num of complaints:", rep_q_water_table_monthly$x))
    layout(p, xaxis = x_axis, yaxis=y_axis, yaxis2 = ay, paper_bgcolor='rgba(0,0,0,0)', plot_bgcolor='rgba(0,0,0,0)') %>% config(displayModeBar = F)
    
  })  
#################### End of Josh's Output ####################

################### Start Richard's Output###################
output$piechart <- renderPlotly({
#   detach("package:plyr",unload=T)
  watertypedata0 <- calculation(dataclean(waternew,fulllist[as.numeric(input$borough)],fulltime[as.numeric(input$year)]))
  q <- plot_ly(type='pie', values=watertypedata0[,4], labels=watertypedata0[,1])%>%
    layout(paper_bgcolor='rgba(0,0,0,0)', plot_bgcolor='rgba(0,0,0,0)') %>% config(displayModeBar = F)
  q
#   library(plyr)
})

output$heat_text = renderText({
  paste("Heat Graph of All Complaints")
})

output$pie_text = renderText({
  paste("Pie Chart of Borough Complaints")
})

output$baseMap  <- renderMap({
  baseMap <- Leaflet$new()
  baseMap$setView(c(40.7577,-73.9857), 10)
  baseMap$tileLayer(provider = "Stamen.TonerLite")
  baseMap
  
  #leaflet() %>% addTiles() %>% addProviderTiles("Stamen.TonerLite") %>%setView(lat=40.7577,lng=-73.9857, zoom=10)
  
})

output$heatMap <- renderUI({
  
  if (input$mapyear == 3 ){
    watermap <- map3}
  #     
  else if (input$mapyear == 2 ){
    watermap <- map2}
  #       
  else {watermap <- map1}
  
  
  watermap1 <- as.data.table(watermap)
  watermap2 <- watermap1[(Latitude != ""), .(count = .N), by=.(Latitude, Longitude)]
  j <- paste0("[",watermap2[,Latitude], ",", watermap2[,Longitude], ",", watermap2[,count], "]", collapse=",")
  j <- paste0("[",j,"]")
  tags$body(tags$script(HTML(sprintf("
                                var addressPoints = %s
if (typeof heat === typeof undefined) {
            heat = L.heatLayer(addressPoints)
            heat.addTo(map)
          } else {
            heat.setOptions()
            heat.setLatLngs(addressPoints)
          }
                                         </script>"
                                     , j))))
  
  
}) 
################## End Richard's Output##############

################## Start Schinria's Output##############

#final_shiny <- readRDS("../data/final_shiny.rds")

#shiny2_stacked <- readRDS("../data/shiny2_stacked.rds")



output$myChart <- renderChart({
 
  shiny2 = (shiny2_stacked_1)
  p6 <- nPlot(Frequency ~ Borough, group = 'Type', data = shiny2, 
              type = input$type, dom = 'myChart', width = 800)
  p6$chart(color = c('green', 'brown'), stacked = input$stack)
  
  p6$yAxis(tickFormat = "#! function(d) {return d3.format(',.2f')(d)} !#")
  
  p6
})

output$barplot_text = renderText({
  paste("Barplot of Heat/Hot Water Duplicates in ", input$burr)
})

output$barplot2_text = renderText({
  paste("Duplicate v Non-Duplicate Heat/Hot Water Complaints Barplot")
})

output$duplicatePlot <- renderPlot({
  
#   # Render a barplot
#   barplot(final_shiny_1[,input$burr],
#           main=input$borough,
#           col = topo.colors(2),
#           ylab="Number of Duplicate Complaints",
#           xlab="2014 and 2015 Complaints", ylim=c(0,max(final_shiny_1)))
  
  if(input$burr=="Bronx"){
    final_shiny_2 <- bor1
  }
  else if(input$burr=="Brooklyn"){
    final_shiny_2 <- bor2
  }
  else if(input$burr=="Manhattan"){
    final_shiny_2 <- bor3
  }
  else if(input$burr=="Queens"){
    final_shiny_2 <- bor4
  }
  else{
    final_shiny_2 <- bor5
  }
  c <- ggplot(final_shiny_2,aes(x=year,y=duplicates))+
    geom_bar(stat="identity",colour=c(topo.colors(2)[1],topo.colors(2)[2]),fill=c(topo.colors(2)[1],topo.colors(2)[2]))+
    theme(
      panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
      panel.grid.major = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "transparent",colour = NA))+
    xlab("Year")+
    ylab("Number of Duplicate Complaints")+
    scale_y_continuous(limits = c(0, max(final_shiny_1)))+
    ggtitle(paste("Borough",input$burr))
  
  c
},bg="transparent")


################## End Schinria's Output##############


#################### XIAOYU's Output ####################  
output$dygraph <- renderDygraph({
  data <- switch(input$time, 
                 "1" = FIVE[,1],
                 "2" = FIVE[,2],
                 "3" = FIVE[,3],
                 "4" = FIVE[,4],
                 "5" = FIVE[,5],
                 "6" = FIVE[,6])
  
  data2 <- switch(input$compare, 
                  "1" = NULL,
                  "2" = FIVE[,2],
                  "3" = FIVE[,3],
                  "4" = FIVE[,4],
                  "5" = FIVE[,5],
                  "6" = FIVE[,6])
  
  data3 <- cbind(data,data2)
  
  name <- paste(names(data),"    ", names(data2))
  dygraph(data3, main = name) %>% dyRangeSelector()
  
})

output$dygraph2 <- renderDygraph({
  
  data_one <- switch(input$type_2, 
                 "1" = THREE[,1],
                 "2" = THREE[,2],
                 "3" = THREE[,3],
                 "4" = THREE[,4])
  
  data_two <- switch(input$com, 
                  "0" = NULL,
                  "1" = THREE[,1],
                  "2" = THREE[,2],
                  "3" = THREE[,3],
                  "4" = THREE[,4])
  
  data_three <- cbind(data_one,data_two)
  
  name <- paste(names(data_one),"    ", names(data_two))
  
  dygraph(data_three, main = name) %>% dyRangeSelector() 
  
})

output$plot <- renderPlotly({
  
  if (input$status == 1) {
    
    x <- list(
      showticklabels = F
    )
    
    y <- list(
      range = c(-2,12)
    )
    
    plot_ly(C, x = Borough, y = Days, color = Borough, type = "box", boxmean = T) %>% 
      layout(paper_bgcolor = 'rgba(0,0,0,0)',plot_bgcolor = 'rgba(0,0,0,0)', xaxis = x, yaxis = y, title = "Resolution Time by Boroughs") %>% config(displayModeBar = F)
    
  }
  
  else {
    
    x <- list(
      showticklabels = F
    )
    
    z <- list(
      range = c(-5,55)
    )
    
    plot_ly(C, x = Complaint.Type, y = Days, color = Complaint.Type, type = "box", boxmean = T) %>% 
      layout(paper_bgcolor = 'rgba(0,0,0,0)',plot_bgcolor = 'rgba(0,0,0,0)', xaxis = x, yaxis = z, title = "Resolution Time by Boroughs") %>% config(displayModeBar = F)
  } 
  
})


output$view <- renderTable({
  
  summary <- data.frame(tapply(
    A$Days[A$Status == "Closed"], list(A$Complaint.Type[A$Status == "Closed"], 
                                       A$Borough[A$Status == "Closed"]), mean, na.rm=TRUE))
  summary
  
})


output$case2 <- renderPlotly({  
  
  if(input$cases == 1) {
    
    Borough <- data.frame(summary(A$Borough[A$Status == "Open"]))
    value <- c(Borough[,1])
    name <- c(rownames(Borough))
    plot_ly(values = value, labels = c(name),type="pie", showlegend = F) %>% 
      layout(paper_bgcolor = 'rgba(0,0,0,0)',plot_bgcolor = 'rgba(0,0,0,0)',title = "Open cases by Borough") %>% config(displayModeBar = F)
    
  }
  else {
    
    Borough <- data.frame(summary(A$Borough[A$Status == "Closed"]))
    value <- c(Borough[,1])
    name <- c(rownames(Borough))
    plot_ly(values = value, labels = c(name),type="pie", showlegend = F) %>% 
      layout(paper_bgcolor = 'rgba(0,0,0,0)',plot_bgcolor = 'rgba(0,0,0,0)', title = "Closed cases by Borough") %>% config(displayModeBar = F)
    
  } 
  
})

output$case3 <- renderPlotly({  
  
  if(input$cases == 1) {
    
    Type <- data.frame(summary(A$Complaint.Type[A$Status == "Open"]))
    value <- c(Type[,1])
    name <- c(rownames(Type))
    plot_ly(values = value, labels = c(name),type="pie", showlegend = F) %>% 
      layout(paper_bgcolor = 'rgba(0,0,0,0)',plot_bgcolor = 'rgba(0,0,0,0)',title = "Open cases by Complaint.Type") %>% config(displayModeBar = F)
    
  }
  else{
    
    Type <- data.frame(summary(A$Complaint.Type[A$Status == "Closed"]))
    value <- c(Type[,1])
    name <- c(rownames(Type))
    plot_ly(values = value, labels = c(name),type="pie" ,showlegend = F) %>% 
      layout(paper_bgcolor = 'rgba(0,0,0,0)',plot_bgcolor = 'rgba(0,0,0,0)',title = "Closed cases by Complaint.Type") %>% config(displayModeBar = F)
    
  } 
  
})

#################### End of XIAOYU's Output ####################

})