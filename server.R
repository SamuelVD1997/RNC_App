library(shinydashboard)
library(leaflet)
library(knitr)
library(dplyr)
library(tidyr)
library(summarytools)
library(plotly)
library(readxl)
library(dygraphs)
library(xts)
library(lubridate)
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(knitr)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(caret)
library(tidyverse)
library(rpart)
library(FactoMineR)
library(cattonum)
library(ggfortify)
library(ggpubr)
library(factoextra)
library(dendextend)
library(readxl)
library(NbClust)
library(ggdendro)
library(summarytools)
library(dygraphs)
library(xts)
library(ggplot2)
library(dplyr)
library(readxl)
library(knitr)
library(kableExtra)
library(tidyverse)
library(rpart)
library(FactoMineR)
library(cattonum)
library(ggfortify)
library(ggpubr)
library(factoextra)
library(dendextend)
library(NbClust)
library(ggdendro)
library(summarytools)
library(dygraphs)
library(xts)
library(tidytext) 
library(topicmodels) 
library(tidyr) 
library(ggrepel) 
library(gridExtra)
library(formattable)
library(tm) 
library(plotly) 
library(wordcloud2)
library(cleanNLP)
library(ldatuning)
library(SnowballC)
library(data.table)
library(cleanNLP)
library(shiny)
library(shinydashboard)

Previous_Button=tags$div(actionButton("Prev_Tab",HTML('<div class="col-sm-4"><i class="fa fa-angle-double-left fa-2x"></i></div>
                                                                  ')))
Next_Button=div(actionButton("Next_Tab",HTML('<div class="col-sm-4"><i class="fa fa-angle-double-right fa-2x"></i></div>')))
# -----------------------Required Functions-----------------------

#define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

#customize ggplot2's default theme settings
#this tutorial doesn't actually pass any parameters, but you June use it again in future tutorials so it's nice to have the options
theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #center the title
        axis.ticks = aticks, #set axis ticks to on or off
        panel.grid.minor = pgminor, #turn on or off the minor grid lines
        legend.title = lt, #turn on or off the legend title
        legend.position = lp) #turn on or off the legend
}

#customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}

word_chart <- function(data, input, title) {
  data %>%
    #set y = 1 to just plot one variable and use word as the label
    ggplot(aes(as.factor(row), 1, label = input, fill = factor(topic) )) +
    #you want the words, not the points
    geom_point(color = "transparent") +
    #make sure the labels don't overlap
    geom_label_repel(nudge_x = .2,  
                     direction = "y",
                     box.padding = 0.1,
                     segment.color = "transparent",
                     size = 3) +
    facet_grid(~topic) +
    theme_lyrics() +
    theme(text = element_text(size=20),axis.text.y = element_blank(), axis.text.x = element_blank(),
          #axis.title.x = element_text(size = 9),
          panel.grid = element_blank(), panel.background = element_blank(),
          panel.border = element_rect("lightgray", fill = NA),
          strip.text.x = element_text(size = 9)) +
    labs(x = NULL, y = NULL, title = title) +
    #xlab(NULL) + ylab(NULL) +
    #ggtitle(title) +
    coord_flip()
}

#create function that accepts the lda model and num word to display
top_terms_per_topic <- function(lda_model, num_words) {
  
  #tidy LDA object to get word, topic, and probability (beta)
  topics_tidy <- tidy(lda_model, matrix = "beta")
  
  
  top_terms <- topics_tidy %>%
    group_by(topic) %>%
    arrange(topic, desc(beta)) %>%
    #get the top num_words PER topic
    slice(seq_len(num_words)) %>%
    arrange(topic, beta) %>%
    #row is required for the word_chart() function
    mutate(row = row_number()) %>%
    ungroup() %>%
    #add the word Topic to the topic labels
    mutate(topic = paste("Topic", topic, sep = " "))
  #create a title to pass to word_chart
  title <- paste("LDA Top Terms for selected number of Topics")
  #call the word_chart function you built in prep work
  word_chart(top_terms, top_terms$term, title)
}
#----------------------------------------------------------------------

options(digits=4)
Master <- read_excel("Raw_Data.xlsx", 
                     sheet = "Data")

TimeSeries <- read_excel("TimeSeries.xlsx")
RNC <- Master

Raw_Data <- read_excel("RNC_Record_Marzo.xlsx",
                       sheet = "Today")

Hist <- read_excel("Historical.xlsx")

TopIssue <- read_excel("May.xlsx",
                       sheet = "Old Data")

TS <- data.frame(seq.Date(as.Date("2021-01-01"), as.Date(today()), by = "day")) %>%
  setNames(c("Date"))

HoursSite <- read_excel("HoursSite.xlsx")

SupervisedL <- read.csv("./ML/SupervisedL.csv")

library(readxl)
RNCDay <- read_excel("Encms.xlsx")
RNCDay$RNC <- paste0(RNCDay$`NCR Number`, '-', RNCDay$`Discrepancy Number`)


if (format(Sys.Date(),"%A") == "Monday") {
  Fecha <- format(Sys.Date()-3,"%Y-%m-%d")
} else {
  Fecha <- as.Date(format(Sys.Date()-1,"%Y-%m-%d"))
}

RNCDay$`Discrepancy Creation Date` <- as.Date(RNCDay$`Discrepancy Creation Date`)
RNCDay <- filter(RNCDay,`Discrepancy Creation Date` == Fecha)
RNCDay$`Discrepancy Creation Date` <-as.Date(format(RNCDay$`Discrepancy Creation Date`,"%d-%m-%Y"))


RNCPU1 <- filter(RNCDay,`Plant Code` == 'Q4')

for (i in 1:nrow(RNCPU1)){
  
  RNCPU1$Plant[i] <- "PU1" 
  
}

RNCPU1 <- RNCPU1  %>% select(
  Plant,
  `RNC`,
  `NCR Type`,
  `Discrepancy Creation Date`,
  `Work Center`,
  `Aircraft Number`,
  `Part Number Affected`,
  `Part Description`,
  `Preliminary Cause Code`
)


RNCPU10 <- filter(RNCDay,`Plant Code` == 'Q1')

for (i in 1:nrow(RNCPU10)){
  
  RNCPU10$Plant[i] <- "PU10" 
  
}

RNCPU10 <- RNCPU10  %>% select(
  Plant,
  `RNC`,
  `NCR Type`,
  `Discrepancy Creation Date`,
  `Work Center`,
  `Aircraft Number`,
  `Part Number Affected`,
  `Part Description`,
  `Preliminary Cause Code`
)

RNCDay$`Work Center` <- substr(RNCDay$`Work Center`, 1, 6)

library(readr)
PU11WC <- read_csv("WC.csv")

RNCDay$`Discrepancy Text`[is.na(RNCDay$`Discrepancy Text`)] <- "UNDEFINED IF METHODS"


RNCPU11 <- filter(RNCDay, `Work Center` %in% PU11WC$`Work Center`)

for (i in 1:nrow(RNCPU11)){
  
  if (str_detect(RNCPU11$`Discrepancy Text`[i], 'METH|DESIGN') == TRUE){
    
    RNCPU11$`Preliminary Cause Code`[i] <- 'METHODS'
    
  }
  
  RNCPU11$Plant[i] <- "PU11" 
  
}

RNCPU11 <- inner_join(RNCPU11,PU11WC, by = 'Work Center')

RNCPU11 <- RNCPU11 %>% select(
  Plant,
  `Zone`,
  `RNC`,
  `NCR Type`,
  `Discrepancy Creation Date`,
  `Work Center`,
  `Aircraft Number`,
  `Part Number Affected`,
  `Part Description`,
  `Preliminary Cause Code`
)

RNCPU2 <- filter(RNCDay, !(`Work Center` %in% PU11WC$`Work Center`) & `Plant Code` == 'Q3')

for (i in 1:nrow(RNCPU2)){
  
  if (str_detect(RNCPU2$`Discrepancy Text`[i], 'METH|DESIGN') == TRUE){
    
    RNCPU2$`Preliminary Cause Code`[i] <- 'METHODS'
    
  }
  
  RNCPU2$Plant[i] <- "PU2" 
  
}

RNCPU2 <- RNCPU2 %>% select(
  Plant,
  `RNC`,
  `NCR Type`,
  `Discrepancy Creation Date`,
  `Work Center`,
  `Aircraft Number`,
  `Part Number Affected`,
  `Part Description`,
  `Preliminary Cause Code`
)





# Write the first data set in a new workbook


m <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)

RNC_TypeMeth <- Hist %>% filter(`Preliminary Cause Code` == "METHODS") %>% group_by(Plant) %>% summarize(count = n())
RNC_TypeTech <- Hist %>% filter(`Preliminary Cause Code` == "TECHNICAL (NOT METH)") %>% group_by(Plant) %>% summarize(count = n())
RNC_TypeHum <- Hist %>% filter(`Preliminary Cause Code` == "WORKMANSHIP") %>% group_by(Plant) %>% summarize(count = n())
RNC_Type <- data.frame(RNC_TypeMeth, RNC_TypeTech$count, RNC_TypeHum$count) %>% setNames(c("Plant", "Methods", "Tech","Workmanship"))

TS11 <- Hist %>% filter(Plant == "PU11" & `Preliminary Cause Code` == "METHODS")
TS11 <- as.data.frame(table(TS11$`Creation Date`)) %>%
  setNames(c("Date", "Quantity"))
TS11$Date <- as.Date(TS11$Date)
TS11 <- merge(TS, TS11, by = "Date", all.x = TRUE ) %>% replace_na(list(Quantity = 0))
TS11$Date <- as.Date(TS11$Date)

TS2 <- Hist%>% filter(Plant == "PU2" & `Preliminary Cause Code` == "METHODS")
TS2 <- as.data.frame(table(TS2$`Creation Date`)) %>%
  setNames(c("Date", "Quantity"))
TS2$Date <- as.Date(TS2$Date)
TS2 <- merge(TS, TS2, by = "Date", all.x = TRUE ) %>% replace_na(list(Quantity = 0))
TS2$Date <- as.Date(TS2$Date)

TS1 <- Hist%>% filter(Plant == "PU1" & `Preliminary Cause Code` != "WORKMANSHIP")
TS1 <- as.data.frame(table(TS1$`Creation Date`)) %>%
  setNames(c("Date", "Quantity"))
TS1$Date <- as.Date(TS1$Date)
TS1 <- merge(TS, TS1, by = "Date", all.x = TRUE ) %>% replace_na(list(Quantity = 0))
TS1$Date <- as.Date(TS1$Date)

TS10 <- Hist%>% filter(Plant == "PU10" & `Preliminary Cause Code` != "WORKMANSHIP")
TS10 <- as.data.frame(table(TS10$`Creation Date`)) %>%
  setNames(c("Date", "Quantity"))
TS10$Date <- as.Date(TS10$Date)
TS10 <- merge(TS, TS10, by = "Date", all.x = TRUE ) %>% replace_na(list(Quantity = 0))
TS10$Date <- as.Date(TS10$Date)

Raw_Data  <- Raw_Data[c(1,3:10)]


issues_w_Act <- TopIssue %>% filter(Action == "YES") %>% group_by(Number) %>% summarize(count = n()) %>% top_n(n=10, count) %>% 
  mutate(Number = reorder(Number,count))

issues_w_Act <- inner_join(issues_w_Act,TopIssue, by = "Number") %>% select("Number","count","ECD_or_AC") %>% unique() %>%
  mutate(Number = reorder(Number,count))

server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  
  
  showModal(modalDialog(
    title = "Welcome to the new Methods RNCs Dashboard!",
    "Using this new tool you can have a better perspective of our everyday RNCs",
    "and therefore a better understanding of how they behave!"
  ))
  
  
  SiteTotal = nrow(filter(RNCPU11,`Preliminary Cause Code` == 'METHODS')) + nrow(filter(RNCPU10,`Preliminary Cause Code` == 'TECHNICAL')) + nrow(filter(RNCPU1,`Preliminary Cause Code` == 'TECHNICAL')) + nrow(filter(RNCPU2,`Preliminary Cause Code` == 'METHODS'))
  
  output$YesterdayRNCs <- renderValueBox({
    valueBox(
      paste0(SiteTotal), "RNCs Yesterday", icon = icon("list"),
      color = "purple"
    )
  })
  
  HoursSite$Hours <- as.numeric(HoursSite$Hours)
  options(digits=4)
  output$RNC1000 <- renderValueBox({
    valueBox(
      paste0(signif(SiteTotal/sum(HoursSite$Hours) * 1000, 3)), "RNCs per 1,000 hr", icon = icon("hourglass"),
      color = "orange"
    )
  })
  output$RepIssues <- renderValueBox({
    valueBox(
      paste0(nrow(filter(SupervisedL, Repetitive == "Yes"))), "Repetitive RNCs Identified", icon = icon("repeat"),
      color = "green"
    )
  })
  
  output$OneoffIssues <- renderValueBox({
    valueBox(
      paste0(nrow(filter(SupervisedL, Repetitive == "No"))), "One-Off RNCs Identified", icon = icon("refresh"),
      color = "red"
    )
  })
  
  output$TotalCost <- renderValueBox({
    valueBox(
      paste0(prettyNum(SiteTotal*500,big.mark = ",")," USD"), "Total Cost", icon = icon("dollar"),
      color = "teal"
    )
  })
  
  
  if (format(Sys.Date(),"%A") == "Monday") {
    Dis <- 0
  } else if (format(Sys.Date(),"%A") == "Tuesday"){
    Dis <- 1
  } else if (format(Sys.Date(),"%A") == "Wednesday"){
    Dis <- 2
  } else if (format(Sys.Date(),"%A") == "Thursday"){
    Dis <- 3
  } else if (format(Sys.Date(),"%A") == "Friday"){
    Dis <- 4
  }
  else if (format(Sys.Date(),"%A") == "Saturday"){
    Dis <- 5
  }
  else if (format(Sys.Date(),"%A") == "Sunday"){
    Dis <- 6
  }
  
  Daysofweek <-seq(Sys.Date()-Dis, Sys.Date(), by = "days")
  
  
  Week11 <- filter(TS11, Date %in% Daysofweek)
  Week2 <- filter(TS2, Date %in% Daysofweek)
  Week1 <- filter(TS1, Date %in% Daysofweek)
  Week10 <- filter(TS10, Date %in% Daysofweek)
  
  TotalWeek <-  sum(Week11$Quantity) + sum(Week2$Quantity) + sum(Week1$Quantity) + sum(Week10$Quantity)  
  
  TotalWeek
  
  output$TotalRNCs <- renderValueBox({
    valueBox(
      paste0(TotalWeek), "Total RNCs So Far This Week", icon = icon("remove"),
      color = "fuchsia"
    )
  })
  
  
  
  output$Total_RNCs <- renderValueBox({
    valueBox(
      paste0(334), "RNCs, Last Month", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$Total_Cost <- renderValueBox({
    valueBox(
      paste0("$ 156,898.68"), "Total Cost", icon = icon("dollar"),
      color = "teal"
    )
  })
  
  output$Total_Identified <- renderValueBox({
    valueBox(
      paste0(55), "Diferent Issues with Actions", icon = icon("repeat"),
      color = "green"
    )
  })
  
  contentPU11 <- paste(sep = "<br/>",
                       "<b>PU11</b>",
                       paste0(nrow(filter(RNCPU11, `Preliminary Cause Code` == 'WORKMANSHIP'))," Human RNCs"),
                       paste0(nrow(filter(RNCPU11, `Preliminary Cause Code` == 'TECHNICAL'))," Technical RNCs"),
                       paste0(nrow(filter(RNCPU11, `Preliminary Cause Code` == 'METHODS'))," Methods RNCs")
  )
  
  contentPU2 <- paste(sep = "<br/>",
                      "<b>PU2</b>",
                      paste0(nrow(filter(RNCPU2, `Preliminary Cause Code` == 'WORKMANSHIP'))," Human RNCs"),
                      paste0(nrow(filter(RNCPU2, `Preliminary Cause Code` == 'TECHNICAL'))," Technical RNCs"),
                      paste0(nrow(filter(RNCPU2, `Preliminary Cause Code` == 'METHODS'))," Methods RNCs")
                      
  )
  
  contentComposites <- paste(sep = "<br/>",
                             "<b>PU10</b>",
                             paste0(nrow(filter(RNCPU10, `Preliminary Cause Code` == 'WORKMANSHIP'))," Human RNCs"),
                             paste0(nrow(filter(RNCPU10, `Preliminary Cause Code` == 'TECHNICAL'))," Technical RNCs")
  )
  
  
  contentPU1 <- paste(sep = "<br/>",
                      "<b>PU1</b>",
                      paste0(nrow(filter(RNCPU1, `Preliminary Cause Code` == 'WORKMANSHIP'))," Human RNCs"),
                      paste0(nrow(filter(RNCPU1, `Preliminary Cause Code` == 'TECHNICAL'))," Technical RNCs")
  )
  
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     minZoom = 17.4, maxZoom = 17.4)) %>% setView(lng = -100.17436389066349, lat = 20.621092741677714, zoom = 17.4) %>% addTiles() %>%
      addPopups(lng=-100.175729, lat=20.621732, contentPU11,
                options = popupOptions(closeButton = TRUE)
      ) %>%
      addPopups(lng=-100.175236, lat=20.620251, contentPU2,
                options = popupOptions(closeButton = TRUE)
      ) %>%
      addPopups(lng=-100.173470, lat=20.620735, contentComposites,
                options = popupOptions(closeButton = TRUE)
      )
  })
  
  output$mymap2 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     minZoom = 17.4, maxZoom = 17.4)) %>% setView(lng = -100.25249196584566, lat = 20.563679490789358, zoom = 17.4) %>% addTiles() %>%
      addPopups(lng=-100.2529, lat=20.563679490789358, contentPU1,
                options = popupOptions(closeButton = TRUE)
      ) 
  })
  
  
  
  
  output$plot_RNC_volume <- renderPlotly({
    
    plot_ly(x = ~TS11$Date, y = ~ TS11$Quantity, name = 'PU11', type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~TS2$Date, y = ~TS2$Quantity, name = 'PU2', mode = 'lines')  %>%
      add_trace(x = ~TS1$Date, y = ~TS1$Quantity, name = 'PU1', mode = 'lines') %>%
      add_trace(x = ~TS10$Date, y = ~TS10$Quantity, name = 'PU10', mode = 'lines') %>%
      config(displayModeBar = FALSE) %>%
      layout(
        
        xaxis = list(title = "Date",
                     rangeselector = list(
                       buttons = list(
                         list(
                           count = 7,
                           label = "Week",
                           step = "day",
                           stepmode = "backward"),
                         list(
                           count = 1,
                           label = "Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 3,
                           label = "3 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 1,
                           label = "Year",
                           step = "year",
                           stepmode = "todate"),
                         list(step = "all"))),
                     
                     rangeslider = list(type = "Date")),
        
        yaxis = list(title = "RNC"))
    
  })
  
  output$plot_RNC1_PU11 <- renderPlotly({
    
    RNCPU11 %>% filter(`Preliminary Cause Code` == 'METHODS') %>% group_by(Zone) %>% summarize(count = n()) %>% plot_ly(labels = ~Zone, values = ~count) %>%
      add_pie(hole = 0.6) %>% layout(title = "Zones",  showlegend = F,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  output$plot_RNC2_PU11 <- renderPlotly({
    
    RNCPU11 %>% filter(`Preliminary Cause Code` == 'METHODS') %>% group_by(`NCR Type`) %>% summarize(count = n()) %>% plot_ly(labels = ~`NCR Type`, values = ~count) %>%
      add_pie(hole = 0.6) %>% layout(title = "NCR Type",  showlegend = F,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  output$plot_RNC3_PU11 <- renderPlotly({
    
    RNCPU11 %>% filter(`Preliminary Cause Code` == 'METHODS') %>% group_by(`Work Center`) %>% summarize(count = n()) %>% plot_ly(labels = ~`Work Center`, values = ~count) %>%
      add_pie(hole = 0.6) %>% layout(title = "WC",  showlegend = F,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  output$plot_RNC4_PU11 <- renderPlotly({
    
    RNCPU11 %>% filter(`Preliminary Cause Code` == 'METHODS') %>% group_by(`Part Description`) %>% summarize(count = n()) %>% plot_ly(labels = ~`Part Description`, values = ~count) %>%
      add_pie(hole = 0.6) %>% layout(title = "Part",  showlegend = F,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  
  
  #----------------------------------------------  
  
  
  output$plot_RNC2_PU1 <- renderPlotly({
    
    RNCPU1 %>% filter(`Preliminary Cause Code` == 'TECHNICAL') %>% group_by(`NCR Type`) %>% summarize(count = n()) %>% plot_ly(labels = ~`NCR Type`, values = ~count) %>%
      add_pie(hole = 0.6) %>% layout(title = "NCR Type",  showlegend = F,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  output$plot_RNC3_PU1 <- renderPlotly({
    
    RNCPU1 %>% filter(`Preliminary Cause Code` == 'TECHNICAL') %>% group_by(`Work Center`) %>% summarize(count = n()) %>% plot_ly(labels = ~`Work Center`, values = ~count) %>%
      add_pie(hole = 0.6) %>% layout(title = "WC",  showlegend = F,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  output$plot_RNC4_PU1 <- renderPlotly({
    
    RNCPU1 %>% filter(`Preliminary Cause Code` == 'TECHNICAL') %>% group_by(`Part Description`) %>% summarize(count = n()) %>% plot_ly(labels = ~`Part Description`, values = ~count) %>%
      add_pie(hole = 0.6) %>% layout(title = "Part",  showlegend = F,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  #------------------------------------------
  
  output$plot_RNC2_PU10 <- renderPlotly({
    
    RNCPU10 %>% filter(`Preliminary Cause Code` == 'TECHNICAL') %>% group_by(`NCR Type`) %>% summarize(count = n()) %>% plot_ly(labels = ~`NCR Type`, values = ~count) %>%
      add_pie(hole = 0.6) %>% layout(title = "NCR Type",  showlegend = F,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  
  output$plot_RNC3_PU10 <- renderPlotly({
    
    RNCPU10 %>% filter(`Preliminary Cause Code` == 'TECHNICAL') %>% group_by(`Work Center`) %>% summarize(count = n()) %>% plot_ly(labels = ~`Work Center`, values = ~count) %>%
      add_pie(hole = 0.6) %>% layout(title = "WC",  showlegend = F,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  output$plot_RNC4_PU10 <- renderPlotly({
    
    RNCPU10 %>% filter(`Preliminary Cause Code` == 'TECHNICAL') %>% group_by(`Part Description`) %>% summarize(count = n()) %>% plot_ly(labels = ~`Part Description`, values = ~count) %>%
      add_pie(hole = 0.6) %>% layout(title = "Part",  showlegend = F,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  #--------------------------------
  output$plot_RNC2_PU2 <- renderPlotly({
    
    RNCPU2 %>% filter(`Preliminary Cause Code` == 'METHODS') %>% group_by(`NCR Type`) %>% summarize(count = n()) %>% plot_ly(labels = ~`NCR Type`, values = ~count) %>%
      add_pie(hole = 0.6) %>% layout(title = "NCR Type",  showlegend = F,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  output$plot_RNC3_PU2 <- renderPlotly({
    
    RNCPU2 %>% filter(`Preliminary Cause Code` == 'METHODS') %>% group_by(`Work Center`) %>% summarize(count = n()) %>% plot_ly(labels = ~`Work Center`, values = ~count) %>%
      add_pie(hole = 0.6) %>% layout(title = "WC",  showlegend = F,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  output$plot_RNC4_PU2 <- renderPlotly({
    
    RNCPU2 %>% filter(`Preliminary Cause Code` == 'METHODS') %>% group_by(`Part Description`) %>% summarize(count = n()) %>% plot_ly(labels = ~`Part Description`, values = ~count) %>%
      add_pie(hole = 0.6) %>% layout(title = "Part",  showlegend = F,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  
  
  
  
  
  #-----------------------------------
  
  
  output$DTRNCPU11 <- DT::renderDT({
    
    
    DisplayTPU11 <- RNCPU11 %>% filter( `Preliminary Cause Code` == 'METHODS') %>% inner_join(SupervisedL, by = "RNC")
    
    
    DT::datatable(DisplayTPU11[-11], class = 'cell-border stripe', rownames = F, filter = 'top',
                  editable = TRUE, extensions = 'Buttons', options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf','print'),
                    pageLength = 10
                  ))
    
  })
  
  output$DTRNCPU2 <- DT::renderDT({
    
    DT::datatable(filter(RNCPU2, `Preliminary Cause Code` == 'METHODS'), class = 'cell-border stripe', rownames = F, filter = 'top',
                  editable = TRUE, extensions = 'Buttons', options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf','print'),
                    pageLength = 10
                  ))
    
  })
  
  output$DTRNCPU1 <- DT::renderDT({
    
    DT::datatable(filter(RNCPU1, `Preliminary Cause Code` == 'TECHNICAL'), class = 'cell-border stripe', rownames = F, filter = 'top',
                  editable = TRUE, extensions = 'Buttons', options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf','print'),
                    pageLength = 10
                  ))
    
  })
  
  
  output$DTRNCPU10 <- DT::renderDT({
    
    DT::datatable(filter(RNCPU10, `Preliminary Cause Code` == 'TECHNICAL'), class = 'cell-border stripe', rownames = F, filter = 'top',
                  editable = TRUE, extensions = 'Buttons', options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf','print'),
                    pageLength = 10
                  ))
    
  })
  #--------------------
  
  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
  
  output$plot_Actions_PU11 <- renderPlotly({
    
    
    TopIssue %>% group_by(Action) %>% summarize(count = n()) %>% plot_ly(labels = ~Action, values = ~count, type = 'pie',
                                                                         textinfo = 'label+percent',
                                                                         insidetextfont = list(color = '#FFFFFF'),
                                                                         hoverinfo = 'text',
                                                                         text = ~paste(count, ' RNCs'),
                                                                         #The 'pull' attribute can also be used to create space between the sectors
                                                                         showlegend = FALSE) %>% 
      layout(title = 'RNCs with Actions',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      
      layout(margin = m)
    
  })
  
  
  
  
  output$plot_Fase_PU11_Yes <- renderPlotly({
    
    TopIssue %>% filter(Action == "YES") %>% group_by(Phase) %>% summarize(count = n())  %>% plot_ly(labels = ~Phase, values = ~count, type = 'pie',
                                                                                                     textinfo = 'label+percent',
                                                                                                     insidetextfont = list(color = '#FFFFFF'),
                                                                                                     hoverinfo = 'text',
                                                                                                     text = ~paste(count, ' RNCs'),
                                                                                                     #The 'pull' attribute can also be used to create space between the sectors
                                                                                                     showlegend = FALSE) %>% 
      layout(title = 'RNCs With Action per Phase',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      
      layout(margin = m)
    
  })  
  
  output$plot_Fase_PU11_No <- renderPlotly({
    
    TopIssue %>% filter(Action == "NO") %>% group_by(Phase) %>% summarize(count = n()) %>% plot_ly(labels = ~Phase, values = ~count, texttemplate = "%{value} <br>(%{percent})",
                                                                                                   hoverinfo = 'label+percent') %>%
      add_pie(hole = 0.6) %>% layout(title = "RNCs Without Action per Phase",  showlegend = T,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
      layout(margin = m)
    
  }) 
  
  
  output$plot_Fase_PU11_No <- renderPlotly({
    
    TopIssue %>% filter(Action == "NO") %>% group_by(Phase) %>% summarize(count = n()) %>% plot_ly(labels = ~Phase, values = ~count, type = 'pie',
                                                                                                   textinfo = 'label+percent',
                                                                                                   insidetextfont = list(color = '#FFFFFF'),
                                                                                                   hoverinfo = 'text',
                                                                                                   text = ~paste(count, ' RNCs'),
                                                                                                   #The 'pull' attribute can also be used to create space between the sectors
                                                                                                   showlegend = FALSE) %>% 
      layout(title = 'RNCs without Action per Phase',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      
      layout(margin = m)
    
  }) 
  
  output$plot_Rep_PU11 <- renderPlotly({
    
    TopIssue %>% group_by(COS) %>% summarize(count = n()) %>% plot_ly(labels = ~COS, values = ~count, type = 'pie',
                                                                      textinfo = 'label+percent',
                                                                      insidetextfont = list(color = '#FFFFFF'),
                                                                      hoverinfo = 'text',
                                                                      text = ~paste(count, ' RNCs'),
                                                                      #The 'pull' attribute can also be used to create space between the sectors
                                                                      showlegend = FALSE) %>% 
      layout(title = 'RNCs Per Type of Action',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      
      layout(margin = m)
    
  })      
  
  output$plot_RNC_per_Type <- renderPlotly({
    
    plot_ly(RNC_Type, x = ~Plant, y = ~Methods, type = 'bar', name = "Methods") %>% 
      add_trace(y = ~Tech, name = 'Tech. Not Meth')%>% 
      add_trace(y = ~Workmanship, name = 'Workmanship') %>% 
      layout(yaxis = list(title = 'Count'), barmode = 'stack')
    
  })
  
  output$plot_top_issues <- renderPlotly({
    
    plot_ly(issues_w_Act, x = ~Number, y = ~count, type = 'bar', 
            text = issues_w_Act$ECD_or_AC,
            hovertemplate = "ECD or A/C:  %{text}")
    
    
  })
  
  
  
  
  ### Analysis
  
  ##General
  year.month.r <- seq.Date(as.Date("2019-01-01"), as.Date("2021-06-01"), by = "month")
  Total <- data.frame(year.month.r, TimeSeries$`Total RNC`)
  Total <- xts(Total$TimeSeries..Total.RNC.,Total$year.month.r)
  Meth <- data.frame(year.month.r, TimeSeries$`Methods RNC`)
  Meth <- xts(Meth$TimeSeries..Methods.RNC.,Meth$year.month.r)
  data_ts <- cbind(Total, Meth)
  data_ts <- data.frame(date=index(data_ts), coredata(data_ts))
  
  output$TS <- renderPlotly({
    
    
    plot_ly(x = ~data_ts$date, y = ~ data_ts$Meth, name = 'Methods', type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~data_ts$date, y = ~data_ts$Total, name = 'Total', mode = 'lines')  %>%
      config(displayModeBar = FALSE) %>%
      layout(
        
        xaxis = list(title = "Date",
                     rangeselector = list(
                       buttons = list(
                         list(
                           count = 3,
                           label = "3 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 6,
                           label = "6 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 12,
                           label = "Year",
                           step = "month",
                           stepmode = "todate"),
                         list(step = "all"))),
                     
                     rangeslider = list(type = "Date")),
        
        yaxis = list(title = "RNC"))
  })
  
  output$ZonesG <- renderPlotly({
    
    Zones = as.data.frame(table(RNC$ZONE))
    Zones = setNames(Zones,c('Zone','Quantity')) %>% mutate(Zone = reorder(Zone,Quantity))
    plot_ly(Zones, x = ~Zone, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Zones", margin = m)
  })
  output$NCRTypeG <- renderPlotly({
    
    Type = as.data.frame(table(RNC[3]))
    Type = setNames(Type,c('Type','Quantity')) %>% mutate( Type = reorder(Type,Quantity))
    plot_ly(Type, x = ~Type, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNC Type", margin = m)
  })
  
  output$WCG <- renderPlotly({
    WC = as.data.frame(table(RNC[6]))
    WC = setNames(WC,c('WC','Quantity')) %>% mutate( WC = reorder(WC,Quantity))
    plot_ly(WC, x = ~WC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Work Centers", margin = m)
  })
  
  output$PartsG <- renderPlotly({
    Part = as.data.frame(table(RNC[14]))
    Part = setNames(Part,c('Part','Quantity'))
    Part <- Part %>% top_n(n=10, Quantity) %>% mutate( Part = reorder(Part,Quantity))
    plot_ly(Part, x = ~Part, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected Parts", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$ACG <- renderPlotly({
    AC = as.data.frame(table(RNC[8]))
    AC = setNames(AC,c('AC','Quantity')) %>% mutate( AC = reorder(AC,Quantity))
    plot_ly(AC, x = ~AC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected AC", margin = m)
  })
  
  output$QAG <- renderPlotly({
    QA = as.data.frame(table(RNC[12]))
    QA = setNames(QA,c('QA','Quantity')) %>% mutate( QA = reorder(QA,Quantity))
    plot_ly(QA, x = ~QA, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Quality Agent", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$DefectG <- renderPlotly({
    Defect = as.data.frame(table(RNC[17]))
    Defect = setNames(Defect,c('Defect','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( Defect = reorder(Defect,Quantity))
    plot_ly(Defect, x = ~Defect, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Defect", margin = m)
  })
  
  output$DayG <- renderPlotly({
    Day = as.data.frame(table(RNC[29]))
    Day = setNames(Day,c('Day','Quantity')) %>% mutate( Day = reorder(Day,Quantity))
    plot_ly(Day, x = ~Day, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Day", margin = m)
  })
  
  output$WBG <- renderPlotly({
    WB = as.data.frame(table(RNC[25]))
    WB = setNames(WB,c('WB','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( WB = reorder(WB,Quantity))
    plot_ly(WB, x = ~WB, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Workbook", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$SkillG <- renderPlotly({
    Skill = as.data.frame(table(RNC[30]))
    Skill = setNames(Skill,c('Skill','Quantity')) %>% mutate( Skill = reorder(Skill,Quantity)) 
    plot_ly(Skill, x = ~Skill, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Skill", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$WorkerG <- renderPlotly({
    Worker = as.data.frame(table(RNC[28]))
    Worker = setNames(Worker,c('Worker','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( Worker = reorder(Worker,Quantity))
    plot_ly(Worker, x = ~Worker, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Worker", margin = m)
  })
  output$MethG <- renderPlotly({
    
    MethC = as.data.frame(table(RNC[26]))
    MethC = setNames(MethC,c('MethC','Quantity')) %>% top_n(n=10, Quantity) %>% mutate(MethC = reorder(MethC,Quantity))
    plot_ly(MethC, x = ~MethC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Meth. Conf. Agent", margin = m)
  })
  
  output$DispG <- renderPlotly({
    
    Disp = as.data.frame(table(RNC[20]))
    Disp = setNames(Disp,c('Disp','Quantity')) %>% mutate( Disp = reorder(Disp,Quantity))  
    plot_ly(Disp, x = ~Disp, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Disposition Type", margin = m)
  })
  
  TimeSeries1 <- read_excel("Raw_Data.xlsx", 
                            sheet = "TSZ1")
  
  year.month.r <- seq.Date(as.Date("2019-01-01"), as.Date("2021-06-01"), by = "month")
  Total <- data.frame(year.month.r, TimeSeries1$`Total RNC`)
  Total <- xts(Total$TimeSeries1..Total.RNC.,Total$year.month.r)
  Meth <- data.frame(year.month.r, TimeSeries1$`Methods RNC`)
  Meth <- xts(Meth$TimeSeries1..Methods.RNC.,Meth$year.month.r)
  data_ts1 <- cbind(Total, Meth)
  data_ts1 <- data.frame(date=index(data_ts1), coredata(data_ts1))
  
  
  ## A Zone 1
  
  
  output$TS1 <- renderPlotly({
    
    plot_ly(x = ~data_ts1$date, y = ~ data_ts1$Meth, name = 'Methods', type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~data_ts1$date, y = ~data_ts1$Total, name = 'Total', mode = 'lines')  %>%
      config(displayModeBar = FALSE) %>%
      layout(
        
        xaxis = list(title = "Date",
                     rangeselector = list(
                       buttons = list(
                         list(
                           count = 3,
                           label = "3 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 6,
                           label = "6 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 12,
                           label = "Year",
                           step = "month",
                           stepmode = "todate"),
                         list(step = "all"))),
                     
                     rangeslider = list(type = "Date")),
        
        yaxis = list(title = "RNC"))
  })
  
  RNC1  <- filter(RNC, ZONE == "Zone 1")
  
  output$WC1 <- renderPlotly({
    
    WC_1 = as.data.frame(table(RNC1[6]))
    WC_1 = setNames(WC_1,c('WC','Quantity'))
    WC_1  %>% mutate( WC = reorder(WC,Quantity)) 
    plot_ly(WC_1, x = ~WC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Work Center", margin = m)
  })
  
  output$AC1 <- renderPlotly({
    AC1 = as.data.frame(table(RNC1[8]))
    AC1 = setNames(AC1,c('AC','Quantity')) %>% mutate( AC = reorder(AC,Quantity))
    plot_ly(AC1, x = ~AC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected AC", margin = m)
  })
  
  output$Part1 <- renderPlotly({
    Part1 = as.data.frame(table(RNC1[14]))
    Part1 = setNames(Part1,c('Part','Quantity'))
    Part1 <- Part1 %>% top_n(n=10, Quantity) %>% mutate( Part = reorder(Part,Quantity))
    plot_ly(Part1, x = ~Part, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected Parts", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$QA1 <- renderPlotly({
    QA1 = as.data.frame(table(RNC1[12]))
    QA1 = setNames(QA1,c('QA','Quantity')) %>% mutate( QA = reorder(QA,Quantity))
    plot_ly(QA1, x = ~QA, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Quality Agent", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Defect1 <- renderPlotly({
    Defect1 = as.data.frame(table(RNC1[17]))
    Defect1 = setNames(Defect1,c('Defect','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( Defect = reorder(Defect,Quantity))
    plot_ly(Defect1, x = ~Defect, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Defect", margin = m)
  })
  
  output$Disp1 <- renderPlotly({
    
    Disp1 = as.data.frame(table(RNC1[20]))
    Disp1 = setNames(Disp1,c('Disp','Quantity')) %>% mutate( Disp = reorder(Disp,Quantity))  
    plot_ly(Disp1, x = ~Disp, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Disposition Type", margin = m)
  })
  
  output$Day1 <- renderPlotly({
    Day1 = as.data.frame(table(RNC1[29]))
    Day1 = setNames(Day1,c('Day','Quantity')) %>% mutate( Day = reorder(Day,Quantity))
    plot_ly(Day1, x = ~Day, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Day", margin = m)
  })
  
  output$WB1 <- renderPlotly({
    WB1 = as.data.frame(table(RNC1[25]))
    WB1 = setNames(WB1,c('WB','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( WB = reorder(WB,Quantity))
    plot_ly(WB1, x = ~WB, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Workbook", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Skill1 <- renderPlotly({
    Skill1 = as.data.frame(table(RNC1[30]))
    Skill1 = setNames(Skill1,c('Skill','Quantity')) %>% mutate( Skill = reorder(Skill,Quantity)) 
    plot_ly(Skill1, x = ~Skill, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Skill", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Worker1 <- renderPlotly({
    Worker1 = as.data.frame(table(RNC1[28]))
    Worker1 = setNames(Worker1,c('Worker','Quantity')) %>% top_n(n=10, Quantity) %>% mutate(Worker = reorder(Worker,Quantity))
    plot_ly(Worker1, x = ~Worker, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Worker", margin = m)
  })
  
  
  
  
  ## E Zone 1
  
  ## S Zone 2
  
  
  TimeSeries2 <- read_excel("Raw_Data.xlsx", 
                            sheet = "TSZ2")
  
  year.month.r <- seq.Date(as.Date("2019-01-01"), as.Date("2021-06-01"), by = "month")
  Total <- data.frame(year.month.r, TimeSeries2$`Total RNC`)
  Total <- xts(Total$TimeSeries2..Total.RNC.,Total$year.month.r)
  Meth <- data.frame(year.month.r, TimeSeries2$`Methods RNC`)
  Meth <- xts(Meth$TimeSeries2..Methods.RNC.,Meth$year.month.r)
  data_ts2 <- cbind(Total, Meth)
  data_ts2 <- data.frame(date=index(data_ts2), coredata(data_ts2))
  
  output$TS2 <- renderPlotly({
    
    plot_ly(x = ~data_ts2$date, y = ~ data_ts2$Meth, name = 'Methods', type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~data_ts2$date, y = ~data_ts2$Total, name = 'Total', mode = 'lines')  %>%
      config(displayModeBar = FALSE) %>%
      layout(
        
        xaxis = list(title = "Date",
                     rangeselector = list(
                       buttons = list(
                         list(
                           count = 3,
                           label = "3 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 6,
                           label = "6 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 12,
                           label = "Year",
                           step = "month",
                           stepmode = "todate"),
                         list(step = "all"))),
                     
                     rangeslider = list(type = "Date")),
        
        yaxis = list(title = "RNC"))
  })
  
  RNC2  <- filter(RNC, ZONE == "Zone 2")
  
  output$WC2 <- renderPlotly({
    
    WC_2 = as.data.frame(table(RNC2[6]))
    WC_2 = setNames(WC_2,c('WC','Quantity'))
    WC_2  %>% mutate( WC = reorder(WC,Quantity)) 
    plot_ly(WC_2, x = ~WC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Work Center", margin = m)
  })
  
  output$AC2 <- renderPlotly({
    AC2 = as.data.frame(table(RNC2[8]))
    AC2 = setNames(AC2,c('AC','Quantity')) %>% mutate( AC = reorder(AC,Quantity))
    plot_ly(AC2, x = ~AC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected AC", margin = m)
  })
  
  output$Part2 <- renderPlotly({
    Part2 = as.data.frame(table(RNC2[14]))
    Part2 = setNames(Part2,c('Part','Quantity'))
    Part2 <- Part2 %>% top_n(n=10, Quantity) %>% mutate( Part = reorder(Part,Quantity))
    plot_ly(Part2, x = ~Part, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected Parts", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$QA2 <- renderPlotly({
    QA2 = as.data.frame(table(RNC2[12]))
    QA2 = setNames(QA2,c('QA','Quantity')) %>% mutate( QA = reorder(QA,Quantity))
    plot_ly(QA2, x = ~QA, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Quality Agent", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Defect2 <- renderPlotly({
    Defect2 = as.data.frame(table(RNC2[17]))
    Defect2 = setNames(Defect2,c('Defect','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( Defect = reorder(Defect,Quantity))
    plot_ly(Defect2, x = ~Defect, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Defect", margin = m)
  })
  
  output$Disp2 <- renderPlotly({
    
    Disp2 = as.data.frame(table(RNC2[20]))
    Disp2 = setNames(Disp2,c('Disp','Quantity')) %>% mutate( Disp = reorder(Disp,Quantity))  
    plot_ly(Disp2, x = ~Disp, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Disposition Type", margin = m)
  })
  
  output$Day2 <- renderPlotly({
    Day2 = as.data.frame(table(RNC2[29]))
    Day2 = setNames(Day2,c('Day','Quantity')) %>% mutate( Day = reorder(Day,Quantity))
    plot_ly(Day2, x = ~Day, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Day", margin = m)
  })
  
  output$WB2 <- renderPlotly({
    WB2 = as.data.frame(table(RNC2[25]))
    WB2 = setNames(WB2,c('WB','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( WB = reorder(WB,Quantity))
    plot_ly(WB2, x = ~WB, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Workbook", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Skill2 <- renderPlotly({
    Skill2 = as.data.frame(table(RNC2[30]))
    Skill2 = setNames(Skill2,c('Skill','Quantity')) %>% mutate( Skill = reorder(Skill,Quantity)) 
    plot_ly(Skill2, x = ~Skill, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Skill", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Worker2 <- renderPlotly({
    Worker2 = as.data.frame(table(RNC2[28]))
    Worker2 = setNames(Worker2,c('Worker','Quantity')) %>% top_n(n=10, Quantity) %>% mutate(Worker = reorder(Worker,Quantity))
    plot_ly(Worker2, x = ~Worker, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Worker", margin = m)
  })
  
  
  
  ## E Zone 2
  
  ## S Zone 31
  
  TimeSeries31 <- read_excel("Raw_Data.xlsx", 
                             sheet = "TSZ31")
  
  year.month.r <- seq.Date(as.Date("2019-01-01"), as.Date("2021-06-01"), by = "month")
  Total31 <- data.frame(year.month.r, TimeSeries31$`Total RNC`)
  Total31 <- xts(Total31$TimeSeries31..Total.RNC.,Total31$year.month.r)
  Meth31 <- data.frame(year.month.r, TimeSeries31$`Methods RNC`)
  Meth31 <- xts(Meth31$TimeSeries31..Methods.RNC.,Meth31$year.month.r)
  data_ts31 <- cbind(Total31, Meth31)
  data_ts31 <- data.frame(date=index(data_ts31), coredata(data_ts31))
  
  
  output$TS31 <- renderPlotly({
    
    plot_ly(x = ~data_ts31$date, y = ~ data_ts31$Meth, name = 'Methods', type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~data_ts31$date, y = ~data_ts31$Total, name = 'Total', mode = 'lines')  %>%
      config(displayModeBar = FALSE) %>%
      layout(
        
        xaxis = list(title = "Date",
                     rangeselector = list(
                       buttons = list(
                         list(
                           count = 3,
                           label = "3 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 6,
                           label = "6 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 12,
                           label = "Year",
                           step = "month",
                           stepmode = "todate"),
                         list(step = "all"))),
                     
                     rangeslider = list(type = "Date")),
        
        yaxis = list(title = "RNC"))
  })
  
  RNC31  <- filter(RNC, WC %in% c('AGX801','AGX802','AGX803','AGX804','AGX805','AGX806','AGX807',
                                  'AGX808', 'AGX809', 'AGX810')) %>% unique()
  
  output$WC31 <- renderPlotly({
    
    WC_31 = as.data.frame(table(RNC31[6]))
    WC_31 = setNames(WC_31,c('WC','Quantity'))
    WC_31  %>% mutate( WC = reorder(WC,Quantity)) 
    plot_ly(WC_31, x = ~WC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Work Center", margin = m)
  })
  
  output$AC31 <- renderPlotly({
    AC31 = as.data.frame(table(RNC31[8]))
    AC31 = setNames(AC31,c('AC','Quantity')) %>% mutate( AC = reorder(AC,Quantity))
    plot_ly(AC31, x = ~AC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected AC", margin = m)
  })
  
  output$Part31 <- renderPlotly({
    Part31 = as.data.frame(table(RNC31[14]))
    Part31 = setNames(Part31,c('Part','Quantity'))
    Part31 <- Part31 %>% top_n(n=10, Quantity) %>% mutate( Part = reorder(Part,Quantity))
    plot_ly(Part31, x = ~Part, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected Parts", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$QA31 <- renderPlotly({
    QA31 = as.data.frame(table(RNC31[12]))
    QA31 = setNames(QA31,c('QA','Quantity')) %>% mutate( QA = reorder(QA,Quantity))
    plot_ly(QA31, x = ~QA, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Quality Agent", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Defect31 <- renderPlotly({
    Defect31 = as.data.frame(table(RNC31[17]))
    Defect31 = setNames(Defect31,c('Defect','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( Defect = reorder(Defect,Quantity))
    plot_ly(Defect31, x = ~Defect, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Defect", margin = m)
  })
  
  output$Disp31 <- renderPlotly({
    
    Disp31 = as.data.frame(table(RNC31[20]))
    Disp31 = setNames(Disp31,c('Disp','Quantity')) %>% mutate( Disp = reorder(Disp,Quantity))  
    plot_ly(Disp31, x = ~Disp, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Disposition Type", margin = m)
  })
  
  output$Day31 <- renderPlotly({
    Day31 = as.data.frame(table(RNC31[29]))
    Day31 = setNames(Day31,c('Day','Quantity')) %>% mutate( Day = reorder(Day,Quantity))
    plot_ly(Day31, x = ~Day, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Day", margin = m)
  })
  
  output$WB31 <- renderPlotly({
    WB31 = as.data.frame(table(RNC31[25]))
    WB31 = setNames(WB31,c('WB','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( WB = reorder(WB,Quantity))
    plot_ly(WB31, x = ~WB, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Workbook", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Skill31 <- renderPlotly({
    Skill31 = as.data.frame(table(RNC31[30]))
    Skill31 = setNames(Skill31,c('Skill','Quantity')) %>% mutate( Skill = reorder(Skill,Quantity)) 
    plot_ly(Skill31, x = ~Skill, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Skill", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Worker31 <- renderPlotly({
    Worker31 = as.data.frame(table(RNC31[28]))
    Worker31 = setNames(Worker31,c('Worker','Quantity')) %>% top_n(n=10, Quantity) %>% mutate(Worker = reorder(Worker,Quantity))
    plot_ly(Worker31, x = ~Worker, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Worker", margin = m)
  })
  
  
  
  ## E Zone 31
  ## S Zone 32  
  
  TimeSeries32 <- read_excel("Raw_Data.xlsx", 
                             sheet = "TSZ32")
  
  year.month.r <- seq.Date(as.Date("2019-01-01"), as.Date("2021-06-01"), by = "month")
  Total32 <- data.frame(year.month.r, TimeSeries32$`Total RNC`)
  Total32 <- xts(Total32$TimeSeries32..Total.RNC.,Total32$year.month.r)
  Meth32 <- data.frame(year.month.r, TimeSeries32$`Methods RNC`)
  Meth32 <- xts(Meth32$TimeSeries32..Methods.RNC.,Meth32$year.month.r)
  data_ts32 <- cbind(Total32, Meth32)
  data_ts32 <- data.frame(date=index(data_ts32), coredata(data_ts32))
  
  
  output$TS32 <- renderPlotly({
    
    plot_ly(x = ~data_ts32$date, y = ~ data_ts32$Meth, name = 'Methods', type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~data_ts32$date, y = ~data_ts32$Total, name = 'Total', mode = 'lines')  %>%
      config(displayModeBar = FALSE) %>%
      layout(
        
        xaxis = list(title = "Date",
                     rangeselector = list(
                       buttons = list(
                         list(
                           count = 3,
                           label = "3 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 6,
                           label = "6 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 12,
                           label = "Year",
                           step = "month",
                           stepmode = "todate"),
                         list(step = "all"))),
                     
                     rangeslider = list(type = "Date")),
        
        yaxis = list(title = "RNC"))
  })
  
  RNC32  <- filter(RNC, WC %in% c('AGX830','AGX850','AGX820','AGX818','AGX852')) %>% unique()
  
  output$WC32 <- renderPlotly({
    
    WC_32 = as.data.frame(table(RNC32[6]))
    WC_32 = setNames(WC_32,c('WC','Quantity'))
    WC_32  %>% mutate( WC = reorder(WC,Quantity)) 
    plot_ly(WC_32, x = ~WC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Work Center", margin = m)
  })
  
  output$AC32 <- renderPlotly({
    AC32 = as.data.frame(table(RNC32[8]))
    AC32 = setNames(AC32,c('AC','Quantity')) %>% mutate( AC = reorder(AC,Quantity))
    plot_ly(AC32, x = ~AC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected AC", margin = m)
  })
  
  output$Part32 <- renderPlotly({
    Part32 = as.data.frame(table(RNC32[14]))
    Part32 = setNames(Part32,c('Part','Quantity'))
    Part32 <- Part32 %>% top_n(n=10, Quantity) %>% mutate( Part = reorder(Part,Quantity))
    plot_ly(Part32, x = ~Part, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected Parts", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$QA32 <- renderPlotly({
    QA32 = as.data.frame(table(RNC32[12]))
    QA32 = setNames(QA32,c('QA','Quantity')) %>% mutate( QA = reorder(QA,Quantity))
    plot_ly(QA32, x = ~QA, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Quality Agent", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Defect32 <- renderPlotly({
    Defect32 = as.data.frame(table(RNC32[17]))
    Defect32 = setNames(Defect32,c('Defect','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( Defect = reorder(Defect,Quantity))
    plot_ly(Defect32, x = ~Defect, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Defect", margin = m)
  })
  
  output$Disp32 <- renderPlotly({
    
    Disp32 = as.data.frame(table(RNC32[20]))
    Disp32 = setNames(Disp32,c('Disp','Quantity')) %>% mutate( Disp = reorder(Disp,Quantity))  
    plot_ly(Disp32, x = ~Disp, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Disposition Type", margin = m)
  })
  
  output$Day32 <- renderPlotly({
    Day32 = as.data.frame(table(RNC32[29]))
    Day32 = setNames(Day32,c('Day','Quantity')) %>% mutate( Day = reorder(Day,Quantity))
    plot_ly(Day32, x = ~Day, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Day", margin = m)
  })
  
  output$WB32 <- renderPlotly({
    WB32 = as.data.frame(table(RNC32[25]))
    WB32 = setNames(WB32,c('WB','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( WB = reorder(WB,Quantity))
    plot_ly(WB32, x = ~WB, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Workbook", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Skill32 <- renderPlotly({
    Skill32 = as.data.frame(table(RNC32[30]))
    Skill32 = setNames(Skill32,c('Skill','Quantity')) %>% mutate( Skill = reorder(Skill,Quantity)) 
    plot_ly(Skill32, x = ~Skill, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Skill", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Worker32 <- renderPlotly({
    Worker32 = as.data.frame(table(RNC32[28]))
    Worker32 = setNames(Worker32,c('Worker','Quantity')) %>% top_n(n=10, Quantity) %>% mutate(Worker = reorder(Worker,Quantity))
    plot_ly(Worker32, x = ~Worker, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Worker", margin = m)
  })
  
  
  
  ## E ZOne 32
  
  ## S Zone 33
  
  TimeSeries33 <- read_excel("Raw_Data.xlsx", 
                             sheet = "TSZ33")
  
  year.month.r <- seq.Date(as.Date("2019-01-01"), as.Date("2021-06-01"), by = "month")
  Total33 <- data.frame(year.month.r, TimeSeries33$`Total RNC`)
  Total33 <- xts(Total33$TimeSeries33..Total.RNC.,Total33$year.month.r)
  Meth33 <- data.frame(year.month.r, TimeSeries33$`Methods RNC`)
  Meth33 <- xts(Meth33$TimeSeries33..Methods.RNC.,Meth33$year.month.r)
  data_ts33 <- cbind(Total33, Meth33)
  data_ts33 <- data.frame(date=index(data_ts33), coredata(data_ts33))
  
  
  output$TS33 <- renderPlotly({
    
    plot_ly(x = ~data_ts33$date, y = ~ data_ts33$Meth, name = 'Methods', type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~data_ts33$date, y = ~data_ts33$Total, name = 'Total', mode = 'lines')  %>%
      config(displayModeBar = FALSE) %>%
      layout(
        
        xaxis = list(title = "Date",
                     rangeselector = list(
                       buttons = list(
                         list(
                           count = 3,
                           label = "3 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 6,
                           label = "6 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 12,
                           label = "Year",
                           step = "month",
                           stepmode = "todate"),
                         list(step = "all"))),
                     
                     rangeslider = list(type = "Date")),
        
        yaxis = list(title = "RNC"))
  })
  
  RNC33  <- filter(RNC, WC %in% c('AGX950')) %>% unique()
  
  output$WC33 <- renderPlotly({
    
    WC_33 = as.data.frame(table(RNC33[6]))
    WC_33 = setNames(WC_33,c('WC','Quantity'))
    WC_33  %>% mutate( WC = reorder(WC,Quantity)) 
    plot_ly(WC_33, x = ~WC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Work Center", margin = m)
  })
  
  output$AC33 <- renderPlotly({
    AC33 = as.data.frame(table(RNC33[8]))
    AC33 = setNames(AC33,c('AC','Quantity')) %>% mutate( AC = reorder(AC,Quantity))
    plot_ly(AC33, x = ~AC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected AC", margin = m)
  })
  
  output$Part33 <- renderPlotly({
    Part33 = as.data.frame(table(RNC33[14]))
    Part33 = setNames(Part33,c('Part','Quantity'))
    Part33 <- Part33 %>% top_n(n=10, Quantity) %>% mutate( Part = reorder(Part,Quantity))
    plot_ly(Part33, x = ~Part, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected Parts", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$QA33 <- renderPlotly({
    QA33 = as.data.frame(table(RNC33[12]))
    QA33 = setNames(QA33,c('QA','Quantity')) %>% mutate( QA = reorder(QA,Quantity))
    plot_ly(QA33, x = ~QA, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Quality Agent", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Defect33 <- renderPlotly({
    Defect33 = as.data.frame(table(RNC33[17]))
    Defect33 = setNames(Defect33,c('Defect','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( Defect = reorder(Defect,Quantity))
    plot_ly(Defect33, x = ~Defect, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Defect", margin = m)
  })
  
  output$Disp33 <- renderPlotly({
    
    Disp33 = as.data.frame(table(RNC33[20]))
    Disp33 = setNames(Disp33,c('Disp','Quantity')) %>% mutate( Disp = reorder(Disp,Quantity))  
    plot_ly(Disp33, x = ~Disp, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Disposition Type", margin = m)
  })
  
  output$Day33 <- renderPlotly({
    Day33 = as.data.frame(table(RNC33[29]))
    Day33 = setNames(Day33,c('Day','Quantity')) %>% mutate( Day = reorder(Day,Quantity))
    plot_ly(Day33, x = ~Day, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Day", margin = m)
  })
  
  output$WB33 <- renderPlotly({
    WB33 = as.data.frame(table(RNC33[25]))
    WB33 = setNames(WB33,c('WB','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( WB = reorder(WB,Quantity))
    plot_ly(WB33, x = ~WB, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Workbook", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Skill33 <- renderPlotly({
    Skill33 = as.data.frame(table(RNC33[30]))
    Skill33 = setNames(Skill33,c('Skill','Quantity')) %>% mutate( Skill = reorder(Skill,Quantity)) 
    plot_ly(Skill33, x = ~Skill, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Skill", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Worker33 <- renderPlotly({
    Worker33 = as.data.frame(table(RNC33[28]))
    Worker33 = setNames(Worker33,c('Worker','Quantity')) %>% top_n(n=10, Quantity) %>% mutate(Worker = reorder(Worker,Quantity))
    plot_ly(Worker33, x = ~Worker, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Worker", margin = m)
  })
  
 
  
  ## E Zone 33
  
  ## S Zone 34
  
  TimeSeries34 <- read_excel("Raw_Data.xlsx", 
                             sheet = "TSZ34")
  
  year.month.r <- seq.Date(as.Date("2019-01-01"), as.Date("2021-06-01"), by = "month")
  Total34 <- data.frame(year.month.r, TimeSeries34$`Total RNC`)
  Total34 <- xts(Total34$TimeSeries34..Total.RNC.,Total34$year.month.r)
  Meth34 <- data.frame(year.month.r, TimeSeries34$`Methods RNC`)
  Meth34 <- xts(Meth34$TimeSeries34..Methods.RNC.,Meth34$year.month.r)
  data_ts34 <- cbind(Total34, Meth34)
  data_ts34 <- data.frame(date=index(data_ts34), coredata(data_ts34))
  
  
  output$TS34 <- renderPlotly({
    
    plot_ly(x = ~data_ts34$date, y = ~ data_ts34$Meth, name = 'Methods', type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~data_ts34$date, y = ~data_ts34$Total, name = 'Total', mode = 'lines')  %>%
      config(displayModeBar = FALSE) %>%
      layout(
        
        xaxis = list(title = "Date",
                     rangeselector = list(
                       buttons = list(
                         list(
                           count = 3,
                           label = "3 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 6,
                           label = "6 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 12,
                           label = "Year",
                           step = "month",
                           stepmode = "todate"),
                         list(step = "all"))),
                     
                     rangeslider = list(type = "Date")),
        
        yaxis = list(title = "RNC"))
  })
  
  RNC34  <- filter(RNC, WC %in% c('AGX952',	'SGX952',	'AGX947',	'SGX948',	'AGX948',	'AGX946',
                                  'AGX951',	'AGX944',	'SGX949')) %>% unique()
  
  output$WC34 <- renderPlotly({
    
    WC_34 = as.data.frame(table(RNC34[6]))
    WC_34 = setNames(WC_34,c('WC','Quantity'))
    WC_34  %>% mutate( WC = reorder(WC,Quantity)) 
    plot_ly(WC_34, x = ~WC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Work Center", margin = m)
  })
  
  output$AC34 <- renderPlotly({
    AC34 = as.data.frame(table(RNC34[8]))
    AC34 = setNames(AC34,c('AC','Quantity')) %>% mutate( AC = reorder(AC,Quantity))
    plot_ly(AC34, x = ~AC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected AC", margin = m)
  })
  
  output$Part34 <- renderPlotly({
    Part34 = as.data.frame(table(RNC34[14]))
    Part34 = setNames(Part34,c('Part','Quantity'))
    Part34 <- Part34 %>% top_n(n=10, Quantity) %>% mutate( Part = reorder(Part,Quantity))
    plot_ly(Part34, x = ~Part, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected Parts", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$QA34 <- renderPlotly({
    QA34 = as.data.frame(table(RNC34[12]))
    QA34 = setNames(QA34,c('QA','Quantity')) %>% mutate( QA = reorder(QA,Quantity))
    plot_ly(QA34, x = ~QA, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Quality Agent", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Defect34 <- renderPlotly({
    Defect34 = as.data.frame(table(RNC34[17]))
    Defect34 = setNames(Defect34,c('Defect','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( Defect = reorder(Defect,Quantity))
    plot_ly(Defect34, x = ~Defect, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Defect", margin = m)
  })
  
  output$Disp34 <- renderPlotly({
    
    Disp34 = as.data.frame(table(RNC34[20]))
    Disp34 = setNames(Disp34,c('Disp','Quantity')) %>% mutate( Disp = reorder(Disp,Quantity))  
    plot_ly(Disp34, x = ~Disp, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Disposition Type", margin = m)
  })
  
  output$Day34 <- renderPlotly({
    Day34 = as.data.frame(table(RNC34[29]))
    Day34 = setNames(Day34,c('Day','Quantity')) %>% mutate( Day = reorder(Day,Quantity))
    plot_ly(Day34, x = ~Day, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Day", margin = m)
  })
  
  output$WB34 <- renderPlotly({
    WB34 = as.data.frame(table(RNC34[25]))
    WB34 = setNames(WB34,c('WB','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( WB = reorder(WB,Quantity))
    plot_ly(WB34, x = ~WB, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Workbook", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Skill34 <- renderPlotly({
    Skill34 = as.data.frame(table(RNC34[30]))
    Skill34 = setNames(Skill34,c('Skill','Quantity')) %>% mutate( Skill = reorder(Skill,Quantity)) 
    plot_ly(Skill34, x = ~Skill, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Skill", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Worker34 <- renderPlotly({
    Worker34 = as.data.frame(table(RNC34[28]))
    Worker34 = setNames(Worker34,c('Worker','Quantity')) %>% top_n(n=10, Quantity) %>% mutate(Worker = reorder(Worker,Quantity))
    plot_ly(Worker34, x = ~Worker, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Worker", margin = m)
  })
  
  
  
  
  
  ## E Zone 34
  ## S Zone 4
  
  TimeSeries4 <- read_excel("Raw_Data.xlsx", 
                            sheet = "TSZ4")
  
  year.month.r <- seq.Date(as.Date("2019-01-01"), as.Date("2021-06-01"), by = "month")
  Total4 <- data.frame(year.month.r, TimeSeries4$`Total RNC`)
  Total4 <- xts(Total4$TimeSeries4..Total.RNC.,Total4$year.month.r)
  Meth4 <- data.frame(year.month.r, TimeSeries4$`Methods RNC`)
  Meth4 <- xts(Meth4$TimeSeries4..Methods.RNC.,Meth4$year.month.r)
  data_ts4 <- cbind(Total4, Meth4)
  data_ts4 <- data.frame(date=index(data_ts4), coredata(data_ts4))
  
  
  output$TS4 <- renderPlotly({
    
    plot_ly(x = ~data_ts4$date, y = ~ data_ts4$Meth, name = 'Methods', type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~data_ts4$date, y = ~data_ts4$Total, name = 'Total', mode = 'lines')  %>%
      config(displayModeBar = FALSE) %>%
      layout(
        
        xaxis = list(title = "Date",
                     rangeselector = list(
                       buttons = list(
                         list(
                           count = 3,
                           label = "3 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 6,
                           label = "6 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 12,
                           label = "Year",
                           step = "month",
                           stepmode = "todate"),
                         list(step = "all"))),
                     
                     rangeslider = list(type = "Date")),
        
        yaxis = list(title = "RNC"))
  })
  
  RNC4  <- filter(RNC, ZONE == "Zone 4")
  
  output$WC4 <- renderPlotly({
    
    WC_4 = as.data.frame(table(RNC4[6]))
    WC_4 = setNames(WC_4,c('WC','Quantity'))
    WC_4  %>% mutate( WC = reorder(WC,Quantity)) 
    plot_ly(WC_4, x = ~WC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Work Center", margin = m)
  })
  
  output$AC4 <- renderPlotly({
    AC4 = as.data.frame(table(RNC4[8]))
    AC4 = setNames(AC4,c('AC','Quantity')) %>% mutate( AC = reorder(AC,Quantity))
    plot_ly(AC4, x = ~AC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected AC", margin = m)
  })
  
  output$Part4 <- renderPlotly({
    Part4 = as.data.frame(table(RNC4[14]))
    Part4 = setNames(Part4,c('Part','Quantity'))
    Part4 <- Part4 %>% top_n(n=10, Quantity) %>% mutate( Part = reorder(Part,Quantity))
    plot_ly(Part4, x = ~Part, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected Parts", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$QA4 <- renderPlotly({
    QA4 = as.data.frame(table(RNC4[12]))
    QA4 = setNames(QA4,c('QA','Quantity')) %>% mutate( QA = reorder(QA,Quantity))
    plot_ly(QA4, x = ~QA, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Quality Agent", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Defect4 <- renderPlotly({
    Defect4 = as.data.frame(table(RNC4[17]))
    Defect4 = setNames(Defect4,c('Defect','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( Defect = reorder(Defect,Quantity))
    plot_ly(Defect4, x = ~Defect, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Defect", margin = m)
  })
  
  output$Disp4 <- renderPlotly({
    
    Disp4 = as.data.frame(table(RNC4[20]))
    Disp4 = setNames(Disp4,c('Disp','Quantity')) %>% mutate( Disp = reorder(Disp,Quantity))  
    plot_ly(Disp4, x = ~Disp, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Disposition Type", margin = m)
  })
  
  output$Day4 <- renderPlotly({
    Day4 = as.data.frame(table(RNC4[29]))
    Day4 = setNames(Day4,c('Day','Quantity')) %>% mutate( Day = reorder(Day,Quantity))
    plot_ly(Day4, x = ~Day, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Day", margin = m)
  })
  
  output$WB4 <- renderPlotly({
    WB4 = as.data.frame(table(RNC4[25]))
    WB4 = setNames(WB4,c('WB','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( WB = reorder(WB,Quantity))
    plot_ly(WB4, x = ~WB, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Workbook", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Skill4 <- renderPlotly({
    Skill4 = as.data.frame(table(RNC4[30]))
    Skill4 = setNames(Skill4,c('Skill','Quantity')) %>% mutate( Skill = reorder(Skill,Quantity)) 
    plot_ly(Skill4, x = ~Skill, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Skill", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Worker4 <- renderPlotly({
    Worker4 = as.data.frame(table(RNC4[28]))
    Worker4 = setNames(Worker4,c('Worker','Quantity')) %>% top_n(n=10, Quantity) %>% mutate(Worker = reorder(Worker,Quantity))
    plot_ly(Worker4, x = ~Worker, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Worker", margin = m)
  })
  
  
  ## S Zone 4
  
  TimeSeries5 <- read_excel("Raw_Data.xlsx", 
                            sheet = "TSZ5")
  
  year.month.r <- seq.Date(as.Date("2019-01-01"), as.Date("2021-06-01"), by = "month")
  Total5 <- data.frame(year.month.r, TimeSeries5$`Total RNC`)
  Total5 <- xts(Total5$TimeSeries5..Total.RNC.,Total5$year.month.r)
  Meth5 <- data.frame(year.month.r, TimeSeries5$`Methods RNC`)
  Meth5 <- xts(Meth5$TimeSeries5..Methods.RNC.,Meth5$year.month.r)
  data_ts5 <- cbind(Total5, Meth5)
  data_ts5 <- data.frame(date=index(data_ts5), coredata(data_ts5))
  
  
  output$TS5 <- renderPlotly({
    
    plot_ly(x = ~data_ts5$date, y = ~ data_ts5$Meth, name = 'Methods', type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~data_ts5$date, y = ~data_ts5$Total, name = 'Total', mode = 'lines')  %>%
      config(displayModeBar = FALSE) %>%
      layout(
        
        xaxis = list(title = "Date",
                     rangeselector = list(
                       buttons = list(
                         list(
                           count = 3,
                           label = "3 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 6,
                           label = "6 Month",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 12,
                           label = "Year",
                           step = "month",
                           stepmode = "todate"),
                         list(step = "all"))),
                     
                     rangeslider = list(type = "Date")),
        
        yaxis = list(title = "RNC"))
  })
  
  RNC5  <- filter(RNC, ZONE == "Zone 5")
  
  output$WC5 <- renderPlotly({
    
    WC_5 = as.data.frame(table(RNC5[6]))
    WC_5 = setNames(WC_5,c('WC','Quantity'))
    WC_5  %>% mutate( WC = reorder(WC,Quantity)) 
    plot_ly(WC_5, x = ~WC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Work Center", margin = m)
  })
  
  output$AC5 <- renderPlotly({
    AC5 = as.data.frame(table(RNC5[8]))
    AC5 = setNames(AC5,c('AC','Quantity')) %>% mutate( AC = reorder(AC,Quantity))
    plot_ly(AC5, x = ~AC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected AC", margin = m)
  })
  
  output$Part5 <- renderPlotly({
    Part5 = as.data.frame(table(RNC5[14]))
    Part5 = setNames(Part5,c('Part','Quantity'))
    Part5 <- Part5 %>% top_n(n=10, Quantity) %>% mutate( Part = reorder(Part,Quantity))
    plot_ly(Part5, x = ~Part, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected Parts", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$QA5 <- renderPlotly({
    QA5 = as.data.frame(table(RNC5[12]))
    QA5 = setNames(QA5,c('QA','Quantity')) %>% mutate( QA = reorder(QA,Quantity))
    plot_ly(QA5, x = ~QA, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Quality Agent", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Defect5 <- renderPlotly({
    Defect5 = as.data.frame(table(RNC5[17]))
    Defect5 = setNames(Defect5,c('Defect','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( Defect = reorder(Defect,Quantity))
    plot_ly(Defect5, x = ~Defect, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Defect", margin = m)
  })
  
  output$Disp5 <- renderPlotly({
    
    Disp5 = as.data.frame(table(RNC5[20]))
    Disp5 = setNames(Disp5,c('Disp','Quantity')) %>% mutate( Disp = reorder(Disp,Quantity))  
    plot_ly(Disp5, x = ~Disp, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Disposition Type", margin = m)
  })
  
  output$Day5 <- renderPlotly({
    Day5 = as.data.frame(table(RNC5[29]))
    Day5 = setNames(Day5,c('Day','Quantity')) %>% mutate( Day = reorder(Day,Quantity))
    plot_ly(Day5, x = ~Day, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Day", margin = m)
  })
  
  output$WB5 <- renderPlotly({
    WB5 = as.data.frame(table(RNC5[25]))
    WB5 = setNames(WB5,c('WB','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( WB = reorder(WB,Quantity))
    plot_ly(WB5, x = ~WB, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Workbook", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Skill5 <- renderPlotly({
    Skill5 = as.data.frame(table(RNC5[30]))
    Skill5 = setNames(Skill5,c('Skill','Quantity')) %>% mutate( Skill = reorder(Skill,Quantity)) 
    plot_ly(Skill5, x = ~Skill, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Skill", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$Worker5 <- renderPlotly({
    Worker5 = as.data.frame(table(RNC5[28]))
    Worker5 = setNames(Worker5,c('Worker','Quantity')) %>% top_n(n=10, Quantity) %>% mutate(Worker = reorder(Worker,Quantity))
    plot_ly(Worker5, x = ~Worker, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Worker", margin = m)
  })
  
  ### Data Mining1
  
 
  ## E Zone 5 
  
  ## S Zone MTS
  
  RNCMTS  <- filter(RNC, ZONE == "MTS")
  
  output$WCMTS <- renderPlotly({
    
    WC_MTS = as.data.frame(table(RNCMTS[6]))
    WC_MTS = setNames(WC_MTS,c('WC','Quantity'))
    WC_MTS  %>% mutate( WC = reorder(WC,Quantity)) 
    plot_ly(WC_MTS, x = ~WC, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Work Center", margin = m)
  })
  
  
  output$PartMTS <- renderPlotly({
    PartMTS = as.data.frame(table(RNCMTS[14]))
    PartMTS = setNames(PartMTS,c('Part','Quantity'))
    PartMTS <- PartMTS %>% top_n(n=10, Quantity) %>% mutate( Part = reorder(Part,Quantity))
    plot_ly(PartMTS, x = ~Part, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "Most affected Parts", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$QAMTS <- renderPlotly({
    QAMTS = as.data.frame(table(RNCMTS[12]))
    QAMTS = setNames(QAMTS,c('QA','Quantity')) %>% mutate( QA = reorder(QA,Quantity))
    plot_ly(QAMTS, x = ~QA, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Quality Agent", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$DefectMTS <- renderPlotly({
    DefectMTS = as.data.frame(table(RNCMTS[17]))
    DefectMTS = setNames(DefectMTS,c('Defect','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( Defect = reorder(Defect,Quantity))
    plot_ly(DefectMTS, x = ~Defect, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Defect", margin = m)
  })
  
  output$DispMTS <- renderPlotly({
    
    DispMTS = as.data.frame(table(RNCMTS[20]))
    DispMTS = setNames(DispMTS,c('Disp','Quantity')) %>% mutate( Disp = reorder(Disp,Quantity))  
    plot_ly(DispMTS, x = ~Disp, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Disposition Type", margin = m)
  })
  
  output$DayMTS <- renderPlotly({
    DayMTS = as.data.frame(table(RNCMTS[29]))
    DayMTS = setNames(DayMTS,c('Day','Quantity')) %>% mutate( Day = reorder(Day,Quantity))
    plot_ly(DayMTS, x = ~Day, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Day", margin = m)
  })
  
  output$WBMTS <- renderPlotly({
    WBMTS = as.data.frame(table(RNCMTS[25]))
    WBMTS = setNames(WBMTS,c('WB','Quantity')) %>% top_n(n=10, Quantity) %>% mutate( WB = reorder(WB,Quantity))
    plot_ly(WBMTS, x = ~WB, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Workbook", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$SkillMTS <- renderPlotly({
    SkillMTS = as.data.frame(table(RNCMTS[30]))
    SkillMTS = setNames(SkillMTS,c('Skill','Quantity')) %>% mutate( Skill = reorder(Skill,Quantity)) 
    plot_ly(SkillMTS, x = ~Skill, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Skill", margin = m,
                                                                xaxis = list(showticklabels = FALSE))
  })
  
  output$WorkerMTS <- renderPlotly({
    WorkerMTS = as.data.frame(table(RNCMTS[28]))
    WorkerMTS = setNames(WorkerMTS,c('Worker','Quantity')) %>% top_n(n=10, Quantity) %>% mutate(Worker = reorder(Worker,Quantity))
    plot_ly(WorkerMTS, x = ~Worker, y = ~Quantity, type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% layout(title = "RNCs per Worker", margin = m)
  })
  
  
  
 
  
  
  
  ### Data Mining1
  
 
  
  ## E zone MTS
  
  output$Next_Previous=renderUI({
    tab_list=input$List_of_tab[-length(input$List_of_tab)]
    nb_tab=length(tab_list)
    if (which(tab_list==input$tabBox_next_previous)==nb_tab)
      column(1,offset=1,Previous_Button)
    else if (which(tab_list==input$tabBox_next_previous)==1)
      column(1,offset = 10,Next_Button)
    else
      div(column(1,offset=1,Previous_Button),column(1,offset=8,Next_Button))
    
  })
  observeEvent(input$Prev_Tab,
               {
                 tab_list=input$List_of_tab
                 current_tab=which(tab_list==input$tabBox_next_previous)
                 updateTabsetPanel(session,"tabBox_next_previous",selected=tab_list[current_tab-1])
               }
  )
  observeEvent(input$Next_Tab,
               {
                 tab_list=input$List_of_tab
                 current_tab=which(tab_list==input$tabBox_next_previous)
                 updateTabsetPanel(session,"tabBox_next_previous",selected=tab_list[current_tab+1])
               }
  )
  
  #----------------
  
  
  output$Next_Previous1=renderUI({
    tab_list=input$List_of_tab[-length(input$List_of_tab)]
    nb_tab=length(tab_list)
    if (which(tab_list==input$tabBox_next_previous1)==nb_tab)
      column(1,offset=1,Previous_Button)
    else if (which(tab_list==input$tabBox_next_previous1)==1)
      column(1,offset = 10,Next_Button)
    else
      div(column(1,offset=1,Previous_Button),column(1,offset=8,Next_Button))
    
  })
  observeEvent(input$Prev_Tab,
               {
                 tab_list=input$List_of_tab
                 current_tab=which(tab_list==input$tabBox_next_previous1)
                 updateTabsetPanel(session,"tabBox_next_previous1",selected=tab_list[current_tab-1])
               }
  )
  observeEvent(input$Next_Tab,
               {
                 tab_list=input$List_of_tab
                 current_tab=which(tab_list==input$tabBox_next_previous1)
                 updateTabsetPanel(session,"tabBox_next_previous1",selected=tab_list[current_tab+1])
               }
  )
  
  TopicsTS <- read.csv("./ML/Hist_Topics.csv") 
  
  
  
  output$CountZ1_1 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 1 & Issue == 1) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ1_1 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ1_1 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ1_1 <- renderValueBox({
    valueBox(
      paste0("RFC 61930"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ1_1 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 1 & Issue == 1) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ1_1 <- renderValueBox({
    valueBox(
      paste0("13-Aug"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ1_1 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic1TS1 <- TopicsTS %>% filter(Zone == 1 & Issue == 1) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic1TS1, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
            ) %>% layout(title = "Topic 1")
    
  }) 
  
  
  output$CountZ1_2 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 1 & Issue == 2) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ1_2 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ1_2 <- renderValueBox({
    valueBox(
      paste0("3th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ1_2 <- renderValueBox({
    valueBox(
      paste0("PCR 61582"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ1_2 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 1 & Issue == 2) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ1_2 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ1_2 <- renderPlotly({
    
    TopicsTS <- read.csv("./ML/Hist_Topics.csv") 
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic1TS <- TopicsTS %>% filter(Zone == 1 & Issue == 2) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic1TS, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
 #----
  output$CountZ2_1 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 2 & Issue == 1) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ2_1 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ2_1 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ2_1 <- renderValueBox({
    valueBox(
      paste0("RFC 169938"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ2_1 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 2 & Issue == 1) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ2_1 <- renderValueBox({
    valueBox(
      paste0("70123/70125"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ2_1 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic2TS1 <- TopicsTS %>% filter(Zone == 2 & Issue == 1) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic2TS1, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  #-------
  
  output$CountZ2_2 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 2 & Issue == 2) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ2_2 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
    
    
    
    
  })
  
  output$PhaseZ2_2 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ2_2 <- renderValueBox({
    valueBox(
      paste0("RFC 168585"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ2_2 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 2 & Issue == 2) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ2_2 <- renderValueBox({
    valueBox(
      paste0("70120"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ2_2 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic2TS2 <- TopicsTS %>% filter(Zone == 2 & Issue == 2) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic2TS2, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  #------------
  
  output$CountZ2_3 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 2 & Issue == 3) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ2_3 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ2_3 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ2_3 <- renderValueBox({
    valueBox(
      paste0("COS G05363136-009-01"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ2_3 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 2 & Issue == 3) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ2_3 <- renderValueBox({
    valueBox(
      paste0("70120"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ2_3 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic2TS3 <- TopicsTS %>% filter(Zone == 2 & Issue == 3) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic2TS3, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  #----------
  
  output$CountZ31_1 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 31 & Issue == 1) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ31_1 <- renderValueBox({
    valueBox(
      paste0("No"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ31_1 <- renderValueBox({
    valueBox(
      paste0("2nd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ31_1 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ31_1 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 31 & Issue == 1) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ31_1 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ31_1 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic31TS1 <- TopicsTS %>% filter(Zone == 31 & Issue == 1) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic31TS1, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  }) 
  
  #-----------
  
  output$CountZ31_2 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 31 & Issue == 2) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ31_2 <- renderValueBox({
    valueBox(
      paste0("NO"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ31_2 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ31_2 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ31_2 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 31 & Issue == 2) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ31_2 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ31_2 <- renderPlotly({

    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic31TS2 <- TopicsTS %>% filter(Zone == 31 & Issue == 2) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic31TS2, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 2")
    
  })
  
  #---
  
  output$CountZ31_3 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 31 & Issue == 3) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ31_3 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ31_3 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ31_3 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ31_3 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 31 & Issue == 3) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ31_3 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ31_3 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic31TS3 <- TopicsTS %>% filter(Zone == 31 & Issue == 3) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic31TS3, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  
  #------
  
  
  output$CountZ31_4 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 31 & Issue == 4) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ31_4 <- renderValueBox({
    valueBox(
      paste0("No"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ31_4 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ31_4 <- renderValueBox({
    valueBox(
      paste0("Q "), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ31_4 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 31 & Issue == 4) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ31_4 <- renderValueBox({
    valueBox(
      paste0("70132"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ31_4 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic31TS4 <- TopicsTS %>% filter(Zone == 31 & Issue == 4) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic31TS4, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  #-----
  
  output$CountZ31_5 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 31 & Issue == 5) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ31_5 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ31_5 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ31_5 <- renderValueBox({
    valueBox(
      paste0("RFC 61930"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ31_5 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 31 & Issue == 5) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ31_5 <- renderValueBox({
    valueBox(
      paste0("27-Aug"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ31_5 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic31TS5 <- TopicsTS %>% filter(Zone == 31 & Issue == 5) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic31TS5, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  output$CountZ31_6 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 31 & Issue == 6) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ31_6 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ31_6 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ31_6 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ31_6 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 31 & Issue == 6) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ31_6 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ31_6 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic31TS6 <- TopicsTS %>% filter(Zone == 31 & Issue == 6) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic31TS6, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  
  output$CountZ31_7 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 31 & Issue == 7) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ31_7 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ31_7 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ31_7 <- renderValueBox({
    valueBox(
      paste0("Q 4442"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ31_7 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 31 & Issue == 7) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ31_7 <- renderValueBox({
    valueBox(
      paste0("23-Jul"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ31_7 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic31TS7 <- TopicsTS %>% filter(Zone == 31 & Issue == 7) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic31TS7, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  output$CountZ31_7 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 31 & Issue == 7) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ31_7 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ31_7 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ31_7 <- renderValueBox({
    valueBox(
      paste0("Q 4442"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ31_7 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 31 & Issue == 7) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ31_7 <- renderValueBox({
    valueBox(
      paste0("23-Jul"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ31_7 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic31TS7 <- TopicsTS %>% filter(Zone == 31 & Issue == 7) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic31TS7, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  output$CountZ31_8 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 31 & Issue == 8) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ31_8 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ31_8 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ31_8 <- renderValueBox({
    valueBox(
      paste0("Q"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ31_8 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 31 & Issue == 8) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ31_8 <- renderValueBox({
    valueBox(
      paste0("13 Jul"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ31_8 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic31TS8 <- TopicsTS %>% filter(Zone == 31 & Issue == 8) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic31TS8, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  
  output$CountZ31_9 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 31 & Issue == 9) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ31_9 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ31_9 <- renderValueBox({
    valueBox(
      paste0("2nd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ31_9 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ31_9 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 31 & Issue == 9) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ31_9 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ31_9 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic31TS9 <- TopicsTS %>% filter(Zone == 31 & Issue == 9) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic31TS9, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  
  output$CountZ31_10 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 31 & Issue == 10) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ31_10 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ31_10 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ31_10 <- renderValueBox({
    valueBox(
      paste0("Q 4412"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ31_10 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 31 & Issue == 10) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ31_10 <- renderValueBox({
    valueBox(
      paste0("30-Jul"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ31_10 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic31TS10 <- TopicsTS %>% filter(Zone == 31 & Issue == 10) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic31TS10, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  #####-----32
  
  output$CountZ32_1 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 32 & Issue == 1) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ32_1 <- renderValueBox({
    valueBox(
      paste0("No"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ32_1 <- renderValueBox({
    valueBox(
      paste0("2"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ32_1 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ32_1 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 32 & Issue == 1) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ32_1 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ32_1 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic32TS1 <- TopicsTS %>% filter(Zone == 32 & Issue == 1) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic32TS1, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  }) 
  
  #-----------
  
  output$CountZ32_2 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 32 & Issue == 2) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ32_2 <- renderValueBox({
    valueBox(
      paste0("NO"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ32_2 <- renderValueBox({
    valueBox(
      paste0("2nd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ32_2 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ32_2 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 32 & Issue == 2) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ32_2 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ32_2 <- renderPlotly({
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic32TS2 <- TopicsTS %>% filter(Zone == 32 & Issue == 2) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic32TS2, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 2")
    
  })
  
  #---
  
  output$CountZ32_3 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 32 & Issue == 3) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ32_3 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ32_3 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ32_3 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ32_3 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 32 & Issue == 3) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ32_3 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ32_3 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic32TS3 <- TopicsTS %>% filter(Zone == 32 & Issue == 3) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic32TS3, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  
  #------
  
  
  output$CountZ32_4 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 32 & Issue == 4) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ32_4 <- renderValueBox({
    valueBox(
      paste0("No"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ32_4 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ32_4 <- renderValueBox({
    valueBox(
      paste0("Q "), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ32_4 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 32 & Issue == 4) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ32_4 <- renderValueBox({
    valueBox(
      paste0("70132"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ32_4 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic32TS4 <- TopicsTS %>% filter(Zone == 32 & Issue == 4) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic32TS4, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  #-----
  
  output$CountZ32_5 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 32 & Issue == 5) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ32_5 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ32_5 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ32_5 <- renderValueBox({
    valueBox(
      paste0("RFC 61930"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ32_5 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 32 & Issue == 5) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ32_5 <- renderValueBox({
    valueBox(
      paste0("27-Aug"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ32_5 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic32TS5 <- TopicsTS %>% filter(Zone == 32 & Issue == 5) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic32TS5, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  output$CountZ32_6 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 32 & Issue == 6) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ32_6 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ32_6 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ32_6 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ32_6 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 32 & Issue == 6) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ32_6 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ32_6 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic32TS6 <- TopicsTS %>% filter(Zone == 32 & Issue == 6) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic32TS6, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  
  output$CountZ32_7 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 32 & Issue == 7) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ32_7 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ32_7 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ32_7 <- renderValueBox({
    valueBox(
      paste0("Q 4442"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ32_7 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 32 & Issue == 7) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ32_7 <- renderValueBox({
    valueBox(
      paste0("23-Jul"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ32_7 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic32TS7 <- TopicsTS %>% filter(Zone == 32 & Issue == 7) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic32TS7, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  output$CountZ32_7 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 32 & Issue == 7) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ32_7 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ32_7 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ32_7 <- renderValueBox({
    valueBox(
      paste0("Q 4442"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ32_7 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 32 & Issue == 7) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ32_7 <- renderValueBox({
    valueBox(
      paste0("23-Jul"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ32_7 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic32TS7 <- TopicsTS %>% filter(Zone == 32 & Issue == 7) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic32TS7, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  output$CountZ32_8 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 32 & Issue == 8) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ32_8 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ32_8 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ32_8 <- renderValueBox({
    valueBox(
      paste0("Q"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ32_8 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 32 & Issue == 8) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ32_8 <- renderValueBox({
    valueBox(
      paste0("13 Jul"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ32_8 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic32TS8 <- TopicsTS %>% filter(Zone == 32 & Issue == 8) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic32TS8, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  
  output$CountZ32_9 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 32 & Issue == 9) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ32_9 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ32_9 <- renderValueBox({
    valueBox(
      paste0("2nd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ32_9 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ32_9 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 32 & Issue == 9) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ32_9 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ32_9 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic32TS9 <- TopicsTS %>% filter(Zone == 32 & Issue == 9) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic32TS9, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  
  output$CountZ32_10 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 32 & Issue == 10) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ32_10 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ32_10 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ32_10 <- renderValueBox({
    valueBox(
      paste0("Q 4412"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ32_10 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 32 & Issue == 10) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ32_10 <- renderValueBox({
    valueBox(
      paste0("30-Jul"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ32_10 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic32TS10 <- TopicsTS %>% filter(Zone == 32 & Issue == 10) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic32TS10, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  #----
  output$CountZ32_11 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 32 & Issue == 11) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ32_11 <- renderValueBox({
    valueBox(
      paste0("No"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ32_11 <- renderValueBox({
    valueBox(
      paste0("2nd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ32_11 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ32_11 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 32 & Issue == 11) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ32_11 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ32_11 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic32TS11 <- TopicsTS %>% filter(Zone == 32 & Issue == 11) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic32TS11, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 11")
    
  }) 
  
  
  
  #####----------33
  
  output$CountZ33_1 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 33 & Issue == 1) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ33_1 <- renderValueBox({
    valueBox(
      paste0("No"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ33_1 <- renderValueBox({
    valueBox(
      paste0("2"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ33_1 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ33_1 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 33 & Issue == 1) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ33_1 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ33_1 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic33TS1 <- TopicsTS %>% filter(Zone == 33 & Issue == 1) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic33TS1, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  }) 
  
  #-----------
  
  output$CountZ33_2 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 33 & Issue == 2) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ33_2 <- renderValueBox({
    valueBox(
      paste0("NO"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ33_2 <- renderValueBox({
    valueBox(
      paste0("2nd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ33_2 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ33_2 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 33 & Issue == 2) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ33_2 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ33_2 <- renderPlotly({
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic33TS2 <- TopicsTS %>% filter(Zone == 33 & Issue == 2) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic33TS2, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 2")
    
  })
  
  #---
  
  output$CountZ33_3 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 33 & Issue == 3) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ33_3 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ33_3 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ33_3 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ33_3 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 33 & Issue == 3) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ33_3 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ33_3 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic33TS3 <- TopicsTS %>% filter(Zone == 33 & Issue == 3) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic33TS3, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  
  #------
  
  
  output$CountZ33_4 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 33 & Issue == 4) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ33_4 <- renderValueBox({
    valueBox(
      paste0("No"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ33_4 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ33_4 <- renderValueBox({
    valueBox(
      paste0("Q "), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ33_4 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 33 & Issue == 4) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ33_4 <- renderValueBox({
    valueBox(
      paste0("70133"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ33_4 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic33TS4 <- TopicsTS %>% filter(Zone == 33 & Issue == 4) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic33TS4, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  #-----
  
  output$CountZ33_5 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 33 & Issue == 5) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ33_5 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ33_5 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ33_5 <- renderValueBox({
    valueBox(
      paste0("RFC 61930"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ33_5 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 33 & Issue == 5) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ33_5 <- renderValueBox({
    valueBox(
      paste0("27-Aug"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ33_5 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic33TS5 <- TopicsTS %>% filter(Zone == 33 & Issue == 5) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic33TS5, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  output$CountZ33_6 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 33 & Issue == 6) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ33_6 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ33_6 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ33_6 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ33_6 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 33 & Issue == 6) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ33_6 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ33_6 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic33TS6 <- TopicsTS %>% filter(Zone == 33 & Issue == 6) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic33TS6, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  
  output$CountZ33_7 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 33 & Issue == 7) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ33_7 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ33_7 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ33_7 <- renderValueBox({
    valueBox(
      paste0("Q 4442"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ33_7 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 33 & Issue == 7) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ33_7 <- renderValueBox({
    valueBox(
      paste0("23-Jul"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ33_7 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic33TS7 <- TopicsTS %>% filter(Zone == 33 & Issue == 7) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic33TS7, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  output$CountZ33_7 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 33 & Issue == 7) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ33_7 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ33_7 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ33_7 <- renderValueBox({
    valueBox(
      paste0("Q 4442"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ33_7 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 33 & Issue == 7) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ33_7 <- renderValueBox({
    valueBox(
      paste0("23-Jul"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ33_7 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic33TS7 <- TopicsTS %>% filter(Zone == 33 & Issue == 7) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic33TS7, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  #----------34
  
  
  output$CountZ34_1 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 34 & Issue == 1) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ34_1 <- renderValueBox({
    valueBox(
      paste0("No"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ34_1 <- renderValueBox({
    valueBox(
      paste0("2"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ34_1 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ34_1 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 34 & Issue == 1) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ34_1 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ34_1 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic34TS1 <- TopicsTS %>% filter(Zone == 34 & Issue == 1) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic34TS1, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  }) 
  
  #-----------
  
  output$CountZ34_2 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 34 & Issue == 2) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ34_2 <- renderValueBox({
    valueBox(
      paste0("NO"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ34_2 <- renderValueBox({
    valueBox(
      paste0("2nd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ34_2 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ34_2 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 34 & Issue == 2) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ34_2 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ34_2 <- renderPlotly({
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic34TS2 <- TopicsTS %>% filter(Zone == 34 & Issue == 2) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic34TS2, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 2")
    
  })
  
  #---
  
  output$CountZ34_3 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 34 & Issue == 3) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ34_3 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ34_3 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ34_3 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ34_3 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 34 & Issue == 3) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ34_3 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ34_3 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic34TS3 <- TopicsTS %>% filter(Zone == 34 & Issue == 3) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic34TS3, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  
  #------
  
  
  output$CountZ34_4 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 34 & Issue == 4) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ34_4 <- renderValueBox({
    valueBox(
      paste0("No"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ34_4 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ34_4 <- renderValueBox({
    valueBox(
      paste0("Q "), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ34_4 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 34 & Issue == 4) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ34_4 <- renderValueBox({
    valueBox(
      paste0("70134"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ34_4 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic34TS4 <- TopicsTS %>% filter(Zone == 34 & Issue == 4) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic34TS4, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  #-----
  
  output$CountZ34_5 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 34 & Issue == 5) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ34_5 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ34_5 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ34_5 <- renderValueBox({
    valueBox(
      paste0("RFC 61930"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ34_5 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 34 & Issue == 5) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ34_5 <- renderValueBox({
    valueBox(
      paste0("27-Aug"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ34_5 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic34TS5 <- TopicsTS %>% filter(Zone == 34 & Issue == 5) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic34TS5, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  output$CountZ34_6 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 34 & Issue == 6) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ34_6 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ34_6 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ34_6 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ34_6 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 34 & Issue == 6) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ34_6 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ34_6 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic34TS6 <- TopicsTS %>% filter(Zone == 34 & Issue == 6) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic34TS6, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  
  output$CountZ34_7 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 34 & Issue == 7) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ34_7 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ34_7 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ34_7 <- renderValueBox({
    valueBox(
      paste0("Q 4442"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ34_7 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 34 & Issue == 7) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ34_7 <- renderValueBox({
    valueBox(
      paste0("23-Jul"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ34_7 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic34TS7 <- TopicsTS %>% filter(Zone == 34 & Issue == 7) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic34TS7, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  output$CountZ34_7 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 34 & Issue == 7) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ34_7 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ34_7 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ34_7 <- renderValueBox({
    valueBox(
      paste0("Q 4442"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ34_7 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 34 & Issue == 7) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ34_7 <- renderValueBox({
    valueBox(
      paste0("23-Jul"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ34_7 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic34TS7 <- TopicsTS %>% filter(Zone == 34 & Issue == 7) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic34TS7, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  output$CountZ34_8 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 34 & Issue == 8) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ34_8 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ34_8 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ34_8 <- renderValueBox({
    valueBox(
      paste0("Q"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ34_8 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 34 & Issue == 8) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ34_8 <- renderValueBox({
    valueBox(
      paste0("13 Jul"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ34_8 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic34TS8 <- TopicsTS %>% filter(Zone == 34 & Issue == 8) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic34TS8, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  #----------
  
  
  output$CountZ4_1 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 4 & Issue == 1) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ4_1 <- renderValueBox({
    valueBox(
      paste0("No"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ4_1 <- renderValueBox({
    valueBox(
      paste0("2"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ4_1 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ4_1 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 4 & Issue == 1) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ4_1 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ4_1 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic4TS1 <- TopicsTS %>% filter(Zone == 4 & Issue == 1) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic4TS1, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  }) 
  
  #-----------
  
  output$CountZ4_2 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 4 & Issue == 2) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ4_2 <- renderValueBox({
    valueBox(
      paste0("NO"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ4_2 <- renderValueBox({
    valueBox(
      paste0("2nd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ4_2 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ4_2 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 4 & Issue == 2) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ4_2 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ4_2 <- renderPlotly({
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic4TS2 <- TopicsTS %>% filter(Zone == 4 & Issue == 2) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic4TS2, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 2")
    
  })
  
  #---
  
  output$CountZ4_3 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 4 & Issue == 3) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ4_3 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ4_3 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ4_3 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ4_3 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 4 & Issue == 3) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ4_3 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ4_3 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic4TS3 <- TopicsTS %>% filter(Zone == 4 & Issue == 3) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic4TS3, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  
  #------
  
  
  output$CountZ4_4 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 4 & Issue == 4) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ4_4 <- renderValueBox({
    valueBox(
      paste0("No"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ4_4 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ4_4 <- renderValueBox({
    valueBox(
      paste0("Q "), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ4_4 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 4 & Issue == 4) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ4_4 <- renderValueBox({
    valueBox(
      paste0("7014"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ4_4 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic4TS4 <- TopicsTS %>% filter(Zone == 4 & Issue == 4) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic4TS4, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  #-----
  
  output$CountZ4_5 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 4 & Issue == 5) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ4_5 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ4_5 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ4_5 <- renderValueBox({
    valueBox(
      paste0("RFC 61930"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ4_5 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 4 & Issue == 5) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ4_5 <- renderValueBox({
    valueBox(
      paste0("27-Aug"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ4_5 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic4TS5 <- TopicsTS %>% filter(Zone == 4 & Issue == 5) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic4TS5, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  output$CountZ4_6 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 4 & Issue == 6) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ4_6 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ4_6 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ4_6 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ4_6 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 4 & Issue == 6) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ4_6 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ4_6 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic4TS6 <- TopicsTS %>% filter(Zone == 4 & Issue == 6) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic4TS6, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  
  output$CountZ4_7 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 4 & Issue == 7) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ4_7 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ4_7 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ4_7 <- renderValueBox({
    valueBox(
      paste0("Q 4442"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ4_7 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 4 & Issue == 7) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ4_7 <- renderValueBox({
    valueBox(
      paste0("23-Jul"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ4_7 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic4TS7 <- TopicsTS %>% filter(Zone == 4 & Issue == 7) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic4TS7, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  output$CountZ4_7 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 4 & Issue == 7) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ4_7 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ4_7 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ4_7 <- renderValueBox({
    valueBox(
      paste0("Q 4442"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ4_7 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 4 & Issue == 7) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ4_7 <- renderValueBox({
    valueBox(
      paste0("23-Jul"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ4_7 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic4TS7 <- TopicsTS %>% filter(Zone == 4 & Issue == 7) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic4TS7, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  output$CountZ4_8 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 4 & Issue == 8) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ4_8 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ4_8 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ4_8 <- renderValueBox({
    valueBox(
      paste0("Q"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ4_8 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 4 & Issue == 8) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ4_8 <- renderValueBox({
    valueBox(
      paste0("13 Jul"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ4_8 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic4TS8 <- TopicsTS %>% filter(Zone == 4 & Issue == 8) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic4TS8, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  
  output$CountZ4_9 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == 4 & Issue == 9) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZ4_9 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ4_9 <- renderValueBox({
    valueBox(
      paste0("2nd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ4_9 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ4_9 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == 4 & Issue == 9) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ4_9 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZ4_9 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    Topic4TS9 <- TopicsTS %>% filter(Zone == 4 & Issue == 9) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(Topic4TS9, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  #-----------------------------
  
  
  output$CountZMTS_1 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == "MTS" & Issue == 1) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZMTS_1 <- renderValueBox({
    valueBox(
      paste0("No"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZMTS_1 <- renderValueBox({
    valueBox(
      paste0("2"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZMTS_1 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZMTS_1 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == "MTS" & Issue == 1) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZMTS_1 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZMTS_1 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    TopicMTSTS1 <- TopicsTS %>% filter(Zone == "MTS" & Issue == 1) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(TopicMTSTS1, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  }) 
  
  #-----------
  
  output$CountZMTS_2 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == "MTS" & Issue == 2) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZMTS_2 <- renderValueBox({
    valueBox(
      paste0("NO"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZMTS_2 <- renderValueBox({
    valueBox(
      paste0("2nd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZMTS_2 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZMTS_2 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == "MTS" & Issue == 2) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZMTS_2 <- renderValueBox({
    valueBox(
      paste0("N/A"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZMTS_2 <- renderPlotly({
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    TopicMTSTS2 <- TopicsTS %>% filter(Zone == "MTS" & Issue == 2) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(TopicMTSTS2, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 2")
    
  })
  
  #---
  
  output$CountZMTS_3 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == "MTS" & Issue == 3) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZMTS_3 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZMTS_3 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZMTS_3 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZMTS_3 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == "MTS" & Issue == 3) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZMTS_3 <- renderValueBox({
    valueBox(
      paste0("Closed"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZMTS_3 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    TopicMTSTS3 <- TopicsTS %>% filter(Zone == "MTS" & Issue == 3) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(TopicMTSTS3, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  
  #------
  
  
  output$CountZMTS_4 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == "MTS" & Issue == 4) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZMTS_4 <- renderValueBox({
    valueBox(
      paste0("No"), "It has not an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZMTS_4 <- renderValueBox({
    valueBox(
      paste0("3rd"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZMTS_4 <- renderValueBox({
    valueBox(
      paste0("Q "), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZMTS_4 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == "MTS" & Issue == 4) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZMTS_4 <- renderValueBox({
    valueBox(
      paste0("70115"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZMTS_4 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    TopicMTSTS4 <- TopicsTS %>% filter(Zone == "MTS" & Issue == 4) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(TopicMTSTS4, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  #-----
  
  output$CountZMTS_5 <- renderValueBox({
    valueBox(
      paste0(TopicsTS %>% filter(Zone == "MTS" & Issue == 5) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$ActionZMTS_5 <- renderValueBox({
    valueBox(
      paste0("Yes"), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZMTS_5 <- renderValueBox({
    valueBox(
      paste0("4th"), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZMTS_5 <- renderValueBox({
    valueBox(
      paste0("RFC 61930"), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZMTS_5 <- renderValueBox({
    valueBox(
      paste0((TopicsTS %>% filter(Zone == "MTS" & Issue == 5) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZMTS_5 <- renderValueBox({
    valueBox(
      paste0("27-Aug"), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$STTopicZMTS_5 <- renderPlotly({
    
    
    
    TopicsTS$Date <- as.factor(TopicsTS$Date)
    
    TopicMTSTS5 <- TopicsTS %>% filter(Zone == "MTS" & Issue == 5) %>% group_by(Date) %>% summarize(count = n())
    
    plot_ly(TopicMTSTS5, x = ~Date, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topic 1")
    
  })
  
  
  
  
  output$Topics1 <- renderPlotly({
    
    
    TopicsTS <- read.csv("./ML/Hist_Topics.csv") 
    
    TopicsTS$Issue <- as.factor(TopicsTS$Issue)
    TopicTS <- TopicsTS %>% filter(Zone == 1) %>% group_by(Issue) %>% summarize(count = n())
    
    plot_ly(TopicTS, x = ~Issue, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topics")
    
    
  })
  
  output$Topics2 <- renderPlotly({
    
    TopicsTS <- read.csv("./ML/Hist_Topics.csv") 
    TopicsTS$Issue <- as.factor(TopicsTS$Issue)
    TopicTS <- TopicsTS %>% filter(Zone == 2) %>% group_by(Issue) %>% summarize(count = n())
    
    plot_ly(TopicTS, x = ~Issue, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topics")
    
    
  })
  
  output$Topics31 <- renderPlotly({
    
    TopicsTS <- read.csv("./ML/Hist_Topics.csv") 
    TopicsTS$Issue <- as.factor(TopicsTS$Issue)
    TopicTS <- TopicsTS %>% filter(Zone == 31) %>% group_by(Issue) %>% summarize(count = n())
    
    plot_ly(TopicTS, x = ~Issue, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topics")
    
    
  })
  
  output$Topics32 <- renderPlotly({
    
    TopicsTS <- read.csv("./ML/Hist_Topics.csv") 
    TopicsTS$Issue <- as.factor(TopicsTS$Issue)
    TopicTS <- TopicsTS %>% filter(Zone == 32) %>% group_by(Issue) %>% summarize(count = n())
    
    plot_ly(TopicTS, x = ~Issue, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topics")
    
    
  })
  
  output$Topics33 <- renderPlotly({
    
    TopicsTS <- read.csv("./ML/Hist_Topics.csv") 
    TopicsTS$Issue <- as.factor(TopicsTS$Issue)
    TopicTS <- TopicsTS %>% filter(Zone == 33) %>% group_by(Issue) %>% summarize(count = n())
    
    plot_ly(TopicTS, x = ~Issue, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topics")
    
    
  })
  
  output$Topics34 <- renderPlotly({
    
    TopicsTS <- read.csv("./ML/Hist_Topics.csv") 
    TopicsTS$Issue <- as.factor(TopicsTS$Issue)
    TopicTS <- TopicsTS %>% filter(Zone == 34) %>% group_by(Issue) %>% summarize(count = n())
    
    plot_ly(TopicTS, x = ~Issue, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topics")
    
    
  })
  
  output$Topics4 <- renderPlotly({
    
    TopicsTS <- read.csv("./ML/Hist_Topics.csv") 
    TopicsTS$Issue <- as.factor(TopicsTS$Issue)
    TopicTS <- TopicsTS %>% filter(Zone == 4) %>% group_by(Issue) %>% summarize(count = n())
    
    plot_ly(TopicTS, x = ~Issue, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topics")
    
    
  })
  
  output$TopicsMTS <- renderPlotly({
    
    TopicsTS <- read.csv("./ML/Hist_Topics.csv") 
    TopicsTS$Issue <- as.factor(TopicsTS$Issue)
    TopicTS <- TopicsTS %>% filter(Zone == "MTS") %>% group_by(Issue) %>% summarize(count = n())
    
    plot_ly(TopicTS, x = ~Issue, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topics")
    
    
  })
  
  #---------------
  Topics1_PU2 <- read_excel("./ML_PU2/Z1.xlsx")
  Issues1_PU2 <- read_excel("./ML_PU2/Issues_PU2.xlsx", 
                       sheet = "Z1")
  
  IssueZ1_1PU2 <- Issues1_PU2 %>% filter(Issue == 1)
  output$CountZ1_1PU2 <- renderValueBox({
    valueBox(
      paste0(Topics1_PU2 %>% filter(Issue == 1) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$Topics1PU2 <- renderPlotly({
    
    
    TopicsTS <- Topics1_PU2 %>% group_by(Issue) %>% summarize(count = n())
    TopicsTS$Issue <- as.factor(TopicsTS$Issue)
    plot_ly(TopicsTS, x = ~Issue, y = ~count, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)), 
            hoverinfo = 'text',
            text = ~paste(count, ' RNCs')
    ) %>% layout(title = "Topics")
    
    
  })
  
  
  output$ActionZ1_1PU2 <- renderValueBox({
    if(IssueZ1_1PU2$Phase == 3){
      Action <- "Yes"
    }else if(IssueZ1_1PU2$Phase == 4){
      Action <- "Closed"
    }else{
      Action <- "No"
    }
    
    valueBox(
      paste0(Action), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ1_1PU2 <- renderValueBox({
    valueBox(
      paste0(IssueZ1_1PU2$Phase), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ1_1PU2 <- renderValueBox({
    valueBox(
      paste0(IssueZ1_1PU2$Ref), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ1_1PU2 <- renderValueBox({
    valueBox(
      paste0((Topics1_PU2 %>% filter(Issue == 1) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ1_1PU2 <- renderValueBox({
    valueBox(
      paste0(IssueZ1_1PU2$ECD), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  #--
  IssueZ1_2PU2 <- Issues1_PU2 %>% filter(Issue == 2)
  
  output$CountZ1_2PU2 <- renderValueBox({
    valueBox(
      paste0(Topics1_PU2 %>% filter(Issue == 2) %>% nrow()), "RNCs This Year", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  
  output$ActionZ1_2PU2 <- renderValueBox({
    if(IssueZ1_2PU2$Phase == 3){
      Action <- "Yes"
    }else if(IssueZ1_2PU2$Phase == 4){
      Action <- "Closed"
    }else{
      Action <- "No"
    }
    
    valueBox(
      paste0(Action), "It has an Action", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$PhaseZ1_2PU2 <- renderValueBox({
    valueBox(
      paste0(IssueZ1_2PU2$Phase), "Phase", icon = icon("list"),
      color = "green"
    )
  })
  
  output$ReferenceZ1_2PU2 <- renderValueBox({
    valueBox(
      paste0(IssueZ1_2PU2$Ref), "As Reference", icon = icon("list"),
      color = "red"
    )
  })
  
  output$CostZ1_2PU2 <- renderValueBox({
    valueBox(
      paste0((Topics1_PU2 %>% filter(Issue == 2) %>% nrow())*500), "USD, Total Cost", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$ECDZ1_2PU2 <- renderValueBox({
    valueBox(
      paste0(IssueZ1_2PU2$ECD), "As ECD", icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  #-----------
  
}

