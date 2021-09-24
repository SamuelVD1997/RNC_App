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
library(DT)
library(rintrojs)
library(shinyBS)
library(shinyjs)
library(lubridate)
fluid_design <- function(id, w, x, y, z) {
  fluidRow(div(
    id = id,
    column(width = 6,
           uiOutput(w),
           uiOutput(y)),
    column(width = 6,
           uiOutput(x),
           uiOutput(z))
  ))
}

Master <- read_excel("Raw_Data.xlsx", 
                     sheet = "Data")

RNC <- Master

if (format(Sys.Date(),"%A") == "Monday") {
  Fecha <- format(Sys.Date()-3,"%Y-%m-%d")
} else {
  Fecha <- as.Date(format(Sys.Date()-2,"%Y-%m-%d"))
}

ui <- dashboardPage(
  dashboardHeader(title = "Methods RNCs"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      menuItem(
        "Analysys",
        tabName = "data_analysis",
        icon = icon("signal"),
        menuItem(
          "PU11",
          tabName = "APU11",
          menuSubItem("General", tabName = "General"),
          menuSubItem("Zone 1", tabName = "Zone1"),
          menuSubItem("Zone 2", tabName = "Zone2"),
          menuSubItem("Zone 3.1", tabName = "Zone31"),
          menuSubItem("Zone 3.2", tabName = "Zone32"),
          menuSubItem("Zone 3.3", tabName = "Zone33"),
          menuSubItem("Zone 3.4", tabName = "Zone34"),
          menuSubItem("Zone 4", tabName = "Zone4"),
          menuSubItem("Zone 5", tabName = "Zone5"),
          menuSubItem("MTS", tabName = "MTS")
        ),
        menuItem(
          "PU2",
          tabName = "APU2",
          menuSubItem("General", tabName = "GeneralPU2"),
          menuSubItem("Zone 1", tabName = "Zone1PU2"),
          menuSubItem("Zone 2", tabName = "Zone2PU2"),
          menuSubItem("Zone 3", tabName = "Zone3PU2"),
          menuSubItem("Tailcone 5/6", tabName = "TC56"),
          menuSubItem("Tailcone 7/8", tabName = "TC78"),
          menuSubItem("Slats", tabName = "Slats")
        )
      ),
      menuItem("Data", tabName = "view_data", icon = icon("stream")),
      menuItem("About", tabName = "tab_about", icon = icon("info"))
    )
    
  ),
  dashboardBody(
    useShinyjs(),
    introjsUI(),
    tabItems(
      # First tab content
      tabItem(
        tabName = "dashboard",
        
        h3(paste0("Yesterday Info. (", Fecha, ")")),
        
        fluidRow(
          valueBoxOutput("YesterdayRNCs"),
          valueBoxOutput("RNC1000"),
          valueBoxOutput("RepIssues"),
          valueBoxOutput("OneoffIssues"),
          valueBoxOutput("TotalCost"),
          valueBoxOutput("TotalRNCs")
        ),
        
        h3("Time Series."),
        
        fluidRow(
          # Frontpage - tweet volume plots - start ----------------------------------
          tabBox(
            width = 12,
            tabPanel(
              status = "primary",
              title = "RNCs Volume",
              plotlyOutput("plot_RNC_volume", height = "250px")
              
            ),
            tabPanel(
              status = "success",
              title = "RNCs per Type",
              plotlyOutput("plot_RNC_per_Type")
              
            )
          )
        ),
        # Frontpage - tweet volume plots - end ------------------------------------),
        
        h3("Map."),
        
        fluidRow(column(3, leafletOutput("mymap2")),
                 column(9, leafletOutput("mymap"))
        ),
        
        h3("Details."),
        
        fluidRow(
          # Frontpage - tweet volume plots - start ----------------------------------
          tabBox(
            width = 12,
            tabPanel(
              status = "primary",
              title = "PU11",
              
              fluidRow(
                column(3, plotlyOutput("plot_RNC1_PU11")),
                column(3, plotlyOutput("plot_RNC2_PU11")),
                column(3, plotlyOutput("plot_RNC3_PU11")),
                column(3, plotlyOutput("plot_RNC4_PU11"))
                
              )
              
              
            ),
            tabPanel(
              status = "success",
              title = "PU2",
              box(
                width = 12,
                column(4, plotlyOutput("plot_RNC2_PU2")),
                column(4, plotlyOutput("plot_RNC3_PU2")),
                column(4, plotlyOutput("plot_RNC4_PU2"))
              )
              
              
            ),
            tabPanel(
              status = "success",
              title = "PU1",
              box(
                width = 12,
                column(4, plotlyOutput("plot_RNC2_PU1")),
                column(4, plotlyOutput("plot_RNC3_PU1")),
                column(4, plotlyOutput("plot_RNC4_PU1"))
              )
              
            ),
            tabPanel(
              status = "success",
              title = "PU10",
              box(
                width = 12,
                column(4, plotlyOutput("plot_RNC2_PU10")),
                column(4, plotlyOutput("plot_RNC3_PU10")),
                column(4, plotlyOutput("plot_RNC4_PU10"))
              )
              
              
            )
          )
          # Frontpage - tweet volume plots - end ------------------------------------),
        ),
        
        h3("Data."),
        
        fluidRow(
          # Frontpage - tweet volume plots - start ----------------------------------
          tabBox(
            width = 12,
            tabPanel(status = "primary",
                     title = "PU11",
                     
                     DTOutput("DTRNCPU11")),
            tabPanel(status = "success",
                     title = "PU2",
                     DTOutput("DTRNCPU2")),
            tabPanel(status = "success",
                     title = "PU1",
                     
                     DTOutput("DTRNCPU1")),
            tabPanel(status = "success",
                     title = "PU10",
                     
                     DTOutput("DTRNCPU10"))
          )
          # Frontpage - tweet volume plots - end ------------------------------------))
        )
      ),
      
      tabItem(
        tabName = "General",
        h2("Last Month Results"),
        h3("Time Series"),
        plotlyOutput("TS", height = "250px"),
        h3("Graphs"),
        
        box(
          width = 12,
          tabBox(
            width = 12,
            id = "tabBox_next_previous",
            tabPanel("Graph 1",
                     plotlyOutput("WCG")),
            tabPanel(
              "Graph 2",
              column(4, plotlyOutput("ZonesG")),
              column(4, plotlyOutput("NCRTypeG")),
              column(4, plotlyOutput("PartsG"))
            ),
            tabPanel("Graph 3",
                     column(4, plotlyOutput("ACG")),
                     column(8, plotlyOutput("QAG"))),
            tabPanel(
              "Graph 4",
              column(4, plotlyOutput("DefectG")),
              column(4, plotlyOutput("DayG")),
              column(4, plotlyOutput("WBG"))
            ),
            
            tabPanel(
              "Graph 5",
              column(4, plotlyOutput("SkillG")),
              column(4, plotlyOutput("WorkerG")),
              column(4, plotlyOutput("MethG"))
              
            ),
            
            tabPanel("Graph 6",
                     column(4, plotlyOutput("DispG"))),
            
            tags$script(
              "
                       $('body').mouseover(function() {
                       list_tabs=[];
                       $('#tabBox_next_previous li a').each(function(){
                       list_tabs.push($(this).html())
                       });
                       Shiny.onInputChange('List_of_tab', list_tabs);})
                       "
            )
          ),
          uiOutput("Next_Previous")
          
        )
      ),
      
      tabItem(
        tabName = "Zone1",
        h1("Last Month Results"),
        h2("Time Series"),
        plotlyOutput("TS1", height = "250px"),
        h2("Topics Details"),
        plotlyOutput("Topics1", height = "300px"),
        br(),
        
        #----
        
        fluidRow(
          box(width = 12,
              tabBox(
                width = 12,
                tabPanel(title = "Topic 1.",
                         status = "primary",
                         box(width = 12,
                             h1("Gap on clamps of Cargo Door"),
                             h1("     "),
                             h1("     "),
                             fluidRow(
                               
                               column(6,
                                      
                                      fluidRow(
                                        valueBoxOutput("CountZ1_1"),
                                        valueBoxOutput("CostZ1_1")
                                      ),
                                      
                                      fluidRow(
                                        valueBoxOutput("ActionZ1_1"),
                                        valueBoxOutput("PhaseZ1_1")
                                      ),
                                      fluidRow(
                                        valueBoxOutput("ReferenceZ1_1"),
                                        valueBoxOutput("ECDZ1_1")
                                      )
                               ),
                               
                               column(6,
                                      img(
                                        class = "img-responsive img-rounded center-block",
                                        src = 'Q321003378-1.PNG',
                                        height = '100%',
                                        width = '100%'
                                      )
                                      
                               )
                               
                             ),
                             
                             h1("     "),
                             h1("     "),
                             h1("     "),
                             h1("     "),
                             
                             
                             plotlyOutput("STTopicZ1_1", height = "250px")
                             
                         )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 2.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Low clearance. Tube and Bulkhead"),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        
                        column(6,
                               
                               fluidRow(
                                 valueBoxOutput("CountZ1_2"),
                                 valueBoxOutput("CostZ1_2")
                               ),
                               
                               fluidRow(
                                 valueBoxOutput("ActionZ1_2"),
                                 valueBoxOutput("PhaseZ1_2")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ1_2"),
                                 valueBoxOutput("ECDZ1_2")
                               )
                        ),
                        
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000806-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                               
                        )
                        
                      ),
                      
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      
                      
                      plotlyOutput("STTopicZ1_2", height = "250px")
                      
                  ))
                
              )),
          
          #---------
          h2("Histograms"),
          
          box(
            width = 12,
            tabBox(
              width = 12,
              tabPanel(
                "Graphs 1",
                column(4, plotlyOutput("WC1")),
                column(4, plotlyOutput("AC1")),
                column(4, plotlyOutput("Part1"))
              ),
              tabPanel(
                "Graphs 2",
                column(4, plotlyOutput("QA1")),
                column(4, plotlyOutput("Defect1")),
                column(4, plotlyOutput("Disp1"))
              ),
              tabPanel(
                "Graphs 3",
                column(4, plotlyOutput("Day1")),
                column(4, plotlyOutput("WB1")),
                column(4, plotlyOutput("Skill1"))
              ),
              tabPanel("Graphs 4",
                       column(4, plotlyOutput("Worker1")))
            )
          )
          
          
          
        )),
      
      # S Zone 2
      
      
      tabItem(
        tabName = "Zone2",
        h2("Last Month Results"),
        h3("Time Series"),
        plotlyOutput("TS2", height = "250px"),
        h2("Topics Details"),
        plotlyOutput("Topics2", height = "300px"),
        br(),
        
        fluidRow(
          box(width = 12,
              tabBox(
                width = 12,
                tabPanel(title = "Topic 1.",
                         status = "primary",
                         box(width = 12,
                             h1("Cleats with fasteners not per dwg. Due access"),
                             h1("     "),
                             h1("     "),
                             fluidRow(
                               column(6,
                                     fluidRow(
                                        valueBoxOutput("CountZ2_1"),
                                        valueBoxOutput("CostZ2_1")
                                      ),
                                      fluidRow(
                                        valueBoxOutput("ActionZ2_1"),
                                        valueBoxOutput("PhaseZ2_1")
                                      ),
                                      fluidRow(
                                        valueBoxOutput("ReferenceZ2_1"),
                                        valueBoxOutput("ECDZ2_1")
                                      )
                               ),

                               column(6,
                                      img(
                                        class = "img-responsive img-rounded center-block",
                                        src = 'Q321000435-1.PNG',
                                        height = '100%',
                                        width = '100%'
                                      )

                               )

                             ),

                             h1("     "),
                             h1("     "),
                             h1("     "),
                             h1("     "),


                             plotlyOutput("STTopicZ2_1", height = "250px")

                         )),

                tabPanel(
                  status = "success",
                  title = "Topic 2.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Strap with fasteners not per dwg. Due access"),
                      h1("     "),
                      h1("     "),
                      fluidRow(

                        column(6,

                               fluidRow(
                                 valueBoxOutput("CountZ2_2"),
                                 valueBoxOutput("CostZ2_2")
                               ),

                               fluidRow(
                                 valueBoxOutput("ActionZ2_2"),
                                 valueBoxOutput("PhaseZ2_2")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ2_2"),
                                 valueBoxOutput("ECDZ2_2")
                               )
                        ),

                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321001134-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )

                        )

                      ),

                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),


                      plotlyOutput("STTopicZ2_2", height = "250px")

                  )),


                tabPanel(
                  status = "success",
                  title = "Topic 3.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Redundant holes Engine mount"),
                      h1("     "),
                      h1("     "),
                      fluidRow(

                        column(6,

                               fluidRow(
                                 valueBoxOutput("CountZ2_3"),
                                 valueBoxOutput("CostZ2_3")
                               ),

                               fluidRow(
                                 valueBoxOutput("ActionZ2_3"),
                                 valueBoxOutput("PhaseZ2_3")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ2_3"),
                                 valueBoxOutput("ECDZ2_3")
                               )
                        ),

                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321001450-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )

                        )

                      ),

                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),


                      plotlyOutput("STTopicZ2_3", height = "250px")

                  ))
              #   
              #   
              # 
              #   
              #   
              )
              ),
          
          #---------
          h2("Histograms"),
          
          box(
            width = 12,
            tabBox(
              width = 12,
              tabPanel(
                "Graphs 1",
                column(4, plotlyOutput("WC2")),
                column(4, plotlyOutput("AC2")),
                column(4, plotlyOutput("Part2"))
              ),
              tabPanel(
                "Graphs 2",
                column(4, plotlyOutput("QA2")),
                column(4, plotlyOutput("Defect2")),
                column(4, plotlyOutput("Disp2"))
              ),
              tabPanel(
                "Graphs 3",
                column(4, plotlyOutput("Day2")),
                column(4, plotlyOutput("WB2")),
                column(4, plotlyOutput("Skill2"))
              ),
              tabPanel("Graphs 4",
                       column(4, plotlyOutput("Worker2")))
            )
          )
          
          
          
        )
        
        
      ),
      
      # E Zone 2
      #S Zone 31
      
      tabItem(
        tabName = "Zone31",
        h2("Last Month Results"),
        h3("Time Series"),
        plotlyOutput("TS31", height = "250px"),
        h2("Topics Details"),
        plotlyOutput("Topics31", height = "300px"),
        br(),
        
        fluidRow(
          box(width = 12,
              tabBox(
                width = 12,
                tabPanel(title = "Topic 1.",
                         status = "primary",
                         box(width = 12,
                             h1("Low Clearamce btwn fitting an Engine Frame"),
                             h1("     "),
                             h1("     "),
                             fluidRow(
                               
                               column(6,
                                      
                                      fluidRow(
                                        valueBoxOutput("CountZ31_1"),
                                        valueBoxOutput("CostZ31_1")
                                      ),
                                      
                                      fluidRow(
                                        valueBoxOutput("ActionZ31_1"),
                                        valueBoxOutput("PhaseZ31_1")
                                      ),
                                      fluidRow(
                                        valueBoxOutput("ReferenceZ31_1"),
                                        valueBoxOutput("ECDZ31_1")
                                      )
                               ),
                               
                               column(6,
                                      img(
                                        class = "img-responsive img-rounded center-block",
                                        src = 'Q321001762-1.PNG',
                                        height = '100%',
                                        width = '100%'
                                      )
                                      
                               )
                               
                             ),
                             
                             h1("     "),
                             h1("     "),
                             h1("     "),
                             h1("     "),
                             
                             
                             plotlyOutput("STTopicZ31_1", height = "250px")
                             
                         )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 2.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Gaps when installing the floor."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                              fluidRow(
                                 valueBoxOutput("CountZ31_2"),
                                 valueBoxOutput("CostZ31_2")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ31_2"),
                                 valueBoxOutput("PhaseZ31_2")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ31_2"),
                                 valueBoxOutput("ECDZ31_2")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000316-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ31_2", height = "250px")
                  )),

                tabPanel(
                  status = "success",
                  title = "Topic 3.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Double hole at fitting of waste system."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ31_3"),
                                 valueBoxOutput("CostZ31_3")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ31_3"),
                                 valueBoxOutput("PhaseZ31_3")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ31_3"),
                                 valueBoxOutput("ECDZ31_3")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321001021-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ31_3", height = "250px")
                  )),

                tabPanel(
                  status = "success",
                  title = "Topic 4.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Mismatch btwn frame and cleat"),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ31_4"),
                                 valueBoxOutput("CostZ31_4")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ31_4"),
                                 valueBoxOutput("PhaseZ31_4")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ31_4"),
                                 valueBoxOutput("ECDZ31_4")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321001153-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ31_4", height = "250px")
                  )),

                tabPanel(
                  status = "success",
                  title = "Topic 5.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Bonding test too high."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ31_5"),
                                 valueBoxOutput("CostZ31_5")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ31_5"),
                                 valueBoxOutput("PhaseZ31_5")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ31_5"),
                                 valueBoxOutput("ECDZ31_5")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321001592-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ31_5", height = "250px")
                  )),
                tabPanel(
                  status = "success",
                  title = "Topic 6.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Exposed tooling hole"),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ31_6"),
                                 valueBoxOutput("CostZ31_6")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ31_6"),
                                 valueBoxOutput("PhaseZ31_6")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ31_6"),
                                 valueBoxOutput("ECDZ31_6")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321003584-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ31_6", height = "250px")
                  )),

                tabPanel(
                  status = "success",
                  title = "Topic 7.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Damaged Holes in bracket assy APS."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ31_7"),
                                 valueBoxOutput("CostZ31_7")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ31_7"),
                                 valueBoxOutput("PhaseZ31_7")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ31_7"),
                                 valueBoxOutput("ECDZ31_7")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321003961-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ31_7", height = "250px")
                  )),

                tabPanel(
                  status = "success",
                  title = "Topic 8.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Mislocated holes at bracket assy LP system."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ31_8"),
                                 valueBoxOutput("CostZ31_8")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ31_8"),
                                 valueBoxOutput("PhaseZ31_8")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ31_8"),
                                 valueBoxOutput("ECDZ31_8")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321010413-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ31_8", height = "250px")
                  )),

                tabPanel(
                  status = "success",
                  title = "Topic 9.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Mislocated hole btwn orbital splice and skin."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ31_9"),
                                 valueBoxOutput("CostZ31_9")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ31_9"),
                                 valueBoxOutput("PhaseZ31_9")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ31_9"),
                                 valueBoxOutput("ECDZ31_9")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321008004-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ31_9", height = "250px")
                  )),

                tabPanel(
                  status = "success",
                  title = "Topic 10.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Parts at lower skin without topcoat."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ31_10"),
                                 valueBoxOutput("CostZ31_10")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ31_10"),
                                 valueBoxOutput("PhaseZ31_10")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ31_10"),
                                 valueBoxOutput("ECDZ31_10")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321010331-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ31_10", height = "250px")
                  ))
                
              )),
          
          #---------
          h2("Histograms"),
          
          box(
            width = 12,
            tabBox(
              width = 12,
              tabPanel(
                "Graphs 1",
                column(4, plotlyOutput("WC31")),
                column(4, plotlyOutput("AC31")),
                column(4, plotlyOutput("Part31"))
              ),
              tabPanel(
                "Graphs 2",
                column(4, plotlyOutput("QA31")),
                column(4, plotlyOutput("Defect311")),
                column(4, plotlyOutput("Disp31"))
              ),
              tabPanel(
                "Graphs 3",
                column(4, plotlyOutput("Day31")),
                column(4, plotlyOutput("WB31")),
                column(4, plotlyOutput("Skill31"))
              ),
              tabPanel("Graphs 4",
                       column(4, plotlyOutput("Worker31")))
            )
          )
          
          
          
        )
        
        
        
      ),
      
      
      #E Zone 31
      
      #S Zone 32
      
      
      tabItem(
        tabName = "Zone32",
        h2("Last Month Results"),
        h3("Time Series"),
        plotlyOutput("TS32", height = "250px"),
        h2("Topics Details"),
        plotlyOutput("Topics32", height = "300px"),
        br(),
        
        fluidRow(
          box(width = 12,
              tabBox(
                width = 12,
                tabPanel(title = "Topic 1.",
                         status = "primary",
                         box(width = 12,
                             h1("Damages on flexible tubes"),
                             h1("     "),
                             h1("     "),
                             fluidRow(
                               
                               column(6,
                                      
                                      fluidRow(
                                        valueBoxOutput("CountZ32_1"),
                                        valueBoxOutput("CostZ32_1")
                                      ),
                                      
                                      fluidRow(
                                        valueBoxOutput("ActionZ32_1"),
                                        valueBoxOutput("PhaseZ32_1")
                                      ),
                                      fluidRow(
                                        valueBoxOutput("ReferenceZ32_1"),
                                        valueBoxOutput("ECDZ32_1")
                                      )
                               ),
                               
                               column(6,
                                      img(
                                        class = "img-responsive img-rounded center-block",
                                        src = 'Q321004839-1.PNG',
                                        height = '100%',
                                        width = '100%'
                                      )
                                      
                               )
                               
                             ),
                             
                             h1("     "),
                             h1("     "),
                             h1("     "),
                             h1("     "),
                             
                             
                             plotlyOutput("STTopicZ32_1", height = "250px")
                             
                         )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 2.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Hi-Lites instead of conventional due to access."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ32_2"),
                                 valueBoxOutput("CostZ32_2")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ32_2"),
                                 valueBoxOutput("PhaseZ32_2")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ32_2"),
                                 valueBoxOutput("ECDZ32_2")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000140-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ32_2", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 3.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Gap on tank structure."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ32_3"),
                                 valueBoxOutput("CostZ32_3")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ32_3"),
                                 valueBoxOutput("PhaseZ32_3")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ32_3"),
                                 valueBoxOutput("ECDZ32_3")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321004631-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ32_3", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 4.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Conflict intalling dampings of BHD."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ32_4"),
                                 valueBoxOutput("CostZ32_4")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ32_4"),
                                 valueBoxOutput("PhaseZ32_4")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ32_4"),
                                 valueBoxOutput("ECDZ32_4")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000116-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ32_4", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 5.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("OS. To clean mismatch btwn LP bracket and Panel."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ32_5"),
                                 valueBoxOutput("CostZ32_5")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ32_5"),
                                 valueBoxOutput("PhaseZ32_5")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ32_5"),
                                 valueBoxOutput("ECDZ32_5")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000180-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ32_5", height = "250px")
                  )),
                tabPanel(
                  status = "success",
                  title = "Topic 6.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Bubbles on Velcro installation."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ32_6"),
                                 valueBoxOutput("CostZ32_6")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ32_6"),
                                 valueBoxOutput("PhaseZ32_6")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ32_6"),
                                 valueBoxOutput("ECDZ32_6")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321005154-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ32_6", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 7.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Missing Fillet Seal Operation Book GX820903080."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ32_7"),
                                 valueBoxOutput("CostZ32_7")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ32_7"),
                                 valueBoxOutput("PhaseZ32_7")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ32_7"),
                                 valueBoxOutput("ECDZ32_7")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321003131-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ32_7", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 8.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Gap Btwn bracket & tube."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ32_8"),
                                 valueBoxOutput("CostZ32_8")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ32_8"),
                                 valueBoxOutput("PhaseZ32_8")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ32_8"),
                                 valueBoxOutput("ECDZ32_8")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321006963-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ32_8", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 9.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Undersize condition on flange bushing."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ32_9"),
                                 valueBoxOutput("CostZ32_9")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ32_9"),
                                 valueBoxOutput("PhaseZ32_9")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ32_9"),
                                 valueBoxOutput("ECDZ32_9")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321005486-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ32_9", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 10.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Ident tap is trapped undeneath clamp."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ32_10"),
                                 valueBoxOutput("CostZ32_10")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ32_10"),
                                 valueBoxOutput("PhaseZ32_10")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ32_10"),
                                 valueBoxOutput("ECDZ32_10")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321006421-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ32_10", height = "250px")
                  )),
                
          tabPanel(
            status = "success",
            title = "Topic 11.",
            solidHeader = TRUE,
            box(width = 12,
                h1("Washers were used to provide sufficient clearance."),
                h1("     "),
                h1("     "),
                fluidRow(
                  column(6,
                         fluidRow(
                           valueBoxOutput("CountZ32_11"),
                           valueBoxOutput("CostZ32_11")
                         ),
                         fluidRow(
                           valueBoxOutput("ActionZ32_11"),
                           valueBoxOutput("PhaseZ32_11")
                         ),
                         fluidRow(
                           valueBoxOutput("ReferenceZ32_11"),
                           valueBoxOutput("ECDZ32_11")
                         )
                  ),
                  column(6,
                         img(
                           class = "img-responsive img-rounded center-block",
                           src = 'Q321005639-1.PNG',
                           height = '100%',
                           width = '100%'
                         )
                  )
                ),
                h1("     "),
                h1("     "),
                h1("     "),
                h1("     "),
                plotlyOutput("STTopicZ32_11", height = "250px")
            ))
          
        )),
      
      
      #---------
      h2("Histograms"),
      
      box(
        width = 12,
        tabBox(
          width = 12,
          tabPanel(
            "Graphs 1",
            column(4, plotlyOutput("WC32")),
            column(4, plotlyOutput("AC32")),
            column(4, plotlyOutput("Part32"))
          ),
          tabPanel(
            "Graphs 2",
            column(4, plotlyOutput("QA32")),
            column(4, plotlyOutput("Defect321")),
            column(4, plotlyOutput("Disp32"))
          ),
          tabPanel(
            "Graphs 3",
            column(4, plotlyOutput("Day32")),
            column(4, plotlyOutput("WB32")),
            column(4, plotlyOutput("Skill32"))
          ),
          tabPanel("Graphs 4",
                   column(4, plotlyOutput("Worker32")))
        )
      )
      
      
      
    )
    

        
      ),
      
      
      #E Zone 32
      
      #S Zone 33
      
      tabItem(
        tabName = "Zone33",
        h2("Last Month Results"),
        h3("Time Series"),
        plotlyOutput("TS33", height = "250px"),
        h2("Topics Details"),
        plotlyOutput("Topics33", height = "300px"),
        br(),
        
        fluidRow(
          box(width = 12,
              tabBox(
                width = 12,
                tabPanel(title = "Topic 1.",
                         status = "primary",
                         box(width = 12,
                             h1("Stringers misaligned."),
                             h1("     "),
                             h1("     "),
                             fluidRow(
                               
                               column(6,
                                      
                                      fluidRow(
                                        valueBoxOutput("CountZ33_1"),
                                        valueBoxOutput("CostZ33_1")
                                      ),
                                      
                                      fluidRow(
                                        valueBoxOutput("ActionZ33_1"),
                                        valueBoxOutput("PhaseZ33_1")
                                      ),
                                      fluidRow(
                                        valueBoxOutput("ReferenceZ33_1"),
                                        valueBoxOutput("ECDZ33_1")
                                      )
                               ),
                               
                               column(6,
                                      img(
                                        class = "img-responsive img-rounded center-block",
                                        src = 'Q321000524-1.PNG',
                                        height = '100%',
                                        width = '100%'
                                      )
                                      
                               )
                               
                             ),
                             
                             h1("     "),
                             h1("     "),
                             h1("     "),
                             h1("     "),
                             
                             
                             plotlyOutput("STTopicZ33_1", height = "250px")
                             
                         )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 2.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Damage Holes while Joining."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ33_2"),
                                 valueBoxOutput("CostZ33_2")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ33_2"),
                                 valueBoxOutput("PhaseZ33_2")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ33_2"),
                                 valueBoxOutput("ECDZ33_2")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000243-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ33_2", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 3.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Trim of Dorsal Angle."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ33_3"),
                                 valueBoxOutput("CostZ33_3")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ33_3"),
                                 valueBoxOutput("PhaseZ33_3")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ33_3"),
                                 valueBoxOutput("ECDZ33_3")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321004862-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ33_3", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 4.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Gap in Cruciform."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ33_4"),
                                 valueBoxOutput("CostZ33_4")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ33_4"),
                                 valueBoxOutput("PhaseZ33_4")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ33_4"),
                                 valueBoxOutput("ECDZ33_4")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000663-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ33_4", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 5.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Conductivity result above."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ33_5"),
                                 valueBoxOutput("CostZ33_5")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ33_5"),
                                 valueBoxOutput("PhaseZ33_5")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ33_5"),
                                 valueBoxOutput("ECDZ33_5")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321001051-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ33_5", height = "250px")
                  )),
                tabPanel(
                  status = "success",
                  title = "Topic 6.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Gaps STGR 3-5."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ33_6"),
                                 valueBoxOutput("CostZ33_6")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ33_6"),
                                 valueBoxOutput("PhaseZ33_6")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ33_6"),
                                 valueBoxOutput("ECDZ33_6")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000886-2.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ33_6", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 7.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Gap in STGR 23."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ33_7"),
                                 valueBoxOutput("CostZ33_7")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ33_7"),
                                 valueBoxOutput("PhaseZ33_7")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ33_7"),
                                 valueBoxOutput("ECDZ33_7")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000524-4.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ33_7", height = "250px")
                  ))
          
        )),
    
    
    #---------
    h2("Histograms"),
    
    box(
      width = 12,
      tabBox(
        width = 12,
        tabPanel(
          "Graphs 1",
          column(4, plotlyOutput("WC33")),
          column(4, plotlyOutput("AC33")),
          column(4, plotlyOutput("Part33"))
        ),
        tabPanel(
          "Graphs 2",
          column(4, plotlyOutput("QA33")),
          column(4, plotlyOutput("Defect331")),
          column(4, plotlyOutput("Disp33"))
        ),
        tabPanel(
          "Graphs 3",
          column(4, plotlyOutput("Day33")),
          column(4, plotlyOutput("WB33")),
          column(4, plotlyOutput("Skill33"))
        ),
        tabPanel("Graphs 4",
                 column(4, plotlyOutput("Worker33")))
      )
    )
    
    
    
    )
    
        
      ),
      
      # S Zone 34
      tabItem(
        tabName = "Zone34",
        h2("Last Month Results"),
        h3("Time Series"),
        plotlyOutput("TS34", height = "250px"),
        h2("Topics Details"),
        plotlyOutput("Topics34", height = "300px"),
        br(),
        
        fluidRow(
          box(width = 12,
              tabBox(
                width = 12,
                tabPanel(title = "Topic 1.",
                         status = "primary",
                         box(width = 12,
                             h1("Double hole Drag Angle LH Upp."),
                             h1("     "),
                             h1("     "),
                             fluidRow(
                               
                               column(6,
                                      
                                      fluidRow(
                                        valueBoxOutput("CountZ34_1"),
                                        valueBoxOutput("CostZ34_1")
                                      ),
                                      
                                      fluidRow(
                                        valueBoxOutput("ActionZ34_1"),
                                        valueBoxOutput("PhaseZ34_1")
                                      ),
                                      fluidRow(
                                        valueBoxOutput("ReferenceZ34_1"),
                                        valueBoxOutput("ECDZ34_1")
                                      )
                               ),
                               
                               column(6,
                                      img(
                                        class = "img-responsive img-rounded center-block",
                                        src = 'Q321000156-1.PNG',
                                        height = '100%',
                                        width = '100%'
                                      )
                                      
                               )
                               
                             ),
                             
                             h1("     "),
                             h1("     "),
                             h1("     "),
                             h1("     "),
                             
                             
                             plotlyOutput("STTopicZ34_1", height = "250px")
                             
                         )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 2.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Shooting of Key Characteristics."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ34_2"),
                                 valueBoxOutput("CostZ34_2")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ34_2"),
                                 valueBoxOutput("PhaseZ34_2")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ34_2"),
                                 valueBoxOutput("ECDZ34_2")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000737-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ34_2", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 3.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Double hole Stiff CTR BOX."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ34_3"),
                                 valueBoxOutput("CostZ34_3")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ34_3"),
                                 valueBoxOutput("PhaseZ34_3")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ34_3"),
                                 valueBoxOutput("ECDZ34_3")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000233-2.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ34_3", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 4.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Hole with Low ED. Doubler, Pylon."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ34_4"),
                                 valueBoxOutput("CostZ34_4")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ34_4"),
                                 valueBoxOutput("PhaseZ34_4")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ34_4"),
                                 valueBoxOutput("ECDZ34_4")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000281-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ34_4", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 5.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Not enough washers to provide sufficient clearance."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ34_5"),
                                 valueBoxOutput("CostZ34_5")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ34_5"),
                                 valueBoxOutput("PhaseZ34_5")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ34_5"),
                                 valueBoxOutput("ECDZ34_5")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000395-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ34_5", height = "250px")
                  )),
                tabPanel(
                  status = "success",
                  title = "Topic 6.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Over S. Holes Fidex Tray."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ34_6"),
                                 valueBoxOutput("CostZ34_6")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ34_6"),
                                 valueBoxOutput("PhaseZ34_6")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ34_6"),
                                 valueBoxOutput("ECDZ34_6")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000845-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ34_6", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 7.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Discrepant Hole Fixed TE & LE."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ34_7"),
                                 valueBoxOutput("CostZ34_7")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ34_7"),
                                 valueBoxOutput("PhaseZ34_7")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ34_7"),
                                 valueBoxOutput("ECDZ34_7")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000767-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ34_7", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 8.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("STRG MISALIGNED."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ34_8"),
                                 valueBoxOutput("CostZ34_8")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ34_8"),
                                 valueBoxOutput("PhaseZ34_8")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ34_8"),
                                 valueBoxOutput("ECDZ34_8")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321003260-2.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ34_8", height = "250px")
                  ))

          
        )),
    
    
    #---------
    h2("Histograms"),
    
    box(
      width = 12,
      tabBox(
        width = 12,
        tabPanel(
          "Graphs 1",
          column(4, plotlyOutput("WC34")),
          column(4, plotlyOutput("AC34")),
          column(4, plotlyOutput("Part34"))
        ),
        tabPanel(
          "Graphs 2",
          column(4, plotlyOutput("QA34")),
          column(4, plotlyOutput("Defect341")),
          column(4, plotlyOutput("Disp34"))
        ),
        tabPanel(
          "Graphs 3",
          column(4, plotlyOutput("Day34")),
          column(4, plotlyOutput("WB34")),
          column(4, plotlyOutput("Skill34"))
        ),
        tabPanel("Graphs 4",
                 column(4, plotlyOutput("Worker34")))
      )
    )
    
    
    
    )
    
        
        
      ),
      
      
      # E Zone 34
      #E Zone 33
      
      # S Zone 4
      
      tabItem(
        tabName = "Zone4",
        h2("Last Month Results"),
        h3("Time Series"),
        plotlyOutput("TS4", height = "250px"),
        h2("Topics Details"),
        plotlyOutput("Topics4", height = "300px"),
        br(),
        
        fluidRow(
          box(width = 12,
              tabBox(
                width = 12,
                tabPanel(title = "Topic 1.",
                         status = "primary",
                         box(width = 12,
                             h1("Clamp size. Harness. J511/P785B"),
                             h1("     "),
                             h1("     "),
                             fluidRow(
                               
                               column(6,
                                      
                                      fluidRow(
                                        valueBoxOutput("CountZ4_1"),
                                        valueBoxOutput("CostZ4_1")
                                      ),
                                      
                                      fluidRow(
                                        valueBoxOutput("ActionZ4_1"),
                                        valueBoxOutput("PhaseZ4_1")
                                      ),
                                      fluidRow(
                                        valueBoxOutput("ReferenceZ4_1"),
                                        valueBoxOutput("ECDZ4_1")
                                      )
                               ),
                               
                               column(6,
                                      img(
                                        class = "img-responsive img-rounded center-block",
                                        src = 'Q321000364-1.PNG',
                                        height = '100%',
                                        width = '100%'
                                      )
                                      
                               )
                               
                             ),
                             
                             h1("     "),
                             h1("     "),
                             h1("     "),
                             h1("     "),
                             
                             
                             plotlyOutput("STTopicZ4_1", height = "250px")
                             
                         )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 2.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Flushness Bullet F."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ4_2"),
                                 valueBoxOutput("CostZ4_2")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ4_2"),
                                 valueBoxOutput("PhaseZ4_2")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ4_2"),
                                 valueBoxOutput("ECDZ4_2")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321010579-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ4_2", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 3.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Fouling Safety device HPTU."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ4_3"),
                                 valueBoxOutput("CostZ4_3")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ4_3"),
                                 valueBoxOutput("PhaseZ4_3")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ4_3"),
                                 valueBoxOutput("ECDZ4_3")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000018-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ4_3", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 4.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Low Clearance btwn engine mount and Harness."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ4_4"),
                                 valueBoxOutput("CostZ4_4")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ4_4"),
                                 valueBoxOutput("PhaseZ4_4")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ4_4"),
                                 valueBoxOutput("ECDZ4_4")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000020-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ4_4", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 5.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Overlap Feedthru/CCM."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ4_5"),
                                 valueBoxOutput("CostZ4_5")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ4_5"),
                                 valueBoxOutput("PhaseZ4_5")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ4_5"),
                                 valueBoxOutput("ECDZ4_5")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000102-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ4_5", height = "250px")
                  )),
                tabPanel(
                  status = "success",
                  title = "Topic 6.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Connector with clocking not as per DMU."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ4_6"),
                                 valueBoxOutput("CostZ4_6")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ4_6"),
                                 valueBoxOutput("PhaseZ4_6")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ4_6"),
                                 valueBoxOutput("ECDZ4_6")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321007582-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ4_6", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 7.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Excessive Fiber glass tape under clamp."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ4_7"),
                                 valueBoxOutput("CostZ4_7")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ4_7"),
                                 valueBoxOutput("PhaseZ4_7")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ4_7"),
                                 valueBoxOutput("ECDZ4_7")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321006285-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ4_7", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 8.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Fouling condition btwn fidex shield & tube assy"),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ4_8"),
                                 valueBoxOutput("CostZ4_8")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ4_8"),
                                 valueBoxOutput("PhaseZ4_8")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ4_8"),
                                 valueBoxOutput("ECDZ4_8")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321001564-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ4_8", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 9.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Depth screws in pylon."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZ4_9"),
                                 valueBoxOutput("CostZ4_9")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZ4_9"),
                                 valueBoxOutput("PhaseZ4_9")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZ4_9"),
                                 valueBoxOutput("ECDZ4_9")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321004155-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZ4_9", height = "250px")
                  ))
                
              )),
          
          #---------
          h2("Histograms"),
          
          box(
            width = 12,
            tabBox(
              width = 12,
              tabPanel(
                "Graphs 1",
                column(4, plotlyOutput("WC4")),
                column(4, plotlyOutput("AC4")),
                column(4, plotlyOutput("Part4"))
              ),
              tabPanel(
                "Graphs 2",
                column(4, plotlyOutput("QA4")),
                column(4, plotlyOutput("Defect41")),
                column(4, plotlyOutput("Disp4"))
              ),
              tabPanel(
                "Graphs 3",
                column(4, plotlyOutput("Day4")),
                column(4, plotlyOutput("WB4")),
                column(4, plotlyOutput("Skill4"))
              ),
              tabPanel("Graphs 4",
                       column(4, plotlyOutput("Worker4")))
            )
          )
          
          
          
        )
        
        
      ),
      
      
      # E Zone 4
      # S Zone 5
      
      tabItem(
        tabName = "Zone5",
        h2("Last Month Results"),
        h3("Time Series"),
        plotlyOutput("TS5", height = "250px"),
        h2("Topics Details"),
        fluidRow(
          column(4, plotlyOutput("WC5")),
          column(4, plotlyOutput("AC5")),
          column(4, plotlyOutput("Part5"))
        ),
        h3("   "),
        fluidRow(
          column(4, plotlyOutput("QA5")),
          column(4, plotlyOutput("Defect5")),
          column(4, plotlyOutput("Disp5"))
        ),
        h3("   "),
        fluidRow(
          column(4, plotlyOutput("Day5")),
          column(4, plotlyOutput("WB5")),
          column(4, plotlyOutput("Skill5"))
        ),
        h3("   "),
        fluidRow(column(4, plotlyOutput("Worker5"))),
        h2("Last Month RNCs Analysis"),
        plotOutput("MFW5", width = "100%", height = "500px"),
        h3("LDA Classification"),
        fluidRow(column(
          6, plotOutput("LDATT5", width = "100%", height = "500px")
        ),
        column(
          6, plotOutput("LDATR5", width = "100%", height = "500px")
        ), ),
        h3("Data Classification"),
        fluidRow(column(
          6, plotOutput("MRT5", width = "100%", height = "500px")
        ),
        column(6, DTOutput("LDAText5", height = "250px")))
        
      ),
      
      
      # E Zone 5
      
      # S Zone MTS
      
      tabItem(
        tabName = "MTS",
        h2("Last Month Results"),
        h2("Topics Details"),
        plotlyOutput("TopicsMTS", height = "300px"),
        br(),
        
        
        fluidRow(
          box(width = 12,
              tabBox(
                width = 12,
                tabPanel(title = "Topic 1.",
                         status = "primary",
                         box(width = 12,
                             h1("High screw on precooler."),
                             h1("     "),
                             h1("     "),
                             fluidRow(
                               
                               column(6,
                                      
                                      fluidRow(
                                        valueBoxOutput("CountZMTS_1"),
                                        valueBoxOutput("CostZMTS_1")
                                      ),
                                      
                                      fluidRow(
                                        valueBoxOutput("ActionZMTS_1"),
                                        valueBoxOutput("PhaseZMTS_1")
                                      ),
                                      fluidRow(
                                        valueBoxOutput("ReferenceZMTS_1"),
                                        valueBoxOutput("ECDZMTS_1")
                                      )
                               ),
                               
                               column(6,
                                      img(
                                        class = "img-responsive img-rounded center-block",
                                        src = 'Q321000085-1.PNG',
                                        height = '100%',
                                        width = '100%'
                                      )
                                      
                               )
                               
                             ),
                             
                             h1("     "),
                             h1("     "),
                             h1("     "),
                             h1("     "),
                             
                             
                             plotlyOutput("STTopicZMTS_1", height = "250px")
                             
                         )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 2.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Bad Bonding results on bracket of pylon."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZMTS_2"),
                                 valueBoxOutput("CostZMTS_2")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZMTS_2"),
                                 valueBoxOutput("PhaseZMTS_2")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZMTS_2"),
                                 valueBoxOutput("ECDZMTS_2")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000414-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZMTS_2", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 3.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Broken switch at intallation."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZMTS_3"),
                                 valueBoxOutput("CostZMTS_3")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZMTS_3"),
                                 valueBoxOutput("PhaseZMTS_3")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZMTS_3"),
                                 valueBoxOutput("ECDZMTS_3")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321000473-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZMTS_3", height = "250px")
                  )),
                
                tabPanel(
                  status = "success",
                  title = "Topic 4.",
                  solidHeader = TRUE,
                  box(width = 12,
                      h1("Deep bolts at plate of cargo door."),
                      h1("     "),
                      h1("     "),
                      fluidRow(
                        column(6,
                               fluidRow(
                                 valueBoxOutput("CountZMTS_4"),
                                 valueBoxOutput("CostZMTS_4")
                               ),
                               fluidRow(
                                 valueBoxOutput("ActionZMTS_4"),
                                 valueBoxOutput("PhaseZMTS_4")
                               ),
                               fluidRow(
                                 valueBoxOutput("ReferenceZMTS_4"),
                                 valueBoxOutput("ECDZMTS_4")
                               )
                        ),
                        column(6,
                               img(
                                 class = "img-responsive img-rounded center-block",
                                 src = 'Q321002599-1.PNG',
                                 height = '100%',
                                 width = '100%'
                               )
                        )
                      ),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      h1("     "),
                      plotlyOutput("STTopicZMTS_4", height = "250px")
                  ))
                
              )),
          
          #---------
          h2("Histograms"),
          
          box(
            width = 12,
            tabBox(
              width = 12,
              tabPanel(
                "Graphs 1",
                column(4, plotlyOutput("WCMTS")),
                column(4, plotlyOutput("ACMTS")),
                column(4, plotlyOutput("PartMTS"))
              ),
              tabPanel(
                "Graphs 2",
                column(4, plotlyOutput("QAMTS")),
                column(4, plotlyOutput("DefectMTS1")),
                column(4, plotlyOutput("DispMTS"))
              ),
              tabPanel(
                "Graphs 3",
                column(4, plotlyOutput("DayMTS")),
                column(4, plotlyOutput("WBMTS")),
                column(4, plotlyOutput("SkillMTS"))
              ),
              tabPanel("Graphs 4",
                       column(4, plotlyOutput("WorkerMTS")))
            )
          )
          
          
          
        )
        
        
      ),
    
      
      # E Zone MTS
      
     tabItem(
      tabName = "GeneralPU2",
     ),
    tabItem(
      tabName = "Zone1PU2",
    ),
    tabItem(
      tabName = "Zone2PU2",
    ),
    tabItem(
      tabName = "Zone3PU2",
    ),
    tabItem(
      tabName = "TC56",
    ),
    tabItem(
      tabName = "TC78",
    ),
    tabItem(
      tabName = "Slats",
    ),
    
      tabItem(
        tabName = "view_data",
        
        h2("Data"),
        
        DT::datatable(
          RNC,
          class = 'cell-border stripe',
          rownames = F,
          filter = 'top',
          editable = TRUE,
          extensions = 'Buttons',
          options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
          )
        )
        
      ),
      
      tabItem("tab_about",
              #
              # Note About Credit
              # =================
              #
              # You June, of course, decide to change this section of the dashboard.
              # But PLEASE provide credit and link to my website: garrickadenbuie.com
              # Something like this would be great:
              #
              # "This dashboard was built using a template provided by
              # <a href="https://garrickadenbuie.com">Garrick Aden-Buie</a>."
              #
              # Thank you! -Garrick
              fluidRow(
                # About - About Me - start ------------------------------------------------
                box(
                  title = "About us",
                  status = "danger",
                  width = "6 col-lg-4",
                  tags$p(
                    class = "text-center",
                    tags$img(class = "rounded float-left", src = "svd.png", style = "max-width: 150px; max-height: 400px;"),
                    tags$img(class = "rounded float-right", src = "Hugo.png", style = "max-width: 150px; max-height: 400px;")
                  ),
                  tags$p(class = "text-center",
                         tags$strong("Hi! We are Hugo & Samuel.")),
                  tags$p(
                    "We are two methods agents from PU11, we had created this dashboard for",
                    "the methods deparment of Mexico, so every agent can see and understand the",
                    "behaviour of the RNCs that are related to our department.",
                    "This Dashboard stills under development and many things can be added!",
                    "So if you have suggestions, please, contact us."
                  )
                ),
                # About - About Me - end --------------------------------------------------
                # About - About Dashboard - start -----------------------------------------
                box(
                  title = "About this Dashboard",
                  # status = "primary",
                  width = "6 col-lg-4",
                  tags$p(
                    class = "text-center",
                    tags$a(
                      href = "https://www.r-project.org",
                      target = "_blank",
                      tags$img(class = "image-responsive",
                               src = "https://www.r-project.org/logo/Rlogo.svg",
                               style = "max-width: 150px;")
                    ),
                    tags$a(href = "https://rstudio.com",
                           target = "_blank"),
                    tags$a(href = "https://rtweet.info",
                           target = "_blank")
                  ),
                  tags$p(
                    "This dashboard was built in",
                    tags$a(href = "https://r-project.org", target = "_blank", "R"),
                    "and",
                    tags$a(href = "https://rstudio.com", target = "_blank", "RStudio"),
                    "with",
                    tags$strong("shiny,"),
                    tags$strong("shinydashboard,"),
                    tags$strong("rtweet,"),
                    tags$strong("plotly,"),
                    "the",
                    tags$strong("tidyverse,"),
                    "and many more packages."
                  )
                )
                # About - About Dashboard - start -----------------------------------------
              )
      )
      
    ))
)


