header <- dashboardHeader(
  title = 'Limosa Demo'
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data Upload & Summary", tabName = "dashboard"),
    menuItem("Data Exploration & Cleaning", tabName = "rawdata"),
    menuItem("Segementation & Story", tabName = "analysis")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("dashboard",
            fluidRow(
              fileInput("file1", "Upload Survey Data in CSV File",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv"))
            ), 
            fluidRow(
              valueBoxOutput("survey_rows"),
              valueBoxOutput("survey_columns")
              )
            ),
    tabItem("rawdata",
            DT::dataTableOutput("rawtable")
            ),
    tabItem("analysis",
            fluidRow(
              column(4, 
                     selectInput("segment_var", 
                                 "1) Segmenter Variables",
                                 colnames(survey_data),
                                 multiple = T)
                     
              ),
              column(4,
                     selectInput("outcome_var", 
                                 "2) Outcome Variables",
                                 colnames(survey_data),
                                 selected = NULL,
                                 multiple = T)
                     ),
              column(4,
                     numericInput("segment_num", 
                                 "3) Number of Segments",
                                 value = 2,
                                 min = 2,
                                 max = 4,
                                 step = 1)
                     )
              
            ),
            
            fluidRow(
              valueBoxOutput("survey_data_segement_1_rows"),
              valueBoxOutput("survey_data_segement_2_rows")
            ),
            
            
            fluidRow(
              column(2,
                     selectInput('segment_var_1_1',
                                 'Group 1 - Segment Variable 1',
                                 choices = c(),
                                 multiple = T
                                 ),
                     selectInput('segment_var_1_2',
                                 'Group 1 - Segment Variable 2',
                                 choices = c(),
                                 multiple = T
                                 
                     )
                  ),
              column(2,
                     selectInput('segment_var_2_1',
                                 'Group 2 - Segment Variable 1',
                                 choices = c(),
                                 multiple = T
                     ),
                     selectInput('segment_var_2_2',
                                 'Group 2 - Segment Variable 2',
                                 choices= c(),
                                 multiple = T
                     )
              )
              
            )
          )
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

