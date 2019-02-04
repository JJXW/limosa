header <- dashboardHeader(
  title = 'SpliceAI Demo'
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data Upload & Summary", tabName = "dashboard"),
    menuItem("Data Exploration & Cleaning", tabName = "rawdata"),
    menuItem("Segementation & Story", tabName = "analysis"),
    menuItem("Question Randomization", tabName = "dataquality")
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
                                 colnames(survey_data_default),
                                 multiple = T)
                     
              ),
              column(4,
                     selectInput("outcome_var", 
                                 "2) Outcome Variables",
                                 colnames(survey_data_default),
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
          ),
    tabItem("dataquality",
            fluidRow(titlePanel("Data Randomization Check"),
            mainPanel("This tool allows you to upload survey responses in choice order, meaning each observation shows if a respondent selected A, B, C, etc. and it does not correspond to answer values. This is to test the quality of your survey writing and randomization.")
            ),
            fluidRow(tags$head(
              tags$style(HTML("hr {border-top: 1px solid #000000;}"))
            ),hr()),
            fluidRow(
              column(6,
              fileInput(inputId = "inFile",label = "CSV of Your Survey Data in Choice Order"))
            ),
            fluidRow(
              column(6,actionButton("go", "Go!"))
            ),
            fluidRow(
              column(6,"Answer Selection Frequency: Actual vs. Expected If Random", align = "center"),
              column(6,"All Actual vs. Expected Differences Plotted Against A Normal Distribution", align = "center")
            ),
            fluidRow(
              column(6,
              wellPanel(plotOutput("expectation"))
            ),
            column(6,
              wellPanel(plotOutput("normal"))
            )),
            fluidRow(column(12,"The Top 10 Answer Choices Demonstrating Non Random Behavior", align = "center")
              
            ),
            fluidRow(column(12, 
             wellPanel(dataTableOutput("testable"))
            )
  )
)
))

ui <- dashboardPage(
  header,
  sidebar,
  body
)

