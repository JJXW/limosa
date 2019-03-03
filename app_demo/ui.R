header <- dashboardHeader(
  title = 'SpliceAI Demo'
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data Upload & Summary", tabName = "dashboard"),
    menuItem("Auto Segementation", tabName = "analysis"),
    menuItem("Manual Segmentation", tabName = "manual"),
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
              ),
            fluidRow(
              DT::dataTableOutput("rawtable")
              )
            ),
    tabItem("analysis",
            fluidRow(titlePanel("Auto Segmentation Tool"),
                     mainPanel("This tool allows you to segment your data automatically based on variables of interest")),
            fluidRow(tags$head(
              tags$style(HTML("hr {border-top: 1px solid #000000;}"))
            ),hr()
            ),
            wellPanel(fluidRow(
              column(3,
                     numericInput("segment_num",
                                 "Number of Segments",
                                 value = 2,
                                 min = 2,
                                 max = 4,
                                 step = 1)
                     )
            )),
            wellPanel(fluidRow(
            column(3,
                     selectInput("segvar1",
                                 "Variable 1: Select",
                                 c('',colnames(survey_data_default)),
                                 multiple = F)
                     
              ),
            column(3,
                     selectInput("segvar2",
                                 "Variable 2: Select",
                                 c('',colnames(survey_data_default)),
                                 selected = NULL,
                                 multiple = F)
              ), 
            column(3,
                    selectInput("segvar3",
                                "Variable 3: Select",
                                c('',colnames(survey_data_default)),
                                selected = NULL,
                                multiple = F)
            ),
            column(3,
                   selectInput("segvar4",
                               "Variable 4: Select",
                               c('',colnames(survey_data_default)),
                               selected = NULL,
                               multiple = F)
            )
            ),
            
            fluidRow(
              column(3,selectInput('segvar1_type', 'Variable 1: Type',choices = c('','categorical','numeric'),multiple = F)
              ),
              column(3,selectInput('segvar2_type', 'Variable 2: Type',choices = c('','categorical','numeric'),multiple = F)
              ),
              column(3,selectInput('segvar3_type', 'Variable 3: Type',choices = c('','categorical','numeric'),multiple = F)
              ),
              column(3,selectInput('segvar4_type', 'Variable 4: Type',choices = c('','categorical','numeric'),multiple = F)
              )
            ),
            
            fluidRow(
              column(3,selectInput('segvar1_purp', 'Variable 1: Purpose',choices = c('','targeting','outcome'),multiple = F)
              ),
              column(3,selectInput('segvar2_purp', 'Variable 2: Purpose',choices = c('','targeting','outcome'),multiple = F)
              ),
              column(3,selectInput('segvar3_purp', 'Variable 3: Purpose',choices = c('','targeting','outcome'),multiple = F)
              ),
              column(3,selectInput('segvar4_purp', 'Variable 4: Purpose',choices = c('','targeting','outcome'),multiple = F)
              )
            ),
            
            fluidRow(
              column(2,actionButton("UseTheseVars", "Choose These Variables & Run Segmentation"))
              )
    ),
    fluidRow((dataTableOutput("segtable"))
    )
    ),
            
           
    # tabItem("manual",
    #         fluidRow(
    #           column(4, 
    #                  selectInput("segment_var", 
    #                              "1) Segmenter Variables",
    #                              colnames(survey_data_default),
    #                              multiple = T)
    #                  
    #           ),
    #           column(4,
    #                  selectInput("outcome_var", 
    #                              "2) Outcome Variables",
    #                              colnames(survey_data_default),
    #                              selected = NULL,
    #                              multiple = T)
    #           ),
    #           column(4,
    #                  numericInput("segment_num", 
    #                               "3) Number of Segments",
    #                               value = 2,
    #                               min = 2,
    #                               max = 4,
    #                               step = 1)
    #           )
    #           
    #         ),
    #         
    #         fluidRow(
    #           valueBoxOutput("survey_data_segement_1_rows"),
    #           valueBoxOutput("survey_data_segement_2_rows")
    #         ),
    #         
    #         
    #         fluidRow(
    #           column(2,
    #                  selectInput('segment_var_1_1',
    #                              'Group 1 - Segment Variable 1',
    #                              choices = c(),
    #                              multiple = T
    #                  ),
    #                  selectInput('segment_var_1_2',
    #                              'Group 1 - Segment Variable 2',
    #                              choices = c(),
    #                              multiple = T
    #                              
    #                  )
    #           ),
    #           column(2,
    #                  selectInput('segment_var_2_1',
    #                              'Group 2 - Segment Variable 1',
    #                              choices = c(),
    #                              multiple = T
    #                  ),
    #                  selectInput('segment_var_2_2',
    #                              'Group 2 - Segment Variable 2',
    #                              choices= c(),
    #                              multiple = T
    #                  )
    #           )
    #           
    #         )
    # ),
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

