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
            
    #THIS WAS CODE TO LET SOMEONE ADD A VARIABLE TO THE OUTPUT TABLE THAT IS NOT INCLUDED IN THE ACTUAL SEGMENTATION
    #THIS CAN BE A LATER FEATURE, HAVE NOT ADDED IT TO THE SERVER CODE
            # fluidRow(
            #   column(3,selectInput('segvar1_purp', 'Variable 1: Purpose',choices = c('','targeting','outcome'),multiple = F)
            #   ),
            #   column(3,selectInput('segvar2_purp', 'Variable 2: Purpose',choices = c('','targeting','outcome'),multiple = F)
            #   ),
            #   column(3,selectInput('segvar3_purp', 'Variable 3: Purpose',choices = c('','targeting','outcome'),multiple = F)
            #   ),
            #   column(3,selectInput('segvar4_purp', 'Variable 4: Purpose',choices = c('','targeting','outcome'),multiple = F)
            #   )
            # ),
            
            fluidRow(
              column(2,actionButton("UseTheseVars", "Choose These Variables & Run Segmentation"))
              )
    ),
    fluidRow(
      valueBoxOutput("QualityScore")
    ),
    fluidRow(wellPanel(
      column(3,textOutput("Nseg1")),
      column(3,textOutput("Nseg2")),
      column(3,textOutput("Nseg3")),
      column(3,textOutput("Nseg4")))
    ),
    fluidRow((dataTableOutput("segtable"))
    ),
    fluidRow(downloadButton(outputId = "down", label = "Download Analysis"))
    ),
            
 ###MANUAL SEGMENTATION###          
    tabItem("manual",
            fluidRow(titlePanel("Manual Segmentation Tool"),
                     mainPanel("This tool allows you to segment your data manually based on ranges of variables")),
            fluidRow(tags$head(
              tags$style(HTML("hr {border-top: 1px solid #000000;}"))
            ),hr()
            ),
               
                column(2,
                  textOutput("col1")
                ,
                  selectInput("segment_var_1",
                               "Variable 1:",
                               colnames(survey_data_default),
                               multiple = F)
                ,
                  selectInput("segment_var_2",
                              "Variable 2:",
                              colnames(survey_data_default),
                              multiple = F)
                ,
                  selectInput("segment_var_3",
                              "Variable 3:",
                              colnames(survey_data_default),
                              multiple = F)
                ,
                  selectInput("segment_var_4",
                              "Variable 4:",
                              colnames(survey_data_default),
                              multiple = F)
                 ),
                column(2,
                  textOutput("col2"),
                  selectInput("type_var_1",
                              "Type:",
                              c("","categorical","numeric"),
                              multiple = F),
                  selectInput("type_var_2",
                              "Type:",
                              c("","categorical","numeric"),
                              multiple = F),
                  selectInput("type_var_3",
                              "Type:",
                              c("","categorical","numeric"),
                              multiple = F),
                  selectInput("type_var_4",
                              "Type:",
                              c("","categorical","numeric"),
                              multiple = F)
                      )
            ,
            
        ##SEGMENT 1##
            column(2,

                textOutput("col3"),

                uiOutput("var1seg1"),
                uiOutput("var2seg1"),
                uiOutput("var3seg1"),
                uiOutput("var4seg1")
                       ),
           
       ##SEGMENT 2##
            column(2,
                   
                   textOutput("col4"),
                   
                   uiOutput("var1seg2"),
                   uiOutput("var2seg2"),
                   uiOutput("var3seg2"),
                   uiOutput("var4seg2")
            ),
       
    ##SEGMENT 3##
       column(2,
              
              textOutput("col5"),
              uiOutput("var1seg3"),
              uiOutput("var2seg3"),
              uiOutput("var3seg3"),
              uiOutput("var4seg3")
       ),
    
    ##SEGMENT 4##
    column(2,
           
           textOutput("col6"),
           uiOutput("var1seg4"),
           uiOutput("var2seg4"),
           uiOutput("var3seg4"),
           uiOutput("var4seg4")
           
    )

    
    ,
    ##STYLING THE COLUMN HEADERS##
          tags$head(tags$style("#col1{color: blue;
                                 font-size: 24px;
                      font-style: bold;
                      }"
                         )
           ,
          tags$style("#col2{color: blue;
                                 font-size: 24px;
                      font-style: bold;
                      }"
                  ),
          tags$style("#col3{color: grey;
                                 font-size: 24px;
               font-style: bold;
               }"
                  ),
          tags$style("#col4{color: grey;
                                 font-size: 24px;
                      font-style: bold;
                      }"
                 ),
          tags$style("#col5{color: grey;
                                 font-size: 24px;
                      font-style: bold;
                      }"
                   ),
          tags$style("#col6{color: grey;
                                 font-size: 24px;
                     font-style: bold;
                     }"
                   )

                  )
    )
 ,
 
 
 
 
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
)
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

