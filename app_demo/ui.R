header <- dashboardHeader(
  title = 'SpliceAI Demo'
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data Upload & Summary", tabName = "dashboard"),
    menuItem("Auto Segementation", tabName = "analysis"),
    menuItem("Manual Segmentation", tabName = "manual"),
    menuItem("Question Randomization", tabName = "dataquality"),
    menuItem("Visualize", tabName = "visual")
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
            wellPanel(tags$style(type='text/css',
                                 ".selectize-dropdown-content{
                                 height: 1000px;
                                 width: 1000px;
                                 background-color: white;
                                 }"),fluidRow(
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
         wellPanel(fluidRow(
           column(2,titlePanel("Variable")),
           column(2,titlePanel("Type")),
           column(2,titlePanel("Segment 1")),
           column(2,titlePanel("Segment 2")),
           column(2,titlePanel("Segment 3")),
           column(2,titlePanel("Segment 4"))
         )
         ,
         fluidRow(
           column(2,selectInput("segment_var_1",
                       "Variable 1:",
                       c('',colnames(survey_data_default)),
                       multiple = F)),
           column(2,selectInput("type_var_1",
                       "Type:",
                       c("","categorical","numeric"),
                       multiple = F)),
           column(2,selectInput("var1_seg1","Pick Range",choices = c("nothing selected"),multiple = T)),
           column(2,selectInput("var1_seg2","Pick Range",choices = c("nothing selected"),multiple = T)),
           column(2,selectInput("var1_seg3","Pick Range",choices = c("nothing selected"),multiple = T)),
           column(2,selectInput("var1_seg4","Pick Range",choices = c("nothing selected"),multiple = T))

         ),

         fluidRow(
           column(2,selectInput("segment_var_2",
                       "Variable 2:",
                       c('',colnames(survey_data_default)),
                       multiple = F))
           ,
           column(2,selectInput("type_var_2",
                       "Type:",
                       c("","categorical","numeric"),
                       multiple = F)),
           column(2,selectInput("var2_seg1","Pick Range",choices = colnames(survey_data_default),multiple = T)),
           column(2,selectInput("var2_seg2","Pick Range",choices = colnames(survey_data_default),multiple = T)),
           column(2,selectInput("var2_seg3","Pick Range",choices = colnames(survey_data_default),multiple = T)),
           column(2,selectInput("var2_seg4","Pick Range",choices = colnames(survey_data_default),multiple = T))
         ),
         fluidRow(
           column(2,selectInput("segment_var_3",
                       "Variable 3:",
                       c('',colnames(survey_data_default)),
                       multiple = F))
           ,
           column(2,selectInput("type_var_3",
                       "Type:",
                       c("","categorical","numeric"),
                       multiple = F)),
           column(2,selectInput("var3_seg1","Pick Range",choices = colnames(survey_data_default),multiple = T)),
           column(2,selectInput("var3_seg2","Pick Range",choices = colnames(survey_data_default),multiple = T)),
           column(2,selectInput("var3_seg3","Pick Range",choices = colnames(survey_data_default),multiple = T)),
           column(2,selectInput("var3_seg4","Pick Range",choices = colnames(survey_data_default),multiple = T))
         ),
         fluidRow(
           column(2,selectInput("segment_var_4",
                       "Variable 4:",
                       c('',colnames(survey_data_default)),
                       multiple = F))
           ,
           column(2,selectInput("type_var_4",
                       "Type:",
                       c("","categorical","numeric"),
                       multiple = F)),
           column(2,selectInput("var4_seg1","Pick Range",choices = colnames(survey_data_default),multiple = T)),
           column(2,selectInput("var4_seg2","Pick Range",choices = colnames(survey_data_default),multiple = T)),
           column(2,selectInput("var4_seg3","Pick Range",choices = colnames(survey_data_default),multiple = T)),
           column(2,selectInput("var4_seg4","Pick Range",choices = colnames(survey_data_default),multiple = T))
         )),

         ##END OF SELECTION PANE#

         fluidRow(
           column(2,actionButton("UseTheseVars_man", "Choose These Variables & Run Segmentation"))
         ),
         fluidRow(
           valueBoxOutput("QualityScore_man")
         ),
         fluidRow(wellPanel(
           column(3,textOutput("Nseg1_man")),
           column(3,textOutput("Nseg2_man")),
           column(3,textOutput("Nseg3_man")),
           column(3,textOutput("Nseg4_man")))
         ),
         fluidRow((dataTableOutput("segtable_man"))
         ),
         fluidRow(downloadButton(outputId = "down_man", label = "Download Analysis"))


         ###here ends the TAB ITEM MANUAL###
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
              fileInput(inputId = "inFile",label = "CSV of Your Survey Data in Choice Order")),

                     valueBoxOutput("randomScore")
            ),
            fluidRow(
              column(6,actionButton("go", "Go!"))
            ),

            fluidRow(column(12,"The Top 10 Answer Choices Demonstrating Non Random Behavior", align = "center")

            ),
            fluidRow(column(12,
                            wellPanel(dataTableOutput("testable"))
            )
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
            ))
      ),
    tabItem("visual",
            )
)
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)
