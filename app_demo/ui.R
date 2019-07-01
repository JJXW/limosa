header <- dashboardHeader(
  title = 'FolsomAnalyics Beta'
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data Upload & Summary", tabName = "dashboard"),
    menuItem("Prediction Finder", tabName = "tree"),
    menuItem("Difference Finder", tabName = "diffy"),
    menuItem("Info & Contact", tabName = "help")
    # ,
    # menuItem("Auto-Segment  ", tabName = "analysis")
    # menuItem("Segment Storyliner", tabName = "manual")
    
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
              tryCatch(DT::dataTableOutput("rawtable"),error = function(err){print("No data")})
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
                                 c('',""),
                                 multiple = F)
                     
              ),
            column(3,
                     selectInput("segvar2",
                                 "Variable 2: Select",
                                 c('',""),
                                 selected = NULL,
                                 multiple = F)
              ), 
            column(3,
                    selectInput("segvar3",
                                "Variable 3: Select",
                                c('',""),
                                selected = NULL,
                                multiple = F)
            ),
            column(3,
                   selectInput("segvar4",
                               "Variable 4: Select",
                               c('',""),
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
                       c('',""),
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
                       c('',""),
                       multiple = F))
           ,
           column(2,selectInput("type_var_2",
                       "Type:",
                       c("","categorical","numeric"),
                       multiple = F)),
           column(2,selectInput("var2_seg1","Pick Range",choices = "",multiple = T)),
           column(2,selectInput("var2_seg2","Pick Range",choices = "",multiple = T)),
           column(2,selectInput("var2_seg3","Pick Range",choices = "",multiple = T)),
           column(2,selectInput("var2_seg4","Pick Range",choices = "",multiple = T))
         ),
         fluidRow(
           column(2,selectInput("segment_var_3",
                       "Variable 3:",
                       c('',""),
                       multiple = F))
           ,
           column(2,selectInput("type_var_3",
                       "Type:",
                       c("","categorical","numeric"),
                       multiple = F)),
           column(2,selectInput("var3_seg1","Pick Range",choices = "",multiple = T)),
           column(2,selectInput("var3_seg2","Pick Range",choices = "",multiple = T)),
           column(2,selectInput("var3_seg3","Pick Range",choices = "",multiple = T)),
           column(2,selectInput("var3_seg4","Pick Range",choices = "",multiple = T))
         ),
         fluidRow(
           column(2,selectInput("segment_var_4",
                       "Variable 4:",
                       c('',""),
                       multiple = F))
           ,
           column(2,selectInput("type_var_4",
                       "Type:",
                       c("","categorical","numeric"),
                       multiple = F)),
           column(2,selectInput("var4_seg1","Pick Range",choices = "",multiple = T)),
           column(2,selectInput("var4_seg2","Pick Range",choices = "",multiple = T)),
           column(2,selectInput("var4_seg3","Pick Range",choices = "",multiple = T)),
           column(2,selectInput("var4_seg4","Pick Range",choices = "",multiple = T))
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
 
    tabItem("tree", 
            fluidRow(titlePanel("Prediction Finder"),
                     mainPanel("This tool allows you to find the best models of datacuts that predict a given target variable.")),
            wellPanel(fluidRow(
              column(3,
                     selectInput("tree_target_var",
                                 "Target Variable",
                                 c('',""),
                                 multiple = F)
                     
              ),
              
              column(3,
                     selectInput("tree_split_var",
                                 "Range of Variables To Include",
                                 c('',""),
                                 selected = NULL,
                                 multiple = T)
              ))),
            wellPanel(fluidRow(
              column(3,
                     numericInput("min_leaf",
                                  "Minimum Numer of Observations in a Model",
                                  value = 30
                                  )
              ),
              column(3,
                     numericInput("pvalue_thresh",
                                  "Advanced Parameter: Max pvalue to include",
                                  value = 0.5,step = 0.01
                     )
              ),
              column(3,
                    checkboxInput("cpchoice","Advanced Parameter: Check to apply Complexity Threshold",
                                  value = FALSE))
            )
            
          ),
          fluidRow(
            column(2,actionButton("UseTheseVars_tree", "Choose These Variables & Run"))
          ),
          br(),
          fluidRow(plotlyOutput("tree_plot")),
          br(),
          fluidRow(DT::dataTableOutput("tableTREE"))
          ,
          fluidRow(
            downloadButton("report", "Download Table"),
            downloadButton("mainchart", "Download Chart")
          )
          
    ),
 tabItem("help",
         fluidRow(titlePanel("Information and Contact")),
          br(),
         h4(strong("Beta Software Product")),
         "This is Folsom Analytics, Inc.'s Beta Product aimed to help non-technical users leverage data science techniques to improve the way they harness information from data.",
        "Software is prerelease code and is not at the level of performance or compatibility of a final, generally available product offering. Software may not operate correctly and may be substantially modified prior to first commercial shipment, or withdrawn. Software is provided 'AS IS' without warranty of any kind. The entire risk arising out of the use or performance of Software remains with user",
        br(),
        br(),
        h4(strong("Methodology")),
        "Folsom Analytics' Prediction Finder leverages the machine learning technique of classification trees to produce relevant models for your consideration.",
        "Pvalue is calculated using a chi-squared test for categorical target variables and a t-test for numeric target values.",
        "For more assitance on methodology please contact us.",
        br(),
        br(),
        h4(strong("Contact")),
        "Please reach out with feedback or if you encounter difficulties. We are excited to build our offering to add the most value possible to your company.",
        br(),
        "Carl Moos: carlmoos@mit.edu",
        br(),
         "Jonathan Wang: jonathan352@gmail.com",
        br(),
         "Phone Number: 714-745-0455"
         ),
 tabItem("diffy",
         fluidRow(titlePanel("Difference Finder")),
          br(),
         mainPanel("This tool uses allows you to search a whole range of data to find which show significant trends when split by a categorical variable of your choice"),
         br(),
         br(),
         wellPanel(fluidRow(
           column(3,
                  selectInput("diff_split_var",
                              "Cut The Data By...",
                              c('',""),
                              multiple = F)
           ),
           
           column(4,
                  selectInput("diff_range_vars",
                              "Search For Trends Over...",
                              c('',""),
                              selected = NULL,
                              multiple = T)
           )),
           fluidRow(
             column(3, numericInput("pvalue_diffy",
                          "Advanced Parameter: Max pvalue to include",
                          value = 0.5,step = 0.01
             ))
           ),
           
           fluidRow(
             column(2,actionButton("UseTheseVars_diffy", "Choose These Variables & Run"))
           ),
           
           fluidRow(DT::dataTableOutput("DiffyTable"))
           )
 )
)
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

