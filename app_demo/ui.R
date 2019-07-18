header <- dashboardHeader(
  title = 'FolsomAnalyics Pilot'
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data Upload & Summary", tabName = "dashboard"),
    menuItem("Prediction Finder", tabName = "tree"),
    menuItem("Difference Finder", tabName = "diffy"),
    menuItem("Cohort Analysis", tabName = 'cohort'),
    menuItem("Info & Contact", tabName = "help")
    
  )
)


body <- dashboardBody(
  tabItems(
    tabItem("dashboard",
            fluidRow(
              column(3, fileInput("file1", "Upload Data in CSV File",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")))
             
              
            ),
            fluidRow(
              valueBoxOutput("survey_rows"),
              valueBoxOutput("survey_columns")
              ),
            fluidRow(
              tryCatch(DT::dataTableOutput("rawtable"),error = function(err){print("No data")})
              ),
            br(),
            fluidRow(column(3, div(style="display:inline-block",actionButton("redefine","Use These Data (check classes below)",icon = icon("play-circle"), width = "250%", style="color: #fff; background-color: #000000; border-color: #2e6da4; font-size:150%")))),
            br(),
            uiOutput("sliders")
            
            
            ),
 
    tabItem("tree", 
            fluidRow(h1(strong("Prediction Finder")),
                      h3(em("What subsets in my data most predict differences in X?"))),
                     hr(),
            wellPanel(fluidRow(
              column(3,
                     selectInput("tree_target_var",
                                 "I Want to Predict...",
                                 c('',""),
                                 multiple = F)
                     
              ),
              
              column(3,
                     selectInput("tree_split_var",
                                 "Search For Trends Over...",
                                 c('',""),
                                 selected = NULL,
                                 multiple = T)
              ))),
            wellPanel(fluidRow(
              column(3,
                     numericInput("min_leaf",
                                  "Minimum N Per Model",
                                  value = 30
                                  )
              ),
              column(3,
                     numericInput("pvalue_thresh",
                                  "Max Pvalue to Include",
                                  value = 0.5,step = 0.01
                     )
              )
            #   column(3,
            #         checkboxInput("cpchoice","Advanced Parameter: Check to apply Complexity Threshold",
            #                       value = FALSE))
            # )
          )),
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
            downloadButton("mainchart", "Download Plot")
          )
          
    ),
 tabItem("help",
         fluidRow(titlePanel("Information and Contact")),
          br(),
         h4(strong("Pilot Software Product")),
         "This is Folsom Analytics, Inc.'s Pilot Product aimed to help non-technical users leverage data science techniques to improve the way they harness information from data.",
        "Software is prerelease code and is not at the level of performance or compatibility of a final, generally available product offering. Software may not operate correctly and may be substantially modified prior to first commercial shipment, or withdrawn. Software is provided 'AS IS' without warranty of any kind. The entire risk arising out of the use or performance of Software remains with user",
        br(),
        br(),
        h4(strong("Questions")),
        "Please refer to the latest Pilot Handbook for basic instructions and a description of the statistics used in this tool. ",
        "We want to build for your needs, so please contact us with any issues, bugs, questions you may encounter.",
        br(),
        br(),
        h4(strong("Contact")),
        "Carl Moos and Jonathan Wang",
        br(),
         "carlmoos@mit.edu",
        br(),
         "Phone Number: 714-745-0455"
         ),
 tabItem("diffy",
         fluidRow(h1(strong("Difference Finder"),
                  h3(em("When split by X, what are the main insights in my data?")))),
         
         br(),
         wellPanel(fluidRow(
           column(3,
                  selectInput("diff_split_var",
                              "Split The Data By...",
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
                          "Max Pvalue to Include",
                          value = 0.5,step = 0.01
             )),
             column(3,
                    checkboxInput("weighted_avg","Advanced Parameter: Weighted Distribution Across Search",
                                  value = FALSE))
           ),
           
           fluidRow(
             column(2,actionButton("UseTheseVars_diffy", "Choose These Variables & Run"))
           )),
           br(),
           br(),
           fluidRow(DT::dataTableOutput("DiffyTable"))
           ,
         fluidRow(
           downloadButton("report_diffy", "Download Table")
         )
 ),
 tabItem("cohort",
         fluidRow(titlePanel("Cohort Analysis")),
         br(),
         mainPanel("This tool allows you to complete cohort analyses on transaction level data"),
         br(),
         br(),
         wellPanel(fluidRow(
           column(3,
                  selectInput("cohort_id",
                              "Select the Unique ID var to identify individuals",
                              c('',""),
                              multiple = F)),
           column(3,
                  selectInput("cohort_time",
                              "Select the time var to track the cohort across",
                              c('',""),
                              multiple = F)),
           column(3,
                  selectInput("cohort_time_group",
                              "Select the time granularity to track the cohort across",
                              c('day', 'month'),
                              multiple = F)),
           # column(3,
           #        selectInput("cohort_group",
           #                    "Group the cohorts using (e.g. by gender)...",
           #                    c('',""),
           #                    multiple = F)),
           column(3,
                  selectInput("cohort_target",
                              "Select the quantity variable to analyze the cohorts",
                              c('',""),
                              multiple = F))
         )),
         fluidRow(
           column(2,actionButton("UseTheseVars_cohort", "Choose These Variables & Run"))
         ),
         br(),
         fluidRow(plotlyOutput("cohort_plot")),
         br(),
         fluidRow(plotlyOutput("cohort_count_plot")),
         br(),
         fluidRow(plotlyOutput("cohort_retention_plot"))
         )
)
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

