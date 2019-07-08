header <- dashboardHeader(
  title = 'FolsomAnalyics Beta'
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
              column(3, fileInput("file1", "Upload Survey Data in CSV File",
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
            fluidRow(titlePanel("Prediction Finder"),
                     mainPanel("This tool allows you to find the best models of datacuts that predict a given target variable.")),
            wellPanel(fluidRow(
              column(3,
                     selectInput("tree_target_var",
                                 "I Want to Predict...",
                                 c('',""),
                                 multiple = F)
                     
              ),
              
              column(3,
                     selectInput("tree_split_var",
                                 "Search Over...",
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
                              "Select the id var to identify individuals",
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
           column(3,
                  selectInput("cohort_group",
                              "Group the cohorts using (e.g. by gender)...",
                              c('',""),
                              multiple = F)),
           column(3,
                  selectInput("cohort_target",
                              "Select the target variable to analyze the cohorts on",
                              c('',""),
                              multiple = F))
         )),
         fluidRow(
           column(2,actionButton("UseTheseVars_cohort", "Choose These Variables & Run"))
         ),
         br(),
         fluidRow(plotlyOutput("cohort_plot"))
         )
)
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

