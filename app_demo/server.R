server <- function(input, output, session) { 
  
  survey_data_reactive <-
    reactive({
      
      if(!is.null(input$file1)) {
        survey_data <- input$file1  
      } else{
        survey_data <- survey_data_default
      }
      
      return(survey_data)
      
    })
  
  output$rawtable <- DT::renderDataTable({
    DT::datatable(data = survey_data_reactive(),
                  options = list(scrollX = T))
  })
  
  output$survey_rows <- renderValueBox({
    
    rows <- nrow(survey_data_reactive())
    
    valueBox(
      value = formatC(rows),
      subtitle = "Number of responses"
      
    )
  })
  
  output$survey_columns <- renderValueBox({
    
    columns <- ncol(survey_data_reactive())
    
    valueBox(
      value = formatC(columns),
      subtitle = "Number of questions"
    )
  
  })
  
  ### segment 1 choice filtering ###
  
  observe({
    
    if(length(input$segment_var > 0)){
      x <- input$segment_var[1]  
    
      question <- survey_data_reactive()[, x]
      question_values <- unique(question)
      
      updateSelectInput(session, "segment_var_1_1",
                        choices = question_values,
                        selected = c())
      
    } else(
      x <- NULL
    )
    
  })

  ### segment 2 choice filtering ###
  
  observe({
    
    if(length(input$segment_var) > 1){
      x <- input$segment_var[2] 
      
      question <- survey_data_reactive()[, x]
      question_values <- unique(question)
      
      updateSelectInput(session, "segment_var_1_2",
                        choices = question_values,
                        selected = c())

    } else(
      x <- NULL 
    )

  })
  
  ### segment 1 choice filtering ###
  
  observe({
    
    if(length(input$segment_var > 0)){
      x <- input$segment_var[1]  
      
      question <- survey_data_reactive()[, x]
      question_values <- unique(question)
      
      updateSelectInput(session, "segment_var_2_1",
                        choices = question_values,
                        selected = c())
      
    } else(
      x <- NULL
    )
    
  })
  
  ### segment 2 choice filtering ###
  
  observe({
    
    if(length(input$segment_var) > 1){
      x <- input$segment_var[2] 
      
      question <- survey_data_reactive()[, x]
      question_values <- unique(question)
      
      updateSelectInput(session, "segment_var_2_2",
                        choices = question_values,
                        selected = c())
      
    } else(
      x <- NULL 
    )
    
  })
  
  
  survey_data_segement_1 <-
    reactive({
      
      survey_data <- survey_data_reactive()
      
      if(length(input$segment_var) > 0){
        q1_filter_question <- input$segment_var[1]  
      } else{
        q1_filter_question <- NULL
      }
      
      if(length(input$segment_var) > 1){
        q2_filter_question <- input$segment_var[2]  
      } else{
        q2_filter_question <- NULL
      }

      
      q1_filter_value <- input$segment_var_1_1
      q2_filter_value <- input$segment_var_1_2
      
      print(paste0("'", q1_filter_value, "'"))
      print(paste0("'", q2_filter_value, "'"))
      
      query <- 
        '
      SELECT *
      FROM survey_data
      WHERE 1=1 
      '
      
      
      if(length(q1_filter_value > 0)) {
        query <- paste0(query, 
                        ' AND ', q1_filter_question, ' in (', paste0(paste0("'", q1_filter_value, "'"), collapse = ','), ')')
      }
      
      if(length(q2_filter_value > 0)) {
        query <- paste0(query, 
                        ' AND ', q2_filter_question, ' in (', paste0(paste0("'", q2_filter_value, "'"), collapse = ','), ')')
        
      }
      
      print(query)
      
      survey_data_filtered <- sqldf(query)
      
  })
  
  survey_data_segement_2 <-
    reactive({
      
      survey_data <- survey_data_reactive()
      
      if(length(input$segment_var) > 0){
        q1_filter_question <- input$segment_var[1]  
      } else{
        q1_filter_question <- NULL
      }
      
      if(length(input$segment_var) > 1){
        q2_filter_question <- input$segment_var[2]  
      } else{
        q2_filter_question <- NULL
      }
      
      
      q1_filter_value <- input$segment_var_2_1
      q2_filter_value <- input$segment_var_2_2
      
      print(paste0("'", q1_filter_value, "'"))
      print(paste0("'", q2_filter_value, "'"))
      
      query <- 
        '
      SELECT *
      FROM survey_data
      WHERE 1=1 
      '
      
      
      if(length(q1_filter_value > 0)) {
        query <- paste0(query, 
                        ' AND ', q1_filter_question, ' in (', paste0(paste0("'", q1_filter_value, "'"), collapse = ','), ')')
      }
      
      if(length(q2_filter_value > 0)) {
        query <- paste0(query, 
                        ' AND ', q2_filter_question, ' in (', paste0(paste0("'", q2_filter_value, "'"), collapse = ','), ')')
        
      }
      
      print(query)
      
      survey_data_filtered <- sqldf(query)
      
    })
  
  survey_data_non_filtered <-
    reactive({
      
      survey_data_filtered <- survey_data_filtered()
      
      query <- "
      SELECT s.* 
      FROM survey_data s
      LEFT JOIN survey_data_filtered sf ON s.id = sf.id
      WHERE sf.id IS NULL
      "
      
      survey_data_non_filtered <- sqldf(query)
      
      
  })
  
  output$survey_data_segement_1_rows <- renderValueBox({
    
    valueBox(
      value = formatC(nrow(survey_data_segement_1())),
      subtitle = "Number of segment 1 responses"
    )
  })
  
  output$survey_data_segement_2_rows <- renderValueBox({
    
    valueBox(
      value = formatC(nrow(survey_data_segement_2())),
      subtitle = "Number of segment 2 filtered responses"
    )
    
    
  })
  
  
    
}