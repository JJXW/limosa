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
  
  ####Adding Quality Data Stuff###
  Tablefx = function (QualData){
    
    builder <- as.data.frame(apply(QualData[,2:ncol(QualData)],MARGIN = 2, FUN = function (x) length(unique(x))))
    builder$Question <- rownames(builder)
    
    #function that takes a set of vlaues and appends it to a list
    builder2 <- as.data.frame(rep(row.names(builder),builder[,1]))
    
    
    numbuild = function(listx){
      x=c()
      for(i in 1:nrow(listx)){
        y=seq(1, listx[i,1])
        x=append(x,y)
      }
      return(x)
    }
    
    output = as.data.frame(builder2[,1],ncol = 3)
    output[,2] <- as.array((numbuild(builder)))
    colnames(output)<- c("Question","Choice")
    
    #adding aggregation count
    output_join <- left_join(output,builder,by="Question")
    colnames(output_join)[3] <- "Expected"
    output_join$Expected = round(nrow(QualData)/output_join$Expected,2)
    
    indexm = function(quest, vals) {
      return(sum(QualData[,match(quest,colnames(QualData))] == vals)) 
    }
    
    output_join$Actual <- mapply(indexm,output_join$Question,vals=output_join$Choice)
    output_join$Counter <- c(1: nrow(output_join))
    return(output_join)
  }
  
  
  
  observeEvent(input$go,{
    
    output$expectation <- renderPlot({
      
      #Count how many answer choices for each question
      ##Note: assumes that at least one of each answer choice was selected (to modify that assumption in the future)
      
      #Receiving the input file
      inFile <- input$inFile
      
      if(is.null(inFile))
        return(NULL)
      
      QData <- read.csv(inFile$datapath)
      
      #running
      output_table <- Tablefx(QData)
      
      #FUNCTION TO OUTPUT PLOT
      answer_freq = function(outputtable){
        return(ggplot(outputtable, aes(x=Counter,y=Expected))      +
                 geom_point() +
                 geom_point(aes(y=Actual, color="disp")) +
                 ylab(label = "Distribution") +
                 xlab(label = "All Questions And Answer Choices")
        )
      }
      
      #running
      answer_freq(output_table)
    })
    
    
    
    output$normal <- renderPlot({
      
      
      #Receiving the input file
      inFile <- input$inFile
      
      if(is.null(inFile))
        return(NULL)
      
      QData <- read.csv(inFile$datapath)
      
      #running
      output_table <- Tablefx(QData)
      ###END REPEAT###
      
      
      #STATISTICAL TESTING??
      statblock <- output_table
      statblock$chi <- (statblock$Actual-statblock$Expected)/statblock$Expected
      
      #unused so far#
      #statresults <- shapiro.test(statblock$chi)  
      
      
      #NORMAL ERROR PLOT
      normalhist = function(stattable){
        return(ggplot(stattable, aes(x = chi)) + 
                 geom_histogram(aes(y =..density..),
                                breaks = seq(min(stattable$chi), max(stattable$chi), by = (max(stattable$chi)-min(stattable$chi))/50),
                                colour = "black",
                                fill = "white") +
                 stat_function(fun = dnorm, args = list(mean = mean(stattable$chi), sd = sd(stattable$chi)))
        )
      }
      
      #running
      normalhist(statblock)
      
    })
    
    
    output$testable <- renderDataTable({
      ####REPEAT###
      #Count how many answer choices for each question
      ##Note: assumes that at least one of each answer choice was selected (to modify that assumption in the future)
      
      #Receiving the input file
      inFile <- input$inFile
      
      if(is.null(inFile))
        return(NULL)
      
      QData <- read.csv(inFile$datapath)
      
      #running
      output_table <- Tablefx(QData)
      
      
      #STATISTICAL TESTING??
      statblock <- output_table
      statblock$chi <- (statblock$Actual-statblock$Expected)/statblock$Expected
      
      ###END REPEAT###
      
      
      #OUTPUT TOP 10 ERRORS
      tabletop10 = function(stattable){
        l <- order(abs(stattable$chi),decreasing = TRUE)
        sorttable <- stattable[l,]
        sorttable_sub <- sorttable[1:10,1:4]
        return(datatable(sorttable_sub))
        
      }
      
      #running
      tabletop10(statblock)
      
      
    })
    
  })
  

}