server <- function(input, output, session) { 

####DATA INPUT AND EXPLORATION####
  survey_data_reactive <-
    reactive({
      
      if(!is.null(input$file1)) {
        dataFile <- input$file1
        survey_data <- read.csv(dataFile$datapath)
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

####AUTO SEGMENTATION####
  round_mean = function(x){
    return(round(mean(x),2))
  }
  round_sd = function(x){
    return(round(sd(x),2))
  }
  
   ##THIS FUNCTION CREATES THE SUMMARY DATA##
   create_data = function(cluster_data){
    
    orignames <- colnames(cluster_data)
    
    #separating the factor data from numerical
    typelist <- lapply(cluster_data,class)
    numer_block <- cluster_data[,typelist=='numerical' | colnames(cluster_data) == 'cluster']
    colnames(numer_block) <- colnames(cluster_data)[typelist=='numerical' | colnames(cluster_data) == 'cluster']
    
    cat_block <- cluster_data[,typelist=='factor'| colnames(cluster_data) == 'cluster']
    colnames(cat_block) <- colnames(cluster_data)[typelist=='factor' | colnames(cluster_data) == 'cluster']

    #analysis for categorical data#
    if(length(colnames(cat_block))>1){
      ###START HERE FOR EDITS ON THIS CAT TABLE OUTPUT###
      categorical_table = function(datacube){
        cluster_list <- unique(datacube$cluster)
        
        #assumes 'cluster' is final column in datacube
        question_list <- colnames(datacube)[-length(colnames(datacube))]
        
        output_frame = as.data.frame(matrix(0,nrow=))
        
        for(i in question_list){
          
        }
        
      }
      
    }
    
    #analysis for numerical data#
    mean_block <- ddply(cluster_data, .(cluster), numcolwise(round_mean))
    mean_block[,1] <- c(seq(1,length(mean_block[,1])*2-1,by=2))
    #datatable((mean_block))
    
    sd_block <- ddply(cluster_data, .(cluster), numcolwise(round_sd))
    sd_block[,1] <- c(seq(2,length(sd_block[,1])*2,by=2))
    #datatable(sd_block)
    
    stack_data <- rbind(mean_block,sd_block)
    stack_data <- stack_data[order(stack_data$cluster),]
    
    output_data <- transpose(stack_data)
    output_data <- output_data[2:length(output_data[,1]),]
    
    #??check the number of rows here... not sure why i need to put the "nrow" piece in otherwise there is mismatch
    output_data$Questions <- orignames[1:nrow(output_data)]
    # return_data <- output_data %>% select(Questions, everything())
    return_data <- output_data[,c(ncol(output_data),1:(ncol(output_data)-1))]
    colnames(return_data)[2:length(colnames(return_data))] <- rep(c("Mean","SD"),ncol(return_data)/2)
    return(as.data.frame(return_data))
  }
  
  ##THIS FUCNTION IS PRETTY MUCH JUST TO GET THE COLOR CODING##
  output_table = function(tablex,NumSeg,cluster_block){
   
    #CREATING TABLES
    #constructing the straight forward tables
    seglabel = c(seq(1,NumSeg))
    segdf = data.frame(matrix(nrow = NumSeg,ncol=nrow(tablex)),row.names=seglabel)
    colnames(segdf) = colnames(row.names(tablex))
    seg_meana <- ddply(cluster_block, .(cluster), numcolwise(round_mean))
    seg_meana = seg_meana[,2:length(seg_meana)]
    seg_sda <- ddply(cluster_block, .(cluster), numcolwise(round_sd))
    seg_sda = seg_sda[,2:length(seg_sda)]
    
    orignames <- colnames(cluster_block)
    
    #constructing the inverse tables (mean or sd of the data for data NOT EQUAL to the cluster of choice)
    seg_meani <- transpose(data.frame(sapply(seglabel, FUN = function(x) sapply(cluster_block[cluster_block$cluster != x,1:ncol(cluster_block)-1], function(y) mean(y,na.rm=TRUE)))))
    
    #no idea what i'm doing with this subsetting of orignames and why it is needed but whatever
    colnames(seg_meani) <- orignames[1:ncol(seg_meani)]
    seg_sdi <- transpose(data.frame(sapply(seglabel, FUN = function(x) sapply(cluster_block[cluster_block$cluster != x,1:ncol(cluster_block)-1], function(y) sd(y,na.rm=TRUE)))))
    colnames(seg_sdi) <- orignames[1:ncol(seg_sdi)]
    
    #constructing the table of differences
    segdiffs <- transpose((seg_meana-seg_meani)/seg_sdi)
    
    
    #COLOR CODING
    #including defining the significance cutoffs for color coding
    color_vector <- c("rgb(222,124,124)","rgb(219,23,23)","rgb(255,255,255)","rgb(218,252,229)","rgb(56,166,93)")
    first_sd_vector <- c(-4,-1,-0.5,0.5,1)
    sec_sd_vector <- c(-1,-0.5,0.5,1,4)
    #Color function creator
    colordic = function(colors, less, greater, ref){
      return(colors[greater>ref & less<=ref])
    }
    
    #Creating color coding dataframe based on difference tables
    colorcode <- sapply(seglabel,FUN = function(y) sapply(segdiffs[,y],FUN = function(x) colordic(color_vector,first_sd_vector,sec_sd_vector,x)))
    
    #creating formatted container
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Segments'),
          lapply(c(seq(1:NumSeg)),th,colspan=2)
        ),
        tr(
          lapply(rep(c('Mean', 'SD'),NumSeg),th)
        )
      )
    ))
    
    
    
    #outputting data in the formatted table
    return(datatable(tablex, container = sketch, rownames = FALSE)%>% formatStyle(columns = c(seq(2,NumSeg*2,by=2)),target="cell", backgroundColor = styleEqual(as.matrix(tablex[,c(seq(2,NumSeg*2,by=2))]),colorcode)))
  }
  
  framex <- eventReactive(input$UseTheseVars,{
   
    #create strings of the important inputs
    SegNames <- c(input$segvar1,input$segvar2,input$segvar3,input$segvar4)
    SegVarTypes <- c(input$segvar1_type,input$segvar2_type,input$segvar3_type,input$segvar4_type)
    SegVarPurp <- c(input$segvar1_purp,input$segvar2_purp,input$segvar3_purp,input$segvar4_purp)
    CatNames <- SegNames[SegVarTypes == 'categorical']
    NumNames <- SegNames[SegVarTypes == 'numerical']
    
    Name_Count <- sum(SegNames!="")
    Unique_Name_Count <- length(unique(SegNames[SegNames!=""]))
    CatCount <- sum(SegVarTypes =='categorical')
    NumerCount <- sum(SegVarTypes == 'numerical')

    #returning error if selected multiple of the same variable
    if(Name_Count != Unique_Name_Count) {
    return(data.table("error, please check your inputs!"))
    } else {
      
      if(NumerCount == Name_Count){
        data_1 <- as.data.frame(survey_data_reactive())
        data_2 <- na.omit(data_1[,colnames(data_1) %in% SegNames])
        data_3 <- scale(data_2)
        #??how to handle these nas instead of removing?
        data_3[is.na(data_3)] <- 0
        k_analysis <- kmeans(data_3,input$segment_num)
        #??check the number of rows here... not sure why i need to put the "nrow" piece in otherwise there is mismatch
        data_2$cluster <- as.factor(c(k_analysis$cluster,1,1))[1:nrow(data_2)]
      } else{
          
          if(CatCount == Name_Count){
            data_1 <- as.data.frame(survey_data_reactive())
            #changing values to factors
            data_2 <- sapply(na.omit(data_1[,colnames(data_1) %in% CatNames]),factor)
            data_3 <- as.data.frame(data_2)
            #??how to handle these nas instead of removing?
            data_3[is.na(data_3)] <- 0
            k_analysis <- kmodes(data_3,input$segment_num)
            #??check the number of rows here... not sure why i need to put the "nrow" piece in otherwise there is mismatch
            data_2$cluster <- as.factor(c(k_analysis$cluster,1,1))[1:nrow(data_2)]    
          } else{
            
            data_1 <- as.data.frame(survey_data_reactive())
            data_2 <- na.omit(data_1[,colnames(data_1) %in% SegNames])
            ###ACCOUNTING FOR USING LAPPLY ON A SINGLE ITEM LIST###
            if(length(CatNames)>1){
            data_2[,colnames(data_2) %in% CatNames] <- lapply(data_2[,colnames(data_2) %in% CatNames],as.factor)
            } else {
              data_2[,colnames(data_2) %in% CatNames] <- as.factor(data_2[,colnames(data_2) %in% CatNames])
            }
            ###ALSO ENSURING NUMERIC VARIABLES ARE CLASS NUMERIC###
            if(length(NumNames)>1){
              data_2[,colnames(data_2) %in% NumNames] <- lapply(data_2[,colnames(data_2) %in% NumNames],as.numeric)
            } else {
              data_2[,colnames(data_2) %in% NumNames] <- as.numeric(data_2[,colnames(data_2) %in% NumNames])
            }
            
            data_3 <- data_2
           
            #??how to handle these nas instead of removing?
            data_3[is.na(data_3)] <- 0
            k_analysis <- kproto(data_3,input$segment_num)
            #??check the number of rows here... not sure why i need to put the "nrow" piece in otherwise there is mismatch
            data_2$cluster <- as.factor(c(k_analysis$cluster,1,1))[1:nrow(data_2)]
          }
        
        }
      
    }
    
    #running the aggregation function on the data_1 that we just created
    orignames <- colnames(data_2)
    tablex <- create_data(data_2)
    print(tablex[1:10,1:3])
    returntable <- output_table(tablex,input$segment_num,data_2)
    return(returntable)
    
   
  })
 
  output$segtable <- renderDataTable({framex()
    })
  
####JONS FILTERING CODE####
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
  
####DATA QUALITY CHECK####
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