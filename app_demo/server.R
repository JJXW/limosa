server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2) 
  
  
####DATA INPUT AND EXPLORATION####

  original_data_form <-
    eventReactive(input$file1, {

      if(!is.null(input$file1)) {
        dataFile <- input$file1
        survey_data <- read.csv(dataFile$datapath,na.strings = c(""," ","NA"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "latin1")
      } else{
        survey_data <- as.data.frame(matrix(ncol=1,nrow=1,"No Data Uploaded"))
      }

      return(survey_data)

    })


  
  
#ATTEMPTING TO ALLOW CLASS MODIFICATION OF DATAFRAME
  output$sliders <- renderUI({
    class_of_cols <- sapply((1:length(colnames(original_data_form()))),FUN = function(i) class(original_data_form()[,i]))

    class_of_cols <- sapply(class_of_cols,FUN = function(x) if(x %in% c("integer", "numeric")){"numeric"}
                            else if(x %in% c("Date")) {"date"}
      else{"categorical"}
      )
    lapply(1:length(colnames(original_data_form())), function(i) {
      selectInput(inputId = paste0("col_", i), label = paste(colnames(original_data_form())[i]),choices = c("numeric","categorical","date"),multiple = FALSE,
                  selected = class_of_cols[i]
                  )
    })
    
  })  
  

#REASSIGNING CLASS  
 survey_data_reactive <- eventReactive(input$redefine, {
   classes_updated <- sapply((1:length(colnames(original_data_form()))),FUN = function(i) input[[paste("col_",i,sep = "")]])

   dataFile <- input$file1
   survey_data <- read.csv(dataFile$datapath,na.strings = c(""," ","NA"),header = TRUE, stringsAsFactors = FALSE, as.is = TRUE, fileEncoding = "latin1")


   for(i in 1:length(colnames(original_data_form()))){
     if(classes_updated[i] == "numeric") {survey_data[,i] = as.numeric(survey_data[,i])}
     else if(classes_updated[i] == "date") {survey_data[,i] = as.Date(survey_data[,i], origin = "1899-12-30")}
       else {survey_data[,i] = as.character(survey_data[,i])}
   }
   
  final_classes <- sapply((1:length(colnames(original_data_form()))),FUN = function(i) class(survey_data[,i]))

   return(survey_data)
   
 })
                                          
   

  
#OUTPUT THE RAW DATA TABLE AND SUMMARY ON NUMBER OF COLS/ROWS

  output$rawtable <- DT::renderDataTable({
    DT::datatable(data = original_data_form(),
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





####PREDICTION FINDER / TREE CLASSIFICATION####

  observe({values <- colnames(survey_data_reactive())

  #MAKING SURE VARIABLES UPDATE ACROSS SELECTION VARIABLES#
  #automatic segmentation
  updateSelectInput(session,"tree_target_var",label = "I Want To Predict...",choices = c('',values))
  updateSelectInput(session,"tree_split_var",label = "Search For Trends Over...",choices = c('',values))
  })


  unique_outcomes <- eventReactive(input$UseTheseVars_tree, {
    no_blank_data <- filter(survey_data_reactive(),(!!as.symbol(input$tree_target_var)) != "")
    return(length(unique(no_blank_data[,input$tree_target_var])))

  })


  #outputting the frame to populate the table and categorical chart
  tree_model <- eventReactive(input$UseTheseVars_tree, {
    return(masterframeFX(survey_data_reactive(),input$tree_split_var,input$tree_target_var,input$min_leaf, unique_outcomes(), input$pvalue_thresh))

  })
  

  #outputting the dataset for each relevant node in the same order as the masterframe rows
  numeric_nodeframe <- eventReactive(input$UseTheseVars_tree, {
        return(masterframe_nodecuts(survey_data_reactive(),input$tree_split_var,input$tree_target_var,input$min_leaf, unique_outcomes(), input$pvalue_thresh))

  })

 #TABLE OUTPUT
  out_table <- reactive({

    #creating the output table
    out_table = tree_model()
    out_table$model = 1:nrow(out_table)
    
    #allowing the right column name for numeric average (it was outputting oddly)
    if(!(class(survey_data_reactive()[,input$tree_target_var]) %in% c("integer","numeric"))){
      out_table = cbind(out_table[,c('model','n','rule','pvalue')],out_table[,str_detect(colnames(out_table),"avg_")])
      colnames(out_table)[1:3] = c("Model","N","Insight")
      
    } else{
      out_table = cbind(out_table[,c('model','n','rule','pvalue')],Subset_Mean = out_table[,'yval'],Overall_Mean = out_table[,str_detect(colnames(out_table),"avg_")],Difference = out_table[,'Dif_Score'])
      colnames(out_table)[1:3] = c("Model","N","Insight")
      
    }
    if(nrow(out_table)==1){
      out_table[1,2] = "No models found that improve prediction beyond the average distribution"
    }

    return(out_table)

  })

  output$tableTREE <- DT::renderDataTable({

    DT::datatable(data = out_table(),
                  options = list(scrollX = T),rownames = F)
  })



  #download this table
  output$report = downloadHandler(
    filename = function() {
      paste("Prediction Finder Report"," ",Sys.time(),".csv")
    },
    content = function(file){
      write.csv(out_table(), file)
    }
  )
  
  #download the plotly that is outputted below
  output$mainchart = downloadHandler(
    filename = function() {
      paste("Prediction Finder Report"," ",Sys.time(),".png")
    },
    content = function(file){
      export(plot_1(), file=file)
    }
  )
  



  #PLOT OUTPUT
  
  plot_1 <- eventReactive(input$UseTheseVars_tree, {
    
    if(!(class(survey_data_reactive()[,input$tree_target_var]) %in% c("integer","numeric"))){
      
      #CATEGORICAL
      if(nrow(tree_model())>1){
        model_data <- tree_model()
        unique_outcomes <- unique_outcomes()
        
        #creating an average row
        avg_data <- model_data[1, c(1:3, (9+unique_outcomes):(ncol(model_data)-2))]
        avg_data[,c(1:3)] = ""
        
        #binding together
        model_data <- model_data[, c(1:3, 4:(3+unique_outcomes))]
        colnames(avg_data) = colnames(model_data)
        model_data = rbind(avg_data,model_data)
        
        model_data$model <- c(nrow(model_data), 1:(nrow(model_data)-1))
        plot_data <-melt(model_data, id=c(1:3, ncol(model_data)), measure=4:(unique_outcomes+3))
        plot_data$value = round(plot_data$value,2)
        

        p <-
          ggplot() +
          geom_bar(aes(y=value, x=model, fill = variable),
                   data = plot_data,
                   stat = 'identity', position = "fill") + ylab(input$tree_target_var) +
          scale_y_continuous(labels = scales::percent_format())
        
        #using plotly so we can hover
        options(digits = 2)
        p <- ggplotly(p) %>%
          layout(xaxis = list(tickvals = c(1:nrow(model_data)), ticktext = c(1:(nrow(model_data)-2),"","Avg")))
      }
      else{
        overalldata = as.data.frame(table(survey_data_reactive()[,input$tree_target_var]))
        overalldata$overall = "Overall"
        colnames(overalldata)[match('Var1',colnames(overalldata))] <- 'Target_Variable'
        
        p  <- ggplot(overalldata, aes(x = overall, y = Freq, fill = Target_Variable)) +
          geom_col() +
          geom_text(aes(label = ""),
                    position = position_stack(vjust = 0.5)) +
          scale_fill_brewer(palette = "Set2") +
          theme_minimal(base_size = 16) +
          ylab(input$tree_target_var) +
          xlab(NULL)
        
        p <- ggplotly(p)
        
      }
    }
    
    else{
      #NUMERIC
      if(length(numeric_nodeframe())>1){
      response_data <- numeric_nodeframe()
      responses_alone = sapply(response_data, FUN = function(x) (x[,ncol(x)])) #appending all response vars together
      responses_alone[[length(responses_alone)+1]] = as.vector(survey_data_reactive()[,input$tree_target_var]) #adding all the response data to create an average

      response_string = unlist(responses_alone) #appending all response vars together
      
      #creating appropriate model labels for the dataframe
      model_label_1 = (c(rep(1:length(response_data),sapply(response_data, FUN = function(x) (length(x[,ncol(x)]))))))
      model_label_2 = rep("Overall",length(survey_data_reactive()[,input$tree_target_var]))
      model_string = c(model_label_1, model_label_2)

      sort_levels = c(unique(model_string))
      # sort_levels = c((rep(1:length(response_data))-1),"Avg")
      
      p <- ggplot(data.frame(Target_Variable = response_string, Model = factor(model_string, levels = sort_levels, ordered = TRUE )), aes(x=Model, y=Target_Variable, fill=Model)) + geom_boxplot() +
        ylab(input$tree_target_var)
      
      p <- ggplotly(p) %>% layout(autosize = T)
      }
    
    else{
      response_string = survey_data_reactive()[,input$tree_target_var]
      model_string = rep(1, length(response_string))
      
      p <- ggplot(data.frame(Target_Variable = response_string, Overall = factor(model_string)), aes(x=Overall, y=Target_Variable, fill=Overall)) + geom_boxplot() +
        ylab(input$tree_target_var)
      
      p <- ggplotly(p) %>% layout(autosize = T)
    }
    }
    
    

    return(p)
    
  })
  
  output$tree_plot <- renderPlotly({
    withProgress(message = "Rendering plot...", value = 60, max = 100,{
    plot_1()
    })
  })



####DIFFERENCE FINDER####

observe({values <- colnames(survey_data_reactive())

#MAKING SURE VARIABLES UPDATE ACROSS SELECTION VARIABLES#
#automatic splitting
updateSelectInput(session,"diff_split_var",label = "Split The Data By...",choices = c('',values))
updateSelectInput(session,"diff_range_vars",label = "Search For Trends Over...",choices = c('',values))

#COHORT INPUT UPDATES
updateSelectInput(session, "cohort_time", choices=c('',values))
# updateSelectInput(session, "cohort_group", choices=c('',values))
updateSelectInput(session, "cohort_target", choices=c('',values))
updateSelectInput(session, "cohort_id", choices=c('',values))
})
  

diffs_tree <- eventReactive(input$UseTheseVars_diffy, {
if(input$weighted_avg == FALSE){
  #progress bar
  withProgress(message = "Assessing all potential datacuts...", value = 0, max = 100, {
  
  #Upfront variable definition
  split = input$diff_split_var
  search = input$diff_range_vars
  class_of_alldata = sapply(colnames(survey_data_reactive()), FUN = function(x) class(survey_data_reactive()[,x]))
  class_of_search = class_of_alldata[match(search,names(class_of_alldata))]
  numbof_numeric = sum(class_of_search %in% c("integer","numeric"))
  search_numeric = names(class_of_search)[class_of_search %in% c("integer","numeric")]
  numbof_categorical = sum(!class_of_search %in% c("integer","numeric"))
  search_categorical = names(class_of_search)[!(class_of_search %in% c("integer","numeric"))]

  setProgress(value = 1)
  
  
#running numeric
  mlml_num = fulldata_numeric(numbof_numeric,survey_data_reactive(), split, search_numeric)
  setProgress(value = 5)
  
  numstep1 = num_splitframe(numbof_numeric,mlml_num, split, search)
  setProgress(value = 10)
  
  numstep2 = num_frame(numbof_numeric,mlml_num,numstep1,search)
  setProgress(value = 15)
  
  numstep3 = num_split_mean_frame(numbof_numeric,mlml_num,numstep1,search)
  setProgress(value = 20)
  
  numstep4 = num_overall_mean_frame(numbof_numeric,mlml_num, numstep1, search)
  setProgress(value = 25)
  
  numstep5 = num_category_split(numbof_numeric,mlml_num,split,search)
  setProgress(value = 30)
  
  numstep6 = num_question_list(numbof_numeric,mlml_num,split,search)
  setProgress(value = 35)
  
  numstep6a = N_in_category(numbof_numeric,mlml_num,numstep1,search)
  
  numstep7 = numbey_frame(numstep6a, numbof_numeric,numstep5,numstep6,numstep2,numstep3,numstep4,split)
  
  setProgress(value = 40)
  


#running categorical
  mlml_cat = fulldata_categorical(numbof_categorical,survey_data_reactive(), split, search_categorical)
  catstep1 = cat_splitframe(numbof_categorical,mlml_cat, split)
  catstep2 = cat_frame(numbof_categorical,mlml_cat,catstep1,search)
  catstep3 = cat_split_mean_frame(numbof_categorical,mlml_cat,catstep1,search)
  catstep4 = cat_overall_mean_frame(numbof_categorical,mlml_cat,catstep1,search)
  catstep5 = cat_category_split(numbof_categorical,mlml_cat,catstep1,search_categorical)
  catstep6 = cat_variable_list(numbof_categorical,mlml_cat, catstep1, search)
  catstep7 = answer_list(numbof_categorical,mlml_cat, catstep1, search)
  catstep7a = n_list_categorical(numbof_categorical,mlml_cat, catstep1, search)
  
  catstep8 = cattey_frame(catstep7a,numbof_categorical,catstep5,catstep6,catstep7,catstep2,catstep3,catstep4,split)
  
  setProgress(value = 60)
  
  #NEED TO ADD N FOR CATEGORICAL STILL
  if((numbof_categorical>0) & (numbof_numeric>0)){
    final = rbind(catstep8,numstep7) #both numerical and categorical
  }
  else if((numbof_categorical>0) & (numbof_numeric==0)){
    final = catstep8
  }
  else if((numbof_categorical==0) & (numbof_numeric>0)){
    final = numstep7
  }
  
  
  setProgress(value = 90)
  }) #end progress bar for first statement
  
  
  ###IF WEIGHTED AVERAGE DISTRIBUTION
  } else {
    withProgress(message = "Assessing all potential datacuts...", value = 0, max = 100, {
      
      #Upfront variable definition
      split = input$diff_split_var
      search = input$diff_range_vars
     
      #Adding total rows for distribution
      splitframes = num_splitframe_percent(survey_data_reactive(), split, search)
      splitframes = add_totalcol(splitframes,search)
      overall_data = add_totalcol_fulldata(survey_data_reactive(),search)
      setProgress(value = 10)

      #Converting into percents
      splitframes = percent_transform(splitframes,search)
      overall_data = percent_transform_fulldata(overall_data,search)
      setProgress(value = 25)

      perc_pvals = p_value_creation(overall_data,splitframes,search)
      Sys.sleep(1)
      setProgress(value = 50)
      

      perc_groupmeans = group_means_percent(splitframes,search)
      perc_overallmeans = overall_means_percent(overall_data,splitframes,search)
      Sys.sleep(1)
      setProgress(value = 80)
      

      category_labels = num_category_split_percent(overall_data,splitframes,split,search)
      setProgress(value = 90)
      question_labels = num_question_list_percent(overall_data,splitframes,split,search)
      n_per_question = n_list_weighted(overall_data,splitframes,split,search)

      final = percey_frame(n_per_question,category_labels,question_labels,perc_pvals,perc_groupmeans,perc_overallmeans,split)
      
    })#end progress bar for weighted average piece
}
  
  
  
  
 
#filtering and completing the table
  final = filter(final,pval<=input$pvalue_diffy)
  final = final[,c('n_category','cat','var','ans','rule','pval','mean_cat','mean_overall','dif')]
  final = final[order(final$cat,final$var,final$pval),]
  colnames(final) = c("N","Split_Value", "Question","Response","Insight","P-Value","Subset_Mean","Overall_Mean","Difference")

  
  return(final)

  
})#close Difference Finder
  
output$DiffyTable <- DT::renderDataTable({
  DT::datatable(data = diffs_tree(),rownames = FALSE,
                options = list(scrollX = T))
  
})


#download for Difference Finder
diffytree <- reactive({
  diffytree = diffs_tree()
  return(diffytree)
  })

output$report_diffy = downloadHandler(
  filename = function() {
    paste("Difference Finder Report"," ",Sys.time(),".csv")
  },
  content = function(file){
    write.csv(diffytree(), file)
  }
)

cohort_data <- eventReactive(input$UseTheseVars_cohort, {
  
  data <- survey_data_reactive()
  
  data_subset <- data[, c(input$cohort_id, input$cohort_time, input$cohort_target)]
  
  data_subset_names <- names(data_subset)
  
  names(data_subset) <- c('cohort_id', 'time_var', 'cohort_target')
  
  if(input$cohort_time_group == 'day'){
    data_subset <- data_subset %>% 
      mutate(cohort_time_group = as.Date(time_var, format = '%m/%d/%y'))
    group_min <- data_subset %>%
      dplyr::group_by(cohort_id) %>%
      dplyr::summarize(group_time_min = min(cohort_time_group))
    
    data_subset <- data_subset %>%
        dplyr::left_join(group_min)
    
    data_subset <- data_subset %>% 
      mutate(date_diff = as.numeric(cohort_time_group - group_time_min))
      
  } else if(input$cohort_time_group == 'month'){
    data_subset <- data_subset %>% 
      mutate(cohort_time_group = floor_date(as.Date(time_var, format = '%m/%d/%y'), unit = 'month'))
    
    group_min <- data_subset %>%
      dplyr::group_by(cohort_id) %>%
      dplyr::summarize(group_time_min = min(cohort_time_group))
    
    data_subset <- data_subset %>%
      dplyr::left_join(group_min)
    
    data_subset <- data_subset %>%
      mutate(date_diff = interval(ymd(group_time_min),ymd(cohort_time_group)) %/% months(1))
  }
  
  #printing to see what happened to the data

  
  cohort_data <- data_subset %>%
    dplyr::group_by(group_time_min, cohort_time_group) %>% #removed the grouping by date_diff variable for now
    dplyr::summarise(cohort_target = sum(as.numeric(cohort_target), na.rm=T)) 
print(cohort_data)
  

###adding more cohort data modifications
  casted_cohort = dcast(cohort_data,group_time_min ~ cohort_time_group)
  casted_cohort[,2:ncol(casted_cohort)][is.na(casted_cohort[,2:ncol(casted_cohort)])] = 0

  casted_cohort =  casted_cohort[,!(names(casted_cohort)=="NA")]

  cohort_data = melt(casted_cohort,id = 'group_time_min')
  colnames(cohort_data) = c('group_time_min','cohort_time_group','cohort_target')
  return(cohort_data)
  
  })

cohort_count_data <- eventReactive(input$UseTheseVars_cohort, {
  
  data <- survey_data_reactive()
  
  data_subset <- data[, c(input$cohort_id, input$cohort_time)]
  
  data_subset_names <- names(data_subset)
  
  names(data_subset) <- c('cohort_id', 'time_var')
  
  if(input$cohort_time_group == 'day'){
    data_subset <- data_subset %>% 
      mutate(cohort_time_group = as.Date(time_var, format = '%m/%d/%y')) %>%
      group_by(cohort_time_group) %>%
      dplyr::summarize(count = n_distinct(cohort_id))


  } else if(input$cohort_time_group == 'month'){
    data_subset <- data_subset %>% 
      mutate(cohort_time_group = floor_date(as.Date(time_var, format = '%m/%d/%y'), 
                                            unit = 'months')) %>%
      group_by(cohort_time_group) %>%
      dplyr::summarize(count = n_distinct(cohort_id))
    
  }
  
  cohort_count_data <- data_subset
  
  return(cohort_count_data)
  
  
})

cohort_retention_data <- eventReactive(input$UseTheseVars_cohort, {
  
  data <- survey_data_reactive()
  
  data_subset <- data[, c(input$cohort_id, input$cohort_time)]
  
  data_subset_names <- names(data_subset)
  
  names(data_subset) <- c('cohort_id', 'time_var')
  
  if(input$cohort_time_group == 'day'){
    data_subset <- data_subset %>% 
      mutate(cohort_time_group = as.Date(time_var, format = '%m/%d/%y'))
    group_min <- data_subset %>%
      dplyr::group_by(cohort_id) %>%
      dplyr::summarize(group_time_min = min(cohort_time_group))
    
    data_subset <- data_subset %>%
      dplyr::left_join(group_min)
    
    data_subset <- data_subset %>% 
      mutate(date_diff = as.numeric(cohort_time_group - group_time_min))
    
  } else if(input$cohort_time_group == 'month'){
    data_subset <- data_subset %>% 
      mutate(cohort_time_group = floor_date(as.Date(time_var, format = '%m/%d/%y'), unit = 'months'))
    
    group_min <- data_subset %>%
      dplyr::group_by(cohort_id) %>%
      dplyr::summarize(group_time_min = min(cohort_time_group))
    
    data_subset <- data_subset %>%
      dplyr::left_join(group_min)
    
  
    
    ##modifiying to create "0" values for all dates so chart makes sense --- #N33D JON'S HELP HERE
    # casted_cohort = dcast(data_subset,group_time_min ~ cohort_time_group)
    # casted_cohort[,2:ncol(casted_cohort)][is.na(casted_cohort[,2:ncol(casted_cohort)])] = 0
    # 
    # casted_cohort =  casted_cohort[,!(names(casted_cohort)=="NA")]
    # 
    # cohort_data = melt(casted_cohort,id = 'group_time_min')
    # colnames(cohort_data) = c('group_time_min','cohort_time_group','cohort_target')
    # ####
    # data_subset <- cohort_data
    
    data_subset <- data_subset %>%
      mutate(date_diff = interval(ymd(group_time_min),ymd(cohort_time_group)) %/% months(1))
  }
  
  #WILL NEED TO ADD IN THE ZEROS FOR MONTHS AND CAST+MELT LIKE I DID IN THE COHORT PIECE TO GET CLEAN SET OF DATA
  base_count <- data_subset %>%
    # filter(date_diff==0) %>%
    group_by(group_time_min) %>%
    summarise(base_count = n_distinct(cohort_id, na.rm=T))
  print(base_count)
  print("2")
  
  cohort_retention_data <- data_subset %>%
    dplyr::group_by(group_time_min, cohort_time_group) %>%
    dplyr::summarise(count = n_distinct(cohort_id, na.rm=T)) %>%
    dplyr::left_join(base_count) %>%
    mutate(pct = count / base_count) %>%
    mutate(date_diff = interval(ymd(group_time_min),ymd(cohort_time_group)) %/% months(1))
  
  print(cohort_retention_data)
  print("3")
  return(cohort_retention_data)
  
  
})


output$cohort_plot <- renderPlotly({
  
  data <- cohort_data()
  
  
  p <- ggplot(data,
              aes(x=cohort_time_group, y=cohort_target, group = as.character(group_time_min))) +
    geom_area(aes(fill = as.character(group_time_min))) +
    scale_fill_brewer(palette = "Paired") +
    theme_minimal(base_size = 16) +
    xlab("Date") +
    ylab("Value") +
    labs(fill="Cohort Time Group")


  p <- ggplotly(p) %>% layout(autosize = T)
  
  
})





output$cohort_count_plot <- renderPlotly({
  
  data <- cohort_count_data()
  
  p <- ggplot(data, 
              aes(x=cohort_time_group, y=count)) + 
    geom_line() + 
    scale_fill_brewer(palette = "Paired") + 
    theme_minimal(base_size = 16) +
    xlab("Date") +
    ylab("Customer Count") +
    labs(fill="Cohort Time Group")
  
  
  p <- ggplotly(p) %>% layout(autosize = T)
  
  
})

output$cohort_retention_plot <- renderPlotly({
  
  data <- cohort_retention_data()
  
  p <- ggplot(data, 
              aes(x=date_diff, y=pct, group=as.character(group_time_min), colour = as.character(group_time_min))) + 
    geom_line(aes(fill = as.character(group_time_min))) + 
    scale_fill_brewer(palette = "Paired") + 
    theme_minimal(base_size = 16) +
    xlab("Time Period") +
    ylab("Customer Percent Retained") +
    labs(fill="Cohort Time Group")
  
  
  p <- ggplotly(p) %>% layout(autosize = T)
  
  
})

}#close server block
