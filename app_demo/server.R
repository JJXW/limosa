server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2) 
  
####DATA INPUT AND EXPLORATION####

  survey_data_reactive <-
    reactive({

      if(!is.null(input$file1)) {
        dataFile <- input$file1
        survey_data <- read.csv(dataFile$datapath, header = TRUE)
      } else{
        survey_data <- as.data.frame(matrix(ncol=1,nrow=1,"No Data Uploaded"))
      }

      return(survey_data)

    })

 observe({values <- colnames(survey_data_reactive())

 #MAKING SURE VARIABLES UPDATE ACROSS SELECTION VARIABLES#
    #automatic segmentation
 updateSelectInput(session,"segvar1",label = "Variable 1: Select",choices = c('',values))
 updateSelectInput(session,"segvar2",label = "Variable 2: Select",choices = c('',values))
 updateSelectInput(session,"segvar3",label = "Variable 3: Select",choices = c('',values))
 updateSelectInput(session,"segvar4",label = "Variable 4: Select",choices = c('',values))
 })


 ##MANUAL SEGMENTATION INPUT DEFINITION
 observe({values <- colnames(survey_data_reactive())
    #manual segmentation
 updateSelectInput(session,"segment_var_1",label = "Variable 1:",choices = c('',values))
 updateSelectInput(session,"segment_var_2",label = "Variable 2:",choices = c('',values))
 updateSelectInput(session,"segment_var_3",label = "Variable 3:",choices = c('',values))
 updateSelectInput(session,"segment_var_4",label = "Variable 4:",choices = c('',values))
 })

    # manual segmentation choices
    ##VARIABLE 1
observe({
 if(input$segment_var_1 == ''){
   var1_choice <- ""
 } else{
   if(input$type_var_1 == 'categorical'){
  var1_choice <- as.character(unique(isolate({survey_data_reactive()})[,input$segment_var_1]))
   } else{
     var1_choice <- unique(isolate({survey_data_reactive()})[,input$segment_var_1])
   }


 updateSelectInput(session,"var1_seg1",label = "Pick Range:",choices = c('',var1_choice))
 updateSelectInput(session,"var1_seg2",label = "Pick Range:",choices = c('',var1_choice))
 updateSelectInput(session,"var1_seg3",label = "Pick Range:",choices = c('',var1_choice))
 updateSelectInput(session,"var1_seg4",label = "Pick Range:",choices = c('',var1_choice))
 }
})


observe({
  if(input$segment_var_2 == ''){
    var2_choice <- ""
  } else{
    if(input$type_var_2 == 'categorical'){
      var2_choice <- as.character(unique(isolate({survey_data_reactive()})[,input$segment_var_2]))
    } else{
      var2_choice <- unique(isolate({survey_data_reactive()})[,input$segment_var_2])
    }

 updateSelectInput(session,"var2_seg1",label = "Pick Range:",choices = c('',var2_choice))
 updateSelectInput(session,"var2_seg2",label = "Pick Range:",choices = c('',var2_choice))
 updateSelectInput(session,"var2_seg3",label = "Pick Range:",choices = c('',var2_choice))
 updateSelectInput(session,"var2_seg4",label = "Pick Range:",choices = c('',var2_choice))
  }
})

observe({
  if(input$segment_var_3 == ''){
    var3_choice <- ""
  } else{
    if(input$type_var_3 == 'categorical'){
      var3_choice <- as.character(unique(isolate({survey_data_reactive()})[,input$segment_var_3]))
    } else{
      var3_choice <- unique(isolate({survey_data_reactive()})[,input$segment_var_3])
    }

 updateSelectInput(session,"var3_seg1",label = "Pick Range:",choices = c('',var3_choice))
 updateSelectInput(session,"var3_seg2",label = "Pick Range:",choices = c('',var3_choice))
 updateSelectInput(session,"var3_seg3",label = "Pick Range:",choices = c('',var3_choice))
 updateSelectInput(session,"var3_seg4",label = "Pick Range:",choices = c('',var3_choice))
  }
})

observe({
  if(input$segment_var_4 == ''){
    var4_choice <- ""
  } else{
    if(input$type_var_4 == 'categorical'){
      var4_choice <- as.character(unique(isolate({survey_data_reactive()})[,input$segment_var_4]))
    } else{
      var4_choice <- unique(isolate({survey_data_reactive()})[,input$segment_var_4])
    }

   updateSelectInput(session,"var4_seg1",label = "Pick Range:",choices = c('',var4_choice))
   updateSelectInput(session,"var4_seg2",label = "Pick Range:",choices = c('',var4_choice))
   updateSelectInput(session,"var4_seg3",label = "Pick Range:",choices = c('',var4_choice))
   updateSelectInput(session,"var4_seg4",label = "Pick Range:",choices = c('',var4_choice))
  }
 })


##END OF MANUAL TABLE INPUT DEFINITION##

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


###EVERYTHING FOR AUTOSEGMENTATION IS ESSENTIALLY NESTED WITHIN THIS LARGER REACTIVE FUNCTION###
  framex <- eventReactive(input$UseTheseVars,{

    #create strings of the important inputs
    SegNames <- c(input$segvar1,input$segvar2,input$segvar3,input$segvar4)
    SegNames <- SegNames[SegNames != ""]
    SegVarTypes <- c(input$segvar1_type,input$segvar2_type,input$segvar3_type,input$segvar4_type)
    SegVarTypes <- SegVarTypes[SegVarTypes != ""]
    SegVarTypes[length(SegVarTypes)+1] <- 'cluster'
    SegVarPurp <- c(input$segvar1_purp,input$segvar2_purp,input$segvar3_purp,input$segvar4_purp)
    CatNames <- SegNames[SegVarTypes == 'categorical']
    NumNames <- SegNames[SegVarTypes == 'numeric']

    Name_Count <- sum(SegNames!="")
    Unique_Name_Count <- length(unique(SegNames[SegNames!=""]))
    CatCount <- sum(SegVarTypes =='categorical')
    NumerCount <- sum(SegVarTypes == 'numeric')

    ##THIS FUNCTION CREATES THE DATA TO BE PASSED TO COLOR CODING##
    create_data = function(cluster_data,Types){

      orignames <- colnames(cluster_data)

      numer_block <- cluster_data[,Types == 'numeric' | colnames(cluster_data) == 'cluster']

      if(sum(Types =='numeric')>0){
        numer_block <- as.data.frame(apply(numer_block, MARGIN =2, FUN = as.numeric))
        colnames(numer_block) = colnames(cluster_data)[Types =='numeric' | colnames(cluster_data) == 'cluster']

      } else{
        #no numerics to create
        numer_block <- NULL
      }


      cat_block <- cluster_data[,Types == 'categorical' | colnames(cluster_data) == 'cluster']


      if(sum(Types=='categorical')>0){
        cat_block <- as.data.frame(sapply(colnames(cat_block), FUN = function(x) as.factor(cat_block[,x])))
        colnames(cat_block) = colnames(cluster_data)[Types =='categorical' | colnames(cluster_data) == 'cluster']
      } else{
        cat_block <- as.factor(cat_block)
      }


      #analysis for CATEGORICAL data#
      if(CatCount > 0){
        categorical_table = function(datacube){
          cluster_list <- unique(datacube$cluster)
          cluster_count <- length(cluster_list)

          #list of categorical questions but without the "cluster" column
          # colnames(cat_block) <- colnames(cluster_data)[Types=='factor' | colnames(cluster_data) == 'cluster']
          question_list <- colnames(cat_block)[-match('cluster',colnames(cat_block))]


          #function to create the number of rows needed
          rowcount = function(x) {
            return(length(unique(x)))
          }

          rows_of_catframe = sum(unlist(lapply(as.data.frame(cat_block[,-match('cluster',colnames(cat_block))]),rowcount)))

          output_frame = as.data.frame(matrix(0,nrow=rows_of_catframe,ncol=cluster_count*2+2))
          colnames(output_frame) = c('Question','Answer',rep(c('Mean','SD'),cluster_count))

          #loop to place values correctly in a table getting ready for output
          #runs for every unique answer choice of every categorical question
          rowq = 1
          for(q in question_list){
            for(u in (unique(datacube[,q]))){
              colq = 3
              for (c in cluster_list[order(cluster_list)]) {
                output_frame[rowq,1] = q
                output_frame[rowq,2] = u
                output_frame[rowq,colq] = sum(datacube[,q]==u & datacube$cluster==c)/sum(datacube$cluster==c)
                colq = colq + 2
              }
              rowq = rowq+1
            }

          }
          #here ends the categorical table function

          return(output_frame)
        }
      } else {
        #ADD CASE#
      }
      #here ends the if length of col names of cat block >1 ... need to add the 1 case)
      #}

      #??May have to edit this numerical analysis block to only operate on numerical columns#
      if(length(colnames(numer_block))>1){
        #analysis for numeric data#

        mean_block <- ddply(numer_block, .(cluster), numcolwise(round_mean))
        sd_block <- ddply(numer_block, .(cluster), numcolwise(round_sd))
        stack_data <- rbind(mean_block,sd_block)

        stack_data <- stack_data[order(stack_data$cluster),]
        output_data <- transpose(stack_data)
        output_data <- output_data[-1,]

        #??check the number of rows here... not sure why i need to put the "nrow" piece in otherwise there is mismatch
        output_data$Question <- orignames[-length(orignames)][SegVarTypes == 'numeric']
        output_data$Answer <- ""

        # return_data <- output_data %>% select(Question, everything())
        return_data <- output_data[,c(((ncol(output_data)-1):ncol(output_data)),1:(ncol(output_data)-2))]
        colnames(return_data)[3:length(colnames(return_data))] <- rep(c("Mean","SD"),(ncol(return_data)-2)/2)
      }else{
        #add case for only one numberical to output return data
      }

      #RETURNING THE RIGHT DATA ACROSS CAT AND NUM CATEGORIES

      #ONLY CATEGORICAL CASE
      if(CatCount == Name_Count){
          if(CatCount>1){
            output_frame = categorical_table(cluster_data)
            compiled_data = output_frame
          }
          else {
            #may not need this clause, but i think there is something in categorical_table() that will need to be changed for single case
            #need to think through what will happen if there will be one vector
          }
      }
      #ONLY NUMERIC CASE
          else if(NumerCount == Name_Count){
            compiled_data = return_data
      }

      #CATEGORICAL + NUMERIC CASE
              else{
                output_frame = categorical_table(cat_block)
                output_frame$type = "categorical"
                return_data$type = "numeric"
                compiled_data <- rbind(return_data,output_frame)

              }

      return(as.data.frame(compiled_data))
    }
    ###END CREATE DATA###



    ###BEGIN COLOR CODING FUNCTION###
    output_table = function(tablex,NumSeg,cluster_block){

      #CREATING TABLES
      #constructing the straight forward tables
      #IF NUMERICAL ALONE
      seglabel = c(seq(1,NumSeg))
        ##NUMERIC ONLY##
       if(NumerCount == Name_Count){
          segdf = data.frame(matrix(nrow = NumSeg,ncol=nrow(tablex)),row.names=seglabel)
          #changed colnames from being row.names
          colnames(segdf) = tablex[,1]
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
          if(seg_sdi>0){
            segdiffs <- transpose((seg_meana - seg_meani) / seg_sdi)
          } else{
            segdiffs <- transpose(seg_meana - seg_meani)
          }
       }
      ##CATEGORICAL ONLY##
      else if(CatCount==Name_Count){
        tablexcol = seq(3,ncol(tablex),by=2)
        obs_val = tablex[,tablexcol]


        expected_val <- sapply(1:nrow(obs_val), FUN = function(y) sapply(1:ncol(obs_val), FUN = function(x) mean(as.numeric(obs_val[x,-y]))))
        segdiffs <- (obs_val - expected_val)/expected_val
        ##?? HERE not working to calculate segdiffs

        segdiffs[segdiffs == Inf | segdiffs == -Inf | segdiffs == NaN] = 0
        ##?? HERE - COME BACK TO CORRECT THIS CHI SQUARE and make the color coding meaningful###

        #To format as percent
        percent <- function(x, digits = 2, format = "f", ...) {
          paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
        }
        tablex[,3:ncol(tablex)] = apply(tablex[,3:ncol(tablex)],MARGIN = c(1,2),FUN = percent)
        tablex[,seq(4,ncol(tablex),by=2)]=""
      }
      ##BOTH NUMERIC AND CATEGORICAL##
      else{

        #breaking the table into its pieces
        cat_tablex <- tablex[tablex[,'type']=='categorical',]
        num_tablex <- tablex[tablex[,'type']=='numeric',]


        ##NUMERIC PART OF SEGDIFFS##
        segdf = data.frame(matrix(nrow = NumSeg,ncol=nrow(num_tablex)),row.names=seglabel)
        #getting rid of the type columns
        num_tablex = num_tablex[,-ncol(num_tablex)]
        #changed colnames from being row.names
        colnames(segdf) = num_tablex[,1]
        seg_meana <- ddply(cluster_block, .(cluster), numcolwise(round_mean))


        seg_meana = seg_meana[,2:length(seg_meana)]

        seg_sda <- ddply(cluster_block, .(cluster), numcolwise(round_sd))
        seg_sda = seg_sda[,2:length(seg_sda)]

        orignames <- colnames(cluster_block)

        #constructing the inverse tables (mean or sd of the data for data NOT EQUAL to the cluster of choice)
        num_cluster_block <- cluster_block[,SegVarTypes=='numeric' | colnames(cluster_block) == 'cluster']

        if(NumerCount>1){
        seg_meani <- transpose(data.frame(sapply(seglabel, FUN = function(x) sapply(num_cluster_block[num_cluster_block$cluster != x,1:ncol(num_cluster_block)-1], function(y) mean(y,na.rm=TRUE)))))
        } else{
          seg_meani <- data.frame(sapply(seglabel, FUN = function(x) mean(num_cluster_block[,1][num_cluster_block$cluster !=x])))
      }


        #no idea what i'm doing with this subsetting of orignames and why it is needed but whatever
        colnames(seg_meani) <- orignames[1:ncol(seg_meani)]



        if(NumerCount>1){
        seg_sdi <- transpose(as.data.frame(sapply(seglabel, FUN = function(x) sapply(num_cluster_block[num_cluster_block$cluster != x,1:ncol(num_cluster_block)-1], function(y) sd(y,na.rm=TRUE)))))
        }
        else{
          seg_sdi <- data.frame(sapply(seglabel, FUN = function(x) sd(num_cluster_block[,1][num_cluster_block$cluster !=x])))
        }
        colnames(seg_sdi) <- orignames[1:ncol(seg_sdi)]



        #constructing the table of differences
        if(seg_sdi>0){
          segdiffs_num <- ((seg_meana - seg_meani) / seg_sdi)
        } else{
          segdiffs_num <- (seg_meana - seg_meani)
        }


        segdiffs_num <- transpose(segdiffs_num)

        colnames(segdiffs_num) <- c(paste(rep("seg",ncol(segdiffs_num)),1:ncol(segdiffs_num)))
        ##END NUMERICAL PART OF SEGDIFFS##



        ##CATEGORICAL PART OF SEGDIFFS##

        tablexcol = seq(3,ncol(cat_tablex),by=2)
        obs_val = cat_tablex[,tablexcol]
        obs_val = obs_val[,-ncol(obs_val)]

        expected_val <- sapply(1:nrow(obs_val), FUN = function(y) sapply(1:ncol(obs_val), FUN = function(x) mean(as.numeric(obs_val[x,-y]))))

        segdiffs_cat <- (obs_val - expected_val)/expected_val
        colnames(segdiffs_cat) <- c(paste(rep("seg",ncol(segdiffs_cat)),1:ncol(segdiffs_cat)))

        segdiffs_cat[segdiffs_cat == Inf | segdiffs_cat == -Inf | segdiffs_cat == NaN] = 0
        ##?? HERE - COME BACK TO CORRECT THIS CHI SQUARE and make the color coding meaningful###

        #To format as percent
        percent <- function(x, digits = 2, format = "f", ...) {
          paste0(formatC(100 * as.numeric(x), format = format, digits = digits, ...), "%")
        }



        tablex[tablex[,'type']=='categorical',seq(4,ncol(tablex),by=2)]=""

        tablex[tablex[,'type']=='categorical',seq(3,ncol(tablex),by=2)] = apply(tablex[tablex[,'type']=='categorical',seq(3,ncol(tablex),by=2)],MARGIN = c(1,2),FUN = percent)



        ##END OF CATEGORICAL PART OF SEGDIFFS##

        #Getting rid of the "TYPE" variable in tablex
        tablex = tablex[,-ncol(tablex)]

        segdiffs = rbind(segdiffs_num,segdiffs_cat)


      }



      #COLOR CODING
      #including defining the significance cutoffs for color coding
      color_vector <- c("rgb(222,124,124)","rgb(219,23,23)","rgb(255,255,255)","rgb(218,252,229)","rgb(56,166,93)")
      first_sd_vector <- c(-1000,-1,-0.5,0.5,1)
      sec_sd_vector <- c(-1,-0.5,0.5,1,1000)
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
            th(rowspan = 1, ""),
            th(rowspan = 1, 'Segments'),
            lapply(c(seq(1:NumSeg)),th,colspan=2)
          ),
          tr(
            th(rowspan = 1, 'Question'),
            th(rowspan = 1, 'Answer'),
            lapply(rep(c('Mean', 'SD'),NumSeg),th)
          )
        )
      ))



      #outputting data in the formatted table

      #to remove SD values for these vate
      return(datatable(tablex, container = sketch, options = list(paging = FALSE), rownames = FALSE)%>% formatStyle(columns = c(seq(3,NumSeg*2+1,by=2)),target="cell", backgroundColor = styleEqual(as.matrix(tablex[,c(seq(3,NumSeg*2+1,by=2))]),colorcode)))
    }
    ###END COLOR CODING###

    #THIS STUFF IS RUN OUTSIDE THE FUNCTIONS INDEPENDENTLY
    #returning error if selected multiple of the same variable
    if(Name_Count != Unique_Name_Count) {
    return(data.table("error, please check your inputs!"))
    } else {
      #ONLY NUMERIC VARIABLES
      if(NumerCount == Name_Count){
        data_1 <- as.data.frame(survey_data_reactive())
        data_2 <- na.omit(data_1[,colnames(data_1) %in% SegNames])
        data_3 <- scale(data_2)
        #??how to handle these nas instead of removing?
        data_3[is.na(data_3)] <- 0
        k_analysis <- kmeans(data_3,input$segment_num)
        #??check the number of rows here... not sure why i need to put the "nrow" piece in otherwise there is mismatch

        data_2$cluster <- as.factor(k_analysis$cluster)
        # data_2$cluster <- as.factor(c(k_analysis$cluster,1,1))[1:nrow(data_2)]

      } else{
          #ONLY CATEGORICAL VARIABLES
          if(CatCount == Name_Count){
            data_1 <- as.data.frame(survey_data_reactive())
            #changing values to factors
            data_2 <- as.data.frame(sapply(na.omit(data_1[,colnames(data_1) %in% CatNames]),as.factor))
            data_3 <- as.data.frame(data_2)
            #??how to handle these nas instead of removing?
            data_3[is.na(data_3)] <- 0
            k_analysis <- kmodes(data_3,input$segment_num)
            #??check the number of rows here... not sure why i need to put the "nrow" piece in otherwise there is mismatch
            #HERE IS THER ISSUE - make sure that this does not get "coerced into a list"
            data_2$cluster <- c(k_analysis$cluster,1,1)[1:nrow(data_2)]
                }
                  # BOTH CATEGORICAL AND NUMERIC VARIABLES
                  else{

                  data_1 <- as.data.frame(survey_data_reactive())
                  data_2 <- na.omit(data_1[,colnames(data_1) %in% SegNames])
                  data_2 <- data_2[,SegNames]

                  ###ACCOUNTING FOR USING LAPPLY ON A SINGLE ITEM LIST###
                  if(sum(CatNames != "")>1){
                    data_2[,colnames(data_2) %in% CatNames] <- lapply(data_2[,colnames(data_2) %in% CatNames],as.factor)
                  } else {
                    data_2[,colnames(data_2) %in% CatNames] <- as.factor(data_2[,colnames(data_2) %in% CatNames])
                  }

                  ###ALSO ENSURING NUMERIC VARIABLES ARE CLASS NUMERIC###
                  if(sum(NumNames != "")>1){
                    data_2[,colnames(data_2) %in% NumNames] <- lapply(data_2[,colnames(data_2) %in% NumNames],as.numeric)
                  } else {
                    data_2[,colnames(data_2) %in% NumNames] <- as.numeric(data_2[,colnames(data_2) %in% NumNames])
                  }

                  data_3 <- data_2
                  data_3[,SegVarTypes=='numeric'] <- scale(data_3[,SegVarTypes=='numeric'])
                  k_analysis <- kproto(data_3,input$segment_num)
                  data_2$cluster <- as.factor(k_analysis$cluster)

          }
        }

    }

    #seeing if I can correct for the fact that sometimes k analysis coerces data into fewer clusters
    Segment_Number <- length(unique(data_2$cluster))


    #creating the N outputs
    output$Nseg1 <- renderText(paste("N Segment 1: ",sum(data_2$cluster==1)))
    output$Nseg2 <- renderText(paste("N Segment 2: ",sum(data_2$cluster==2)))
    output$Nseg3 <- renderText(paste("N Segment 3: ",sum(data_2$cluster==3)))
    output$Nseg4 <- renderText(paste("N Segment 4: ",sum(data_2$cluster==4)))

  ####OUTPUTTING QUALITY SCORE####
    output$QualityScore <- renderValueBox({
      tablex <- create_data(data_2, SegVarTypes)

      dif_table <- c(1:nrow(tablex))
      counter = 1
      for(i in 1:nrow(tablex)){
        #for numerics
        if(tablex[i,ncol(tablex)] == "numeric"){
        #finding the overall sd and means
        mean_whole = mean(data_2[,tablex[i,1]])
        sd_whole = sd(data_2[,tablex[i,1]])

        t_table <- transpose(tablex[,seq(3,ncol(tablex)-1,by=2)])
        sd_segs = sd(t_table[,i])
        dif_table[i] <- sd_segs/(sd_whole/3)
        i=i+1
        }
        else{
          t_table <- transpose(tablex[,seq(3,ncol(tablex)-1,by=2)])
          sd_segs = sd(t_table[,i])
          dif_table[i] <- sd_segs/(0.7/3)
          i=i+1
        }
      }

      QS = sum(dif_table)/length(dif_table)

    if(QS>2){
      valueBox(
        value = formatC("Highly Different"),
        subtitle = "Significance of Your Segments"
      )}
      else if (QS<0.5){
        valueBox(
          value = formatC("Not Very Different"),
          subtitle = "Significance of Your Segments"
        )
      } else{
        valueBox(
          value = formatC("Fairly Different"),
          subtitle = "Significance of Your Segments"
        )
      }

    })



  #RUNNING THE FUNCTIONS AND OUPUTTING OUR DESIRED TABLE
    orignames <- colnames(data_2)
    tablex <- create_data(data_2, SegVarTypes)
    returntable <- output_table(tablex,Segment_Number,data_2)

  #TRYING TO CREATE DOWNLOAD ABILITY - DOES NOT WORK#
    output$down <- downloadHandler(


      #Specify the filename
      filename = function() {
        "yoursegements.csv"
      },
      content = function(file){
        write.csv()
      }
    )


    return(returntable)



  })
  ###END OF FRAMEX MASSIVE REACTIVE FUNCTION###
  output$segtable <- renderDataTable({framex()
    })


############################ BREAK FOR NEXT SECTION ########################################

#### MANUAL SEGMENTATION STUFF ####
  ###EVERYTHING FOR MANUAL SEG IS ESSENTIALLY NESTED WITHIN THIS LARGER REACTIVE FUNCTION###
  framex_man <- eventReactive(input$UseTheseVars_man,{

    #create strings of the important inputs
    SegNames_man <- c(input$segment_var_1,input$segment_var_2,input$segment_var_3,input$segment_var_4)
    SegNames_man <- SegNames_man[SegNames_man != ""]

    SegVarTypes_man <- c(input$type_var_1,input$type_var_2,input$type_var_3,input$type_var_4)
    SegVarTypes_man <- SegVarTypes_man[SegVarTypes_man != ""]
    SegVarTypes_man[length(SegVarTypes_man)+1] <- 'cluster'
    # SegVarPurp <- c(input$segvar1_purp,input$segvar2_purp,input$segvar3_purp,input$segvar4_purp)
    CatNames_man <- SegNames_man[SegVarTypes_man == 'categorical']
    NumNames_man <- SegNames_man[SegVarTypes_man == 'numeric']

    Name_Count_man <- sum(SegNames_man!="")
    Unique_Name_Count_man <- length(unique(SegNames_man[SegNames_man!=""]))
    CatCount_man <- sum(SegVarTypes_man =='categorical')
    NumerCount_man <- sum(SegVarTypes_man == 'numeric')

    ##THIS FUNCTION CREATES THE DATA TO BE PASSED TO COLOR CODING##
    create_data_man = function(cluster_data,Types){

      orignames <- colnames(cluster_data)

      numer_block <- cluster_data[,Types == 'numeric' | colnames(cluster_data) == 'cluster']

      if(sum(Types =='numeric')>0){
        numer_block <- as.data.frame(apply(numer_block, MARGIN =2, FUN = as.numeric))
        print("OKOKOK")
        print(head(cluster_data))
        print(head(numer_block))
        print(colnames(cluster_data)[Types =='numeric' | colnames(cluster_data) == 'cluster'])
        colnames(numer_block) = colnames(cluster_data)[Types =='numeric' | colnames(cluster_data) == 'cluster']

      } else{
        #no numerics to create
        numer_block <- NULL
      }


      cat_block <- cluster_data[,Types == 'categorical' | colnames(cluster_data) == 'cluster']


      if(sum(Types=='categorical')>0){
        cat_block <- as.data.frame(sapply(colnames(cat_block), FUN = function(x) as.factor(cat_block[,x])))
        colnames(cat_block) = colnames(cluster_data)[Types =='categorical' | colnames(cluster_data) == 'cluster']
      } else{
        cat_block <- as.factor(cat_block)
      }


      #analysis for CATEGORICAL data#
      if(CatCount_man > 0){
        categorical_table = function(datacube){
          cluster_list <- unique(datacube$cluster)
          cluster_count <- length(cluster_list)

          #list of categorical questions but without the "cluster" column
          # colnames(cat_block) <- colnames(cluster_data)[Types=='factor' | colnames(cluster_data) == 'cluster']
          question_list <- colnames(cat_block)[-match('cluster',colnames(cat_block))]


          #function to create the number of rows needed
          rowcount = function(x) {
            return(length(unique(x)))
          }

          rows_of_catframe = sum(unlist(lapply(as.data.frame(cat_block[,-match('cluster',colnames(cat_block))]),rowcount)))

          output_frame = as.data.frame(matrix(0,nrow=rows_of_catframe,ncol=cluster_count*2+2))
          colnames(output_frame) = c('Question','Answer',rep(c('Mean','SD'),cluster_count))

          #loop to place values correctly in a table getting ready for output
          #runs for every unique answer choice of every categorical question
          rowq = 1
          for(q in question_list){
            for(u in (unique(datacube[,q]))){
              colq = 3
              for (c in cluster_list[order(cluster_list)]) {
                output_frame[rowq,1] = q
                output_frame[rowq,2] = u
                output_frame[rowq,colq] = sum(datacube[,q]==u & datacube$cluster==c)/sum(datacube$cluster==c)
                colq = colq + 2
              }
              rowq = rowq+1
            }

          }
          #here ends the categorical table function

          return(output_frame)
        }
      } else {
        #ADD CASE#
      }
      #here ends the if length of col names of cat block >1 ... need to add the 1 case)
      #}

      #??May have to edit this numerical analysis block to only operate on numerical columns#
      if(length(colnames(numer_block))>1){
        #analysis for numeric data#

        mean_block <- ddply(numer_block, .(cluster), numcolwise(round_mean))
        sd_block <- ddply(numer_block, .(cluster), numcolwise(round_sd))
        stack_data <- rbind(mean_block,sd_block)

        stack_data <- stack_data[order(stack_data$cluster),]
        output_data <- transpose(stack_data)
        output_data <- output_data[-1,]

        #??check the number of rows here... not sure why i need to put the "nrow" piece in otherwise there is mismatch
        output_data$Question <- orignames[-length(orignames)][SegVarTypes_man == 'numeric']
        output_data$Answer <- ""

        # return_data <- output_data %>% select(Question, everything())
        return_data <- output_data[,c(((ncol(output_data)-1):ncol(output_data)),1:(ncol(output_data)-2))]
        colnames(return_data)[3:length(colnames(return_data))] <- rep(c("Mean","SD"),(ncol(return_data)-2)/2)
      }else{
        #add case for only one numberical to output return data
      }

      #RETURNING THE RIGHT DATA ACROSS CAT AND NUM CATEGORIES

      #ONLY CATEGORICAL CASE
      if(CatCount_man == Name_Count_man){
        if(CatCount_man>1){
          output_frame = categorical_table(cluster_data)
          compiled_data = output_frame
        }
        else {
          #may not need this clause, but i think there is something in categorical_table() that will need to be changed for single case
          #need to think through what will happen if there will be one vector
        }
      }
      #ONLY NUMERIC CASE
      else if(NumerCount_man == Name_Count_man){
        compiled_data = return_data
      }

      #CATEGORICAL + NUMERIC CASE
      else{
        output_frame = categorical_table(cat_block)
        output_frame$type = "categorical"
        return_data$type = "numeric"
        compiled_data <- rbind(return_data,output_frame)

      }
      return(as.data.frame(compiled_data))
    }
    ###END CREATE DATA MANUAL###



    ###BEGIN COLOR CODING FUNCTION###
    output_table_man = function(tablex,NumSeg,cluster_block){

      #CREATING TABLES
      #constructing the straight forward tables
      #IF NUMERICAL ALONE
      seglabel = c(seq(1,NumSeg))
      ##NUMERIC ONLY##
      if(NumerCount_man == Name_Count_man){
        segdf = data.frame(matrix(nrow = NumSeg,ncol=nrow(tablex)),row.names=seglabel)
        #changed colnames from being row.names
        colnames(segdf) = tablex[,1]
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

        print(seg_sdi)
        print(cluster_block)
        #constructing the table of differences
        if(seg_sdi>0){
          segdiffs <- transpose((seg_meana - seg_meani) / seg_sdi)
        } else{
          segdiffs <- transpose(seg_meana - seg_meani)
        }
      }
      ##CATEGORICAL ONLY##
      else if(CatCount_man==Name_Count_man){
        tablexcol = seq(3,ncol(tablex),by=2)
        obs_val = tablex[,tablexcol]


        expected_val <- sapply(1:nrow(obs_val), FUN = function(y) sapply(1:ncol(obs_val), FUN = function(x) mean(as.numeric(obs_val[x,-y]))))
        segdiffs <- (obs_val - expected_val)/expected_val

        segdiffs[segdiffs == Inf | segdiffs == -Inf | segdiffs == NaN] = 0
        ##?? HERE - COME BACK TO CORRECT THIS CHI SQUARE and make the color coding meaningful###

        #To format as percent
        percent <- function(x, digits = 2, format = "f", ...) {
          paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
        }
        tablex[,3:ncol(tablex)] = apply(tablex[,3:ncol(tablex)],MARGIN = c(1,2),FUN = percent)
        tablex[,seq(4,ncol(tablex),by=2)]=""
      }
      ##BOTH NUMERIC AND CATEGORICAL##
      else{

        #breaking the table into its pieces
        cat_tablex <- tablex[tablex[,'type']=='categorical',]
        num_tablex <- tablex[tablex[,'type']=='numeric',]


        ##NUMERIC PART OF SEGDIFFS##
        segdf = data.frame(matrix(nrow = NumSeg,ncol=nrow(num_tablex)),row.names=seglabel)
        #getting rid of the type columns
        num_tablex = num_tablex[,-ncol(num_tablex)]
        #changed colnames from being row.names
        colnames(segdf) = num_tablex[,1]

        #constructing the inverse tables (mean or sd of the data for data NOT EQUAL to the cluster of choice)
        num_cluster_block <- cluster_block[,SegVarTypes_man=='numeric' | colnames(cluster_block) == 'cluster']

        ##??START HERE HOW TO GET IT SUCH THAT ONLY INCLUDES NUMERIC VARIABLES!
        seg_meana <- ddply(num_cluster_block, .(cluster), numcolwise(round_mean))


        seg_meana = seg_meana[,2:length(seg_meana)]

        seg_sda <- ddply(cluster_block, .(cluster), numcolwise(round_sd))
        seg_sda = seg_sda[,2:length(seg_sda)]

        orignames <- colnames(cluster_block)


        if(NumerCount_man>1){
          seg_meani <- transpose(data.frame(sapply(seglabel, FUN = function(x) sapply(num_cluster_block[num_cluster_block$cluster != x,1:ncol(num_cluster_block)-1], function(y) mean(y,na.rm=TRUE)))))
        } else{
          seg_meani <- data.frame(sapply(seglabel, FUN = function(x) mean(num_cluster_block[,1][num_cluster_block$cluster !=x])))
        }

        #no idea what i'm doing with this subsetting of orignames and why it is needed but whatever
        colnames(seg_meani) <- orignames[1:ncol(seg_meani)]



        if(NumerCount_man>1){
          seg_sdi <- transpose(as.data.frame(sapply(seglabel, FUN = function(x) sapply(num_cluster_block[num_cluster_block$cluster != x,1:ncol(num_cluster_block)-1], function(y) sd(y,na.rm=TRUE)))))
        }
        else{
          seg_sdi <- data.frame(sapply(seglabel, FUN = function(x) sd(num_cluster_block[,1][num_cluster_block$cluster !=x])))
        }
        colnames(seg_sdi) <- orignames[1:ncol(seg_sdi)]

        print("ok check here")
        print(seg_meana)
        print(seg_meani)
        print(seg_sdi)

        #constructing the table of differences
        if(seg_sdi>0){
          segdiffs_num <- ((seg_meana - seg_meani) / seg_sdi)
        } else{
          segdiffs_num <- (seg_meana - seg_meani)
        }


        segdiffs_num <- transpose(segdiffs_num)

        colnames(segdiffs_num) <- c(paste(rep("seg",ncol(segdiffs_num)),1:ncol(segdiffs_num)))
        ##END NUMERICAL PART OF SEGDIFFS##



        ##CATEGORICAL PART OF SEGDIFFS##

        tablexcol = seq(3,ncol(cat_tablex),by=2)
        obs_val = cat_tablex[,tablexcol]
        obs_val = obs_val[,-ncol(obs_val)]

        expected_val <- sapply(1:nrow(obs_val), FUN = function(y) sapply(1:ncol(obs_val), FUN = function(x) mean(as.numeric(obs_val[x,-y]))))

        segdiffs_cat <- (obs_val - expected_val)/expected_val
        colnames(segdiffs_cat) <- c(paste(rep("seg",ncol(segdiffs_cat)),1:ncol(segdiffs_cat)))

        segdiffs_cat[segdiffs_cat == Inf | segdiffs_cat == -Inf | segdiffs_cat == NaN] = 0
        ##?? HERE - COME BACK TO CORRECT THIS CHI SQUARE and make the color coding meaningful###

        #To format as percent
        percent <- function(x, digits = 2, format = "f", ...) {
          paste0(formatC(100 * as.numeric(x), format = format, digits = digits, ...), "%")
        }



        tablex[tablex[,'type']=='categorical',seq(4,ncol(tablex),by=2)]=""

        tablex[tablex[,'type']=='categorical',seq(3,ncol(tablex),by=2)] = apply(tablex[tablex[,'type']=='categorical',seq(3,ncol(tablex),by=2)],MARGIN = c(1,2),FUN = percent)



        ##END OF CATEGORICAL PART OF SEGDIFFS##

        #Getting rid of the "TYPE" variable in tablex
        tablex = tablex[,-ncol(tablex)]
        segdiffs = rbind(segdiffs_num,segdiffs_cat)


      }
      ##END OF BOTH NUMERICAL AND CATEGORICAL##


      #COLOR CODING
      #including defining the significance cutoffs for color coding
      color_vector <- c("rgb(222,124,124)","rgb(219,23,23)","rgb(255,255,255)","rgb(218,252,229)","rgb(56,166,93)")
      first_sd_vector <- c(-1000,-1,-0.5,0.5,1)
      sec_sd_vector <- c(-1,-0.5,0.5,1,1000)
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
            th(rowspan = 1, ""),
            th(rowspan = 1, 'Segments'),
            lapply(c(seq(1:NumSeg)),th,colspan=2)
          ),
          tr(
            th(rowspan = 1, 'Question'),
            th(rowspan = 1, 'Answer'),
            lapply(rep(c('Mean', 'SD'),NumSeg),th)
          )
        )
      ))



      #outputting data in the formatted table

      #to remove SD values for these vate
      return(datatable(tablex, container = sketch, options = list(paging = FALSE), rownames = FALSE)%>% formatStyle(columns = c(seq(3,NumSeg*2+1,by=2)),target="cell", backgroundColor = styleEqual(as.matrix(tablex[,c(seq(3,NumSeg*2+1,by=2))]),colorcode)))
    }
    ###END COLOR CODING###
    ###END OF THE OUTPUT TABLE FUNCTION ###

    #THIS STUFF IS RUN OUTSIDE THE FUNCTIONS INDEPENDENTLY
    #returning error if selected multiple of the same variable

    if(Name_Count_man != Unique_Name_Count_man) {
      return(data.table("error, please check your HERE inputs!"))
    } else
      data_1 <- as.data.frame(survey_data_reactive())
      data_2 <- na.omit(data_1[,colnames(data_1) %in% SegNames_man])
      rng_seg1 <- list(input$var1_seg1,input$var2_seg1,input$var3_seg1,input$var4_seg1)
      rng_seg1 <- rng_seg1[rng_seg1!=""]
      rng_seg2 <- list(input$var1_seg2,input$var2_seg2,input$var3_seg2,input$var4_seg2)
      rng_seg2 <- rng_seg2[rng_seg2!=""]
      rng_seg3 <- list(input$var1_seg3,input$var2_seg3,input$var3_seg3,input$var4_seg3)
      rng_seg3 <- rng_seg3[rng_seg3!=""]
      rng_seg4 <- list(input$var1_seg4,input$var2_seg4,input$var3_seg4,input$var4_seg4)
      rng_seg4 <- rng_seg4[rng_seg4!=""]

      #getting the number of segments
      SegNum_man <- 4 - sum(c(is.null(rng_seg4), is.null(rng_seg3), is.null(rng_seg2), is.null(rng_seg1)))



      ##IF THERE ARE 4 VARIABLES##
      if(length(SegNames_man) == 4){

      ##FILTERING THE CLUSTERS##
        data_2$cluster[as.logical((as.character(data_2[,SegNames_man[1]]) %in% as.character(unlist(rng_seg1[1]))) * (as.character(data_2[,SegNames_man[2]]) %in% as.character(unlist(rng_seg1[2]))) * (as.character(data_2[,SegNames_man[3]]) %in% as.character(unlist(rng_seg1[3]))) * (as.character(data_2[,SegNames_man[4]]) %in% as.character(unlist(rng_seg1[4]))))] = 1
        data_2$cluster[as.logical((as.character(data_2[,SegNames_man[1]]) %in% as.character(unlist(rng_seg2[1]))) * (as.character(data_2[,SegNames_man[2]]) %in% as.character(unlist(rng_seg2[2]))) * (as.character(data_2[,SegNames_man[3]]) %in% as.character(unlist(rng_seg2[3]))) * (as.character(data_2[,SegNames_man[4]]) %in% as.character(unlist(rng_seg2[4]))))] = 2
        if(SegNum_man > 2){
        data_2$cluster[as.logical((as.character(data_2[,SegNames_man[1]]) %in% as.character(unlist(rng_seg3[1]))) * (as.character(data_2[,SegNames_man[2]]) %in% as.character(unlist(rng_seg3[2]))) * (as.character(data_2[,SegNames_man[3]]) %in% as.character(unlist(rng_seg3[3]))) * (as.character(data_2[,SegNames_man[4]]) %in% as.character(unlist(rng_seg3[4]))))] = 3
        }
        if(SegNum_man > 3){
        data_2$cluster[as.logical((as.character(data_2[,SegNames_man[1]]) %in% as.character(unlist(rng_seg4[1]))) * (as.character(data_2[,SegNames_man[2]]) %in% as.character(unlist(rng_seg4[2]))) * (as.character(data_2[,SegNames_man[3]]) %in% as.character(unlist(rng_seg4[3]))) * (as.character(data_2[,SegNames_man[4]]) %in% as.character(unlist(rng_seg4[4]))))] = 4
        }
        #REGARDLESS OF IF CLAUSES SET ALL NA TO SEGMENT 0
        data_2 = data_2[is.na(data_2$cluster)==FALSE,]
      }

      ##IF THERE ARE THREE VARIABLES##
      if(length(SegNames_man) == 3){

        ##FILTERING THE CLUSTERS##
        data_2$cluster[as.logical((as.character(data_2[,SegNames_man[1]]) %in% as.character(unlist(rng_seg1[1]))) * (as.character(data_2[,SegNames_man[2]]) %in% as.character(unlist(rng_seg1[2]))) * (as.character(data_2[,SegNames_man[3]]) %in% as.character(unlist(rng_seg1[3]))))] = 1
        data_2$cluster[as.logical((as.character(data_2[,SegNames_man[1]]) %in% as.character(unlist(rng_seg2[1]))) * (as.character(data_2[,SegNames_man[2]]) %in% as.character(unlist(rng_seg2[2]))) * (as.character(data_2[,SegNames_man[3]]) %in% as.character(unlist(rng_seg2[3]))))] = 2
        if(SegNum_man > 2){
          data_2$cluster[as.logical((as.character(data_2[,SegNames_man[1]]) %in% as.character(unlist(rng_seg3[1]))) * (as.character(data_2[,SegNames_man[2]]) %in% as.character(unlist(rng_seg3[2]))) * (as.character(data_2[,SegNames_man[3]]) %in% as.character(unlist(rng_seg3[3]))))] = 3
        }
        if(SegNum_man > 3){
          data_2$cluster[as.logical((as.character(data_2[,SegNames_man[1]]) %in% as.character(unlist(rng_seg4[1]))) * (as.character(data_2[,SegNames_man[2]]) %in% as.character(unlist(rng_seg4[2]))) * (as.character(data_2[,SegNames_man[3]]) %in% as.character(unlist(rng_seg4[3]))))] = 4
        }
        #REGARDLESS OF IF CLAUSES SET ALL NA TO SEGMENT 0
        data_2 = data_2[is.na(data_2$cluster)==FALSE,]
      }

      ##IF THERE ARE TWO VARIABLES##
      if(length(SegNames_man) == 2){

        ##START HERE MAKE ALL THE SEGMENTING THE SAME LOGIC AS THIS WITH THE AS.NUMERIC + UNLIST)
        ##NOTE: THE CATEGORICAL + NUMERICAL COMBINATION DOES NOT WORK AS OF NOW; SOMETHING BROKEN IN THE AGGREGATION
        ##FILTERING THE CLUSTERS##
        data_2$cluster[as.logical((as.character(data_2[,SegNames_man[1]]) %in% as.character(unlist(rng_seg1[1]))) * (as.character(data_2[,SegNames_man[2]]) %in% as.character(unlist(rng_seg1[2]))))] = 1

        data_2$cluster[as.logical((as.character(data_2[,SegNames_man[1]]) %in% as.character(unlist(rng_seg2[1]))) * (as.character(data_2[,SegNames_man[2]]) %in% as.character(unlist(rng_seg2[2]))))] = 2
        if(SegNum_man > 2){
          data_2$cluster[as.logical((as.character(data_2[,SegNames_man[1]]) %in% as.character(unlist(rng_seg3[1]))) * (as.character(data_2[,SegNames_man[2]]) %in% as.character(unlist(rng_seg3[2]))))] = 3
        }
        if(SegNum_man > 3){
          data_2$cluster[as.logical((as.character(data_2[,SegNames_man[1]]) %in% as.character(unlist(rng_seg4[1]))) * (as.character(data_2[,SegNames_man[2]]) %in% as.character(unlist(rng_seg4[2]))))] = 4
        }


        #REGARDLESS OF IF CLAUSES SET ALL NA TO SEGMENT 0
        data_2 = data_2[is.na(data_2$cluster)==FALSE,]
      }

      ##IF THERE IS ONE VARIABLE##
      if(length(SegNames_man) == 1){

        ##FILTERING THE CLUSTERS##
        data_2$cluster[as.logical((as.character(data_2[,SegNames_man[1]]) %in% as.character(unlist(rng_seg1[1]))))] = 1
        data_2$cluster[as.logical((as.character(data_2[,SegNames_man[1]]) %in% as.character(unlist(rng_seg2[1]))))] = 2
        if(SegNum_man > 2){
          data_2$cluster[as.logical((as.character(data_2[,SegNames_man[1]]) %in% as.character(unlist(rng_seg3[1]))))] = 3
        }
        if(SegNum_man > 3){
          data_2$cluster[as.logical((as.character(data_2[,SegNames_man[1]]) %in% as.character(unlist(rng_seg4[1]))))] = 4
        }
        #REGARDLESS OF IF CLAUSES SET ALL NA TO SEGMENT 0
        data_2 = data_2[is.na(data_2$cluster)==FALSE,]

      }

      #ONLY NUMERIC VARIABLES
      # if(NumerCount_man == Name_Count_man){
      #   data_1 <- as.data.frame(survey_data_reactive())
      #   data_2 <- na.omit(data_1[,colnames(data_1) %in% SegNames_man])
      #   data_3 <- scale(data_2)
      #   #??how to handle these nas instead of removing?
      #   data_3[is.na(data_3)] <- 0
      #   k_analysis <- kmeans(data_3,input$segment_num)
      #   #??check the number of rows here... not sure why i need to put the "nrow" piece in otherwise there is mismatch
      #
      #   data_2$cluster <- as.factor(k_analysis$cluster)
      #   # data_2$cluster <- as.factor(c(k_analysis$cluster,1,1))[1:nrow(data_2)]
      #
      # } else{
      #   #ONLY CATEGORICAL VARIABLES
      #   if(CatCount == Name_Count){
      #     data_1 <- as.data.frame(survey_data_reactive())
      #     #changing values to factors
      #     data_2 <- as.data.frame(sapply(na.omit(data_1[,colnames(data_1) %in% CatNames]),as.factor))
      #     data_3 <- as.data.frame(data_2)
      #     #??how to handle these nas instead of removing?
      #     data_3[is.na(data_3)] <- 0
      #     k_analysis <- kmodes(data_3,input$segment_num)
      #     #??check the number of rows here... not sure why i need to put the "nrow" piece in otherwise there is mismatch
      #     #HERE IS THER ISSUE - make sure that this does not get "coerced into a list"
      #     data_2$cluster <- c(k_analysis$cluster,1,1)[1:nrow(data_2)]
      #   }
      #   # BOTH CATEGORICAL AND NUMERIC VARIABLES
      #   else{
      #
      #     data_1 <- as.data.frame(survey_data_reactive())
      #     data_2 <- na.omit(data_1[,colnames(data_1) %in% SegNames])
      #     data_2 <- data_2[,SegNames]
      #
      #     ###ACCOUNTING FOR USING LAPPLY ON A SINGLE ITEM LIST###
      #     if(sum(CatNames != "")>1){
      #       data_2[,colnames(data_2) %in% CatNames] <- lapply(data_2[,colnames(data_2) %in% CatNames],as.factor)
      #     } else {
      #       data_2[,colnames(data_2) %in% CatNames] <- as.factor(data_2[,colnames(data_2) %in% CatNames])
      #     }
      #
      #     ###ALSO ENSURING NUMERIC VARIABLES ARE CLASS NUMERIC###
      #     if(sum(NumNames != "")>1){
      #       data_2[,colnames(data_2) %in% NumNames] <- lapply(data_2[,colnames(data_2) %in% NumNames],as.numeric)
      #     } else {
      #       data_2[,colnames(data_2) %in% NumNames] <- as.numeric(data_2[,colnames(data_2) %in% NumNames])
      #     }
      #
      #     data_3 <- data_2
      #     data_3[,SegVarTypes=='numeric'] <- scale(data_3[,SegVarTypes=='numeric'])
      #     k_analysis <- kproto(data_3,input$segment_num)
      #     data_2$cluster <- as.factor(k_analysis$cluster)
      #
      #   }
      # }



    #seeing if I can correct for the fact that sometimes k analysis coerces data into fewer clusters
    Segment_Number_man <- length(unique(data_2$cluster))


    #creating the N outputs
    output$Nseg1_man <- renderText(paste("N Segment 1: ",sum(data_2$cluster==1)))
    output$Nseg2_man <- renderText(paste("N Segment 2: ",sum(data_2$cluster==2)))
    output$Nseg3_man <- renderText(paste("N Segment 3: ",sum(data_2$cluster==3)))
    output$Nseg4_man <- renderText(paste("N Segment 4: ",sum(data_2$cluster==4)))

    ####OUTPUTTING QUALITY SCORE####
    output$QualityScore_man <- renderValueBox({
      tablex <- create_data_man(data_2, SegVarTypes_man)

      dif_table <- c(1:nrow(tablex))
      counter = 1
      for(i in 1:nrow(tablex)){
        #for numerics
        if(tablex[i,ncol(tablex)] == "numeric"){
          #finding the overall sd and means
          mean_whole = mean(data_2[,tablex[i,1]])
          sd_whole = sd(data_2[,tablex[i,1]])

          t_table <- transpose(tablex[,seq(3,ncol(tablex)-1,by=2)])
          sd_segs = sd(t_table[,i])
          dif_table[i] <- sd_segs/(sd_whole/3)
          i=i+1
        }
        else{
          t_table <- transpose(tablex[,seq(3,ncol(tablex)-1,by=2)])
          sd_segs = sd(t_table[,i])
          dif_table[i] <- sd_segs/(0.7/3)
          i=i+1
        }
      }

      QS = sum(dif_table)/length(dif_table)

      if(QS>2){
        valueBox(
          value = formatC("Highly Different"),
          subtitle = "Significance of Your Segments"
        )}
      else if (QS<0.5){
        valueBox(
          value = formatC("Not Very Different"),
          subtitle = "Significance of Your Segments"
        )
      } else{
        valueBox(
          value = formatC("Fairly Different"),
          subtitle = "Significance of Your Segments"
        )
      }

    })



    #RUNNING THE FUNCTIONS AND OUPUTTING OUR DESIRED TABLE
    orignames <- colnames(data_2)
    tablex <- create_data_man(data_2, SegVarTypes_man)
    returntable <- output_table_man(tablex,Segment_Number_man,data_2)

    #TRYING TO CREATE DOWNLOAD ABILITY - DOES NOT WORK#
    # output$down <- downloadHandler(
    #   #Specify the filename
    #   filename = function() {
    #     "yoursegements.png"
    #   },
    #   content = function(file){
    #     #open device
    #     png(file)
    #     returntable
    #     dev.off()
    #     #create the plot
    #     #close the device
    #   }
    # )


    return(returntable)



  })
  ###END OF FRAMEX MASSIVE REACTIVE FUNCTION###
  output$segtable_man <- renderDataTable({framex_man()
  })


####TREE CLASSIFICATION####

  observe({values <- colnames(survey_data_reactive())

  #MAKING SURE VARIABLES UPDATE ACROSS SELECTION VARIABLES#
  #automatic segmentation
  updateSelectInput(session,"tree_target_var",label = "Target Variable.",choices = c('',values))
  updateSelectInput(session,"tree_split_var",label = "Range of Variables to Include.",choices = c('',values))
  })


  unique_outcomes <- eventReactive(input$UseTheseVars_tree, {
    no_blank_data <- filter(survey_data_reactive(),(!!as.symbol(input$tree_target_var)) != "")
    return(length(unique(no_blank_data[,input$tree_target_var])))

  })

  #outputting the frame to populate the table and categorical chart
  tree_model <- eventReactive(input$UseTheseVars_tree, {
    return(masterframeFX(survey_data_reactive(),input$tree_split_var,input$tree_target_var,input$min_leaf, input$cpchoice, unique_outcomes(), input$pvalue_thresh))

  })

  #outputting the dataset for each relevant node in the same order as the masterframe rows
  numeric_nodeframe <- eventReactive(input$UseTheseVars_tree, {
        return(masterframe_nodecuts(survey_data_reactive(),input$tree_split_var,input$tree_target_var,input$min_leaf, input$cpchoice, unique_outcomes(), input$pvalue_thresh))

  })

 
  out_table <- reactive({

    #creating the output table
    out_table = tree_model()
    print(out_table)
    out_table$model = 1:nrow(out_table)
    out_table = cbind(out_table[,c('model','n','rule','pvalue')],out_table[,str_detect(colnames(out_table),"avg_")])
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
      "yoursegements.csv"
    },
    content = function(file){
      write.csv(out_table(), file)
    }
  )



  ####FOR JON TO CREATE CHART###
  
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
        
        p <-
          ggplot() +
          geom_bar(aes(y=value, x=model, fill = variable),
                   data = plot_data,
                   stat = 'identity', position = "fill") 
        
        #using plotly so we can hover
        p <- ggplotly(p) %>%
          layout(xaxis = list(tickvals = c(1:nrow(model_data)), ticktext = c(1:(nrow(model_data)-2),"","Avg")))
      }
      else{
        overalldata = as.data.frame(table(survey_data_reactive()[,input$tree_target_var]))
        overalldata$overall = "Overall"
        overalldata <- rename(overalldata, Target_Variable = Var1)
        
        p  <- ggplot(overalldata, aes(x = overall, y = Freq, fill = Target_Variable)) +
          geom_col() + 
          geom_text(aes(label = ""),
                    position = position_stack(vjust = 0.5)) +
          scale_fill_brewer(palette = "Set2") +
          theme_minimal(base_size = 16) +
          ylab("Value") +
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
      
      p <- ggplot(data.frame(Target_Variable = response_string, Model = factor(model_string)), aes(x=Model, y=Target_Variable, fill=Model)) + geom_boxplot()
      
      p <- ggplotly(p) %>% layout(autosize = T)
      
      }
    
    else{
      response_string = survey_data_reactive()[,input$tree_target_var]
      model_string = rep(1, length(response_string))
      
      p <- ggplot(data.frame(Target_Variable = response_string, Overall = factor(model_string)), aes(x=Overall, y=Target_Variable, fill=Overall)) + geom_boxplot()
      
      p <- ggplotly(p) %>% layout(autosize = T)
    }
    }
    return(p)
    
  })
  
  output$tree_plot <- renderPlotly({
   plot_1()

  })


####CONDITIONAL INFERENCE TREE SEGMENTATION####

observe({values <- colnames(survey_data_reactive())

#MAKING SURE VARIABLES UPDATE ACROSS SELECTION VARIABLES#
#automatic segmentation
updateSelectInput(session,"ctree_target_vars",label = "Segment Drivers",choices = c('',values))
updateSelectInput(session,"ctree_split_vars",label = "Result Variables to Maximize Segment Differences",choices = c('',values))
})
  
ctree_model <- eventReactive(input$UseTheseVars_ctree,{
  f <- as.formula(
    paste(
      paste(input$ctree_split_vars, collapse = " + "),
          paste(input$ctree_target_vars, collapse = " + "), 
          sep = " ~ "))
  
  return(ctree(f, data = survey_data_reactive(), na.action = na.omit))
  
})


output$ctree_plot <- renderPlot({
  plot(ctree_model(),margins = c(3, 0, 0, 0),
       tp_args = list(rot = 45, just = c("right", "top")))
  })


}
