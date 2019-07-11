#THESE FUNCTIONS ALLOW CALCULATION OF WEIGHTED DISTRIBUTION DIFFERENCES IN THE DIFFERENECE FINDER
#ONLY NUMERIC VARIABLES CAN BE PASSED IN THIS PART OF THE TOOL


#SPLITTING THE FRAMES
num_splitframe_percent = function(fulldata, split_col, search_cols){

    num_search_cols = search_cols[search_cols %in% colnames(fulldata)]
    pieces = unique(fulldata[,split_col])
    less_data = fulldata[,c(split_col,num_search_cols)]
    listof_frames = lapply(pieces,function(x) filter(less_data,(!!as.symbol(split_col) == x)))
    return(listof_frames)
  
}


#ADDING TOTALS BY ROW
  #frame data
add_totalcol <- function(list_of_splitframes,search_cols){
  for(i in 1:length(list_of_splitframes)){
    list_of_splitframes[[i]]$totalvals = rowSums(list_of_splitframes[[i]][,search_cols])
  }
  return(list_of_splitframes)
  
}

  #overall data
add_totalcol_fulldata <- function(fulldata,search_cols){
  fulldata$totalvals = rowSums(fulldata[,search_cols])
  return(fulldata)
}


#PERCENTAGE TRANSFORM OF THE DATA
  #frame data
percent_transform <- function(list_of_splitframes_withtotal,search_cols){
  for(i in 1:length(list_of_splitframes_withtotal)){
    for(col in search_cols){
      list_of_splitframes_withtotal[[i]][,col] =  list_of_splitframes_withtotal[[i]][,col] /  list_of_splitframes_withtotal[[i]]$totalvals
    }
  }
  
  return(list_of_splitframes_withtotal)
}

  #overall data
percent_transform_fulldata <- function(fulldata_withtotal,search_cols){
  for(col in search_cols){
    fulldata_withtotal[,col] = fulldata_withtotal[,col] / fulldata_withtotal$totalvals
  }
  
  return(fulldata_withtotal)
}


#PVALUE
p_value_creation = function(fulldata_percent, list_of_splitframes_percent,search_cols){
    given_pvals = lapply(list_of_splitframes_percent,
                         FUN = function(frame_from_list) sapply(search_cols, 
                                                                FUN = function(search_col) 
                                                                  if(length(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col])>2){
                                                                    pval = wtd.t.test(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col],filter(fulldata_percent,!is.na(!!as.symbol(search_col)))[,search_col],weight = filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,"totalvals"],weighty = filter(fulldata_percent,!is.na(!!as.symbol(search_col)))[,"totalvals"])$coefficients['p.value']
                                                                    if(is.null(pval)==TRUE){1} else{pval}
                                                                  }
                         ))
    
    
    return(round(as.numeric((unlist(given_pvals))),3))
  
}


#GROUP MEANS
group_means_percent = function(list_of_splitframes_percent,search_cols){
  cat_means = lapply(list_of_splitframes_percent,
                       FUN = function(frame_from_list) sapply(search_cols, 
                                                              FUN = function(search_col) 
                                                                if(length(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col])>2){
                                                                  round(weighted.mean(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col],w = filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,'totalvals']),3)}
                                                                
                       ))
  
  
  return(unlist(cat_means))
  
}



#OVERALL MEANS
overall_means_percent = function(fulldata_percent,list_of_splitframes_percent,search_cols){
  overall_means = lapply(list_of_splitframes_percent,
                     FUN = function(frame_from_list) sapply(search_cols, 
                                                            FUN = function(search_col) 
                                                              if(length(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col])>2){
                                                                round(weighted.mean(filter(fulldata_percent,!is.na(!!as.symbol(search_col)))[,search_col],w = filter(fulldata_percent,!is.na(!!as.symbol(search_col)))[,'totalvals']),3)}
                                                            
                     ))
  
  
  return(unlist(overall_means))
  
}

#creating the category split
num_category_split_percent = function(fulldata_percent, list_of_splitframes_percent,split_col, search_cols){
 
  cat_list = lapply(list_of_splitframes_percent,
                         FUN = function(frame_from_list) sapply(search_cols, 
                                                                FUN = function(search_col) 
                                                                  if(length(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col])>2){
                                                                      frame_from_list[1,1]}
                         ))
  
  

    return(unlist(cat_list))
  
}



#creating the variable list of questions
num_question_list_percent = function(fulldata_percent, list_of_splitframes_percent, split_col, search_cols){
    
  quest_list = lapply(list_of_splitframes_percent,
                         FUN = function(frame_from_list) sapply(search_cols, 
                                                                FUN = function(search_col) 
                                                                  if(length(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col])>2){
                                                                    search_col}
                         ))

    return(unlist(quest_list))
}


#outputting N per group
n_list_weighted = function(fulldata_percent, list_of_splitframes_percent, split_col, search_cols){
  
  n_list = lapply(list_of_splitframes_percent,
                      FUN = function(frame_from_list) sapply(search_cols, 
                                                             FUN = function(search_col) 
                                                               if(length(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col])>2){
                                                                 nrow(frame_from_list)}
                      ))
  
  return(unlist(n_list))
}


#building the output frame
percey_frame = function(n_category,cat,var,pval,mean_cat,mean_overall,split_col){
    ans = rep("",length(cat))
   
    percey_dataframe <- cbind.data.frame(n_category,cat,var,ans,pval,mean_cat,mean_overall)
    percey_dataframe$dif = round((mean_cat - mean_overall)/mean_overall,3)+1
    percey_dataframe[,'mean_cat'] = percent(percey_dataframe[,'mean_cat'])
    percey_dataframe[,'mean_overall'] = percent(percey_dataframe[,'mean_overall'])  
    
    percey_dataframe$rule = mapply(function(category, variable, difference,split_column) {
      paste("When ",split_column," is ",category,", ",variable," differs by ", difference,"X vs the overall average",sep = "")
    }, category = cat, variable = var, difference = percey_dataframe$dif,split_column = split_col)
    
    return(percey_dataframe)
  
}
