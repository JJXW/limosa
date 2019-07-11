
##SEPARATING THE DATASET##
#output the numeric piece to work with
fulldata_numeric = function(number_of_numeric,alldata, split_col, search_num_only){
  if(number_of_numeric == 0){return("")}
    else if(number_of_numeric ==1){
      split_and_num = cbind.data.frame(alldata[,split_col],alldata[,search_num_only])
      colnames(split_and_num) = c(split_col,search_num_only)
      return(split_and_num)
    }
  else {
  alldata2 = alldata[,search_num_only]
  alldata2 = cbind.data.frame(alldata[,split_col],alldata2)
  colnames(alldata2)[1] = split_col #keeping original column name
  
  return(alldata2)
  }
}

# mlml_num = fulldata_numeric(mlml, split, search_numeric)
# mlml_num

#output the categorical piece to work with
fulldata_categorical = function(number_of_categorical,alldata, split_col, search_cat_only){
  if(number_of_categorical == 0){return("")}
  else if(number_of_categorical ==1){
    split_and_cat = cbind.data.frame(alldata[,split_col],alldata[,search_cat_only])
    colnames(split_and_cat) = c(split_col,search_cat_only)
    return(split_and_cat)
  }
  else {
  alldata2 = alldata[,search_cat_only]
  alldata2 = cbind.data.frame(alldata[,split_col],alldata2)
  colnames(alldata2)[1] = split_col #keeping original column name
  
  
  return(alldata2)
  }
}


# mlml_cat = fulldata_categorical(mlml, split, search_categorical)
# mlml_cat

#####NUMERICAL PART#####
#NUMSTEP 1: return list of split dataframes by a given column
num_splitframe = function(number_of_numeric,fulldata, split_col, search_cols){
  if(number_of_numeric == 0){return("")}
  else {
  num_search_cols = search_cols[search_cols %in% colnames(fulldata)]
  pieces = unique(fulldata[,split_col])
  less_data = fulldata[,c(split_col,num_search_cols)]
  listof_frames = lapply(pieces,function(x) filter(less_data,(!!as.symbol(split_col) == x)))
  return(listof_frames)
  }
}



#NUMSTEP 2: take individual dataframe and return what we need for the pvalue insights, returning unlisted version
num_frame = function(number_of_numeric,fulldata, list_of_splitframes,search_cols){
  if(number_of_numeric == 0){return("")}
  else {
  num_search_cols = search_cols[search_cols %in% colnames(fulldata)]
  given_pvals = lapply(list_of_splitframes,
                  FUN = function(frame_from_list) sapply(num_search_cols, 
                     FUN = function(search_col) 
                       if(length(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col])>2){
                         t.test(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col],filter(fulldata,!is.na(!!as.symbol(search_col)))[,search_col],na.omit = TRUE)$p.value
                       } else{1}
                          ))

  

return(as.numeric(round(unlist(given_pvals),3)))
  }
}


#NUMSTEP 3: returning the means of each question by the split variable, returning unlisted version
num_split_mean_frame = function(number_of_numeric,fulldata, list_of_splitframes,search_cols){
  if(number_of_numeric == 0){return("")}
  else {
  num_search_cols = search_cols[search_cols %in% colnames(fulldata)]
  
  cat_means = lapply(list_of_splitframes,
                       FUN = function(frame_from_list) sapply(num_search_cols, 
                                                              FUN = function(search_col) 
                                                                if(length(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col])>2){
                                                                round(mean(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col],na.rm = TRUE),3)}
                                                              else{round(mean(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col],na.rm = TRUE),3)}
                                                                ))
  
  return(as.numeric(unlist(cat_means)))
  }
}


#NUMSTEP 4: returning the means of the overall population by the split variable, returning unlisted version
num_overall_mean_frame = function(number_of_numeric,fulldata, list_of_splitframes,search_cols){
  if(number_of_numeric == 0){return("")}
  else {
  num_search_cols = search_cols[search_cols %in% colnames(fulldata)]
  overall_means = lapply(list_of_splitframes,
                     FUN = function(frame_from_list) sapply(num_search_cols, 
                                                            FUN = function(search_col) 
                                                              if(length(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col])>2){
                                                              round(mean(filter(fulldata,!is.na(!!as.symbol(search_col)))[,search_col],na.rm = TRUE),3)}
                                                                else{round(mean(filter(fulldata,!is.na(!!as.symbol(search_col)))[,search_col],na.rm = TRUE),3)}
                                                                ))
  
  return(as.numeric(unlist(overall_means)))
  }
}



#NUMSTEP 5: creating the category split
num_category_split = function(number_of_numeric,fulldata, split_col, search_cols){
  if(number_of_numeric == 0){return("")}
  else {
  num_search_cols = search_cols[search_cols %in% colnames(fulldata)]
  cat_list = rep(unique(fulldata[,split_col]),rep(length(num_search_cols),length(unique(fulldata[,split_col]))))
 return(cat_list)
  }
}


#NUMSTEP 6: creating the variable list of questions
num_question_list = function(number_of_numeric,fulldata, split_col, search_cols){
  if(number_of_numeric == 0){return("")}
  else {
  num_search_cols = search_cols[search_cols %in% colnames(fulldata)]
  quest_list = rep(num_search_cols,length(unique(fulldata[,split_col])))
  return(quest_list)
  }
}

#NUMSTEP 6A: creating the n for each category
N_in_category = function(number_of_numeric,fulldata, list_of_splitframes,search_cols){
  if(number_of_numeric == 0){return("")}
  else {
    num_search_cols = search_cols[search_cols %in% colnames(fulldata)]
    n_of_category = lapply(list_of_splitframes,
                           FUN = function(frame_from_list) sapply(num_search_cols, 
                                                                  FUN = function(search_col) 
                                                                    if(length(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col])>2){
                                                                      nrow(frame_from_list)}
                                                                  else{nrow(frame_from_list)}
                           ))
    
    return(as.numeric(unlist(n_of_category)))
  }
}


#NUMSTEP7: creating the numeric dataframe and then adding rules
numbey_frame = function(n_category,number_of_numeric,cat,var,pval,mean_cat,mean_overall,split_col){
  if(number_of_numeric == 0){return("")}
  else {
  ans = rep("",length(cat))
  numbey_dataframe <- cbind.data.frame(n_category,cat,var,ans,pval,mean_cat,mean_overall)
  numbey_dataframe$dif = round((mean_cat - mean_overall)/mean_overall,3)+1
  
  numbey_dataframe$rule = mapply(function(category, variable, difference,split_column) {
    paste("When ",split_column," is ",category,", ",variable," differs by ", difference,"X vs the overall average",sep = "")
    }, category = cat, variable = var, difference = numbey_dataframe$dif,split_column = split_col)
  
  return(numbey_dataframe)
  }
}


#TEST
# numstep7 = numbey_frame(numstep5,numstep6,numstep2,numstep3,numstep4,split)
# numstep7

#####CATEGORICAL PART#####

#CATSTEP1: return list of split dataframes by a given column
cat_splitframe = function(number_of_categorical,fulldata, split_col){
  if(number_of_categorical == 0){return("")}
  else {
  pieces = unique(fulldata[,split_col])
  listof_frames = lapply(pieces,function(x) filter(fulldata,(!!as.symbol(split_col) == x)))
  return(listof_frames)
  }
}



#CATSTEP2: take individual dataframe and return what we need for the pvalue insights, returning unlisted version
cat_frame = function(number_of_categorical,fulldata, list_of_splitframes,search_cols){
  if(number_of_categorical == 0){return("")}
  else {
  cat_search_cols = search_cols[search_cols %in% colnames(fulldata)]
  given_pvals = lapply(list_of_splitframes,
                       FUN = function(frame_from_list) sapply(cat_search_cols, 
                                                              FUN = function(search_col) sapply(unique(frame_from_list[,search_col])[unique(frame_from_list[,search_col])!=""],
                                                                                                FUN = function(question_val) 
                                                                                                  if(sum(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col]==question_val)>4 & sum(filter(fulldata,!is.na(!!as.symbol(search_col)))[,search_col]==question_val)>4){
                                                                                                    t.test(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col]==question_val,filter(fulldata,!is.na(!!as.symbol(search_col)))[,search_col]==question_val)$p.value
                                                                                                  }
                                                                                                  ))) 
                                                                                                  

                                                                
  
  return(as.numeric(round(unlist(given_pvals),3)))
    }
  }


#CATSTEP 3: return the mean proportion of each question and answer combination in the split frames
cat_split_mean_frame = function(number_of_categorical,fulldata, list_of_splitframes,search_cols){
  if(number_of_categorical == 0){return("")}
  else {
  cat_search_cols = search_cols[search_cols %in% colnames(fulldata)]
  cat_proportions = lapply(list_of_splitframes,
                       FUN = function(frame_from_list) sapply(cat_search_cols, 
                                                              FUN = function(search_col) sapply(unique(frame_from_list[,search_col])[unique(frame_from_list[,search_col])!=""],
                                                                                                FUN = function(question_val) if(sum(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col]==question_val)>4 & sum(filter(fulldata,!is.na(!!as.symbol(search_col)))[,search_col]==question_val)>4){
                                                                                                  sum(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col]==question_val)/nrow(filter(frame_from_list,!is.na(!!as.symbol(search_col))))
                                                                                                }
                                                                                                )))
                                                            
  
  
  return(as.numeric(round(unlist(cat_proportions),3)))
  }
}


#CATSTEP 4: return the mean proportion of each question and answer combination in the overall population
cat_overall_mean_frame = function(number_of_categorical,fulldata, list_of_splitframes,search_cols){
  if(number_of_categorical == 0){return("")}
  else {
  cat_search_cols = search_cols[search_cols %in% colnames(fulldata)]
  overall_proportions = lapply(list_of_splitframes,
                           FUN = function(frame_from_list) sapply(cat_search_cols, 
                                                                  FUN = function(search_col) sapply(unique(frame_from_list[,search_col])[unique(frame_from_list[,search_col])!=""],
                                                                                                    FUN = function(question_val) if(sum(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col]==question_val)>4){
                                                                                                      sum(filter(fulldata,!is.na(!!as.symbol(search_col)))[,search_col]==question_val)/nrow(filter(fulldata,!is.na(!!as.symbol(search_col))))
                                                                                                      
                                                                                                    }
                                                                                                    )))
  
  
  
  return(as.numeric(round(unlist(overall_proportions),3)))
  }
}



#CATSTEP 5: creating the category split list
cat_category_split = function(number_of_categorical, fulldata,list_of_splitframes,search_cols){
  if(number_of_categorical == 0){return("")}
  else {
  cat_search_cols = search_cols
  catsplits = lapply(list_of_splitframes,
                               FUN = function(frame_from_list) sapply(cat_search_cols, 
                                                                      FUN = function(search_col) sapply(as.character(unique(frame_from_list[,search_col])),
                                                                                                        FUN = function(question_val) if(sum(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col]==question_val)>4){
                                                                                                          as.character(unlist(unique(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,1])))
                                                                                                        }
                                                                                                        )))
  return(unlist(catsplits))
  }
}



#CATSTEP 6: creating the variable list
cat_variable_list = function(number_of_categorical,fulldata, list_of_splitframes,search_cols){
  if(number_of_categorical == 0){return("")}
  else {
  cat_search_cols = search_cols[search_cols %in% colnames(fulldata)]
  varseach = lapply(list_of_splitframes,
                               FUN = function(frame_from_list) sapply(cat_search_cols, 
                                                                      FUN = function(search_col) sapply(unique(frame_from_list[,search_col]),
                                                                                                        FUN = function(question_val) if(sum(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col]==question_val)>4){
                                                                                                          search_col
                                                                                                        }
                                                                                                        )))
  
  
  
  return(unlist(varseach))
  }
}


#CATSTEP 7: creating the list of answer choices
answer_list = function(number_of_categorical,fulldata, list_of_splitframes,search_cols){
  if(number_of_categorical == 0){return("")}
  else {
  cat_search_cols = search_cols[search_cols %in% colnames(fulldata)]
  answerseach = lapply(list_of_splitframes,
                    FUN = function(frame_from_list) sapply(cat_search_cols, 
                                                           FUN = function(search_col) sapply(unique(frame_from_list[,search_col]),
                                                                                             FUN = function(question_val) if(sum(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col]==question_val)>4){
                                                                                               as.character(unlist(question_val))
                                                                                             }
                                                                                              )))
  
  
  
  return(unlist(answerseach))
  }
}

#CATSTEP 7A: outputting the N for each question/answer combination
n_list_categorical = function(number_of_categorical,fulldata, list_of_splitframes,search_cols){
  if(number_of_categorical == 0){return("")}
  else {
    cat_search_cols = search_cols[search_cols %in% colnames(fulldata)]
    n_of_each = lapply(list_of_splitframes,
                         FUN = function(frame_from_list) sapply(cat_search_cols, 
                                                                FUN = function(search_col) sapply(unique(frame_from_list[,search_col]),
                                                                                                  FUN = function(question_val) if(sum(filter(frame_from_list,!is.na(!!as.symbol(search_col)))[,search_col]==question_val)>4){
                                                                                                    sum(frame_from_list[,search_col]==question_val)
                                                                                                  }
                                                                )))
    
    
    
    return(unlist(n_of_each))
  }
}


#CATSTEP 8: creating the numeric dataframe and then adding rules
cattey_frame = function(n_category,number_of_categorical,cat,var,ans,pval,mean_cat,mean_overall,split_col){
  if(number_of_categorical == 0){return("")}
  else {
  cattey_dataframe <- cbind.data.frame(n_category,cat,var,ans,pval,mean_cat,mean_overall)
  cattey_dataframe$dif = round(((mean_cat - mean_overall)/mean_overall)+1,3)
  cattey_dataframe[,'mean_cat'] = percent(cattey_dataframe[,'mean_cat'])
  cattey_dataframe[,'mean_overall'] = percent(cattey_dataframe[,'mean_overall'])
  
  cattey_dataframe$rule = mapply(function(category, variable, answer, difference,split_column) {
    paste("When ",split_column," is ",category,", ",variable," is ",answer, " with ", difference,"X the frequency vs the overall average",sep = "")
  }, category = cat, variable = var, answer = ans, difference = cattey_dataframe$dif,split_column = split_col)
  
  return(cattey_dataframe)
  }
}


