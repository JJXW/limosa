

masterframeFX = function(original_data, vars, target_var, min_leaf, cpinput, unique_outcomes, pvalue_thresh){
  
  if(cpinput == TRUE){
    cpinput = 0.01
  } else{
    cpinput = -0.01
  }
  
  test_data = filter(original_data,!is.na((!!as.symbol(target_var))))
  # test_data = filter(original_data,(!!as.symbol(target_var)) != "")
  # print("hihihihih")
  # print(unique(test_data[,target_var]))
  # print(length(unique(test_data[,target_var])))

  #progress bar
  withProgress(message = "Assessing all potential datacuts...", value = 0, {
    
    if(!(class(test_data[,target_var]) %in% c("integer","numeric"))){
      ###CATEGORICAL###
      
      #creating the masterframe, note the rows is capped at 10000 for now (do not know how to predict exact rows)
      masterframe = as.data.frame(matrix(nrow = 10000, ncol = (10 + 2*unique_outcomes)))
      colnames = c("Leaf","n","yval",as.character(unique(test_data[,target_var])[order(unique(test_data[,target_var]))]),"w","x","y","z","rule",paste("avg",unique(test_data[,target_var])[order(unique(test_data[,target_var]))],sep="_"),"Dif_Score","pvalue")
      # colnames[colnames==""] = "Blank"
      # colnames[colnames=="avg_"] = "avg_Blank"
      colnames(masterframe) = colnames
      var_comb_frame = combn(vars,min(4, length(vars)))
      
      
      j = 1
      i=1
      for(i in 1:ncol(var_comb_frame)){
        #assigning variables
        w = var_comb_frame[1,i]
        #allowing for less than 4 variables
        tryCatch({x = var_comb_frame[2,i]},error = function(err){})
        tryCatch({y = var_comb_frame[3,i]},error = function(err){})
        tryCatch({z = var_comb_frame[4,i]},error = function(err){})
        

        #running the tree
        if(length(vars)>3){
          model = rpart(get(target_var) ~ get(w) + get(x) + get(y) + get(z), data = test_data, control = rpart.control(cp = cpinput, minbucket = min_leaf))
        } else if (length(vars)>2){
          model = rpart(get(target_var) ~ get(w) + get(x) + get(y), data = test_data, control = rpart.control(cp = cpinput, minbucket = min_leaf))
        } else if (length(vars)>1){
          model = rpart(get(target_var) ~ get(w) + get(x), data = test_data, control = rpart.control(cp = cpinput, minbucket = min_leaf))
        } else {
          model = rpart(get(target_var) ~ get(w), data = test_data, control = rpart.control(cp = cpinput, minbucket = min_leaf))
        }
        
        #CHANGE
        print(model$frame)
        
        #choosing only the leaves
        if(nrow(model$frame)<=1){
          #catching models that do not yield leaves
        }
        else {
          modframe_sort = model$frame[order(as.numeric(row.names(model$frame))),]
          modframe_sort_leafonly = modframe_sort[modframe_sort[,1]=="<leaf>",]
          modframe_yval2s = model$frame$yval2[order(as.numeric(row.names(model$frame))),]
          modframe_yval2s = modframe_yval2s[modframe_sort[,1]=="<leaf>",]
          numleafs = nrow(modframe_sort_leafonly)
          
          #setting masterframe to values in model frame
          #leaf and n
          masterframe[j:(j+numleafs-1),1:2] = modframe_sort_leafonly[,1:2]
          #yval
          masterframe[j:(j+numleafs-1),3] = modframe_sort_leafonly[,5]
          #yval2...
          masterframe[j:(j+numleafs-1),4:(4+unique_outcomes-1)] = round(modframe_yval2s[,(2+unique_outcomes):(1+2*unique_outcomes)],2)
          #vars used
          masterframe[j:(j+numleafs-1),(5+unique_outcomes-1)] = w
          masterframe[j:(j+numleafs-1),(6+unique_outcomes-1)] = tryCatch({x},error = function(err){return("N/A")})

          masterframe[j:(j+numleafs-1),(7+unique_outcomes-1)] = tryCatch({y},error = function(err){return("N/A")})
          masterframe[j:(j+numleafs-1),(8+unique_outcomes-1)] = tryCatch({z},error = function(err){return("N/A")})

          #rules 
          mod_rules = rpart.rules(model)
          temprule = rpart.rules(model, nn=T)
          mod_rules2 = mod_rules[match(sort(as.numeric(temprule$nn),decreasing=F),temprule$nn),]
          

          masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)] = trimws(gsub("\\s+"," ",apply(mod_rules2,1,FUN = paste, collapse = " ")))
          masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)] = str_replace_all(masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)],"get\\(w\\)",w)
          masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)] = tryCatch({str_replace_all(masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)],"get\\(x\\)",x)},error = function(err){return(masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)])})
          masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)] = tryCatch({str_replace_all(masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)],"get\\(y\\)",y)},error = function(err){return(masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)])})
          masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)] = tryCatch({str_replace_all(masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)],"get\\(z\\)",z)},error = function(err){return(masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)])})
         
          
          #overall avg
          masterframe[j:(j+numleafs-1),(10+unique_outcomes-1):(9+2*unique_outcomes-1)] = round(t(replicate(numleafs,model$frame$yval2[1,(2+unique_outcomes):(1+2*unique_outcomes)])),2)
          

          #difference vs average
          #JON TO ASSESS AND CHANGE AS NEEDED
          masterframe[j:(j+numleafs-1),(10+2*unique_outcomes-1)] = round(apply(abs(masterframe[j:(j+numleafs-1),4:(4+unique_outcomes-1)] - masterframe[j:(j+numleafs-1),(10+unique_outcomes-1):(9+2*unique_outcomes-1)]),1,sum),2)
          
          
          #chi square p value
          
          for(k in 1:numleafs){
            not_node = model$frame$yval2[1,2:(unique_outcomes+1)]
            chi_tab = data.frame(cbind(modframe_yval2s[k,2:(unique_outcomes+1)],not_node))
            chi_tab = filter(chi_tab, not_node !=0)
            masterframe$pvalue[j+k-1] = round(chisq.test(chi_tab)$p.value,2)
          }
          
          j = j + numleafs
          
        }
        incProgress(1/ncol(var_comb_frame))
        Sys.sleep(0.01)
      }

      #removing duplicates from the model (e.g., two different groups of 4 vars give the same outputted best tree) and sorting by Dif_Score
      masterframe = masterframe[!duplicated(masterframe$rule),]
      masterframe = masterframe[masterframe$pvalue < pvalue_thresh,]
      masterframe = masterframe[order(masterframe$pvalue,decreasing = F),]

      #end if
    } else {
      
      ###NUMERIC###
      masterframe = as.data.frame(matrix(nrow = 10000, ncol = (11)))
      colnames = c("Leaf","n","yval","w","x","y","z","rule","avg_yval","Dif_Score","pvalue")
      # colnames[colnames==""] = "Blank"
      # colnames[colnames=="avg_"] = "avg_Blank"
      colnames(masterframe) = colnames
      var_comb_frame = combn(vars,min(4, length(vars)))
      
      j = 1
      i=1
      for(i in 1:ncol(var_comb_frame)){
        #assigning variables
        w = var_comb_frame[1,i]
        #allowing for less than 4 variables
        tryCatch({x = var_comb_frame[2,i]},error = function(err){})
        tryCatch({y = var_comb_frame[3,i]},error = function(err){})
        tryCatch({z = var_comb_frame[4,i]},error = function(err){})
        
        #running the tree
        if(length(vars)>3){
          model = rpart(get(target_var) ~ get(w) + get(x) + get(y) + get(z), data = test_data, control = rpart.control(cp = cpinput, minbucket = min_leaf))
        } else if (length(vars)>2){
          model = rpart(get(target_var) ~ get(w) + get(x) + get(y), data = test_data, control = rpart.control(cp = cpinput, minbucket = min_leaf))
        } else if (length(vars)>1){
          model = rpart(get(target_var) ~ get(w) + get(x), data = test_data, control = rpart.control(cp = cpinput, minbucket = min_leaf))
        } else {
          model = rpart(get(target_var) ~ get(w), data = test_data, control = rpart.control(cp = cpinput, minbucket = min_leaf))
        }
        
        #choosing only the leaves
        if(nrow(model$frame)<=1){
          #catching models that do not yield leaves
        }
        else {
          
          modframe = model$frame[model$frame[,1]=="<leaf>",]
          modframe = modframe[order(as.numeric(row.names(modframe)),decreasing = F),]
          numleafs = nrow(modframe)
          
          #setting masterframe to values in model frame
          #leaf and n
          masterframe[j:(j+numleafs-1),1:2] = modframe[,1:2]
          #yval
          masterframe[j:(j+numleafs-1),3] = round(modframe[,5],2)
          
          #vars used
          masterframe[j:(j+numleafs-1),(4)] = w
          masterframe[j:(j+numleafs-1),(5)] = tryCatch({x},error = function(err){return("N/A")})
          masterframe[j:(j+numleafs-1),(6)] = tryCatch({y},error = function(err){return("N/A")})
          masterframe[j:(j+numleafs-1),(7)] = tryCatch({z},error = function(err){return("N/A")})
          
          
          #rules
          mod_rules = rpart.rules(model)
          temprule = rpart.rules(model, nn=T)
          mod_rules2 = mod_rules[match(sort(as.numeric(temprule$nn),decreasing=F),temprule$nn),]
          
          masterframe[j:(j+numleafs-1),(8)] = trimws(gsub("\\s+"," ",apply(mod_rules2,1,FUN = paste, collapse = " ")))
          masterframe[j:(j+numleafs-1),(8)] = tryCatch({str_replace_all(masterframe[j:(j+numleafs-1),(8)],"get\\(w\\)",w)},error = function(err){return(masterframe[j:(j+numleafs-1),(8)])})
          masterframe[j:(j+numleafs-1),(8)] = tryCatch({str_replace_all(masterframe[j:(j+numleafs-1),(8)],"get\\(x\\)",x)},error = function(err){return(masterframe[j:(j+numleafs-1),(8)])})
          masterframe[j:(j+numleafs-1),(8)] = tryCatch({str_replace_all(masterframe[j:(j+numleafs-1),(8)],"get\\(y\\)",y)},error = function(err){return(masterframe[j:(j+numleafs-1),(8)])})
          masterframe[j:(j+numleafs-1),(8)] = tryCatch({str_replace_all(masterframe[j:(j+numleafs-1),(8)],"get\\(z\\)",z)},error = function(err){return(masterframe[j:(j+numleafs-1),(8)])})
          
          #overall avg
          masterframe[j:(j+numleafs-1),(9)] = round(model$frame[1,5],2)
          
          #difference from avg
          #JON TO ASSESS AND CHANGE AS NEEDED
          masterframe[j:(j+numleafs-1),(10)] = round(masterframe[j:(j+numleafs-1),3] - masterframe[j:(j+numleafs-1),(9)],2)
          
          #chi square p value
          party_model = as.party(model)
          party_index = 1:nrow(model$frame)
          party_nodes = party_index[match(row.names(modframe),row.names(model$frame))]
          pop_mean = model$frame[1,5]
          
          i = 1
          for(k in party_nodes){
            #return the values in a leaf
            leaf_of_choice = data_party(party_model,k)
            leaf_response = leaf_of_choice[,ncol(leaf_of_choice)] #returning the last column which is the response variable
            masterframe$pvalue[j+i-1] = round(t.test(leaf_response,mu = pop_mean)$p.value,2)
            i = i+1
          }
          
          j = j + numleafs
          
        }
      }
      
      #removing duplicates and sorting by pvalue
      masterframe = masterframe[!duplicated(masterframe$rule),]
      masterframe = masterframe[masterframe$pvalue < pvalue_thresh,]
      masterframe = masterframe[order(masterframe$pvalue,decreasing = F),]
      
      
    }
    
  })  
  ##end if/else
  return(masterframe)
}


####OUTPUTTING THE LIST OF DATAFRAMES FOR NUMERIC BOXPLOT OUTPUT####
masterframe_nodecuts = function(original_data, vars, target_var, min_leaf, cpinput, unique_outcomes, pvalue_thresh){
  if(cpinput == TRUE){
    cpinput = 0.01
  } else{
    cpinput = -0.01
  }
    
  test_data = filter(original_data,!is.na((!!as.symbol(target_var))))
  
    if(!(class(test_data[,target_var]) %in% c("integer","numeric"))){
      ###CATEGORICAL###
      
      #creating the masterframe, note the rows is capped at 10000 for now (do not know how to predict exact rows)
      masterframe = as.data.frame(matrix(nrow = 10000, ncol = (10 + 2*unique_outcomes)))
      colnames = c("Leaf","n","yval",as.character(unique(test_data[,target_var])[order(unique(test_data[,target_var]))]),"w","x","y","z","rule",paste("avg",unique(test_data[,target_var])[order(unique(test_data[,target_var]))],sep="_"),"Dif_Score","pvalue")
      # colnames[colnames==""] = "Blank"
      # colnames[colnames=="avg_"] = "avg_Blank"
      colnames(masterframe) = colnames
      var_comb_frame = combn(vars,min(4, length(vars)))
      
      
      j = 1
      i=1
      for(i in 1:ncol(var_comb_frame)){
        #assigning variables
        w = var_comb_frame[1,i]
        #allowing for less than 4 variables
        tryCatch({x = var_comb_frame[2,i]},error = function(err){})
        tryCatch({y = var_comb_frame[3,i]},error = function(err){})
        tryCatch({z = var_comb_frame[4,i]},error = function(err){})
        
        #running the tree
        if(length(vars)>3){
          model = rpart(get(target_var) ~ get(w) + get(x) + get(y) + get(z), data = test_data, control = rpart.control(cp = cpinput, minbucket = min_leaf))
        } else if (length(vars)>2){
          model = rpart(get(target_var) ~ get(w) + get(x) + get(y), data = test_data, control = rpart.control(cp = cpinput, minbucket = min_leaf))
        } else if (length(vars)>1){
          model = rpart(get(target_var) ~ get(w) + get(x), data = test_data, control = rpart.control(cp = cpinput, minbucket = min_leaf))
        } else {
          model = rpart(get(target_var) ~ get(w), data = test_data, control = rpart.control(cp = cpinput, minbucket = min_leaf))
        }
        
        #choosing only the leaves
        if(nrow(model$frame)<=1){
          #catching models that do not yield leaves
        }
        else {
          modframe_sort = model$frame[order(as.numeric(row.names(model$frame))),]
          modframe_sort_leafonly = modframe_sort[modframe_sort[,1]=="<leaf>",]
          modframe_yval2s = model$frame$yval2[order(as.numeric(row.names(model$frame))),]
          modframe_yval2s = modframe_yval2s[modframe_sort[,1]=="<leaf>",]
          numleafs = nrow(modframe_sort_leafonly)
          
          #setting masterframe to values in model frame
          #leaf and n
          masterframe[j:(j+numleafs-1),1:2] = modframe_sort_leafonly[,1:2]
          #yval
          masterframe[j:(j+numleafs-1),3] = modframe_sort_leafonly[,5]
          #yval2...
          masterframe[j:(j+numleafs-1),4:(4+unique_outcomes-1)] = round(modframe_yval2s[,(2+unique_outcomes):(1+2*unique_outcomes)],2)
          
          #vars used
          masterframe[j:(j+numleafs-1),(5+unique_outcomes-1)] = w
          masterframe[j:(j+numleafs-1),(6+unique_outcomes-1)] = tryCatch({x},error = function(err){return("N/A")})
          masterframe[j:(j+numleafs-1),(7+unique_outcomes-1)] = tryCatch({y},error = function(err){return("N/A")})
          masterframe[j:(j+numleafs-1),(8+unique_outcomes-1)] = tryCatch({z},error = function(err){return("N/A")})
          #rules
          mod_rules = rpart.rules(model)
          temprule = rpart.rules(model, nn=T)
          mod_rules2 = mod_rules[match(sort(as.numeric(temprule$nn),decreasing=F),temprule$nn),]
          
          masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)] = trimws(gsub("\\s+"," ",apply(mod_rules2,1,FUN = paste, collapse = " ")))
          masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)] = str_replace_all(masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)],"get\\(w\\)",w)
          masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)] = tryCatch({str_replace_all(masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)],"get\\(x\\)",x)},error = function(err){return(masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)])})
          masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)] = tryCatch({str_replace_all(masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)],"get\\(y\\)",y)},error = function(err){return(masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)])})
          masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)] = tryCatch({str_replace_all(masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)],"get\\(z\\)",z)},error = function(err){return(masterframe[j:(j+numleafs-1),(9+unique_outcomes-1)])})
          
          #overall avg
          masterframe[j:(j+numleafs-1),(10+unique_outcomes-1):(9+2*unique_outcomes-1)] = round(t(replicate(numleafs,model$frame$yval2[1,(2+unique_outcomes):(1+2*unique_outcomes)])),2)
          
          #difference vs average
          #JON TO ASSESS AND CHANGE AS NEEDED
          masterframe[j:(j+numleafs-1),(10+2*unique_outcomes-1)] = round(apply(abs(masterframe[j:(j+numleafs-1),4:(4+unique_outcomes-1)] - masterframe[j:(j+numleafs-1),(10+unique_outcomes-1):(9+2*unique_outcomes-1)]),1,sum),2)
          
          #chi square p value
          
          for(k in 1:numleafs){
            # not_node = model$frame$yval2[1,2:(unique_outcomes+1)]
            # chi_tab = cbind(modframe_yval2s[k,2:(unique_outcomes+1)],not_node)
            # masterframe$pvalue[j+k-1] = round(chisq.test(chi_tab)$p.value,2)
            
            
            not_node = model$frame$yval2[1,2:(unique_outcomes+1)]
            chi_tab = data.frame(cbind(modframe_yval2s[k,2:(unique_outcomes+1)],not_node))
            chi_tab = filter(chi_tab, not_node !=0)
            masterframe$pvalue[j+k-1] = round(chisq.test(chi_tab)$p.value,2)
          }
          
          j = j + numleafs
          
        }
        incProgress(1/ncol(var_comb_frame))
        Sys.sleep(0.01)
      }
      
      #removing duplicates from the model (e.g., two different groups of 4 vars give the same outputted best tree) and sorting by Dif_Score
      masterframe = masterframe[!duplicated(masterframe$rule),]
      masterframe = masterframe[masterframe$pvalue < pvalue_thresh,]
      masterframe = masterframe[order(masterframe$pvalue,decreasing = F),]
      
      #end if
    } else {
      
      ###NUMERIC###
      
      masterframe = as.data.frame(matrix(nrow = 10000, ncol = (11)))
      colnames = c("Leaf","n","yval","w","x","y","z","rule","avg_yval","Dif_Score","pvalue")
      colnames[colnames==""] = "Blank"
      colnames[colnames=="avg_"] = "avg_Blank"
      colnames(masterframe) = colnames
      var_comb_frame = combn(vars,min(4, length(vars)))
      node_list = list()
      
      j = 1
      for(i in 1:ncol(var_comb_frame)){
        #assigning variables
        w = var_comb_frame[1,i]
        #allowing for less than 4 variables
        tryCatch({x = var_comb_frame[2,i]},error = function(err){})
        tryCatch({y = var_comb_frame[3,i]},error = function(err){})
        tryCatch({z = var_comb_frame[4,i]},error = function(err){})
        
        #running the tree
        if(length(vars)>3){
          model = rpart(get(target_var) ~ get(w) + get(x) + get(y) + get(z), data = test_data, control = rpart.control(cp = cpinput, minbucket = min_leaf))
        } else if (length(vars)>2){
          model = rpart(get(target_var) ~ get(w) + get(x) + get(y), data = test_data, control = rpart.control(cp = cpinput, minbucket = min_leaf))
        } else if (length(vars)>1){
          model = rpart(get(target_var) ~ get(w) + get(x), data = test_data, control = rpart.control(cp = cpinput, minbucket = min_leaf))
        } else {
          model = rpart(get(target_var) ~ get(w), data = test_data, control = rpart.control(cp = cpinput, minbucket = min_leaf))
        }
        
        #choosing only the leaves
        if(nrow(model$frame)<=1){
          #catching models that do not yield leaves
        }
        else {
          
          modframe = model$frame[model$frame[,1]=="<leaf>",]
          modframe = modframe[order(as.numeric(row.names(modframe)),decreasing = F),]
          numleafs = nrow(modframe)
          
          #setting masterframe to values in model frame
          #leaf and n
          masterframe[j:(j+numleafs-1),1:2] = modframe[,1:2]
          #yval
          masterframe[j:(j+numleafs-1),3] = round(modframe[,5],2)
          
          #vars used
          masterframe[j:(j+numleafs-1),(4)] = w
          masterframe[j:(j+numleafs-1),(5)] = tryCatch({x},error = function(err){return("N/A")})
          masterframe[j:(j+numleafs-1),(6)] = tryCatch({y},error = function(err){return("N/A")})
          masterframe[j:(j+numleafs-1),(7)] = tryCatch({z},error = function(err){return("N/A")})
          
          
          #rules
          mod_rules = rpart.rules(model)
          temprule = rpart.rules(model, nn=T)
          mod_rules2 = mod_rules[match(sort(as.numeric(temprule$nn),decreasing=F),temprule$nn),]
          
          masterframe[j:(j+numleafs-1),(8)] = trimws(gsub("\\s+"," ",apply(mod_rules2,1,FUN = paste, collapse = " ")))
          masterframe[j:(j+numleafs-1),(8)] = tryCatch({str_replace_all(masterframe[j:(j+numleafs-1),(8)],"get\\(w\\)",w)},error = function(err){return(masterframe[j:(j+numleafs-1),(8)])})
          masterframe[j:(j+numleafs-1),(8)] = tryCatch({str_replace_all(masterframe[j:(j+numleafs-1),(8)],"get\\(x\\)",x)},error = function(err){return(masterframe[j:(j+numleafs-1),(8)])})
          masterframe[j:(j+numleafs-1),(8)] = tryCatch({str_replace_all(masterframe[j:(j+numleafs-1),(8)],"get\\(y\\)",y)},error = function(err){return(masterframe[j:(j+numleafs-1),(8)])})
          masterframe[j:(j+numleafs-1),(8)] = tryCatch({str_replace_all(masterframe[j:(j+numleafs-1),(8)],"get\\(z\\)",z)},error = function(err){return(masterframe[j:(j+numleafs-1),(8)])})
          
          #overall avg
          masterframe[j:(j+numleafs-1),(9)] = round(model$frame[1,5],2)
          
          #difference from avg
          #JON TO ASSESS AND CHANGE AS NEEDED
          masterframe[j:(j+numleafs-1),(10)] = round(masterframe[j:(j+numleafs-1),3] - masterframe[j:(j+numleafs-1),(9)],2)
          
          #OUTPUTTING THE FULL NODE FRAME FOR EACH NODE IN A LIST
          party_model = as.party(model)
          party_index = 1:nrow(model$frame)
          party_nodes = party_index[match(row.names(modframe),row.names(model$frame))]
          
          pop_mean = model$frame[1,5]
          
          q = 1
          for(k in party_nodes){
            #return the values in a leaf
            leaf_of_choice = data_party(party_model,k)
            node_list[[j+q-1]] = leaf_of_choice
            
            #returning the pvalue
            leaf_response = leaf_of_choice[,ncol(leaf_of_choice)] #returning the last column which is the response variable
            masterframe$pvalue[j+q-1] = round(t.test(leaf_response,mu = pop_mean)$p.value,2)
            
            q = q+1
            
          }
          j = j + numleafs
          
        }
      }
      

      #removing duplicates and sorting by Dif_Score
      #note all of the masterframe_X are so that node_list can be subsetted properly
      masterframe_0 = masterframe
      masterframe = masterframe[!duplicated(masterframe$rule),]
      
      masterframe_1 = masterframe
      masterframe = masterframe[masterframe$pvalue < pvalue_thresh,]
      
      masterframe_2 = masterframe
      masterframe = masterframe[order(masterframe$pvalue,decreasing = F),]
      
      node_list = node_list[!duplicated(masterframe_0$rule)]
      node_list = node_list[masterframe_1$pvalue < pvalue_thresh]
   
      node_list = node_list[order(masterframe_2$pvalue,decreasing = F)]
      

    }
    
  ##end if/else
  return(node_list)
}