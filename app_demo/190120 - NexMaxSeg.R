#TESTING A DIFFERENT MAXIMIZATION IDEA
#We would need to nest this within a larger function that receives and then cleans the data into a form to be passed into the maximization

##EXAMPLE DATA CUBE FOR TESTING LATER, BUT ITS WAY TOOOO BIGGGG FOR THIS SET OF CODE AT THE MOMENT###

#age[11-15,21,25]
dima = c(rep(seq(11,15),25),rep(seq(21,25),25)) 

#income[21-25,31-35]
dimb = c(rep(c(rep(21,5),rep(22,5),rep(23,5),rep(24,5),rep(25,5)),5),rep(c(rep(31,5),rep(32,5),rep(33,5),rep(34,5),rep(35,5)),5))

#pref[3-7,13-17]
dimc = c(rep(3,25),rep(4,25),rep(5,25),rep(6,25),rep(7,25),rep(13,25),rep(14,25),rep(15,25),rep(16,25),rep(17,25)) #pref

scatter3D(dima,dimb,dimc)
#if find a way to use bounds...
optimal = c(10,16,20,26,20,26,30,36)


#OUR OWN ALGORITHM# PROPIETARY WATCH OUT CAUTION#
#Creating fake data inputs for our frame
#??DE DUPE THE WAY IT SEARCHES IN CASE THERE ARE MULTIPLE VALUES IN THE DATA (E.G., INSTEAD OF DOING FOR(X IN Y) WHICH SEARCHES ALL OF Y , DO "X IN UNIQUE(Y)")
t1 = c(1,1,2,2,3,3)
t2 = c(6,6,7,7,8,8)
t3 = c(2,2,4,9,9,9)

xlength = length(t1)^2-(length(t1)*((length(t1)-1)/2))
ylength = length(t2)^2-(length(t2)*((length(t2)-1)/2))
totlength = xlength*ylength
outputframe = as.data.frame(matrix(0,ncol=8,nrow=totlength))
colnames(outputframe) = c("x1","x2","y1","y2","zbar","n", "xwidth","ywidth")

counter = 0
for (i in t1){
  for (j in i:max(t1)){
      for(k in t2) {
          for(l in k:max(t2)){
    counter = counter+1
    outputframe$x1[counter]=i
    outputframe$x2[counter]=j
    outputframe$y1[counter]=k
    outputframe$y2[counter]=l
   
      }
    } 
  }
}

#to specific in a larger function#
xcol=t1
ycol=t2
outputcol=t3
##
call_the_mean = function(x1,x2,y1,y2){
  return(round(mean(outputcol[xcol>=x1 & xcol<=x2 & ycol>=y1 & ycol<=y2]),2))
}

call_the_n = function(x1,x2,y1,y2){
  return(round(length(outputcol[xcol>=x1 & xcol<=x2 & ycol>=y1 & ycol<=y2]),2))
}
  
call_the_sd = function(x1,x2,y1,y2){
  return(round(sd(outputcol[xcol>=x1 & xcol<=x2 & ycol>=y1 & ycol<=y2]),2))
}


#?? Note: for easier calculation times could remove na's right away from this step in calculation after doing mean, etc.
outputframe$zbar = round((mapply(call_the_mean,x1=outputframe$x1,x2=outputframe$x2,y1=outputframe$y1,y2=outputframe$y2)),2)
outputframe$n = mapply(call_the_n,x1=outputframe$x1,x2=outputframe$x2,y1=outputframe$y1,y2=outputframe$y2)
outputframe$sd = mapply(call_the_sd,x1=outputframe$x1,x2=outputframe$x2,y1=outputframe$y1,y2=outputframe$y2)
outputframe_pos = outputframe[!is.nan(outputframe$zbar),]
outputframe_pos = outputframe_pos[!is.na(outputframe$sd),]
outputframe_pos$xwidth = outputframe_pos$x2 - outputframe_pos$x1
outputframe_pos$ywidth = outputframe_pos$y2 - outputframe_pos$y1


#Outputting the frame of all possible segments of x and y along with their respective stats (e.g., z bar)
#creating the squared dataframe for all possible pairs of the segments
size = unlist(nrow(outputframe_pos))
comp_frame = as.data.frame(matrix(0, ncol=17,nrow=size^2))
colnames(comp_frame) = c("a_x1","a_x2","a_y1","a_y2","b_x1","b_x2","b_y1","b_y2","Mean_Difference_Sqr","a_n","b_n","a_xwidth","a_ywidth","b_xwidth","b_ywidth")

counter=0
#Creating a frame of all possible pairs of segment cutoffs across x and y
for (i in 1:size){
  for (j in (i+1):size){
    counter = counter+1
   comp_frame$a_x1[counter] = outputframe_pos$x1[i]
   comp_frame$a_x2[counter] = outputframe_pos$x2[i]
   comp_frame$a_y1[counter] = outputframe_pos$y1[i]
   comp_frame$a_y2[counter] = outputframe_pos$y2[i]
   comp_frame$b_x1[counter] = outputframe_pos$x1[j]
   comp_frame$b_x2[counter] = outputframe_pos$x2[j]
   comp_frame$b_y1[counter] = outputframe_pos$y1[j]
   comp_frame$b_y2[counter] = outputframe_pos$y2[j]
   comp_frame$Mean_Difference_Sqr[counter] = (outputframe_pos$zbar[i]-outputframe_pos$zbar[j])^2
   comp_frame$a_n[counter] = outputframe_pos$n[i]
   comp_frame$b_n[counter] = outputframe_pos$n[j]
   comp_frame$a_xwidth[counter] = outputframe_pos$xwidth[i]
   comp_frame$a_ywidth[counter] = outputframe_pos$ywidth[i]
   comp_frame$b_xwidth[counter] = outputframe_pos$xwidth[j]
   comp_frame$b_ywidth[counter] = outputframe_pos$ywidth[j]
   comp_frame$a_sd[counter] = outputframe_pos$sd[i]
   comp_frame$b_sd[counter] = outputframe_pos$sd[j]
     
  }
}
    
#Compframe is the output of all pairs of cutoffs
#Now adding a bunch of calculation columns on which to select our best segmentation
comp_frame$total_n = comp_frame$a_n+comp_frame$b_n
comp_frame$total_width = comp_frame$a_xwidth + comp_frame$a_ywidth + comp_frame$b_xwidth + comp_frame$b_ywidth
comp_frame$Mean_Difference_Sqr_Scale = scale(comp_frame$Mean_Difference_Sqr)
#removing all values with NA sd which effectively removes all choices with n=1 or n=0 in a segment
comp_frame = comp_frame[comp_frame$a_sd >= 0 & comp_frame$b_sd >= 0,]
comp_frame$total_sd = comp_frame$a_sd + comp_frame$b_sd
comp_frame$total_sd_scale = scale(comp_frame$total_sd)
comp_frame$total_n[is.na(comp_frame$total_n)] = 0
comp_frame$total_n_scale = scale(comp_frame$total_n)
comp_frame$total_width_scale = scale(comp_frame$total_width)

#Segments must include at minimum 60% of our data (can specify this as a user input later)
comp_frame = comp_frame[comp_frame$total_n > 0.6*length(t3),]
comp_frame2 = comp_frame

#Removing all 0 values from consideration
comp_frame2 = comp_frame[comp_frame$ratio > 0 & comp_frame$ratio < Inf,]
comp_frame2$ratio_scale = scale(comp_frame2$ratio)

comp_frame2$seg_index_value = comp_frame2$Mean_Difference_Sqr_Scale + comp_frame2$total_n_scale

#Now we are getting the right answer, however we need to think about properly choosing the subset segments according to which makes sense, like we need to show
#possible do the above as a first pass, then among the top ranked choices punish for excess width in the segment legs to get the narrowest value that returns the high score
top_contenders = comp_frame2[comp_frame2$seg_index_value == max(comp_frame2$seg_index_value),]
# top_contenders[order(top_contenders$total_width_scale),]

#Output best choice
best_seg = top_contenders[order(top_contenders$total_width_scale),][1,1:9]
top_contenders[order(top_contenders$total_width_scale),][1:9,1:9]

