#TESTING A DIFFERENT MAXIMIZATION IDEA
#We would need to nest this within a larger function that receives and then cleans the data into a form to be passed into the maximization

numbseg = 2

#age[11-15,21,25]
dima = c(rep(seq(11,15),25),rep(seq(21,25),25)) 

#income[21-25,31-35]
dimb = c(rep(c(rep(21,5),rep(22,5),rep(23,5),rep(24,5),rep(25,5)),5),rep(c(rep(31,5),rep(32,5),rep(33,5),rep(34,5),rep(35,5)),5))

#pref[3-7,13-17]
dimc = c(rep(3,25),rep(4,25),rep(5,25),rep(6,25),rep(7,25),rep(13,25),rep(14,25),rep(15,25),rep(16,25),rep(17,25)) #pref

scatter3D(dima,dimb,dimc)
#if find a way to use bounds...
optimal = c(10,16,20,26,20,26,30,36)
upper = c(max(dima)+1,max(dima)+1,max(dimb)+1,max(dimb)+1,max(dima)+1,max(dima)+1,max(dimb)+1,max(dimb)+1)
lower = c(min(dima)-1,min(dima)-1,min(dimb)-1,min(dimb)-1,min(dima)-1,min(dima)-1,min(dimb)-1,min(dimb)-1)

#the math assuming we already know which vectors are for outcome and which are segmentors
threetest = function(a){
  
  firstbox_mean <- mean(dimc[dima>a[1] & dima<=a[2] & dimb>a[3] & dimb<=a[4]])
  secondbox_mean <- mean(dimc[dima>a[5] & dima<=a[6] & dimb>a[7] & dimb<=a[8]])
  
  
  firstbox_n <- length(which(!is.na(dimc[dima>a[1] & dima<=a[2] & dimb>a[3] & dimb<=a[4]])))
  secondbox_n <- length(which(!is.na(dimc[dima>a[5] & dima<=a[6] & dimb>a[7] & dimb<=a[8]])))

  Difference <- (firstbox_mean - secondbox_mean)^2
  Width <- -1*sum((c(a[1],a[3],a[5],a[7])- c(a[2],a[4],a[6],a[8]))^2)
  N_penalty <- (firstbox_n + secondbox_n)
  N_min <- (sum(c((firstbox_n-50),(secondbox_n-50))>0)-(numbseg-1))*2

  N_weighted_difference <- Difference+Width+N_penalty+N_min
 
  return(N_weighted_difference)
  
}


test_outcome <- round(optim(par = c(min(dima),max(dima),min(dimb),max(dimb),min(dima),max(dima),min(dimb),max(dimb)),fn = threetest,control=list(fnscale=-1),method="Nelder-Mead")$par,0)
test_outcome
threetest(test_outcome)
threetest(optimal)
