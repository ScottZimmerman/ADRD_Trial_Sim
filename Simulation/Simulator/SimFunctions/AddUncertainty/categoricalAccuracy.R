function(d,
         varName,
         jointDist){#uncertainty is a joint distributiton of true vs predicted
  #Randomly assign each predicted value to a true value based on the accuracy of the prediction model
  #Do this by sampling from P(true|pred)
  
  #Construct cumulative distribution
  cumulDist <- jointDist
  for(colIndex in 2:ncol(cumulDist)){
    cumulDist[,colIndex] <- cumulDist[,colIndex-1] + cumulDist[,colIndex]
  }
  
  #Using a temporary table, assign random uniform U to each person 
  # and use it to assign true categories from the cmf=P(true|predicted)
  true_cats <- colnames(cumulDist)
  cmf <- data.frame(cumulDist)
  cmf$pred <- as.character(rownames(cmf))
  
  temp <- data.frame(U=runif(nrow(d)), pred=as.character(d[[varName]]))
  temp <- left_join(temp,cmf,by="pred")
  temp$trueCategoryIndex <- apply(temp[,true_cats] < temp$U,1,sum)+1
  temp$true <- true_cats[temp$trueCategoryIndex]
  d[,varName] <- factor(temp$true,levels =true_cats)
  return(d)
}