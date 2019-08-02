returnconfusion <- function (Name, Formula, Method, datainp, datatest,NApercentage,...) {
  datainp <- as.data.frame(cbind(response = LETTERS[datainp[,1]], datainp[,-1]))
  datatest <- as.data.frame(cbind(response = LETTERS[datainp[,1]], datainp[,-1]))
  
  Model <- train(as.formula(Formula), data = datainp, method = Method,trControl=trainControl(classProbs=TRUE), ...)
  testinput <- as.data.frame(datatest[,-1])
  colnames(testinput) <- colnames(datainp)[-1]
  newmodel <- predict(Model, newdata = testinput,type="raw")
  newmodelprob <- predict(Model, newdata = testinput,type="prob")
  predtest <- as.vector(newmodel)
  Pnew <- newmodelprob
  botNA <- (apply(Pnew,1,max) < sort(apply(Pnew,1,max))[round(NApercentage*dim(Pnew)[1],0)])
  topNA <- (apply(Pnew,1,max) <= sort(apply(Pnew,1,max))[round(NApercentage*dim(Pnew)[1],0)])
  trueNA <- round(NApercentage*dim(Pnew)[1]) 
  mismatchNA <- (topNA != botNA)
  exclNA <- botNA
  if(sum(mismatchNA) > 0)
  {
    set.seed(3)
    samesame <- (1:length(mismatchNA))[mismatchNA]
    if (length(samesame)>1) {samesame <- sample(samesame,trueNA - sum(botNA),replace=FALSE)}
    exclNA[samesame] <- TRUE
    rm(.Random.seed, envir=globalenv())
  }
  predtest[exclNA==TRUE] <- NA
  predtest <- as.factor(predtest)
  actualtest <- as.factor(datatest[,1])
  newconfoutblack <- rbind((confusionMatrix(as.factor(predtest),as.factor(actualtest))$table),table(as.factor(actualtest)[is.na(as.factor(predtest))]))
  tempoutB <-data.frame(Name=confmetrics(newconfoutblack))
  dimnames(tempoutB)[[2]][1] <- Name
  return(tempoutB)
}