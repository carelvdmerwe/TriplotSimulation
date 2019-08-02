bipldrawknn <- function(datatrain, datatest, alphin = 0.6, type= "PCA",kin = 7,plotprop = c(T,T,F,T,F,F,T),method="knn", 
                        expininput = 1.15, evin = c(1,2), hullmultin = c(1.5,0.95), gridin = 200, nintint = 10,legendtextinp = NULL){
  par(xpd=FALSE)
  if(nrow(datatest) == 1) datatestinput <<- as.matrix(datatest[,-1])
  if(nrow(datatest) > 1) datatestinput <<-datatest[,-1]
  colnames(datatestinput) <<- colnames(datatrain)[-1]
  
  #use the indicators below to recreate the plots in the paper
  plotouter <- plotprop[1]
  plotinner <-plotprop[2]
  plotbags<-plotprop[3]
  plotcont <- plotprop[4]
  nolines <-plotprop[5] #make sure everyting else is true if this is to be True
  nospots <- plotprop[6]
  nospotstest <- plotprop[7]
  #datatrain<-DirtyDifCov[1:90,] #CleanSameCov #or any of the other datasets
  #datatest <- read.csv("Unknown.csv")
  #datatest <- datatest[,-6]
  numclasses <- length(unique(datatrain[,1]))
  
  #no_cores <- detectCores()-1
  #cl <- makeCluster(no_cores,type="PSOCK")
  #registerDoParallel(cl)
  expin <- expininput
  if(type=="AOD" & expin < 5){expin<-5}
  ev <- evin
  alphsim <- alphin
  hullmult <- hullmultin
  ax.col <- "grey"
  bluecols <- rgb(114, 147, 203, max = 255)
  yellowcols <- rgb(225, 151, 76, max = 255)
  greencols <- rgb(132, 186, 91, max = 255)
  redcols <- rgb(211, 94, 96, max = 255)
  greycols <- rgb(128, 133, 133 , max = 255)
  fillcols <- c(bluecols,yellowcols,greencols,redcols,greycols)[1:numclasses]
  #fillcols <- gray.colors(numclasses, start = 0.4, end = 0.9, gamma = 2.2, alpha = NULL)
  fillcolslines <- rep("black",numclasses)#gray.colors(numclasses, start = 0.3, end = 0.7, gamma = 2.2, alpha = NULL)
  pchnofill <- c(0,1,2,5)
  #pchsamefill <- c(15,16,17,18,25)
  pchdifffill <- c(22,21,24,23,25)
  datatrain.mean <-  apply(datatrain[,-1], 2, function(x) tapply(x, datatrain[,1], mean))
  if(type=="PCA")
  {
    bipl <- PCAbipl_C(datatrain[,-1], scaled.mat = T, #X.new.samples = datavalidate[,-1], pch.samples = 15:18, 
                      G = indmat(datatrain[, 1]), e.vects = ev, means.plot = F, colours = fillcols, 
                      pch.samples.size = 0, n.int=rep(nintint,dim(datatrain[,-1])[2]),
                      label = FALSE,  exp.factor = expin,  #pos = pos, 
                      predictivity.print = TRUE )
    xMeans <- apply(datatrain[,-1],2,mean)
    xStdevs <- sqrt(apply(datatrain[,-1],2,var))
    Zbase <- bipl$Z[,1:2]
    Zstar <- scale(datatestinput,xMeans,xStdevs)%*%bipl$V[,ev]
    Zstar <- as.data.frame(Zstar)
    colnames(Zstar) <- c("X1","X2")
    Zconstruct <- Zbase
  }
  if(type=="CVA")
  {
    bipl <<- CVAbipl_C(datatrain[,-1], G =indmat(datatrain[, 1]), weightedCVA = "weighted",n.int=rep(nintint,dim(datatrain[,-1])[2]),
                      means.plot = F, colours = fillcols, e.vects = ev,exp.factor = expin,#pch.samples = 15:18,
                      colours.means = fillcols , pch.samples.size =0, label = FALSE, #pos = pos, 
                      predictivity.print = FALSE,legend.type = c(F, F, F), Tukey.median = FALSE) 
    Zbase <<- scale(datatrain[,-1], bipl$X.means, scale = FALSE) %*% bipl$Br %*% bipl$rotate.mat %*% bipl$reflect.mat
    Zbase <<- as.data.frame(Zbase)
    colnames(Zbase) <<- c("X1","X2")
    xMeans <- NA
    xStdevs <- NA
    Zstar <<- scale(datatestinput, bipl$X.means, scale = FALSE) %*% bipl$Br %*% bipl$rotate.mat %*% bipl$reflect.mat
    Zstar <<- as.data.frame(Zstar)
    colnames(Zstar) <<- c("X1","X2")
    Zconstruct <- Zbase
  }
  if(type=="AOD")
  {
    bipl <<- PCAbipl_C(datatrain.mean, scaled.mat = T, #X.new.samples = dataclasses[,-1], 
                      G = indmat(as.numeric(rownames(datatrain.mean))), e.vects = ev, means.plot = F, colours = fillcols, 
                       pch.samples.size = 0, n.int=rep(nintint,dim(datatrain[,-1])[2]), 
                      label = FALSE, exp.factor = expin, #pos = pos,  #orthog.transy = c(25, -6,  2.8, 6), pch.samples = 15:18,
                      predictivity.print = TRUE )
    xMeans <- apply(datatrain.mean,2,mean)
    xStdevs <- sqrt(apply(datatrain.mean,2,var))
    Zbase <- scale(datatrain[,-1],xMeans,xStdevs)%*%bipl$V[,ev]
    Zbase <- as.data.frame(Zbase)
    colnames(Zbase) <- c("X1","X2")
    Zstar <- scale(datatestinput,xMeans,xStdevs)%*%bipl$V[,ev]
    colnames(Zstar) <- c("X1","X2")
    Zconstruct <- bipl$Z[,1:2]
  }
  
  
  #colnames(Z) <- c("X1","X2")
  nbp <- gridin;
  Rangex1 <- c(min(Zconstruct[,1])-abs(min(Zconstruct[,1]))*(expin-1),max(Zconstruct[,1])+abs(max(Zconstruct[,1]))*(expin-1))
  Rangex2 <- c(min(Zconstruct[,2])-abs(min(Zconstruct[,2]))*(expin-1),max(Zconstruct[,2])+abs(max(Zconstruct[,2]))*(expin-1))
  maxrange <- max(Rangex1[2]-Rangex1[1],Rangex2[2] - Rangex2[1])
  if(Rangex1[2]-Rangex1[1]<maxrange) Rangex1 <- c((Rangex1[2]+Rangex1[1])/2-maxrange/2,(Rangex1[2]+Rangex1[1])/2+maxrange/2)
  if(Rangex2[2]-Rangex2[1]<maxrange) Rangex2 <- c((Rangex2[2]+Rangex2[1])/2-maxrange/2,(Rangex2[2]+Rangex2[1])/2+maxrange/2)
  
  PredA <- seq(Rangex1[1],Rangex1[2], length = nbp)
  PredB <- seq(Rangex2[1],Rangex2[2], length = nbp)
  Grid <- expand.grid(X1 = PredA, X2 = PredB)
  
  traindataIND <- datatrain[,1]
  traindataIND[traindataIND==1] <- "p1"
  traindataIND[traindataIND==2] <- "p2"
  traindataIND[traindataIND==3] <- "p3"
  traindataIND[traindataIND==4] <- "p4"
  traindataIND[traindataIND==5] <- "p5"
  traindataIND<-cbind(as.data.frame(Zbase[,1:2]),classes=traindataIND)
  
  traindataINDColor <- fillcols#c("red","blue","darkgreen","orange")#brewer.pal(3,'Set1')[1:3]
  #names(traindataINDColor) <- c("p1","p2","p3","p4")
  
  #knn, nbgm, nbkd,mn,svmpk,svmgk,cart, bagging, rf
  
  if(method=="knn") Model <- train(as.formula("classes ~ ."), data = traindataIND, method = "knn", tuneGrid = data.frame(k = c(kin)))
  if(method=="lda") Model <- train(as.formula("classes ~ ."), data = traindataIND, method = "lda")
  if(method=="qda") Model <- train(as.formula("classes ~ ."), data = traindataIND, method = "qda")
  if(method=="nbgm") Model <- train(as.formula("classes ~ ."), data = traindataIND, method = "naive_bayes", tuneGrid = data.frame(usekernel = c(FALSE), laplace = c(0),adjust=c(0)))
  if(method=="nbkd") Model <- train(as.formula("classes ~ ."), data = traindataIND, method = "naive_bayes", tuneGrid = data.frame(usekernel = c(TRUE), laplace = c(0),adjust=c(0)))
  if(method=="mn") Model <- train(as.formula("classes ~ ."), data = traindataIND, method = "multinom")
  if(method=="svmpk") Model <- train(as.formula("classes ~ ."), data = traindataIND, method = "svmPoly")
  if(method=="svmgk") Model <- train(as.formula("classes ~ ."), data = traindataIND, method = "svmRadial")
  if(method=="cart") Model <- train(as.formula("classes ~ ."), data = traindataIND, method = "rpart",  control = rpart.control(minsplit = 5, cp = 0), tuneGrid = data.frame(cp = .02))
  if(method=="bagging") Model <- train(as.formula("classes ~ ."), data = traindataIND, method = "treebag", control = rpart.control(minsplit = 5))
  if(method=="rf") Model <- train(as.formula("classes ~ ."), data = traindataIND, method = "rf", tuneLength = 1, control = rpart.control(minsplit = 5))
  
  Pred <- predict(Model, newdata = Grid)
  
  #plot.new()
  #bipltestCVA <- CVAbipl_C(datatrain[,-1], X.new.samples = datatrain[,-1], G =indmat(datatrain[, 1]), weightedCVA = "weighted",
  #                                 means.plot = F, colours = c("red", "blue", "green","orange"), e.vects = ev,exp.factor = expin,
  #                                 colours.means = c("red", "blue", "green","orange"),pch.samples = 0:3, pch.samples.size =0, label = FALSE, #pos = pos, 
  #                                 predictivity.print = FALSE, 
  #                                 legend.type = c(F, F, F), Tukey.median = FALSE) #exactly the same as previously
  #biplCVA.Z1 <- scale(datatrain[,-1], bipltestCVA$X.means, scale = FALSE) %*% bipltestCVA$Br %*% bipltestCVA$rotate.mat %*% bipltestCVA$reflect.mat
  if(alphsim!=0)
  {
    if(numclasses == 2) {bagcords_Wnew <-list(compute.bagplot_C(Zbase[,1:2][datatrain[,1]==1,], factor = 1,alph=alphsim)$hull.bag, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==2,], factor = 1,alph=alphsim)$hull.bag)}
    if(numclasses == 3) {bagcords_Wnew <-list(compute.bagplot_C(Zbase[,1:2][datatrain[,1]==1,], factor = 1,alph=alphsim)$hull.bag, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==2,], factor = 1,alph=alphsim)$hull.bag, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==3,], factor = 1,alph=alphsim)$hull.bag)}
    if(numclasses == 4) {bagcords_Wnew <-list(compute.bagplot_C(Zbase[,1:2][datatrain[,1]==1,], factor = 1,alph=alphsim)$hull.bag, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==2,], factor = 1,alph=alphsim)$hull.bag, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==3,], factor = 1,alph=alphsim)$hull.bag,compute.bagplot_C(Zbase[,1:2][datatrain[,1]==4,], factor = 1,alph=alphsim)$hull.bag)}
    if(numclasses == 5) {bagcords_Wnew <-list(compute.bagplot_C(Zbase[,1:2][datatrain[,1]==1,], factor = 1,alph=alphsim)$hull.bag, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==2,], factor = 1,alph=alphsim)$hull.bag, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==3,], factor = 1,alph=alphsim)$hull.bag,compute.bagplot_C(Zbase[,1:2][datatrain[,1]==4,], factor = 1,alph=alphsim)$hull.bag, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==5,], factor = 1,alph=alphsim)$hull.bag)}
  }
  if(alphsim==0)
  {
    if(numclasses == 2) {bagcords_Wnew <- list(matrix(c(NA,NA),ncol=2), matrix(c(NA,NA),ncol=2))}
    if(numclasses == 3) {bagcords_Wnew <- list(matrix(c(NA,NA),ncol=2), matrix(c(NA,NA),ncol=2), matrix(c(NA,NA),ncol=2))}
    if(numclasses == 4) {bagcords_Wnew <- list(matrix(c(NA,NA),ncol=2), matrix(c(NA,NA),ncol=2), matrix(c(NA,NA),ncol=2),matrix(c(NA,NA),ncol=2))}
    if(numclasses == 5) {bagcords_Wnew <- list(matrix(c(NA,NA),ncol=2), matrix(c(NA,NA),ncol=2), matrix(c(NA,NA),ncol=2),matrix(c(NA,NA),ncol=2),matrix(c(NA,NA),ncol=2))}
    
  }
  
  
  if(numclasses == 2) {bagcords_Hull <- list(compute.bagplot_C(Zbase[,1:2][datatrain[,1]==1,], factor = hullmult[1],alph=hullmult[2])$hull.fullloop, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==2,],  factor = hullmult[1],alph=hullmult[2])$hull.fullloop)}
  if(numclasses == 3) {bagcords_Hull <- list(compute.bagplot_C(Zbase[,1:2][datatrain[,1]==1,], factor = hullmult[1],alph=hullmult[2])$hull.fullloop, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==2,],  factor = hullmult[1],alph=hullmult[2])$hull.fullloop, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==3,], factor = hullmult[1],alph=hullmult[2])$hull.fullloop)}
  if(numclasses == 4) {bagcords_Hull <- list(compute.bagplot_C(Zbase[,1:2][datatrain[,1]==1,], factor = hullmult[1],alph=hullmult[2])$hull.fullloop, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==2,],  factor = hullmult[1],alph=hullmult[2])$hull.fullloop, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==3,], factor = hullmult[1],alph=hullmult[2])$hull.fullloop,compute.bagplot_C(Zbase[,1:2][datatrain[,1]==4,], factor = hullmult[1],alph=hullmult[2])$hull.fullloop)}
  if(numclasses == 5) {bagcords_Hull <- list(compute.bagplot_C(Zbase[,1:2][datatrain[,1]==1,], factor = hullmult[1],alph=hullmult[2])$hull.fullloop, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==2,],  factor = hullmult[1],alph=hullmult[2])$hull.fullloop, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==3,], factor = hullmult[1],alph=hullmult[2])$hull.fullloop,compute.bagplot_C(Zbase[,1:2][datatrain[,1]==4,], factor = hullmult[1],alph=hullmult[2])$hull.fullloop, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==5,], factor = hullmult[1],alph=hullmult[2])$hull.fullloop)}
  
    
  polyorange <- clipcords(numclasses,bagcords_Wnew)
  polygrey <- unioncords(numclasses,bagcords_Hull)
  predGRID <- as.vector(Pred)
  if((length(polyorange)!=0)&(plotinner==T))
  {
    for (h in 1:length(polyorange))
    {
      amberpoints <- pnt.in.poly(Grid,cbind(polyorange[[h]]$x,polyorange[[h]]$y))[,3]
      predGRID[amberpoints==1] <- "orange"
    }
  }
  if(plotouter==T)
  {
    greytemp <- predGRID
    if(length(polygrey)!=0)
    {
      for (h in 1:length(polygrey))
      {
        greypoints <- pnt.in.poly(Grid,cbind(polygrey[[h]]$x,polygrey[[h]]$y))[,3]
        greytemp[greypoints==1] <- "grey"
      }
    }  
    predGRID[greytemp!="grey"] <- "grey" 
  }
  
  Pred15 <- rep(0, length(Pred))
  Pred15[predGRID=="p1"] <- 1
  Pred15[predGRID=="p2"] <- 2
  Pred15[predGRID=="p3"] <- 3
  Pred15[predGRID=="p4"] <- 4
  Pred15[predGRID=="p5"] <- 5
  Pred15[predGRID=="grey"] <- -1
  Pred15[predGRID=="orange"] <- 0
  Pred15 <- matrix(Pred15, length(PredA), length(PredB))
  
  if(plotcont==T)
  {
    .filled.contour(PredA, PredB, Pred15, levels=c(-1,0,1:numclasses,numclasses+1)-0.5,col=c("white","white",fillcols,"black","black"))  
    contour(PredA, PredB, Pred15, levels=c(-1,0,1:numclasses,numclasses+1)-0.5, labels="", xlab="", ylab="", main=
              "", axes=FALSE,add=TRUE,col="white")
  }

  if(nolines==F)
  {
    if((plotinner==T)|(plotbags==T))
    {
      if(numclasses > 1){
        polygon(bagcords_Wnew[[1]],border = fillcolslines[1], lwd=1,lty=2)
        polygon(bagcords_Wnew[[2]],border = fillcolslines[2],lwd=1,lty=2)}
      if(numclasses > 2) {polygon(bagcords_Wnew[[3]],border = fillcolslines[3],lwd=1,lty=2)}
      if(numclasses > 3) {polygon(bagcords_Wnew[[4]],border = fillcolslines[4],lwd=1,lty=2)}
      if(numclasses > 4) {polygon(bagcords_Wnew[[5]],border = fillcolslines[5],lwd=1,lty=2)}
    }
    if(plotouter==T)
    {
      if(numclasses > 1){
        polygon(bagcords_Hull[[1]],border = fillcolslines[1], lwd=1,lty=3)
        polygon(bagcords_Hull[[2]],border = fillcolslines[2],lwd=1,lty=3)}
      if(numclasses > 2) {polygon(bagcords_Hull[[3]],border = fillcolslines[3],lwd=1,lty=3)}
      if(numclasses > 3) {polygon(bagcords_Hull[[4]],border = fillcolslines[4],lwd=1,lty=3)}
      if(numclasses > 4) {polygon(bagcords_Hull[[5]],border = fillcolslines[5],lwd=1,lty=3)}
    }
  }

  if(plotcont==T)
  {
    r.names <- NULL
    usr <- par("usr")
    for (i in 1:dim(datatrain.mean)[[2]]) 
    {
      marker.mat <- bipl$Z.axes[[i]][bipl$Z.axes[[i]][, 4] == 1,1:3] ###
      x.vals <- marker.mat[, 1]
      y.vals <- marker.mat[, 2]
      if (is.null(dimnames(datatrain.mean)[[2]][i])) 
      {
        axis.name <- paste("v", i, sep = "")
      }
      else 
      {
        axis.name <- dimnames(datatrain.mean)[[2]][i]
      }
      r.names[i] <- axis.name
      std.markers <- zapsmall(marker.mat[, 3])
      x.invals <- x.vals[x.vals < usr[2] & x.vals > usr[1] & 
                           y.vals < usr[4] & y.vals > usr[3]]
      if (length(x.invals) < 2) 
      {
        warning(paste("Less than 2 markers on axis ", 
                      i, ". Increase n.int."))
        marker.mat <- bipl$Z.axes[[i]][, 1:3]####
        x.vals <- marker.mat[, 1]
        y.vals <- marker.mat[, 2]
        std.markers <- zapsmall(marker.mat[, 3])
        x.invals <- x.vals[x.vals < usr[2] & x.vals > 
                             usr[1] & y.vals < usr[4] & y.vals > usr[3]]
        y.invals <- y.vals[x.vals < usr[2] & x.vals > 
                             usr[1] & y.vals < usr[4] & y.vals > usr[3]]
        tick.labels <- std.markers[x.vals < usr[2] & 
                                     x.vals > usr[1] & y.vals < usr[4] & y.vals > 
                                     usr[3]]
        Draw.line2(x.vals = x.invals, y.vals = y.invals, 
                   marker.vals = tick.labels, line.name = axis.name, 
                   ax.name.size = 0.75, axis.col = "black", 
                   ax.name.col = "black", offset = rep(0, length(datatrain.mean)), 
                   pos = pos)
        next
      }
      y.invals <- y.vals[x.vals < usr[2] & x.vals > usr[1] & 
                           y.vals < usr[4] & y.vals > usr[3]]
      tick.labels <- std.markers[x.vals < usr[2] & x.vals > 
                                   usr[1] & y.vals < usr[4] & y.vals > usr[3]]
      uit <- Draw.line2(x.vals = x.invals, y.vals = y.invals, 
                        marker.vals = tick.labels, line.name = axis.name, 
                        ax.name.size = 0.75, axis.col = "black", 
                        ax.name.col =  "black", offset = rep(0, length(datatrain.mean)))# , 
      #pos = pos)
      
      gradient <- uit$gradient
      for (j in 1:length(x.invals)) Draw.onecmline(x = x.invals[j], 
                                                   y = y.invals[j], grad = -1/gradient, expand = c(1, 1)[1], 
                                                   both.sides = TRUE, col = "black")
      x.labvals <- x.invals
      y.labvals <- y.invals
      for (j in 1:length(x.labvals)) Plot.marker.new(x = x.labvals[j], 
                                                     y = y.labvals[j], grad = -1/gradient, mark = tick.labels[j], 
                                                     expand = c(1, 1)[1], marker.size = 0.6, 
                                                     col = "black", pos.m = NULL, offset.m = 0.0025, 
                                                     side.label = "right")
      
    }
  }
  
  if(nospots==F)
  {
    for (j in c(1,2,3,4,5)) 
    {
      Z.class <- Zbase[datatrain[,1] == j, , drop = FALSE]
      if (dim(Z.class)[1] !=0)
      {
        Z.class <- data.frame(Z.class[, 1:2], pch.samples.samp = pchdifffill[j], colr = fillcolslines[j], lty = 1, 
                              pch.samples.size = 1, bg=fillcols[j], stringsAsFactors = FALSE)
        for (i in 1:nrow(Z.class)) points(x = Z.class[i, 1], y = Z.class[i, 2], pch = Z.class[i, 3], 
                                          col = Z.class[i, 4], cex = Z.class[i, 6], bg = Z.class[i,  7])
      }
    }
  }
  #stopCluster(cl)
  
  if(nospotstest==F)
  {
    for (j in c(1,2,3,4,5)) 
    {
      Z.class <- Zstar[datatest[,1] == j, , drop = FALSE]
      if (dim(Z.class)[1] !=0)
      {
        Z.class <- data.frame(Z.class[, 1:2], pch.samples.samp = pchdifffill[j], colr = fillcolslines[j], lty = 1, 
                              pch.samples.size = 1, bg=fillcols[j],stringsAsFactors = FALSE)
        for (i in 1:nrow(Z.class)) points(x = Z.class[i, 1], y = Z.class[i, 2], pch = Z.class[i, 3], 
                                          col = Z.class[i, 4], cex = Z.class[i, 6], bg = Z.class[i,  7])
      }
    }
    
    if(nrow(datatestinput) == 1)
    {
      predictions<- rep(NA,dim(datatestinput)[2])
      for(i in 1:(dim(datatestinput)[2]))
      {
        points(x = Zstar[1,1], y = Zstar[1,2],pch=15) 
        predictions[i] <- round(DrawOrthogline(x1 = bipl$Z.axes[[i]][1,1], 
                                               y1 = bipl$Z.axes[[i]][1,2], x2 =  bipl$Z.axes[[i]][length(bipl$Z.axes[[i]][,1]),1], 
                                               y2 = bipl$Z.axes[[i]][length(bipl$Z.axes[[i]][,2]),2], val1 = bipl$Z.axes[[i]][1,3], 
                                               val2 = bipl$Z.axes[[i]][length(bipl$Z.axes[[i]][,3]),3], 
                                               px = Zstar[1,1], py = Zstar[1, 2], ort.lty = 2), 
                                digits = 4)
        points(approx(x = bipl$Z.axes[[i]][,3], y= bipl$Z.axes[[i]][,1], xout= predictions[i])$y,
               approx(x = bipl$Z.axes[[i]][,3], y= bipl$Z.axes[[i]][,2], xout= predictions[i])$y,pch=1,cex = 1.5)  
        
        #points(approx(x = bipl$Z.axes[[i]][,3], y= bipl$Z.axes[[i]][,1], xout= datatest[1,i+1])$y,
        #       approx(x = bipl$Z.axes[[i]][,3], y= bipl$Z.axes[[i]][,2], xout= datatest[1,i+1])$y, pch = 16)  
      }
    }
    
  }
  namesclass <- datatrain[,1]
  namesclass[namesclass==1] <- "A"
  namesclass[namesclass==2] <- "B"
  namesclass[namesclass==3] <- "C"
  namesclass[namesclass==4] <- "D"
  namesclass[namesclass==5] <- "E"
  if(is.null(legendtextinp))
  {
  legendtextinp <- as.character(unique(namesclass))
  count <- 0
  for (i in unique(namesclass))
  {
    count <- count+1
    legendtextinp[count] <- paste("Class ",legendtextinp[count],"   ",sep="")
  }
  }
  
  par(xpd=TRUE)
  legend("bottom", legend=legendtextinp, pt.lwd=0.5, pt.cex=1,lty=1, cex=0.85,pt.bg = fillcols ,pch=pchdifffill ,bg="white",lwd=1,bty="n",horiz=T,inset=c(0,-0.07))
  par(xpd=FALSE)
  

  
  graphics::box(which = "plot", lty = "solid")
  
  ##############CORRECTNESS####################
  
  predtest <- as.vector(predict(Model, newdata = Zstar))
  if(length(polyorange)!=0) 
  {
    for (h in 1:length(polyorange))
    {
      pip <- pnt.in.poly(Zstar,cbind(polyorange[[h]]$x,polyorange[[h]]$y))[,3]
      predtest[pip==1] <- NA
    }
  }
  greytemp <- predtest  
  if(length(polygrey)!=0)
  {
    for (h in 1:length(polygrey))
    {
      greypoints <- pnt.in.poly(Zstar,cbind(polygrey[[h]]$x,polygrey[[h]]$y))[,3]
      greytemp[greypoints==1] <- "grey"
    }
  }  
  predtest[greytemp!="grey"] <- NA
  predtest[predtest=="p1"] <- 1
  predtest[predtest=="p2"] <- 2
  predtest[predtest=="p3"] <- 3
  predtest[predtest=="p4"] <- 4
  predtest[predtest=="p5"] <- 5
  predtest<- as.integer(predtest)
  Correct <- sum(datatest[,1]==predtest, na.rm = TRUE) / length(predtest)
  Incorrect <- sum(!datatest[,1]==predtest, na.rm = TRUE)/ length(predtest)
  NAclass <- sum(is.na(predtest))/ length(predtest)
  ValdateV <- data.frame(Alpha = alphin, Type = type, K = kin, Correct = Correct, Incorrect = Incorrect, NAclass = NAclass)
  finalout <- cbind(datatest,predtest)
  colnames(finalout)[dim(finalout)[2]] <- "Predicted"
  ##############CORRECTNESS####################
  
  rm(bipl,   Zbase,Zstar,datatrain,Grid, marker.mat, Pred15,traindataIND, alphsim, amberpoints, ax.col, axis.name, bagcords_Hull, #bagcords_W,
     bagcords_Wnew, ev, expin, gradient, greypoints, greytemp, h, hullmult, i, j, maxrange, Model, nbp, 
     nospots, nolines, plotbags, plotcont, plotinner, plotouter, polygrey, polyorange, Pred, PredA, PredB, predGRID, r.names, Rangex1, Rangex2, 
     std.markers, tick.labels, traindataINDColor, uit, usr, x.invals, x.labvals, x.vals, y.invals, y.labvals, y.vals)
  
  
  
  return(list(ValdateV = ValdateV, tempoutput = predtest, datatestout = finalout))
}
