createdata2 <- function(Meansin, varin,portin,corrin,numobsin = 150,seedin= 3)
{
  
  R <-  matrix(c(varin[[1]][1]^2,varin[[1]][1]*varin[[1]][2]*corrin[[1]][1],varin[[1]][1]*varin[[1]][3]*corrin[[1]][2],varin[[1]][1]*varin[[1]][4]*corrin[[1]][3],
                 varin[[1]][2]*varin[[1]][1]*corrin[[1]][1],varin[[1]][2]^2,varin[[1]][2]*varin[[1]][3]*corrin[[1]][4],varin[[1]][2]*varin[[1]][4]*corrin[[1]][5],
                 varin[[1]][3]*varin[[1]][1]*corrin[[1]][2],varin[[1]][3]*varin[[1]][2]*corrin[[1]][4],varin[[1]][3]^2,varin[[1]][3]*varin[[1]][4]*corrin[[1]][6],
                 varin[[1]][4]*varin[[1]][1]*corrin[[1]][3],varin[[1]][4]*varin[[1]][2]*corrin[[1]][5],varin[[1]][4]*varin[[1]][3]*corrin[[1]][6],varin[[1]][4]^2),byrow=T,ncol=4)
  R <- as.matrix(nearPD(R)$mat)
  U <- t(chol(R))
  nvars <- dim(U)[1]
  numobs <- as.numeric(round(numobsin*portin[1]))   #number of observations per class simulated
  set.seed(seedin)
  random.normal <- matrix(rnorm(as.matrix(nvars*numobs),0,1), nrow=nvars, ncol=numobs)
  X <- Meansin[[1]] + U %*% random.normal
  newX <- t(X)
  raw1 <- as.data.frame(newX)
  orig.raw <- as.data.frame(t(random.normal))
  raw1 <- cbind(rep(1,numobs),raw1)
  names(raw1) <- c("response","V1","V2","V3","V4")
  R <-  matrix(c(varin[[2]][1]^2,varin[[2]][1]*varin[[2]][2]*corrin[[2]][1],varin[[2]][1]*varin[[2]][3]*corrin[[2]][2],varin[[2]][1]*varin[[2]][4]*corrin[[2]][3],
                 varin[[2]][2]*varin[[2]][1]*corrin[[2]][1],varin[[2]][2]^2,varin[[2]][2]*varin[[2]][3]*corrin[[2]][4],varin[[2]][2]*varin[[2]][4]*corrin[[2]][5],
                 varin[[2]][3]*varin[[2]][1]*corrin[[2]][2],varin[[2]][3]*varin[[2]][2]*corrin[[2]][4],varin[[2]][3]^2,varin[[2]][3]*varin[[2]][4]*corrin[[2]][6],
                 varin[[2]][4]*varin[[2]][1]*corrin[[2]][3],varin[[2]][4]*varin[[2]][2]*corrin[[2]][5],varin[[2]][4]*varin[[2]][3]*corrin[[2]][6],varin[[2]][4]^2),byrow=T,ncol=4)
  R <- as.matrix(nearPD(R)$mat)
  U <- t(chol(R))
  nvars <- dim(U)[1]
  numobs <- round(numobsin*portin[2]) 
  random.normal <- matrix(rnorm(as.matrix(nvars*numobs),0,1), nrow=nvars, ncol=numobs)
  X <- Meansin[[2]] + U %*% random.normal
  newX <- t(X)
  raw2 <- as.data.frame(newX)
  orig.raw <- as.data.frame(t(random.normal))
  raw2 <- cbind(rep(2,numobs),raw2)
  names(raw2) <- c("response","V1","V2","V3","V4")
  R <-  matrix(c(varin[[3]][1]^2,varin[[3]][1]*varin[[3]][2]*corrin[[3]][1],varin[[3]][1]*varin[[3]][3]*corrin[[3]][2],varin[[3]][1]*varin[[3]][4]*corrin[[3]][3],
                 varin[[3]][2]*varin[[3]][1]*corrin[[3]][1],varin[[3]][2]^2,varin[[3]][2]*varin[[3]][3]*corrin[[3]][4],varin[[3]][2]*varin[[3]][4]*corrin[[3]][5],
                 varin[[3]][3]*varin[[3]][1]*corrin[[3]][2],varin[[3]][3]*varin[[3]][2]*corrin[[3]][4],varin[[3]][3]^2,varin[[3]][3]*varin[[3]][4]*corrin[[3]][6],
                 varin[[3]][4]*varin[[3]][1]*corrin[[3]][3],varin[[3]][4]*varin[[3]][2]*corrin[[3]][5],varin[[3]][4]*varin[[3]][3]*corrin[[3]][6],varin[[3]][4]^2),byrow=T,ncol=4)
  R <- as.matrix(nearPD(R)$mat)
  U <- t(chol(R))
  nvars <- dim(U)[1]
  numobs <- round(numobsin*portin[3]) 
  random.normal <- matrix(rnorm(as.matrix(nvars*numobs),0,1), nrow=nvars, ncol=numobs)
  X <- Meansin[[3]] + U %*% random.normal
  newX <- t(X)
  raw3 = as.data.frame(newX)
  orig.raw <- as.data.frame(t(random.normal))
  raw3 <- cbind(rep(3,numobs),raw3)
  names(raw3) <- c("response","V1","V2","V3","V4")
  rawout <- rbind(raw1,raw2,raw3)
  return(rawout)
}