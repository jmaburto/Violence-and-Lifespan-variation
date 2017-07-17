
my_reshape.function <- function(i=names(x)[1],DecompIn=x){
  
  Z        <- DecompIn[[i]]
  Z.names  <- names(Z)
  ZZ        <- lapply(Z.names, function(ii,Z,i){
    Z2      <- Z[[as.character(ii)]]
    XX      <- cbind(state=as.integer(i),year=as.integer(ii),age=0:109,Z2)
    XX
  }, Z = Z,i=i)
  # now stick it together
  D        <- as.data.frame(do.call(rbind, ZZ))
  D        <- data.table(D)
  #DT       <- as.data.table(melt(D, 
  #                               id.vars = list("state","year","age"),
  #                               variable.name = "Cause"))
  D
}




mysmoothing <- function(MDx = MDx, nage =  nage, ages= ages, range.smooth = c(10^0 , 10^8)){
  Cause          <- MDx$Cause.ind
  if (Cause[1] == 4) {Dxs <- MDx$Dx} else {
  EX             <- MDx$Pop
  W     		     <- EX*0
  W[EX != 0] 	   <- 1
  DX             <- MDx$Dx
  fit            <- Mort1Dsmooth(x = ages[nage:length(ages)], y = DX[nage:length(ages)],offset = log(EX[nage:length(ages)]), 
                                 w = W[nage:length(ages)],control = list(RANGE=range.smooth))
  Dxs            <- DX
  Dxs[nage:length(ages)] <- fit$fitted.values
#Dxs            <- c(DX[1:(nage-1)],fit$fitted.values,DX[(length(ages)+1):length(DX)])
  }
  return(Dxs)
}

mysmooth.warning <- function(MDx = .SD, nage =  nage, ages= ages,...){
  out <- tryCatch(
    {
      mysmoothing(MDx = MDx, nage =  nage, ages= ages, range.smooth = c(10^0 , 10^8))
    },
    error = function(e){
      message('Not smoothing this one')
    }
  )
  if (is.null(out)) {return(MDx$Dx)} else {return(out)}
}

mysmooth.DT <- function(DT = Data_Counts, nage = 3, ages,...){
  DT.melt           <- melt.data.table(data = DT, id.vars = 1:5, variable.name = 'Cause',measure.vars = 6:20,value.name = 'Dx',fill = 0, drop = F)
  DT.melt$Cause     <- as.integer(as.character(DT.melt$Cause))
  DT.melt$Cause.ind <- DT.melt$Cause
  DT.smooth         <- DT.melt[,list(age = 0:109, Dxs = mysmooth.warning(MDx = .SD, nage =  nage, ages= ages)), by = list(year,sex,state,Cause)]
  return(DT.smooth)
}

#Decomposition function
mydecomp <- function (func, rates1, rates2, N, ...) {
  y1 <- func(rates1, ...)
  y2 <- func(rates2, ...)
  d <- rates2 - rates1
  n <- length(rates1)
  delta <- d/N
  x <- rates1 + d * matrix(rep(0.5:(N - 0.5)/N, length(rates1)), 
                           byrow = TRUE, ncol = N)
  cc <- matrix(0, nrow = n, ncol = N)
  for (j in 1:N) {
    for (i in 1:n) {
      z <- rep(0, n)
      z[i] <- delta[i]/2
      cc[i, j] <- func((x[, j] + z), ...) - func((x[, j] - 
                                                    z), ...)
    }
  }
  return(rowSums(cc))
}

#Some lifetable functions from LTuniform
AKm02a0 <- function(m0, sex = "m"){
  sex <- rep(sex, length(m0))
  ifelse(sex == "m", 
         ifelse(m0 < .0230, {0.14929 - 1.99545 * m0},
                ifelse(m0 < 0.08307, {0.02832 + 3.26201 * m0},.29915)),
         # f
         ifelse(m0 < 0.01724, {0.14903 - 2.05527 * m0},
                ifelse(m0 < 0.06891, {0.04667 + 3.88089 * m0}, 0.31411))
  )
}

LifeTable <- function(mx,sex = "f"){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]
  if (ax[i.openage]==Inf){ax[i.openage] <- .5}
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  dx[i.openage] <-0
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  
  v        <- (ax*c(ex[-1L],0) + (1-ax)*ex)
  v[length(ex)] <- ex[length(ex)]
  v <- dx*v
  e.dagger <- rev(cumsum(v))/lx
  Lifetable       <- cbind(age=0:109,lx=lx,dx=dx,ex=ex,e.dagger=e.dagger)
  return(Lifetable)
  #list(e0=ex[1,],ex=ex,lx=lx,mx=mx)
}

LifeTable.DT <- function(.SD){
  mx <- .SD$mx
  sex <- 'm'
  if (.SD$sex[1] > 1) {sex <- 'f'}
  LT.DT <- LifeTable(mx=mx,sex=sex)
  return(LT.DT)
}

LifeExpectancy <- compiler::cmpfun(function(mx,sex = "f"){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]
  if (ax[i.openage]==Inf){ax[i.openage] <- .5}
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  dx[i.openage] <-0
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  ex[1]
})


edagger.frommx <- function(mx,sex){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]
  if (ax[i.openage]==Inf){ax[i.openage] <- .5}
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  dx[i.openage] <-0
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  
  v        <- (ax*c(ex[-1L],0) + (1-ax)*ex)
  v[length(ex)] <- ex[length(ex)]
  v <- dx*v
  e.dagger <- rev(cumsum(v))/lx
  e.dagger[1]
}

e0frommxc <- function(mxcvec,sex){
  dim(mxcvec) <- c(110,length(mxcvec)/110)
  mx          <- rowSums(mxcvec)
  LifeExpectancy(mx,sex)
}

edaggerfrommxc <- function(mxcvec,sex){
  dim(mxcvec) <- c(110,length(mxcvec)/110)
  mx          <- rowSums(mxcvec)
  edagger.frommx(mx,sex)
}


