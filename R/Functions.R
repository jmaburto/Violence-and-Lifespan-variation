#fuction to subset objects with less categories, useful for summary tables at the national level

get.data.function2 <-  function(Data2 = DT.Decomp.ex,initial = initial.ind,final = final.ind){
  Data <- cbind(Data2[,1:6], AMS = rowSums(Data2[,c(7,10,8,9,12)]), Diabetes = Data2[,c(11)], IHD = Data2[,c(13)],
                LungCancer = Data2[,c(16)], Cirrhosis = Data2[,c(17)],Homicide = Data2[,c(18)],TAcc = Data2[,c(19)],
                Rest = rowSums(Data2[,c(14,21,15,20)]))
  
  colnames(Data) <- c('Name','Region','Sex','State','year','age1',1:8)
  Data.melt               <- melt(Data, id.vars = 1:6,variable.name = 'Cause',value.name = 'Contribution')
  levels(Data.melt$Cause) <- c('AMS','Diabetes','IHD','Lung Cancer','Cirrhosis',
                               'Homicide','Traffic accidents','Rest')
  Data.melt <- Data.melt[Data.melt$age1 < 100,]
  Labels.age            <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                             '40-44','45-49','50-54','55-59','60-64','65-69',
                             "70-74","75-79","80-84","85-89","90-94","95+")
  Data.melt$Age       <- (cut(Data.melt$age1+1, breaks=c(seq(0,95,5),Inf),labels=Labels.age))
  Data.melt5 <- Data.melt[,list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,year,Cause,Age)]
  
  Data.fig   <- Data.melt5[Data.melt5$year >= initial & Data.melt5$year < final, ]
  Data.fig   <- Data.fig[, list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,Cause,Age)] 
  Data.fig
}

getData.function.g <- function(Data2 = DT.Decomp.ex,state = state.ind,initial = initial.ind,final = final.ind){
  Data <- cbind(Data2[,1:6], AMS = rowSums(Data2[,c(7,10,8,9,12)]), Diabetes = Data2[,c(11)], IHD = Data2[,c(13)],
                LungCancer = Data2[,c(16)], Cirrhosis = Data2[,c(17)],Homicide = Data2[,c(18)],TAcc = Data2[,c(19)],
                Rest = rowSums(Data2[,c(14,21,15,20)]))
  
  colnames(Data) <- c('Name','Region','Sex','State','year','age1',1:8)
  Data.melt               <- melt(Data, id.vars = 1:6,variable.name = 'Cause',value.name = 'Contribution')
  levels(Data.melt$Cause) <- c('AMS','Diabetes','IHD','Lung Cancer','Cirrhosis',
                               'Homicide','Traffic accidents','Rest')
  Data.melt <- Data.melt[Data.melt$age1 < 100,]
  Labels.age            <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                             '40-44','45-49','50-54','55-59','60-64','65-69',
                             "70-74","75-79","80-84","85-89","90-94","95+")
  Data.melt$Age       <- (cut(Data.melt$age1+1, breaks=c(seq(0,95,5),Inf),labels=Labels.age))
  Data.melt5 <- Data.melt[,list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,year,Cause,Age)]
  
  Data.fig   <- Data.melt5[Data.melt5$year >= initial & Data.melt5$year < final, ]
  Data.fig   <- Data.fig[, list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,Cause,Age)] 
  Data.fig   <- Data.fig[Data.fig$Name == state,]
  Data.fig$Contribution <- round(Data.fig$Contribution,2)
  Data.fig
}

#fuction to subset objects with more categories, useful for summary tables at the national level

getData.function <- function(Data = DT.Decomp.ex,state = state.ind,initial = initial.ind,final = final.ind){
  
  colnames(Data) <- c('Name','Region','Sex','State','year','age1',1:14,16)
  Data.melt               <- melt(Data, id.vars = 1:6,variable.name = 'Cause',value.name = 'Contribution')
  levels(Data.melt$Cause) <- c('Infect & respiratory','Cancers','Circulatory','Birth conditions',
                               'Diabetes','Other AMS','IHD','HIV','Suicide','Lung Cancer','Cirrhosis',
                               'Homicide','Traffic accidents','Other HD', 'Rest')
  Labels.age            <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                             '40-44','45-49','50-54','55-59','60-64','65-69',
                             "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105+")
  Data.melt$Age       <- (cut(Data.melt$age1+1, breaks=c(seq(0,105,5),Inf),labels=Labels.age))
  Data.melt5 <- Data.melt[,list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,year,Cause,Age)]
  
  Data.fig   <- Data.melt5[Data.melt5$year >= initial & Data.melt5$year < final, ]
  Data.fig   <- Data.fig[, list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,Cause,Age)]
  Data.fig   <- Data.fig[Data.fig$Name == state,]
  Data.fig$Contribution <- round(Data.fig$Contribution,2)
  Data.fig
}

my.kannisto.fun <- function(mx = mx, x=80:94){
  mx2 <- mx
  fit_kan    <- Kannisto(mx=mx[x+1],x=x)
  mx.new     <- predict.Kannisto(fit_kan,94:109)
  mx2[95:110] <- mx.new
  mx2
}



fun_data_prep <- function(mx, x = 80:94, n_parameters = 2){
  # Format input data
  mx <- as.matrix(mx)
  x  <- as.numeric(x)
  c_names <- if (is.null(ncol(mx)) | ncol(mx) == 1) 'mx' else colnames(mx)
  dimnames(mx) <- list(x, c_names)
  c_no    <- ncol(mx)
  mx <- mx + (mx == 0)*1e-04 # If death rate is 0 we assign a very small value
  # Scale the age vectors in order to obtain meaningful parameter estimates
  x_scaled <- x - min(x)
  # Create storage objects for parametes and fitted mx's
  pars <- matrix(NA, c_no, n_parameters)
  dimnames(pars) <- list(c_names, letters[1:n_parameters] )
  fitted.values <- mx*0
  # Output
  return(list(mx = mx, x = x, x_scaled = x_scaled,
              pars = pars, fitted.values = fitted.values,
              n_parameters = n_parameters))
}

Fun_ux <- function(model){
  switch(model,
         kannisto = function(par, x) {
           with(as.list(par), a*exp(b*x) / (1 + a*exp(b*x)) ) 
         })
}


Kannisto <- function(mx = dta, x = 80:90, parS = NULL){
  all_data <- fun_data_prep(mx, x, n_parameters = 2)
  with(all_data,
       {
         mx <- as.matrix(mx)
         x  <- as.numeric(x)
         if (min(x) < 80 | max(x) > 100) {
           cat('The Kannisto model is usually fitted in the 80-100 age-range\n')
         }
         model_name <- "Kannisto (1992): u(x) = a*exp(b*x) / [1 + a*exp(b*x)]"
         parS_default <- c(a = 0.5, b = 0.13)
         parS <- if (is.null(parS)) parS_default else parS 
         if (is.null(names(parS))) names(parS) <- letters[1:length(parS)]
         # Model ------------------------------------------
         fun_ux <- Fun_ux('kannisto')
         # Find parameters / Optimization -----------------
         fun_resid <- function(par, x, ux) {
           sum(ux*log(fun_ux(par, x)) - fun_ux(par, x), na.rm = TRUE)
         }
         for (i in 1:nrow(pars)) {
           opt_i <- optim(par = parS, fn = fun_resid, x = x_scaled,
                          ux = mx[, i], method = 'L-BFGS-B',
                          lower = 1e-15, control = list(fnscale = -1))
           pars[i, ] <- opt_i$par
         }
         # Compute death rates ---------------------------
         for (i in 1:nrow(pars)) fitted.values[, i] = fun_ux(pars[i, ], x_scaled)
         residuals <- mx - fitted.values
         
         # Retun results ----------------------------------
         out <- structure(class = 'Kannisto', 
                          list(x = x, mx.input = mx, fitted.values = fitted.values,
                               residuals = residuals, model_name = model_name, 
                               coefficients = pars))
         #out$call <- match.call(definition = Kannisto)
         return(out)
       })
}

predict.Kannisto <- function(object, newdata=NULL, ...) {
  if (is.null(newdata)) { pred.values <- fitted(object)
  }else{
    x <- newdata
    x_scaled <- x - min(object$x) 
    pars <- coef(object)
    pred.values <- matrix(NA, nrow = length(x), ncol = nrow(pars))
    dimnames(pred.values) <- list(x, rownames(pars))
    fun_ux <- Fun_ux('kannisto')
    for (i in 1:nrow(pars)) {pred.values[,i] = fun_ux(pars[i,], x_scaled)}
  }
  return(pred.values)
}


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
  x         <- 0:109
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  if (mx[i.openage] < 0.5 | is.na(mx[i.openage])) mx[i.openage] = mx[i.openage - 1]*1.1 
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  ax[i.openage] <- if (mx[i.openage] == 0) 0.5 else 1/mx[i.openage]
  
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  #qx[x >= 95 & mx == 0] <- 1
  
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  #dx[i.openage] <-0
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- dx[i.openage ] * ax[i.openage ]
  Lx[is.na(Lx)] <- 0
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  ex[is.na(ex)] <- 0
  ex[i.openage] <- if (ex[OPENAGE] == 0) 0 else ax[i.openage]
  
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
  if (mx[i.openage] < 0.5 | is.na(mx[i.openage])) mx[i.openage] = mx[i.openage - 1]*1.1 
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  ax[i.openage] <- if (mx[i.openage] == 0) 0.5 else 1/mx[i.openage]
  
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  
  
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  #dx[i.openage] <-0
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- dx[i.openage ] * ax[i.openage ]
  Lx[is.na(Lx)] <- 0
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  ex[is.na(ex)] <- 0
  ex[i.openage] <- if (ex[OPENAGE] == 0) 0 else ax[i.openage]
  ex[1]
})


edagger.frommx <- function(mx,sex){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  if (mx[i.openage] < 0.5 | is.na(mx[i.openage])) mx[i.openage] = mx[i.openage - 1]*1.1 
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  ax[i.openage] <- if (mx[i.openage] == 0) 0.5 else 1/mx[i.openage]
  
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  
  
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  #dx[i.openage] <-0
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- dx[i.openage ] * ax[i.openage ]
  Lx[is.na(Lx)] <- 0
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  ex[is.na(ex)] <- 0
  ex[i.openage] <- if (ex[OPENAGE] == 0) 0 else ax[i.openage]
  
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


