
my_reshape.function <- function(i,DecompIn, lower = 0, upper = 15){
  Z        <- DecompIn[[i]]
  # and I'm going to append Age and State before
  # rbind.list.ing
  
  Z        <- lapply(1:length(Z), function(ii,Z){
    XX       <- as.data.frame(Z[[ii]])
    XX$State <- ii
    XX$Age   <- 0:109
    XX
  }, Z = Z)
  # now stick it together
  D        <- do.call(rbind.data.frame, Z)
  # select age range
  D        <- subset(D, Age >= lower & Age < upper)
  # toggle to long
  
  DT       <- as.data.table(melt(D, 
                                 id.vars = list("State","Age"),
                                 variable.name = "AMCategory"))
  # replaces aggregate()
  DT       <- DT[, list(Contribution = sum(value)), by = list(State,AMCategory)]
  # add year
  DT$Year  <- 1989 + i
  DT
}

# now apply this function the the crazy list objects containing 
# decomposition results.

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
  mx <- as.matrix(mx)
  #install.packages("/home/tim/git/HMDLifeTables/HMDLifeTables/HMDLifeTables",repos=NULL)
  #require(HMDLifeTables)
  i.openage <- nrow(mx)
  ax        <- mx * 0 + .5                                          # ax = .5, pg 38 MPv5
  
  ax[1, ]   <- AKm02a0(m0 = mx[1, ], sex = sex)
  
  #  if (testa0){
  #    ax[1, ]   <- AKm02a0_direct(m0 = mx[1, ], sex = sex)
  #  }
  # multiplying 2 matrices using '*' does the hadamard product in R (elementwise).
  qx        <- mx / (1 + (1 - ax) * mx)                             # Eq 60 MPv5
  # ---------------------------------------------------------------------------------
  # set open age qx to 1
  qx[i.openage, ]       <- ifelse(is.na(qx[i.openage, ]), NA, 1)
  ax[i.openage, ]       <- 1 / mx[i.openage, ]                   
  # ---------------------------------------------------------------------------------
  # define remaining lifetable columns:
  px 				      <- 1 - qx 																				# Eq 64 MPv5
  px[is.nan(px)]  <- 0 # skips BEL NAs, as these are distinct from NaNs
  # lx needs to be done columnwise over px, argument 2 refers to the margin.
  lx 			        <- apply(px, 2, function(px., RADIX, OPENAGE){ 		# Eq 65 MPv5
    if (all(is.na(px.))) {
      px.
    } else {
      c(RADIX, RADIX * cumprod(px.[1:OPENAGE]))
    }
  }, RADIX = 1, OPENAGE = i.openage - 1
  )
  rownames(lx)    <- 0:(i.openage - 1) # these got thrown off because l0 imputed.
  # NA should only be possible if there was a death with no Exp below age 80- impossible, but just to be sure
  # lx[is.na(lx)]   <- 0 # removed for BEL testing        
  dx 				      <- lx * qx 																				# Eq 66 MPv5
  Lx 				      <- lx - (1 - ax) * dx 														# Eq 67 MPv5
  
  Lx[i.openage, ]	<- lx[i.openage, ] * ax[i.openage, ]
  # we need to do operations on Lx, but taking its NAs to mean 0
  # Lx[is.na(Lx)] 	<- 0 # removed for BEL testing
  # Tx needs to be done columnwise over Lx, argument 2 refers to the column margin.
  Tx 				      <- apply(Lx, 2, function(Lx., i.openage, OPENAGE){
    c(rev(cumsum(rev(Lx.[1:OPENAGE]))),0) + Lx.[i.openage]	# Eq 68 MPv5
  }, OPENAGE = i.openage - 1, i.openage = i.openage
  )
  rownames(Tx)    <- rownames(lx)
  ex 				      <- Tx / lx 	                                      # Eq 69 MPv5
  list(e0=ex[1,],ex=ex,lx=lx,mx=mx)
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
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  v <- (sum(dx[-i.openage]* (ex[-i.openage] + ax[-i.openage]*(ex[-1]-ex[-i.openage]) )) + ex[i.openage])
  v[1]
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

sm.mat.2   <- function(DX, EX, Years = years){
  ages  		  <- 0:109
  W     		  <- EX*0
  W[EX == 0] 	<- 1
  #W[W > 0] 	  <- 0
  mxs         <- DX * 0
  for (j in 1:ncol(mxs)){
    fit   	<- Mort1Dsmooth(
      x = ages, 
      y = DX[,j],
      offset = log(EX[,j]), 
      w = W[,j],
      #control=list(MAX.IT=200),
      method = 3,
      lambda = 1e3
      )
    
    mxsi 	<- exp(fit$logmortality)
    mxs[,j] <- mxsi
  }
  
  mxs <- melt(mxs, varnames=c("age","year"), value.name = "mxs")
  mxs
}

sm.chunk.2 <- function(.SD,i,...){
  DX      <- acast(.SD, age~year, value.var = colnames(National)[i], fill = 0,drop = F)
  EX      <- acast(.SD, age~year, value.var = "Pop", fill = 0,drop=F)
  
  mxs     <- sm.mat.2(DX,EX)
  
  .SD$mxs <- mxs$mxs
  
  # still need to normalize exposure?
  .SD
}

# useful labels
CoD.name.vec <- c('Infectious and respiratory', 'Cancers', 'Circulatory',
                  'Birth conditions', 'Diabetes', 'Other AMS', 'IHD', 'HIV', 
                  'Suicide', 'Lung Cancer', 'Cirrhosis', 'Homicide',
                  'Road traffic accidents', 'Other heart diseases', 'ILL-defined', 'All-NonAm')

CoD.code.vec <- 1:16

CoD.name.vec2 <- c('Amenable','Diabetes','IHD', 'HIV', 
                  'Suicide', 'Lung Cancer', 'Cirrhosis', 'Homicide',
                  'Road traffic accidents', 'Other')

CoD.code.vec2 <- 1:10

state.name.vec <- c("Aguascalientes","Baja California","Baja California Sur","Campeche",
                    "Coahuila","Colima","Chiapas","Chihuahua","Mexico City","Durango",
                    "Guanajuato","Guerrero","Hidalgo","Jalisco","Mexico State","Michoacan",
                    "Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla","Queretaro",
                    "Quintana Roo","San Luis Potosi","Sinaloa","Sonora","Tabasco","Tamaulipas",
                    "Tlaxcala","Veracruz","Yucatan","Zacatecas")

state.code.vec <- 1:32

region.recvec            <- c(2,3,3,1,3,2,1,3,2,3,2,1,2,2,2,
                              2,1,2,3,1,1,2,1,3,3,3,1,3,2,1,1,3)

names(region.recvec)     <- 1:32
