#' Clustered VCOV matrix
#'
#' \code{ClusteredVCOV} produced clusted VCOV matrix
#'

ClusteredVCOV<-function(model,data,cluster){
#   library(sandwich, quietly = TRUE)
#   library(lmtest, quietly = TRUE)
  #calculate degree of freedom adjustment

  Dropped<- as.numeric(model$na.action)

  Rows<- 1:dim(data)[1]

  Used<- data[!(Rows %in% Dropped),]

  ClusterId<- Used[,cluster]

  M <- length(unique(ClusterId))
  N <- length(ClusterId)
  K <- model$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))

  #calculate the uj's
  uj  <- apply(estfun(model),2, function(x) tapply(x, ClusterId, sum))

  #use sandwich to get the var-covar matrix
  vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
  return(vcovCL)
}