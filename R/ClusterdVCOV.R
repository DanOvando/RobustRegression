#' Clustered VCOV matrix
#'
#' \code{ClusteredVCOV} produced clusted VCOV matrix
#' @param model the model object passed
#' @param dat the raw data used in the model
#' @param cluster a character designating the variable to cluster errors by
#' @export

ClusteredVCOV<-function(model,dat,cluster){

  #calculate degree of freedom adjustment

  Dropped<- as.numeric(model$na.action)

  Rows<- 1:dim(dat)[1]

  Used<- dat[!(Rows %in% Dropped),]

  ClusterId<- Used[,cluster]

  ClusterId<- as.matrix(select_(Used,cluster))

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
