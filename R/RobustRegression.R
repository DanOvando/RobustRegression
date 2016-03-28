#' Runs robust regression
#'
#' \code{RobustRegression} runs a robust regression with
#' heteroskedastic robust standard errors
#' @param model a linear regression object of the form lm(a ~ b)
#' @param dat the data frame object used in the linear regression model
#' @param cluster_var a character indicating variables to cluster errors by
#' @return A list object with components: model - the regression object, TidyModel,
#'  a cleaned up version of the model, GlanceModel - tidy summary of the model,
#'  AugModel -  the original data with some augmented things
#' @example
#' data('iris')
#' regression <-  RobustRegression(model = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Species, data = iris),
#' dat = iris, cluster_var = 'Species')

RobustRegression<- function(model,dat,cluster_var = 'None')
{

  model$VCOV<- vcovHC(model,type='HC1')

  if (cluster_var !='None')
  {
    model$VCOV<- ClusteredVCOV(model,dat = dat, cluster = cluster_var)
  }

  SEs<- data.frame(t(sqrt(diag(model$VCOV))),stringsAsFactors=F) %>% gather('variable','RobustSE')

  SEs$variable<- as.character(SEs$variable)

  SEs$variable[SEs$variable=='X.Intercept.']<- '(Intercept)'

  model$RobustSEs<- SEs

  RobustTest<- (coeftest(model,vcov.=model$VCOV))

  StatNames<- colnames(RobustTest)

  VarNames<- rownames(RobustTest)

  RobustMat<- as.data.frame(matrix(NA,nrow=length(VarNames),ncol=2))

  colnames(RobustMat)<- c('variable','RobustPvalue')

  for (i in 1:length(VarNames))
  {
    RobustMat[i,]<- data.frame(as.character(VarNames[i]),RobustTest[i,'Pr(>|t|)'],stringsAsFactors=F)
  }
  TidyModel<- tidy(model) %>%
    dplyr::rename(variable=term) %>%
    left_join(SEs,by='variable') %>%
    left_join(RobustMat,by='variable')

  AugModel<- augment(model)

  GlanceModel<- glance(model)

  TidyModel$variable<- as.factor(TidyModel$variable)

  TidyModel$variable <- reorder(TidyModel$variable, TidyModel$RobustPval)

  TidyModel$ShortPval<- pmin(TidyModel$RobustPval,0.2)

  RegPlot<- (ggplot(data=TidyModel,aes(x=variable,y=estimate,fill=ShortPval))+
               geom_bar(position='dodge',stat='identity',color='black')+
               scale_fill_gradient2(high='black',mid='gray99',low='red',midpoint=0.1,
                                    breaks=c(0.05,0.1,0.15,0.2),labels=c('0.05','0.10','0.15','>0.20')
                                    ,name='P-Value',guide=guide_colorbar(reverse=T))
             +theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9))+
               geom_errorbar(mapping=aes(ymin=estimate-1.96*RobustSE,ymax=estimate+1.96*RobustSE))+
               xlab('Variable')+
               ylab(paste('Marginal Effect on ',names(model$model)[1],sep='')))

  TidyModel$ShortPval<- NULL

  TCrit<-(qt(c(0.025,0.975),df=model$df.residual)[2])

  TidyModel$LCI95<- TidyModel$estimate-TCrit*TidyModel$RobustSE

  TidyModel$UCI95<- TidyModel$estimate+TCrit*TidyModel$RobustSE

  return(list(model=model,TidyModel=TidyModel,AugModel=AugModel,GlanceModel=GlanceModel,RegPlot=RegPlot))

}
