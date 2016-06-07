#' NormDiff
#'
#' @param Data the data
#' @param BalanceVars the variables to check for balance in
#' @param GroupVar the groups to check for balance in
#' @param TreatVar the treatement we want balanced
#'
#' @return
#' @export
#'
NormDiff<- function(Data,BalanceVars,GroupVar,TreatVar)
{

  ColDex<- which(colnames(Data) %in% BalanceVars)

  FlatDat<- gather(Data,Variable,Value,ColDex)
  FlatDat$Treatment<- as.logical(FlatDat[,TreatVar])
  BalanceCheck<- FlatDat %>%
    group_by_(GroupVar,"Variable") %>%
    summarize(ND=(mean(Value[Treatment==T])-mean(Value[Treatment==F]))
        /sqrt((var(Value[Treatment==T])+var(Value[Treatment==F]))),
        MeanTreat=mean(Value[Treatment==T]),MeanNoTreat=mean(Value[Treatment==F]),
        Pval=t.test(Value[Treatment==T],Value[Treatment==F])$p.value)

  NDPlot<- (ggplot(data=BalanceCheck,aes(x=factor(Variable),y=ND,fill=factor(Block)))+
    geom_bar(position='dodge',stat='identity')
  +scale_fill_brewer(palette='Dark2',name='Block')+
    geom_hline(yintercept=c(-0.25,0.25),size=1.5)+xlab('Variable'))

  return(list(BalanceCheck=BalanceCheck,NDPlot=NDPlot))

}
