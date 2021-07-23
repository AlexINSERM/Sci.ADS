#' Incremental AUC
#'
#' This function allows to calculate the incremental AUC (IAUC) per sample
#' for an ITT (Insulin Tolerance Test) or a GTT (Glucose Tolerance Test).
#'
#' @importFrom graphics boxplot par
#' @param df dataframe (measure times in minutes in columns, samples in rows)
#' @param graph if TRUE, plot a boxplot, Default: TRUE
#' @param grp sample grouping column
#' @return the function returns a dataframe with the AUC per sample and a boxplot (if graph=T)

#'
#' @seealso Brouns et al. 2005 (Nutrition Research Reviews)
#' @seealso Wolever et al. 2004 (Diabetes Care)
#' @examples
#' data(OGTT)
#' # Artificial data from an OGTT experiment in KO and WT animals
#' IncrAUC(OGTT, lineage)
#' @export

IncrAUC <- function(df, grp, graph=T){

  l<-which(colnames(df)==deparse(substitute(grp)))
  status<-as.factor(as.character(df[,l]))
  df2<-df[,-l]
  nb<-dim(df2)[2] # définition du nombre de temps de mesure
  nbS<-dim(df2)[1] # définition du nombre d'échantillons
  Val_AUC<-matrix(nrow = nbS, ncol=nb-1) # tableau final vide

  # variables de temps
  temps<-names(df2)
  temps_2<-sub(".", "", temps)
  temps_2<-as.numeric(temps_2)

  for(i in 1:nb-1)
    if(is.numeric(df2[,i])==T) # on ne prend que les variables quantitatives
    {
      for(j in 1:nbS){
        Val_AUC[j,i] <- c(((df2[j,i]+df2[j,i+1])/2)*(temps_2[i+1]-temps_2[i]))
      }
    }

  sum_auc<-rowSums(Val_AUC)

  res<- data.frame(sample=rownames(df),status=status,
                   AUC=as.numeric(sum_auc))

  if(graph==T){
    par(las=1, bty="l")
    colBox<-c("white", "grey87", "grey67", "grey47", "grey27", "grey7", "black")
    boxplot(data=res, AUC/1000~status , col=colBox[1:nlevels(status)],
            xlab="", ylab="AUC (x 10³)")
  }
  return(res) # Renvoi des résultats vers un objet
}
# *****************************************************************
