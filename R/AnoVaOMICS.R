#' Automatic calculation of an ANOVA for OMICS data
#'
#' @param df dataframe (variable in columns, samples in rows)
#' @param grp sample grouping column
#' @param threshold threshold for significance (expressed as a percentage)
#' @param method.Adj method used for multiple comparison adjustment ("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
#'
#' @return dataframe with significant variables
#' @export
#'
#' @examples
#' data(LIHC_pnas)
#' # subset of TCGA data from LIHC
#' res<-AnoVaOMICS(LIHC_pnas, iHCC, threshold=1) # 1% threshold
#' @importFrom stats aov p.adjust
#'
AnoVaOMICS<-function(df, grp, threshold=5, method.Adj="BH"){

  # df est un dataframe avec en colonne les variables (gènes, protéines etc.) et en lignes les échantillons
  # grp est la colonne de regroupement des échantillons (WT, TG, KO etc.)
  # threshold est la limite en pourcentage pour le tri des p-values (par défaut = 5%)
  # method.Adj est la méthode de correction utilisé pour des multiples comparaisons (par défaut = BH; Benjamini-Hochberg)
  # autre choix "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"

  nb<-dim(df)[2] # définition du nombre de variables potentielles
  threshold<-threshold/100 # conversion en valeur de la p-value
  output<-matrix(nrow = nb, ncol=2) # Création d'un df vide pour l'écriture des résultats
  l<-which(colnames(df)==deparse(substitute(grp))) # deparse(subsitute(grp)) sert à insérer des guillemets pour trouver le numéro de la
  # colonne de regroupement des échantillons (c'est bizarre comme ça mais c'est le seul moyen que j'ai trouvé)

  # Démarrage de la boucle for
  for(i in 1:nb)
    if(is.numeric(df[,i])==T) # on ne prend que les variables quantitatives
    {
      pvalueAOV<-((summary(aov(data=df, df[,i]~df[,l])))[[1]][["Pr(>F)"]])[1] # Extraction de la p-value
      output[i,] <- cbind(names(df[i]),round(pvalueAOV, digits=8)) # Incrémentation à chaque variable
    }
  # fin de la boucle for

  output<-as.data.frame(output) # Conversion en df
  names(output)<-c("GS", "p_value") # renommage des colonnes
  output$p_value<-sapply(output$p_value, function(x) as.numeric(as.character(x))) # transformation en numérique de la colonne p-value
  output$adj_p_value<-p.adjust(output$p_value, n=nb, method = method.Adj) # Calcul de la p-value ajustée
  output2 <- subset(output, adj_p_value<threshold) # tri des résultats en fonction de la p-value souhaitée
  output2$p_value<-format(output2$p_value, digits=3, scientific=T) # Valeurs après la virgule et notation scientifique
  output2$adj_p_value<-format(output2$adj_p_value, digits=4, scientific=F)

  # Définition de la sortie écran des résultats
  print(output2)
  output2[,1]<-factor(output2[,1])
  correction<-ifelse(method.Adj=="none", "without correction", paste("with the", method.Adj, "correction method"))
  cat("\n There are", dim(output2)[1], "significant variables at p <", threshold, correction)

  # Renvoi des résultats vers un objet
  return(output2)

  # FIN DE LA FONCTION
}
# **********************************************************************************************************************************

