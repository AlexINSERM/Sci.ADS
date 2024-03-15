#' Automatic calculation of a Wilcoxon test for OMICS data
#'
#' @param df dataframe (variable in columns, samples in rows)
#' @param groupVar sample grouping column
#' @param threshold threshold for significance (expressed as a percentage)
#' @param method.Adj method used for multiple comparison adjustment ("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
#'
#' @return dataframe with significant variables
#' @export
#'
#' @examples
#' data(LIHC_pnas)
#' # subset of TCGA data from LIHC
#' LIHC_pnas <- subset(LIHC_pnas, iHCC != "iHCC3")
#' res<-wilcoxBatch(LIHC_pnas, iHCC, threshold=1) # 1% threshold
#' @importFrom stats p.adjust
#'

wilcoxBatch <- function(df, groupVar, threshold = 5, method.Adj = "BH") {
  # Arguments preparation
  groupVar<- deparse(substitute(groupVar))
  threshold <- threshold / 100
  # Extract the grouping variable and the other variables
  group <- df[[groupVar]]
  data <- df[, !(names(df) %in% groupVar)]

  # Perform the Wilcoxon tests
  tests <- lapply(data, function(x) wilcox.test(x ~ group))

  # Extract the p-values and variable names
  pvalues <- sapply(tests, function(t) t$p.value)
  variables <- names(data)

  # Adjust the p-values for multiple comparisons
  n <- length(variables)
  adjustedPvalues <- p.adjust(pvalues, method = method.Adj)

  # Create a data frame of the results
  results <- data.frame(Variable = variables,
                        P.Value = pvalues,
                        Adjusted.P.Value = adjustedPvalues)

  # Filter the results to only include significant variables
  results <- results[results$Adjusted.P.Value < threshold, ]

  # Format the p-values
  results$P.Value <- format(results$P.Value, digits = 3, scientific = TRUE)
  results$Adjusted.P.Value <- format(results$Adjusted.P.Value, digits = 3, scientific = TRUE)

  # Sort the results by adjusted p-value
  results <- results[order(as.numeric(results$Adjusted.P.Value)), ]

  return(results)
}
