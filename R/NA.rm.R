#' Delete samples or variables with predefined number of NA
#'
#' @param df dataframe
#' @param n maximum number of NA (if 0 --> all NAs)
#' @param direction in which direction the deletion will take place ("rows" or "columns")
#'
#' @return dataframe after sorting and deleting NA
#'
#'
#' @examples
#' data(simulate_data1)
#' # Simulated data via rnorm function
#' df2 <- NA.rm(simulate_data1, 2, direction="rows")
#' head(simulate_data1[,1:5]) # before
#' head(df2[,1:5]) # after
#' # Sample 5 removed
#' @export
NA.rm <- function(df, n=0, direction="rows") {
  if(direction=="columns"){
    df[,colSums(is.na(df)) <= n]
  } else if(direction=="rows"){
    df[rowSums(is.na(df)) <= n,]
  } else cat("Error in direction ! You must choose either rows either columns")
}
