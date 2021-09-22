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
#' #' # Simulated data via rnorm function
#' df<-as.data.frame(matrix(rnorm(40*20, mean=0, sd=1),40,20))
#' set.seed(1234)
#' df<-as.data.frame(lapply(df, function(cc) cc[sample(c(TRUE, NA),
#'                                                   prob = c(0.85, 0.15),
#'                                                    size = length(cc),
#'                                                      replace = TRUE) ]))
#' df2 <- NA.rm(df, 8, direction="columns")
#' head(df[,1:5]) # before
#' head(df2[,1:5]) # after
#' # Variable V2 removed
#' @export
NA.rm <- function(df, n=0, direction="rows") {
  if(direction=="columns"){
    df[,colSums(is.na(df)) <= n]
  } else if(direction=="rows"){
    df[rowSums(is.na(df)) <= n,]
  } else cat("Error in direction ! You must choose either rows either columns")
}
