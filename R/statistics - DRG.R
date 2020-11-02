#' A function that can calculate the mean, median and standard deviation of Average Medicare Payments
#'
#' @param df A dataframe
#' @param Fun Choose a function (mean,median,std) to analysis DRG data
#'
#' @return a matrix of DRG code and the related statistics of Average Medicare Payments
#' @export
#'
#' @examples stat(DRG,"mean")
#'

# Make a statistic function
stat <- function(df, Fun){

  # Separate the column with DRG code and DRG-definition
  DRG1 <- df %>%
    separate(DRG.Definition, c("DRG_code", "DRG_definition"), " - ")

  # Statistics for different DRG codes
  m<-aggregate(Average.Medicare.Payments~DRG_code,data = DRG1,mean)
  me<-aggregate(Average.Medicare.Payments~DRG_code,data = DRG1,median)
  s<-aggregate(Average.Medicare.Payments~DRG_code,data = DRG1,sd)

  # Select options for the function
  if (Fun == "mean"){
    return(m)
  } else if (Fun == "median") {
    return(me)
  } else if (Fun == "std"){
    return(sd)
  } else {
    return("Please put correct function name:'mean', 'median', 'std'")
  }
}
