
#' A function to make a boxplot of payments by DRG code.
#'
#' @param payment  Choose average medicare payments,the average total payment, or the average covered charges as the y axis for the plot
#' @param data a dataframe that we want to analysis (DRG)
#'
#' @return output a boxplot with the average payment you choose
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @importFrom tidyr separate
#'
#' @examples boxplot_DRG(DRG,'Average.Medicare.Payments')

# Make a boxplot
boxplot_DRG <- function(data,payment){

  # Remove the "." of the variable
  payments <- gsub("\\.", " ", payment)

  # Separate the column with DRG code and DRG-definition
  DRG_plot <- data %>%
    separate(DRG.Definition, c("DRG_code", "DRG_definition"), " - ")

  # Make a boxplot
  ggplot(DRG_plot, aes(x=get(payment),y=as.factor(DRG_code)))+
    geom_boxplot(outlier.size = 0.3)+
    ggtitle(paste("The boxplot of", payments))+ # Add a title to the graph
    labs(x = payments, y = 'DRG code') # Add x&y-axis label to the graph
}

