#' Generate labels for Tukey test analyses
#'
#' For a proper example, see http://stackoverflow.com/questions/18771516/is-there-a-function-to-add-aov-post-hoc-testing-results-to-ggplot2-boxplot
#'
#' @param my.data dataframe from which anova and Tukey tests were performed
#' @param HSD Tukey test
#' @param flev level over which to generate labels
#' @param vspace vertical spacing above the boxplot
#'
#' @return dataframe with labels for each level, suitable for using as geom_text in ggplot2 plots
#' @export
#'
GenerateLabelDf <- function(my.data,HSD, flev, vspace = 0.5){
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- HSD[[flev]][,4]
  Tukey.labels <- multcompView::multcompLetters(Tukey.levels)['Letters']
  plot.labels <- names(Tukey.labels[['Letters']])
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  # upper quantile and label placement
  boxplot.df <- plyr::ddply(my.data, flev, function (x) max(fivenum(x$gambin.alpha)) + vspace)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = Tukey.labels[['Letters']],
                            stringsAsFactors = FALSE)
  
  # Merge it with the labels
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  return(labels.df)
}