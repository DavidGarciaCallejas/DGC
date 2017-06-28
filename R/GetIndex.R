
#' dplyr equivalent to which
#'
#' @param data dataframe
#' @param conditions logical vector
#'
#' @return numeric vector of indices satisfying conditions
#' @export
#'
GetIndex <- function(data,conditions){

  data %>% tibble::rownames_to_column() %>%
  filter(conditions) %>% `[[`("rowname") %>% as.numeric() 

}
