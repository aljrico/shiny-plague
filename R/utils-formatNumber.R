#' Format Numbers
#'
#'
#' @param x number to be formatted
#' @param accuracy accuracy parameter
formatNumbers <- function(x, accuracy = 1){
  scales::label_number_si(accuracy = accuracy)(x)
}