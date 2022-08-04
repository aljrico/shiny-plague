formatNumbers <- function(x){
  scales::label_number_si(accuracy = 1)(x)
}