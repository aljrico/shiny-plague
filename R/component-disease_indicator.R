#' Disease indicator progress bar
#' 
#' Create a progress bar to visualise disease indicator 
#'
#' @param id An id used to update the progress bar.
#' @param label Label to be displayed within progress bar
#' @param value Value of the progress bar between 0 and 100
#'
#' @return A progress bar that can be added to a UI definition.
#' @name disease-indicator
#' 
#' @export
#' 
#' @examples
#'if (interactive()) {
#'  
#'  library("shiny")
#'  
#'  ui <- fluidPage(
#'    
#'    fluidRow(
#'      column(
#'        width = 3,
#'        diseaseIndicator("lethality", "lethality",value = 55)
#'      )
#'    )
#'  )
#'  
#'  server <- function(input, output, session) {
#'    new_value <- reactive({
#'      invalidateLater(3000)
#'      sample(1:100, 1)
#'    })
#'    
#'    observeEvent(new_value(),{
#'      updateDiseaseIndicator(session, id = "lethality", value = new_value())
#'    })
#'  }
#'  shinyApp(ui = ui, server = server)
#'}
diseaseIndicator <- function(id, label, value) {
  stopifnot(value >= 0,
            value <= 100)
  
  progress_colour <- .progress_bar_colours(value)
  
  style <- glue::glue(HTML("
    width: {value}% ;
    background-color: {progress_colour};"))
  
  div(id = id, class = "progress",
      div(id = paste0(id, "-value"), class = "progress-bar" ,style = style,
          div(id = paste0(id, "-label"), class= "progress-text", label)
      )
  )
}

#' @param session The 'session' object passed to function given to shinyServer.
#'
#' @export
#' @rdname disease-indicator
updateDiseaseIndicator <- function(session = getDefaultReactiveDomain(), id, value, label = NULL ){
  message <- "update-diseaseIndicator"
  
  colour <- .progress_bar_colours(value)
  
  session$sendCustomMessage(type = message,
                            message = list(id = id, 
                                           value = value, 
                                           label = label,
                                           colour = colour))
}

.progress_bar_colours <- function(value){
  progress_colour <- dplyr::case_when(value < 33 ~ "#478ACC", #blue
                                      value < 66 ~ "#8A9199CC", #grey
                                      TRUE ~ "#E65050") #red
  
}