#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_ui <- function(id = "map") {
  ns <- NS(id)
  tagList(
    leafletjs,
    leaflet::leafletOutput(outputId = ns("cloropleth"), height = 500)
  )
}

#' cloropleth Server Function
#'
#' @noRd
mod_map_server <- function(id = "map", gameState) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(input$cloropleth_shape_click, {
      cli::cli_alert("click")
      print(input$cloropleth_shape_click$id)
    })
    
    output$cloropleth <-
      leaflet::renderLeaflet({
        data("map_data", envir = environment())
        # Define options
        leaflet_options <- function() {
          leaflet::leafletOptions(
            minZoom = 2,
            controlZoom = FALSE
          )
        }
        
        # Design legend
        legend_hint <- function() {
          tags$p("Click on a specific country for more details.")
        }
        
        # Build Cloropleth
        leaflet::leaflet(map_data, options = leaflet_options()) |>
          leaflet::setMaxBounds(
            lng1 = -120,
            lat1 = -80,
            lng2 = 120,
            lat2 = 90
          ) |>
          # leaflet::addTiles() |>
          leaflet::setView(lat = 20, lng = 10, zoom = 2.2) |>
          leaflet::addControl(
            actionButton(
              inputId = ns("disease_design"),
              label = "Disease Design",
              class = "disease-shop"
            ),
            position = "bottomleft", className = "fieldset {border: 0;}"
          ) |>
          leaflet::addPolygons(
            layerId = ~ISO3,
            group = ~ISO3,
            fill = "white",
            stroke = TRUE,
            smoothFactor = 1,
            fillOpacity = 1,
            color = "#1F2430",
            weight = 1,
            highlightOptions = leaflet::highlightOptions(
              color = "#FAFAFA", opacity = 1, weight = 2, fillOpacity = 1,
              bringToFront = TRUE, sendToBack = TRUE)
          )
      })
    
    observe({
      leaflet::leafletProxy(
        mapId = "cloropleth",
        data = map_data
      ) |>
        update_polygons(gameState()$getMapData())|> 
        leaflet::removeControl('progressIndicators') |> 
        leaflet::addControl(
          position = 'topright',
          class = 'progress-indicators',
          layerId = 'progressIndicators',
          progressIndicators(gameState())
        )
    })
    
    disease_shop_modal_server(ns("shop_modal"), gameState, reactive(input$disease_design))
  })
}


update_polygons <- function(map, map_data) {
  data("global", envir = environment())
  get_quantiles <- function(metric, n = 666) {
    qs <- seq(from = 0, to = 1, by = 1 / n)
    bins <- unique(floor(quantile(log(metric), qs, na.rm = TRUE) |> as.vector()))
    c(bins, Inf)
  }
  
  create_gradient <- function(col1, col2) {
    fn_cols <- grDevices::colorRamp(c(col1, col2), space = "Lab", interpolate = "spline")
    cols <- fn_cols(seq(0, 1, length.out = 10)) / 255
    grDevices::rgb(cols[, 1], cols[, 2], cols[, 3], alpha = 1)
  }
  
  get_cases_array <- function() {
    x <- map_data[["confirmed_cases"]]
    x
  }
  
  get_proportion_array <- function() {
    x <- map_data[["proportion"]]
    x[x == 0] <- NA
    x[is.nan(x)] <- NA
    x[is.infinite(x)] <- NA
    x[!is.numeric(x)] <- NA
    x
  }
  
  mytext <- paste0(
    "<b> Country: </b> ", map_data$NAME, "<br/>",
    "<hr>",
    "<b> Infected: </b> ", prettyNum(map_data[["confirmed_cases"]], big.mark = ","), "<br/>",
    "<b> Deaths: </b> ", prettyNum(map_data[["confirmed_deaths"]], big.mark = ","), "<br/>",
    "<b> Recovered: </b> ", prettyNum(map_data[["confirmed_recovered"]], big.mark = ","), "<br/>",
    "<b> Tests: </b> ", prettyNum(map_data[["total_tests"]], big.mark = ","), "<br/>"
  ) |>
    lapply(htmltools::HTML)
  
  colours <- create_gradient(col1 = global$colours$grey, col2 = global$colours$red)

  probs <- seq(0, 1, length.out = length(get_cases_array()) + 1)

  map_data$proportion <- map_data$confirmed_cases / map_data$POP2005
  my_palette <- leaflet::colorBin(
    colours[-1],
    get_cases_array(),
    na.color = "white",
    bins =  seq(0, 1, length.out = 1e3)
  )
  
  map |>
    setShapeStyle(
      layerId = ~ISO3,
      label = mytext,
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px", "background-color" = "#FAFAFA"),
        textsize = "13px",
        direction = "auto"),
      fillColor = ~ my_palette(get_proportion_array())
    )
}

#  extend leaflet from github ---------------------------------------------
# adapted from https://github.com/rstudio/leaflet/issues/496#issuecomment-650122985

setShapeStyle <- function( map, data = leaflet::getMapData(map), layerId,
                           label = NULL, labelOptions = NULL,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL,
                           options = NULL
){
  label <- unlist(label)
  options <- c(list(layerId = layerId),
               options,
               leaflet::filterNULL(list(stroke = stroke, color = color,
                                        weight = weight, opacity = opacity,
                                        fill = fill, fillColor = fillColor,
                                        fillOpacity = fillOpacity, dashArray = dashArray,
                                        smoothFactor = smoothFactor, noClip = noClip,
                                        label = label
               )))
  # evaluate all options
  options <- leaflet::evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  layerId <- options[[1]]
  style <- options[-1] # drop layer column
  #print(list(style=style))
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style, labelOptions);
}

### JS methods
leafletjs <-  tags$head(
  # add in methods from https://github.com/rstudio/leaflet/pull/598
  tags$script(HTML(
    '
window.LeafletWidget.methods.setStyle = function(category, layerId, style, labelOptions){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
  }

  //convert columnstore to row store
  style = HTMLWidgets.dataframeToD3(style);

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){ // or should this raise an error?
      layer.setStyle(style[i]);
      let label = style[i]["label"];
        if (label !== null) {
          if (labelOptions !== null) {
              layer.closeTooltip()
              layer.bindTooltip(label, labelOptions);
            } else {
              layer.closeTooltip()
              layer.bindTooltip(label);
            }
          }
    }
  });
};
'
  ))
)
