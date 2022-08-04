#'
#'
#' 
diseaseShopCard <- function(ns, category, cost, lethality, infectiousness, visibility, state, disabled = TRUE) {
  disabled <- if (disabled) ("disabled") else ""
  column(
    width = 3,
    div(
      id = ns('container'), class = c("shop-card-container", state, disabled),
      h5(id = ns('category'), category, class = "shop-card-category"),
      hr(class = "shop-card-divider"), # replace this with a border
      div(
        class = "shop-card-body",
        div(
          id = ns("attributes"), class = "shop-card-attributes",
          div(
            class = 'shop-card-attribute',
            p(icon("head-side-cough", class = "shop-card-attribute-icon"), "Infectivity: "),
            p(infectiousness, class = "shop-card-attribute-category")
          ),
          div(
            class = 'shop-card-attribute',
            p(icon("skull", class = "shop-card-attribute-icon"), "Lethality: "),
            p(lethality, class = "shop-card-attribute-category")
          ),
          div(
            class = 'shop-card-attribute',
            p(icon("eye", class = "shop-card-attribute-icon"), "Visibility: "),
            p(visibility, class = "shop-card-attribute-category")
          )
        )
      ),
      div(
        class = "shop-card-footer",
        div(
          class = c("footer-left", disabled),
          span(class = "shop-card-cost", icon("dna"), cost)
        ),
        div(
          class = "footer-right",
          buyButton(
            id = ns('buy'),
            disabled = disabled
          )
        )
      )
    )
  )
}


buyButton <- function(id, disabled){
  actionButton(
    inputId = id,
    label = "Buy"
  )
}
