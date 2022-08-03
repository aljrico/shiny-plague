diseaseShopCard <- function(id, category, cost, lethality, infectiousness, visibility, state, disabled = TRUE) {
  disabled <- if(disabled)("disabled") else ""
  tagList(
    div(id = id, class = c("shop-card-container", state),
        div(id = paste0(id, "-category"), category, class = "shop-card-category"),
        hr(class = "shop-card-divider"), # replace this with a border 
        div(class = "shop-card-body",
            div(id = paste0(id, "-attributes"), class = "shop-card-attribute",
                tags$ul(class = "shop-card-attribute-ul",
                  tags$li(
                    class = "shop-card-attribute-li",
                    icon("skull", class = "shop-card-attribute-icon"),
                    p(lethality, class = "shop-card-attribute-category")
                  ),
                  tags$li(
                    class = "shop-card-attribute-li",
                    icon("head-side-cough", class = "shop-card-attribute-icon"),
                    p(infectiousness,  class = "shop-card-attribute-category")
                  ),
                  tags$li(
                    class = "shop-card-attribute-li",
                    icon("eye",  class = "shop-card-attribute-icon"),
                    p(visibility,  class = "shop-card-attribute-category")
                  ),
                )
                ),
            # div(id = paste0(id, "-attributes"), class = "shop-card-attribute",
            #     tags$ul(
            #       tags$li(paste0("lethality: ",lethality )),
            #       tags$li(paste0("infectiousness: ",infectiousness )),
            #       tags$li(paste0("visibility: ",visibility ))
            #     )
            # )
        ),
        div(
          class = "shop-card-footer",
          div(class = "footer-left",
              span(class= "shop-card-cost", icon("dna"), cost)
          ),
          div(class = "footer-right",
              actionButton(
                inputId = paste0(id, "_buy"),
                label =  "Buy",
                class = disabled)
          )
        )
    )
  )
}