#' wp4 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_wp4_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    uiOutput(ns("wp4_ui"))
    
    
  )
}
    
#' wp4 Server Functions
#'
#' @noRd 
mod_wp4_server <- function(id, ecoregion){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    data <- reactive({
      switch(ecoregion,
             "Baltic" = NULL, 
             "Celtic Seas" = NULL, 
             "Greater North Sea" = WP4_NS, 
             "Mediterranean" = NULL)
    })
    
    output$wp4_ui <- renderUI({
      req(ecoregion)
      tabsetPanel(
        tabPanel("RBS",
                 mod_rbs_ui(ns("rbs_1"))),
        tabPanel("Litter",
                 mod_litter_ui(ns("litter_1"))),
        tabPanel("Bycatch",
                 mod_bycatch_ui(ns("bycatch_1"))),
        if(ecoregion %in% c("Greater North Sea", "Western Waters")){
          tabPanel("Ecosystem Depletion Risk",
                   mod_ecosystem_risk_ui(ns("ecosystem_risk_1")))
        }
      )
    })
    
    map_parameters <- reactive({
      req(!is.null(data()))
      data()$map_parameters
    })
    
    mod_bycatch_server("bycatch_1", data()$bycatch, map_parameters)
    
    mod_litter_server("litter_1", data()$litter, map_parameters)
    
    mod_rbs_server("rbs_1", data()$rbs, map_parameters)
    
    mod_ecosystem_risk_server("ecosystem_risk_1", data()$ecosystem)
    
  })
}
    
## To be copied in the UI
# mod_wp4_ui("wp4_1")
    
## To be copied in the server
# mod_wp4_server("wp4_1")
