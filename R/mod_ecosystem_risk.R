#' ecosystem_risk UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ecosystem_risk_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("ecosystem_risk"))
  )
}
    
#' ecosystem_risk Server Functions
#'
#' @noRd 
mod_ecosystem_risk_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    output$ecosystem_risk <- renderPlot({

      cmin<-min(data$ratio, na.rm=TRUE)
      cmax<-max(data$ratio, na.rm=TRUE)
      
      p <- ggplot(data, aes(x = ratio, y = risk)) +
        geom_point(size = 1.5, color = "black") +  # Points
        geom_line(color = "black", size = 1) +     # Line
        scale_x_continuous(limits = c(cmin, cmax)) +
        scale_y_continuous(limits = c(0, 1)) +
        theme_classic() +                          # Simplified theme
        theme(
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.title = element_text(face = "bold")
        ) +
        labs(
          x = "Apex predator/fish biomass ratio",
          y = "Ecosystem depletion risk"
        )
      
      # Adding specific points
      p <- p +
        geom_point(aes(x = ratio[1], y = risk[1]), 
                   shape = 1, size = 5, color = "red", stroke = 1.5) +
        geom_point(aes(x = ratio[5], y = risk[5]), 
                   shape = 1, size = 5, color = "blue", stroke = 1.5) +
        geom_point(aes(x = ratio[12], y = risk[12]), 
                   shape = 1, size = 5, color = "black", stroke = 1.5)
      
      # Adding text annotations
      p <- p +
        annotate("text", x = 0.0016, y = 0.01, label = "No fishing", 
                 size = 4, color = "red") +
        annotate("text", x = 0.0004, y = 0.822, label = "8x 2003-2013 fishing", 
                 size = 4, color = "black") +
        annotate("text", x = 0.00125, y = 0.45, label = "2003-2013 fishing", 
                 size = 4, color = "blue", angle = -15)
      
      
      p
    })
  })
}
    
## To be copied in the UI
# mod_ecosystem_risk_ui("ecosystem_risk_1")
    
## To be copied in the server
# mod_ecosystem_risk_server("ecosystem_risk_1")
