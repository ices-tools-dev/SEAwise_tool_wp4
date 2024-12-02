#' bycatch UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinycssloaders withSpinner
#' @importFrom dplyr filter
mod_bycatch_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("bycatch_species_selector")),
    uiOutput(ns("bycatch_season_selector")),
    uiOutput(ns("bycatch_gear_selector")),
    
    withSpinner(plotOutput(ns("bycatch_plot"))),
    withSpinner(plotOutput(ns("bycatch_facet_plot")))
  )
}
    
#' bycatch Server Functions
#'
#' @noRd 
mod_bycatch_server <- function(id, data, map_parameters){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
      
    output$bycatch_species_selector <- renderUI({
      selectInput(ns("bycatch_species"), "Select bycatch species", choices = unique(data$species))
    })
    output$bycatch_season_selector <- renderUI({
      selectInput(ns("bycatch_season"), "Select bycatch risk season", choices = unique(data$season))
    })
    output$bycatch_gear_selector <- renderUI({
      selectInput(ns("bycatch_gear"), "Select bycatch gear season", choices = unique(data$gear))
    })
    
    bycatch_data <- reactive({
      req(input$bycatch_species)
      data %>% filter(species == input$bycatch_species)
    })
    
    output$bycatch_plot <- renderPlot({
      req(input$bycatch_species, input$bycatch_gear, input$bycatch_season)
      
      nameFilllit <- bquote(atop(
          Predicted ~ fisheries ~ related,
          litter ~ (Numbers/km^2)
      ))
      
      dat <- filter(bycatch_data(),
                    gear == input$bycatch_gear,
                    season == input$bycatch_season)
      
      ggplot()+
        geom_raster(aes(x = x, y = y, fill =value), data = dat, na.rm=T)+
        scale_fill_viridis_d(name= "Bycatch mortality risk" ,na.value="white",labels=c("Low","Medium","High",""),option ="viridis",drop = FALSE)+
        geom_sf(data=land,col=NA,fill="grey")+
        theme_classic()+
        theme(plot.background=element_blank(),
              panel.background=element_blank(),
              axis.text.y   = element_text(size=16),
              axis.text.x   = element_text(size=16),
              axis.title.y  = element_text(size=16),
              axis.title.x  = element_text(size=16),
              panel.border  = element_rect(colour = "grey", linewidth=.5,fill=NA),
              legend.text   = element_text(size=11),
              legend.title  = element_text(size=11))+
        scale_x_continuous(breaks= map_parameters()$coordxmap)+
        scale_y_continuous(breaks= map_parameters()$coordymap,expand=c(0,0))+
        coord_sf(xlim=c(map_parameters()$coordslim[1], map_parameters()$coordslim[2]), ylim=c(map_parameters()$coordslim[3],map_parameters()$coordslim[4]))+
        ylab("Latitude")+
        xlab("Longitude")
      
    })
    
    output$bycatch_facet_plot <- renderPlot({
      req(bycatch_data())
      
      nameFilllit <- bquote(atop(
        Predicted ~ fisheries ~ related,
        litter ~ (Numbers/km^2)
      ))
      
      ggplot()+
        geom_raster(aes(x = x, y = y, fill =value), data = bycatch_data(), na.rm=T)+
        scale_fill_viridis_d(name= "Bycatch mortality risk" ,na.value="white",labels=c("Low","Medium","High",""),option ="viridis",drop = FALSE)+
        geom_sf(data=land,col=NA,fill="grey")+
        theme_classic()+
        theme(plot.background=element_blank(),
              panel.background=element_blank(),
              axis.text.y   = element_text(size=16),
              axis.text.x   = element_text(size=16),
              axis.title.y  = element_text(size=16),
              axis.title.x  = element_text(size=16),
              panel.border  = element_rect(colour = "grey", linewidth=.5,fill=NA),
              legend.text   = element_text(size=11),
              legend.title  = element_text(size=11))+
        scale_x_continuous(breaks=map_parameters()$coordxmap)+
        scale_y_continuous(breaks=map_parameters()$coordymap,expand=c(0,0))+
        coord_sf(xlim=c(map_parameters()$coordslim[1], map_parameters()$coordslim[2]), ylim=c(map_parameters()$coordslim[3],map_parameters()$coordslim[4]))+
        ylab("Latitude")+
        xlab("Longitude")+
        facet_grid(rows = vars(gear),
                   cols = vars(season))
      
    })
  })
}
    
## To be copied in the UI
# mod_bycatch_ui("bycatch_1")
    
## To be copied in the server
# mod_bycatch_server("bycatch_1")
