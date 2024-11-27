#' rbs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_rbs_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("rbs_year_selector")),
    withSpinner(plotOutput(ns("rbs_plot")))
  )
}
    
#' rbs Server Functions
#'
#' @noRd 
mod_rbs_server <- function(id, data, map_parameters){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$rbs_year_selector <- renderUI({
      selectInput(ns("rbs_year"), "Relative Benthic State in Year:", choices = as.character(2009:2018), selected = "2018")
    })
    
    output$rbs_plot <- renderPlot({
      req(input$rbs_year)

      data_range <- data[[input$rbs_year]]
      
      ggplot(data = data,aes(fill = data_range, col=data_range))+
        geom_sf(na.rm=T)+
        scale_fill_viridis_c(option="viridis",na.value = NA, name = "RBS",direction = -1)+
        scale_color_viridis_c(option="viridis",na.value = NA, name = "RBS",direction = -1)+
        # new_scale_fill() +
        # geom_raster(aes(x = x, y = y, fill =state),data = rbs_cs,na.rm=T)+
        # scale_fill_viridis_c(option="plasma",na.value = NA, name = "RBS with the IGFS survey",direction = -1)+
        # new_scale_fill() +
        # geom_raster(aes(x = x, y = y, fill =GSA20_RBS),data = rbs_gsa20,na.rm=T)+
        # scale_fill_viridis_c(option="plasma",na.value = NA, name = "RBS in GSA 20",direction = -1)+
        geom_sf(data=land,col=NA,fill="grey")+
        # geom_sf(data=gsa[gsa$fid %in% c(9:12),],col="orange",alpha=0.2,fill=NA)+
        # geom_sf_label(data=gsa_centroids[gsa$fid %in% c(9:12),],aes(label = GSA),col="black")+
        # #geom_vline(xintercept = c(15.7,15.75) )+
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
        coord_sf(xlim=c(map_parameters()$coordslim[1], map_parameters()$coordslim[2]), 
                 ylim=c(map_parameters()$coordslim[3], map_parameters()$coordslim[4]))+
        ylab("Latitude")+
        xlab("Longitude")
    })
  })
}
    
## To be copied in the UI
# mod_rbs_ui("rbs_1")
    
## To be copied in the server
# mod_rbs_server("rbs_1")
