#' litter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinycssloaders withSpinner
#' @import ggplot2
mod_litter_ui <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(plotOutput(ns("litter_plot")))
  )
}
    
#' litter Server Functions
#'
#' @noRd 
mod_litter_server <- function(id, data, map_parameters){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$litter_plot <- renderPlot({
      
      nameFilllit <- bquote(
        atop(
          Predicted ~ fisheries ~ related,
          litter ~ (Numbers/km^2)
        )
      )
      
      ggplot()+
        geom_tile(aes(x = lon, y = lat, fill =noperkm),data = data,na.rm=T)+
        scale_fill_viridis_c(option="viridis",na.value = NA, name = nameFilllit,direction = -1,trans = "sqrt")+
        #new_scale_fill() +
        # geom_raster(aes(x = x, y = y, fill =RBS_2017_2021_WesternIonianSea_GSA19),data = rbs_gsa19,na.rm=T)+
        # scale_fill_viridis_c(option="magma",na.value = NA, name = "RBSin GSA 19",direction = -1)+
        # new_scale_fill() +
        # geom_raster(aes(x = x, y = y, fill =T4_5_trawlslitter_GSA20),data = overlapprob_gsa20,na.rm=T)+
        # scale_fill_viridis_c(option="plasma",na.value = NA, name = nameFillgsa20,direction = -1,trans = "sqrt",limits=c(0,1))+
        geom_sf(data=land,col=NA,fill="grey")+
        #geom_sf(data=gsa[gsa$fid %in% c(9:12),],col="orange",alpha=0.2,fill=NA)+
        #geom_sf_label(data=gsa_centroids[gsa$fid %in% c(9:12),],aes(label = GSA),col="black")+
        #geom_vline(xintercept = c(15.7,15.75) )+
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
        guides(colour = guide_legend(nrow = 3))+
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
# mod_litter_ui("litter_1")
    
## To be copied in the server
# mod_litter_server("litter_1")
