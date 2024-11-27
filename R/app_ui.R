#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage(title = "SEAwise.tool.wp4",
      
      tabPanel("dev",
               selectInput("ecoregion_selector", "Select Case Study Region", choices = c("Baltic", "Celtic Seas", "Greater North Sea", "Mediterranean"))
               ),
      tabPanel("WP4",
               mod_wp4_ui("wp4_1")
      )
  )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SEAwise.tool.wp6"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
