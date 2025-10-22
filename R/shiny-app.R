#' Launch the interactive map with stratigraphy plots
#'
#' This function launches a Shiny app that displays an interactive map locations
#' with stratigraphic data. If you click on a station it will generate a stratigraphic
#' plot with the plotting function of your choice (default is `ggstrat_samples()`).
#' You can also adjust the height of the plot using the slider below the map.
#'
#' @param df A data frame containing stratigraphic data.
#' @param plot_fun A function that generates a stratigraphic plot.
#'   Defaults to [ggstrat_samples()].
#'
#' @return A Shiny app object.
#' @export
#'
#' @examples
#' if (interactive()) {
#' # Use your default plotting function
#' run_ggstrat_app(example_data_strat)
#'
#' # Or swap in a custom plotting function
#' run_ggstrat_app(example_data_strat, plot_fun = ggstrat_column)
#' }
run_ggstrat_app <- function(df,
                            plot_fun = ggstrat_samples) {
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .slider-row {
          text-align: right;
          padding-top: 10px;
        }
      "))
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        leaflet::leafletOutput("stationmap", height = "600px"),
        shiny::tags$div(
          style = "display: flex; justify-content: flex-end; padding-top: 10px;",
          shiny::sliderInput("plotHeight", "Adjust Plot Height",
                             min = 400, max = 2000, value = 800, step = 50,
                             width = "33%"
          )
        )
      ),
      shiny::column(
        width = 4,
        shiny::uiOutput("plotUI")
      )
    )
  )

  server <- function(input, output, session) {
    # Map of stations
    output$stationmap <- leaflet::renderLeaflet({
      stations <- df |>
        dplyr::select(dplyr::all_of(c("stratsection_name", "Latdd", "Longdd"))) |>
        dplyr::distinct()

      leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenTopoMap") |>
        leaflet::addCircleMarkers(
          lat      = stations[["Latdd"]],
          lng      = stations[["Longdd"]],
          popup    = stations[["stratsection_name"]],
          layerId  = stations[["stratsection_name"]],
          label    = stations[["stratsection_name"]],
          color = "black", fillColor = "blue", radius = 5, stroke = TRUE,
          opacity = 0.5, fillOpacity = 0.5
        )
    })

    # Reactive selection
    SelectStation <- shiny::reactive({
      shiny::req(input$stationmap_marker_click$id)
      input$stationmap_marker_click$id
    })

    # Dynamic plot height
    output$plotUI <- shiny::renderUI({
      shiny::plotOutput("plot", height = paste0(input$plotHeight, "px"))
    })

    # Plot rendering
    output$plot <- shiny::renderPlot({
      plot_fun(df = df, stratsection_name = SelectStation())
    })
  }


  shiny::shinyApp(ui = ui, server = server)
}
