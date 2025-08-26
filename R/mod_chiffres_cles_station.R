#' chiffres_cles_station UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_chiffres_cles_station_ui <- function(id){
  ns <- NS(id)
  tagList(
    htmlOutput(ns("titre")),
    # htmlOutput(ns("FicheStation")),
    tableOutput(ns("tableau"))
  )
}

#' chiffres_cles_station Server Functions
#'
#' @noRd
mod_chiffres_cles_station_server <- function(id, resumes_listes, stations, choix_station, choix_eqb){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({

      resume_listes <- resumes_listes %>%
        filtrer_resumes(choix_station(), choix_eqb()) %>%
        dplyr::select(-code_station_hydrobio, -code_support)

      output$titre <- renderUI({
        with(
          filtrer_station(stations, choix_station()),
          HTML(paste0('<a href="', uri_station_hydrobio, '" target="_blank" title="Fiche station sandre"><b>',
                      libelle_station_hydrobio, '</b></a>'))
        )
      })
      output$tableau <- renderTable(resume_listes)

    })
  })
}

## To be copied in the UI
# mod_chiffres_cles_station_ui("chiffres_cles_station_1")

## To be copied in the server
# mod_chiffres_cles_station_server("chiffres_cles_station_1")
