# # launch_app.R
#
# # Installer et charger tous les packages nécessaires
# source("packages.R")
#
# load("R:/ServicesRegionaux/Service_Connaissance/7-Laboratoire_hydrobiologie/Donnees/Syntheses_et_valorisation/Outil_valorisation_HB_HDF_2025/data_carte.rda", envir = .GlobalEnv)
# load("R:/ServicesRegionaux/Service_Connaissance/7-Laboratoire_hydrobiologie/Donnees/Syntheses_et_valorisation/Outil_valorisation_HB_HDF_2025/data_hydrobio.rda", envir = .GlobalEnv)
# #load("data_hydrobio_merged.rda", envir = .GlobalEnv)
#
# # Charger l'interface et le serveur de l'application
# source("app.R")  # app.R doit contenir les définitions de ui et server
#
#
# # Lancer l'application
# library(shiny)
# port <- httpuv::randomPort()
# browseURL(sprintf("http://127.0.0.1:%d", port))
# shiny::runApp(shinyApp(ui, server), port = port, launch.browser = FALSE)
# #shiny::runApp(".", port = port, launch.browser = TRUE)
