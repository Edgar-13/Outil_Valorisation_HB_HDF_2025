# packages.R

# Active l'environnement renv
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
} else {
  stop("Le fichier renv/activate.R est introuvable. L'application ne peut pas démarrer.")
}

required_packages <- c(
  "dplyr", "tidyr", "purrr", "stringr", "forcats", "lubridate",
  "htmltools", "hubeau", "janitor", "knitr", "leaflet", "leaflet.extras",
  "openxlsx2", "patchwork", "plotly", "sf", "shiny", "shinydashboard",
  "pkgload", "here", "munsell", "readxl", "vroom"
)

# Vérifie les packages installés dans l'environnement renv
installed_pkgs <- rownames(installed.packages())

# Installe les packages manquants via renv (ça installe localement dans renv)
for (pkg in required_packages) {
  if (!pkg %in% installed_pkgs) {
    message("Installation du package manquant : ", pkg)
    renv::install(pkg)
  }
}

# Charge tous les packages
invisible(lapply(required_packages, library, character.only = TRUE))

#library(ggplot2)
# library(dplyr)
# library(tidyr)
# library(purrr)
# library(stringr)
# library(forcats)
# library(lubridate)
#
# library(htmltools)
# library(hubeau)
# library(janitor)
# library(knitr)
# library(leaflet)
# library(leaflet.extras)
# library(openxlsx2)
# library(patchwork)
# library(plotly)
# library(sf)
# library(shiny)
# library(shinydashboard)
# #library(tidyverse)
# library(pkgload)
# library(here)
# library(munsell)
