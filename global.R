.libPaths(c("/workspace/zhengliangtao/04.lib/R/4.0.5","/home/users/zhengliangtao/R/x86_64-pc-linux-gnu-library/4.0"))

############ required packages ##############
getPackage <- function(pkg, check = TRUE, load = TRUE, silent = FALSE, github = NULL) {
  if (check) {
    if (!suppressMessages(suppressWarnings(require(
      pkg, character.only = TRUE, quietly = TRUE
    )))) {
      if (is.null(github)) {
        try(install.packages(pkg), silent = TRUE)
      }
      else{
        try(remotes::install_github(github))
      }
    }
  }
  if (load)
    suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE))
  if (load & !silent)
    message("Loaded ", pkg)
}
packages <- c("shinydashboard", "shinyWidgets", "shinyjs","bsplus", "dplyr",
              "shiny", "DT", "R.utils","rjson","magrittr","plyr","data.table","ggplot2")
lapply(packages, getPackage)

getPackage(pkg = 'shinydashboardPlus', github = "RinteRface/shinydashboardPlus")
getPackage(pkg = 'shinysky', github = "AnalytixWare/ShinySky")
getPackage(pkg = 'shinyauthr', github = "paulC91/shinyauthr")
getPackage(pkg = 'shinyTree', github = "shinyTree/shinyTree")

library("sscVis")

source("./lib/plot.func.R")

