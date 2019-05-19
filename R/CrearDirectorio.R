# Crear Directorio Workgroup!
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# Initial Setup
#' Title Creación de directorio de trabajo
#'
#' @param verbose valor TRUE
#' @author Cristiano Dias / Luiggi Alexis Rodriguez Ruiz
#' @description Package para creación de directorio.
#' @details
#' (tini) variable donde almacenams la hora del sistema
#' (dir.data) Directorio donde iremos alnacenar los ficheros descargados.
#'
CrearDirectorio <- function(verbose) {
  if (verbose) print("[*] Initial setup")
  tini <- Sys.time()
  set.seed(666)
  dir.data <- file.path(getwd(), "data")
  if (!dir.exists(dir.data)) {
    if (verbose) print("[*] Create data directory")
    dir.create(dir.data)
  }
}