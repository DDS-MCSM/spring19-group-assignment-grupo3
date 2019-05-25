############################
#                          #
# Practica DDS -           #
#                          #  
#                          # 
#                          #  
############################

# Definicion de variables


verbose <- TRUE
seed <- 666
if (verbose) print("[*] Initial setup")
tini <- Sys.time()
set.seed(666)
dados.url <- "https://opendata.rapid7.com/sonar.tcp/2019-04-20-1555725774-https_get_16993.csv.gz"

# Crear directorio con nombre data 
#' Title
#'
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
CrearDir <- function(verbose) {
  dir.data <- file.path(getwd(), "data")
  if (!dir.exists(dir.data)) {
    if (verbose) print("[*] Create data directory")
    dir.create(dir.data)
  }
}

# dados.url - Obtener datos en crudo

#' Title
#'
#' @param verbose 
#' @param dados.url 
#' @param R.utils 
#' @param gunzip 
#'
#' @return
#' @export
#'
#' @examples
Descargar <- function(verbose, dados.url, R.utils, gunzip) {
  if (verbose) print("[*] Read RAW data dados.url")
  dados.source <- file.path(getwd(), "data","dados.tcp.csv")
  archivo.gz <- paste(dados.source, ".gz", sep = "")
  download.file(url = dados.url, destfile = archivo.gz)
  R.utils::gunzip(archivo.gz)
  df.tcp <- read.csv(dados.source, stringsAsFactors = FALSE)
  rm(archivo.gz)
}
CrearDir(verbose)
Descargar(verbose, dados.url, R.utils, gunzip)
df.tcp <- read.csv(dados.source, stringsAsFactors = FALSE)
dados.source <- file.path(getwd(), "data","dados.tcp.csv")
df.tcp <- read.csv(dados.source, stringsAsFactors = FALSE)
