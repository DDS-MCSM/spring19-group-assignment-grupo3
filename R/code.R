##################################################################
#
#    Trabajo DDS
#    Cristiano Dias / Luiggi
#
# ' @Codigo Version 1.3
#
##################################################################
#
#
# Crear Directorio Workgroup!
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# Initial Setup
#' Title Función para crear direcotiro de trabajo
#'
#' @param verbose valor TRUE
#' @author Cristiano Dias / Luiggi Alexis Rodriguez Ruiz
#' @description Package para creación de directorio.
#' @details
#' (tini) variable donde almacenams la hora del sistema
#' (dir.path) Directorio donde iremos alnacenar los ficheros descargados.
#'
#' @examples
#' CrearDirectorio("prueba")
#' CrearDirectorio()
#'
filename <- "fichero.csv"
verbose <- TRUE
data.url <- "https://opendata.rapid7.com/sonar.tcp/2019-04-20-1555725774-https_get_16993.csv.gz"

CrearDirectorio <- function(dir.path="dados3") {
  if (verbose) print("[*] Initial setup")
  tini <- Sys.time()
  set.seed(666)
  dir.data <- file.path(getwd(), dir.path)
  if (!dir.exists(dir.data)) {
    if (verbose) print("[*] Create data directory")
    dir.create(dir.data)
  }
return (dir.data)
  }
# Ejecuta la función sin parametros creara el directorio default.
CrearDirectorio()


#' Funcion para descargar y descomprimir
#' @param data.url Introducir la url con el dataset en formato csv que queremos descargar.
#' @param dir.path Nombre del directorio
#' @param filename Fichero que tendrá el dataset
#' @return Devuelve el dataframe descargado con los datos en crudo
#' @examples
#' url="https://opendata.rapid7.com/sonar.tcp/2019-04-20-1555725774-https_get_16993.csv.gz"
#'
#' downloadScanIO(url,"http","scansio.http")
#'
#' dataset=downloadScanIO(url,"http","scansio.http")  # Almacena en la variable dataset
#'
#' \dontrun {
#' downloadScanIO(url,"http","scansio.http")
#'}
#' \dontrun {
#' dataset=downloadScanIO(url,"http","scansio.http")
#' }
downloadScanIO <- function(data.url, dir.path="dados3", filename) {
  verbose <- TRUE
  scansio.url <- data.url

  # Check up carpeta creada
  dir.data <- file.path(getwd(), dir.path)
  if (!dir.exists(dir.data)) {
    if (verbose) print("Directorio esta creado")
    dir.create(dir.data)
  }

  # scans.io - Obtenemos los datos del dataset
  scansio.source <- file.path(getwd(), dir.path ,"fichero.csv")
  scansio.source.csv <- paste(scansio.source, ".csv" , sep = "")
  scansio.file.gz <- paste(scansio.source.csv, ".gz", sep = "")
  download.file(url = scansio.url, destfile = scansio.file.gz)
  R.utils::gunzip(scansio.file.gz)
  df.tcp <- read.csv(scansio.source.csv, stringsAsFactors = FALSE)
  rm(scansio.file.gz)
  return (df.tcp)
}

# Para ejecutar la descarga del fichero dataset
# downloadScanIO(data.url)

#'
#' @title Funcion para sacar las n primeras filas del dataframe
#' @description Con esta función pasamos un data frame y retorna la cantidad de lineas definidas un valor n
#' @details Para el data frame y un valor numerico
#' @param df Variable donde almacenamos el data frame
#' @param nrows Numero de lineas que iremos mostrar
#' @return pre.df
#' @export
#' @examples
#' genera.df(df, 50)
genera.df <- function(df, nrows) {
  # Returna un data frame con n cuantidad de lineas
  pre.df <- df[1:nrows,]
  return(pre.df)
}












