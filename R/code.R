##################################################################
#
#    Trabajo DDS
#    Cristiano Dias / Luiggi Rodríguez
#
# ' @Codigo Version 1.2
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
#' Title Función para crear directorio de trabajo
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

df <- downloadScanIO(data.url)


#------------------------------------------------------------------

#' Generar data frame con 500 primeras filas, convierte direcciones
#' ip a dato numerico y las adiciona como nuevas columnas.
#' @param df.osint
#' @param verbose valor TRUE
#' @param scope valor 500 - número de filas objetivo
#' @author Cristiano Dias / Luiggi Alexis Rodriguez Ruiz
#' @description Package para generación de dataframe
#' @return objeto "gn"
#'
generate <- function(df.osint = df){
  verbose <- TRUE
  scope <- 500
  if (verbose) print("[*] Selección data frame de 500 filas")
  df.osint$saddr.num <- iptools::ip_to_numeric(df.osint$saddr)
  df.osint$daddr.num <- iptools::ip_to_numeric(df.osint$daddr)
  muestra <- sample(1:nrow(df.osint), scope)
  df.selected <- df.osint[muestra,]
  rm(muestra)
  return(df.selected)
}

df.select <- generate(df)

#------------------------------------------------------------------

#' Función de geolocalización a partir de una dirección IP
#' #' @param url
#' #' @author Cristiano Dias / Luiggi Alexis Rodriguez Ruiz
#' #' @return objeto "ret"


install.packages("rjson")
library(rjson)

geolocate <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
  if (1 == length(ip))
  {
    # Obtenemos datos de una sola IP
    require(rjson)
    url <- paste(c("http://api.ipstack.com/", ip,"?access_key=0533db13ed22f5c22e17981abcc4696d"), collapse='')
    ret <- fromJSON(readLines(url, warn=FALSE))
    if (format == 'dataframe')
      ret <- data.frame(t(unlist(ret)))
    return(ret)
  } else {
    ret <- data.frame()
    for (i in 1:length(ip))
    {
      r <- freegeoip(ip[i], format="dataframe")
      ret <- rbind(ret, r)
    }
    return(ret)
  }
}

geo <- geolocate('90.69.17.77')










