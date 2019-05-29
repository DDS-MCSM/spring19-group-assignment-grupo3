##################################################################
#
#    Trabajo DDS
#    Cristiano Dias / Luiggi Rodríguez
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

CrearDirectorio <- function(dir.path="dados5") {
  if (verbose) print("[*] Initial setup")
#  tini <- Sys.time()
#  set.seed(666)
  dir.data <- file.path(getwd(), dir.path)
  if (!dir.exists(dir.data)) {
    if (verbose) print("[*] Create data directory")
    dir.create(dir.data)
  }
#return (dir.data)

  }

# Ejecuta la función sin parametros creara el directorio default.
CrearDirectorio()

#' Download 1
#'
#' Funcion para descargar y descomprimir el dataset con las IPS vulnerables.
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
downloadScanIO <- function(dir.path="dados5") {
  verbose <- TRUE
  scansio.url <- data.url

  # Check up carpeta creada
  dir.data <- file.path(getwd(), dir.path)
  if (!dir.exists(dir.data)) {
    if (verbose) print("Directorio esta creado")
    dir.create(dir.data)
  }

  # scans.io - Obtenemos los datos del dataset

  scansio.source <- file.path(dir.path ,"fichero.csv")
  scansio.file.gz <- paste(scansio.source, ".gz", sep = "")
  print("[*] Inicio de descarga del dataset.gz")
  download.file(url = scansio.url, destfile = scansio.file.gz)
  print("[*] Fin de descarga")
  print("[*] Inicio descompresion del fichero .gz")
  R.utils::gunzip(scansio.file.gz)
  print("[*] Fichero descomprimido")
  df.tcp <- read.csv(scansio.source, stringsAsFactors = FALSE)
  print("[*] # Elmininar fichero.csv")
  file.remove(scansio.source)
  saveRDS(object = df.tcp, file = file.path(dir.data, "scansio.rds"))
  print("[*] # Fichero scansio.rds creado")
  return(df.tcp)

}
# Ejecutar funcion downloadScanIO()
# a <- downloadScanIO()

#' Download 2
#'
#' Funcion para descargar y descomprimir el dataset para sacar las longitudes y latitudes.
#'
 download.geoip <- function(dirdata = "dados5") {

   # Maxmind - Obtener datos en crudo (city)
    geoip.url <- "https://geolite.maxmind.com/download/geoip/database/GeoLite2-City-CSV.zip"

    dir.data <- file.path(getwd(), dirdata)
  if (!dir.exists(dir.data)) {
  dir.create(dir.data)
    }

geoip.file <- file.path(dir.data, "geoip.zip")
print("[*] # Inicio download geip.zip")
download.file(url = geoip.url, destfile = geoip.file)
print("[*] # Fin de download")
zipfiles <- unzip(zipfile = geoip.file, list = T)
geoip.source <- zipfiles$Name[grep(pattern = ".*GeoLite2-City-Blocks-IPv4.csv", x = zipfiles$Name)]
print("[*] # Descomprimir fichero")
unzip(zipfile = geoip.file, exdir = dir.data, files = geoip.source)
geoip.source <- file.path(dir.data, geoip.source)
df.geoip <- read.csv(geoip.source, stringsAsFactors = FALSE)
print("[*] # Remover fichero .csv")
file.remove(geoip.source, geoip.file)
unlink(file.path(dir.data, "GeoLite2-City-CSV_*"), recursive = T)

# Maxmind elegante
df.geoip <- cbind(df.geoip, iptools::range_boundaries(df.geoip$network))
df.geoip$rowname <- as.integer(row.names(df.geoip))
df.geoip$range <- NULL

print("[*] # Crear fichero geoip.rds")
saveRDS(object = df.geoip, file = file.path(dir.data, "geoip.rds"))

return(df.geoip)
 }
# ' Download 2
# ' Ejecutar la funcion download.geoip()
# b <- download.geoip()


# Funcion para sacar la geolocalizacion de las IPs



 addIPgeolocation <- function(ips = "", df.geoip = data.frame(), boost = FALSE) {
   # Para geolocalizar una IP en un rango comprobaremos si está entre la primera
   # y la ultima ip de cada rango en MaxMind.

   if (all(iptools::is_ipv4(ips))) {
     ips <- iptools::ip_to_numeric(ips)
   }
   df <- data.frame(ip = as.numeric(ips))

   if (boost) {
     # Usamos multiples cpu's para geolocalizar IPs en rangos
     no_cores <- parallel::detectCores() - 1
     cl <- parallel::makeCluster(no_cores)
     parallel::clusterExport(cl, "df.geoip", envir = environment())
     df$geoip.rowname <- sapply(ips,
                                  function(ip)
                                    which((ip >= df.geoip$min_numeric) &
                                            (ip <= df.geoip$max_numeric)))
     parallel::stopCluster(cl)
     rm(cl, no_cores)
   } else {
     df$geoip.rowname <- sapply(ips,
                                  function(ip)
                                    which((ip >= df.geoip$min_numeric) &
                                            (ip <= df.geoip$max_numeric)))
   }

   df <- dplyr::left_join(df, df.geoip, by = c("geoip.rowname" = "rowname"))

   df <- dplyr::select(df, ip, network, latitude, longitude, accuracy_radius,
                       is_anonymous_proxy, is_satellite_provider)

   return(df)
 }




#' Merge final IP vs Geolozalizacion
#' Title Funcion para sacar un dataframe con el merge entre las IPs y latitudes y longitudes
#'
#' @param scope Numero de observaciones y informaciones de warning.
#' @param dirdata Directorio donde iremos almacenar el resultado y guardar el fichero RDS
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
#'
# misips <- getScanIPS()

getScanIPS <- function(scope = 150, dirdata = "dados5", seed = 666) {
   set.seed(seed)
   dir.data <- file.path(getwd(), dirdata)
   if (!dir.exists(dir.data)) {
     dir.create(dir.data)
   }

   print("[*] Load source data sets")
   if (file.exists(file.path(dirdata, "scansio.rds"))) {
     df <- readRDS(file.path(dirdata, "scansio.rds"))
   } else {
     df <- download.ftp.scans.io(dirdata)
   }
   if (file.exists(file.path(dirdata, "geoip.rds"))) {
     df.geoip <- readRDS(file.path(dirdata, "geoip.rds"))
   } else {
     df.geoip <- download.geoip(dirdata)
   }

   print("[*] Prepare scans.io data.frame")
   # Seleccionamos una muestra de scans
   df <- df[sample(1:nrow(df), scope),]

   # Transformamos las IPs a formato decimal
   df$saddr.num <- iptools::ip_to_numeric(df$saddr)
   df$daddr.num <- iptools::ip_to_numeric(df$daddr)

   print("[*] Find IP geolocation data")
   # Geolocalizamos las IPs origen y destino
   geo.src <- addIPgeolocation(ips = df$saddr,
                               df.geoip = df.geoip,
                               boost = scope > 1000)
   geo.dst <- addIPgeolocation(ips = df$daddr,
                               df.geoip = df.geoip,
                               boost = scope > 1000)

   print("[*] Tidy data frame")
   names(geo.src) <- paste("src_", names(geo.src), sep = "")
   names(geo.dst) <- paste("dst_", names(geo.dst), sep = "")

   # Preparamos el data frame
   df <- dplyr::bind_cols(df, geo.src, geo.dst)
   df$dst_is_anonymous_proxy <- as.factor(df$dst_is_anonymous_proxy)
   df$src_is_anonymous_proxy <- as.factor(df$src_is_anonymous_proxy)
   df$dst_is_satellite_provider <- as.factor(df$dst_is_satellite_provider)
   df$src_is_satellite_provider <- as.factor(df$src_is_satellite_provider)

   df <- dplyr::select(df, timestamp_ts, ttl,
                       saddr, sport, src_network, src_latitude, src_longitude,
                       src_accuracy_radius, src_is_anonymous_proxy, src_is_satellite_provider,
                       daddr, dport, dst_network, dst_latitude, dst_longitude,
                       dst_accuracy_radius, dst_is_anonymous_proxy, dst_is_satellite_provider)

   saveRDS(object = df, file = file.path(dir.data, "getScansioFTPs.rds"))

   return(df)
 }

 d <- getScanIPS()

#### Dibujar Grafico ###########
# Instalar package leaflet

# install.packages("leaflet")

library(leaflet)

getColor <- function(d) {
  sapply(d$src_accuracy_radius, function(src_accuracy_radius) {
    if(src_accuracy_radius <= 20) {
      "green"
    } else if(src_accuracy_radius <= 50) {
      "orange"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(d)
)


leaflet(d) %>% addTiles() %>%
  addAwesomeMarkers(~dst_longitude, ~dst_latitude, icon=icons, label=~as.character(src_accuracy_radius))


############################################################################


# Package EXTRA ####################


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


# Funcion Extra ############

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
      r <- geolocate(ip[i], format="dataframe")
      ret <- rbind(ret, r)
    }
    return(ret)
  }
}

geo <- geolocate('90.69.17.77')

