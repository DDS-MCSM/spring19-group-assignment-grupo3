##################################################################
#
#    Trabajo DDS
#    Cristiano Dias / Luiggi Rodríguez
#
# ' @Codigo Version 1.4
#
##################################################################

library(rjson)
library(leaflet)
library(ggplot2)
library(RColorBrewer)


#'-------------------------------------CREAR FOLDER DE TRABAJO
#' Función para crear directorio de trabajo
#' @param verbose Variable de inicialización
#' @param n.folder Define nombre de folder o carpeta donde se almacenarán datasets
#' @author Cristiano Dias / Luiggi Alexis Rodriguez Ruiz
#' @description Package para creación de directorio.
#' @details
#' (tini) variable donde almacenams la hora del sistema
#' (n.folder) Directorio donde iremos alnacenar los ficheros descargados.
#' @return Devuelve directorio donde se almacenarán datasets
#' @examples
#' \dontrun{
#' CrearDirectorio()
#' }
#'
#'

CrearDirectorio <- function(n.folder="datasets") {
  verbose <- TRUE
  if (verbose) print("[*] Initial setup")
  dir.data <- file.path(getwd(), n.folder)
  if (!dir.exists(dir.data)) {
    if (verbose) print("[*] Create data directory")
    dir.create(dir.data)
   }
  }


#' --------------------------------------DOWNLOAD 1
#' Función para descargar, descomprimir y almacenar en directorio específico el dataset
#' con las IPs de origen y destino de ataques
#' La función incluye la validación de la creación de la carpeta destino
#' @param data.url Introducir la url con el dataset en formato csv que queremos descargar.
#' @param n.folder Nombre del folder donde se almacenará dataset
#' @param filename Nombre de fichero que tendrá el dataset
#' @return Devuelve el dataframe descargado con los datos en crudo
#' @examples
#' \dontrun{
#' df.tcp <- downloadScanIO()
#' }
#'
downloadScanIO <- function(n.folder="datasets") {
  verbose <- TRUE
  scansio.url <- "https://opendata.rapid7.com/sonar.tcp/2019-05-31-1559342983-http_get_8001.csv.gz"

  # Check up carpeta creada
  dir.data <- file.path(getwd(), n.folder)
  if (!dir.exists(dir.data)) {
    if (verbose) print("Directorio esta creado")
    dir.create(dir.data)
  }

  # scans.io - Obtenemos los datos del dataset
  scansio.source <- file.path(n.folder ,"fichero.csv")
  scansio.file.csv <- paste(scansio.source, ".csv", sep = "")
  scansio.file.gz <- paste(scansio.source, ".gz", sep = "")

  print("[*] Inicio de descarga del dataset")
  download.file(url = scansio.url, destfile = scansio.file.gz)
  print("[*] Fin de descarga")
  print("[*] Inicio descompresion del fichero .gz")
  R.utils::gunzip(scansio.file.gz)
  print("[*] Fichero descomprimido")
  df.tcp <- read.csv(scansio.source, stringsAsFactors = FALSE)
  print("[*] # Elmininar fichero.csv")
  #file.remove(fichero.csv)
  saveRDS(object = df.tcp, file = file.path(dir.data, "scansio.rds"))
  print("[*] # Fichero scansio.rds creado")
  return(df.tcp)
}


#' ------------------------------------------DOWNLOAD 2
#' Funcion para descargar y descomprimir el dataset MAxmind para obtener las longitudes y
#' latitudes.
#' La función incluye la validación de la creación de la carpeta destino
#' @param n.folder Nombre del folder donde se almacenará dataset
#' @return Devuelve dataset descargado con los datos en crudo
#'
#'
download.geoip <- function(n.folder = "datasets") {

   # Maxmind - Obtener datos en crudo (city)
    geoip.url <- "https://geolite.maxmind.com/download/geoip/database/GeoLite2-City-CSV.zip"

    # Check up carpeta creada
    dir.data <- file.path(getwd(), n.folder)
    if (!dir.exists(dir.data)) {
      if (verbose) print("Directorio esta creado")
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

return(df.geoip)
}


#'--------------------------------------SELECCIÓN MUESTRA - SCOPE
#' Generar data frame con 500 primeras filas de dataset TCP scans,
#' convierte direcciones ip a dato numérico y las adiciona como nuevas columnas.
#' @param df.osint variable de inicialización
#' @param verbose valor TRUE
#' @param scope valor 500 - número de filas objetivo
#' @author Cristiano Dias / Luiggi Alexis Rodriguez Ruiz
#' @description Package para generación de dataframe
#' @return objeto "df.muestra"
#' @examples
#' \dontrun{
#' df.muestra <- generate()
#' }
#'
generate <- function(df.osint = df.tcp){
  verbose <- TRUE
  scope <- 500
  if (verbose) print("[*] Selección data frame de 500 filas")
  df.osint$saddr.num <- iptools::ip_to_numeric(df.osint$saddr)
  df.osint$daddr.num <- iptools::ip_to_numeric(df.osint$daddr)
  muestra <- sample(1:nrow(df.osint), scope)
  df.muestra <- df.osint[muestra,]
  rm(muestra)
  return(df.muestra)
}


#'------------------------------------GEOLOCALIZACIÓN DE DATOS
#' Funcion para obtención de geolocalizacion de las IPs
#'
#' @param ips arreglo de caracteres correspondientes a direcciones IPv4
#' @param df.geoip dataset descargado con la función downloadScanIO
#' @param boost variable por defecto inicializada como FALSE. Si es TRUE usará
#' parallel computing using multiple cores.
#' @return dataset df
#' @export
#'
#' @examples
#' \dontrun{
#'  geo.ips <- addIPgeolocation(ips = df.muestra$saddr, df.geoip = download.geoip())
#'  geo.ipd <- addIPgeolocation(ips = df.muestra$daddr, df.geoip = download.geoip())
#'
#' }
#'
#'
 addIPgeolocation <- function(ips = df.muestra$saddr, df.geoip, boost = FALSE) {
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

   df <- dplyr::select(df, ip, network, latitude, longitude, accuracy_radius)
   names(df) <- c("IP_ORIGEN", "RED", "LATITUDE_O", "LONGITUD_O", "ATTACKS")

   return(df)
 }


#'------------------------------------ADICIONAR COLUMNA PAISES
#' Función para adicionar el nombre de país que nos servirá para el posterior análisis
#' Se adiciona la columna PAIS al dataset
#' @param geo En este parámetro se debe ingresar el resultado de la función addIPgeolocation
#' A partir de ella se puede trabajar con IPs de origen o destino
#' @return data set más columna de paises obtenida a partir de longitud, latitud
#' @export
#'
#' @examples
#'  \dontrun{
#' geo.ips <- Adiciona.pais(geo.ips)
#' geo.ipd <- Adiciona.pais(geo.ipd)
#'
#' }

#'
#'
 Adiciona.pais <- function(geo){
   geo$PAIS <-  maps::map.where(database = "world",geo$LONGITUD_O,geo$LATITUDE_O)
   return(geo)
 }


#'------------------------------------OBTENER COLORES SEGÚN CRITICIDAD
#' Función para la obtención de colores según criticidad: verde, naranja, rojo
#'
#' @param geo En este parámetro se debe ingresar el resultado de la función addIPgeolocation
#' A partir de ella se puede trabajar con IPs de origen o destino
#' @return Colores a ser empleados por los íconos del gráfico.
#' @export
#' @examples
#'  \dontrun{
#' getColor(geo.ips)
#' getColor(geo.ipd)
#'
#' }
#'
getColor <- function(geo) {
  sapply(geo$ATTACKS, function(ATTACKS) {
    if(ATTACKS <= 20) {
      "green"
    } else if(ATTACKS <= 50) {
      "orange"
    } else {
      "red"
    } })
}


#'------------------------------------OBTENER ICONOS PARA GRAFICO
#' Función que permite obtener iconos que serán empleados por la función Gráfica
#'
#' @param geo En este parámetro se debe ingresar el resultado de la función addIPgeolocation
#' A partir de ella se puede trabajar con IPs de origen o destino
#'
#' @return Iconos a ser empleados en gráfico.
#' @export
#'
#' @examples
#'  \dontrun{
#' icons <- iconos(geo.ips)
#' icons <- iconos(geo.ipd)
#'
#' }
#'
iconos <- function(geo){
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(geo)
  )
  return(icons)
  }


#'------------------------------------OBTENER UBICACIONES EN MAPA MUNDIAL
#' Función que permite obtener ubicaciones en mapa mundial de Ips Atacantes/Víctimas midiendo
#' la variable precisión que en este caso ha sido llamada ATTACKS.
#' @param geo En este parámetro se debe ingresar el resultado de la función addIPgeolocation
#' A partir de ella se puede trabajar con IPs de origen o destino
#' @return Gráfico de ubicaciones en mapa mundial de Ips Atacantes/Víctimas según caso origen/destino
#' @export
#'
#' @examples
#'   \dontrun{
#' Grafica(geo.ips)
#' Grafica(geo.ipd)
#'
#' }
#'
Grafica <- function(geo){
leaflet(geo) %>% addTiles() %>%
  addAwesomeMarkers(~LONGITUD_O, ~LATITUDE_O, icon=icons, label=~as.character(ATTACKS))
}



#'---------------------------------------PLOTEO DE PAISES QUE ORIGINAN/RECIBEN MAS ATAQUES
#' Función que plotea los países que generan/reciben mayor número de ataques en el mundo
#' de acuerdo a una muestra de 500 IPs
#' @param geo En este parámetro se debe ingresar el resultado de la función addIPgeolocation
#' A partir de ella se puede trabajar con IPs de origen o destino
#' @return Ploteo de paísed que generan/reciben más ataques
#' @export
#'
#' @examples
#'   \dontrun{
#' top.paises(geo.ips)
#' top.paises(geo.ipd)
#'
#' }
#'
top.paises <- function(geo){

  cc  <-  dplyr::count(geo, PAIS, sort = TRUE)
  top <-dplyr::top_n(cc, 5)
  names(top) <- c("PAIS", "NUMERO_ATAQUES")
  myColors <- brewer.pal(9,"Reds")
  ggplot(top, aes(x=PAIS, y=NUMERO_ATAQUES, fill=NUMERO_ATAQUES))+
    geom_tile()+
    scale_fill_gradientn(aesthetics = "fill", colors=myColors, na.value = "grey50")
}



