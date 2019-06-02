##################################################################
#
#    Trabajo DDS
#    Cristiano Dias / Luiggi Rodríguez
#
# ' @Codigo Version 1.3
#
##################################################################

library(rjson)
library(leaflet)


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
#'
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

# Ejecutar la función sin parametros creara el directorio default.
CrearDirectorio()



#' --------------------------------------DOWNLOAD 1
#' Función para descargar, descomprimir y almacenar en directorio específico el dataset
#' con las IPs de origen y destino de ataques
#' La función incluye la validación de la creación de la carpeta destino
#' @param data.url Introducir la url con el dataset en formato csv que queremos descargar.
#' @param n.folder Nombre del folder donde se almacenará dataset
#' @param filename Nombre de fichero que tendrá el dataset
#' @return Devuelve el dataframe descargado con los datos en crudo
#'
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

downloadScanIO()
# Ejecutar funcion downloadScanIO()
 df.tcp <- downloadScanIO()



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

print("[*] # Crear fichero geoip.rds")
saveRDS(object = df.geoip, file = file.path(dir.data, "geoip.rds"))

return(df.geoip)
}

download.geoip()
# ' Ejecutar la funcion download.geoip()
df.geoip <- download.geoip()



#'--------------------------------------SELECCIÓN MUESTRA - SCOPE
#' Generar data frame con 500 primeras filas de dataset TCP scans,
#' convierte direcciones ip a dato numérico y las adiciona como nuevas columnas.
#' @param df.osint variable de inicialización
#' @param verbose valor TRUE
#' @param scope valor 500 - número de filas objetivo
#' @author Cristiano Dias / Luiggi Alexis Rodriguez Ruiz
#' @description Package para generación de dataframe
#' @return objeto "df.muestra"
#'
generate <- function(df.osint = df.tcp){
  verbose <- TRUE
  scope <- 50
  if (verbose) print("[*] Selección data frame de 500 filas")
  df.osint$saddr.num <- iptools::ip_to_numeric(df.osint$saddr)
  df.osint$daddr.num <- iptools::ip_to_numeric(df.osint$daddr)
  muestra <- sample(1:nrow(df.osint), scope)
  df.muestra <- df.osint[muestra,]
  rm(muestra)
  return(df.muestra)
}

df.muestra <- generate()


#'------------------------------------GEOLOCALIZACIÓN DE DATOS
#' Funcion para obtención de geolocalizacion de las IPs
#'
#' @param ips arreglo de caracteres correspondientes a direcciones IPv4
#' @param df.geoip dataset descargado con la función download.geoip
#' @param boost variable por defecto inicializada como FALSE. Si es TRUE usará
#' parallel computing using multiple cores.
#' @return dataset df
#' @export
#'
#' @examples
#' #' \dontrun{
 #' geoips <- addIPgeolocation(ips = c("8.8.8.8", "147.81.23.1"),
 #'                            df.geoip = download.geoip())
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

   df <- dplyr::select(df, ip, network, latitude, longitude, accuracy_radius,
                       is_anonymous_proxy, is_satellite_provider)

   return(df)
 }




 geoips2 <- addIPgeolocation(ips = df.muestra$saddr, df.geoip = download.geoip())
 geoips2$Pais <-  maps::map.where(database = "world",geoips2$longitude,geoips2$latitude)



#' Merge final IP vs Geolocalización
#' Title Funcion para sacar un dataframe con el merge entre las IPs y latitudes y longitudes
#'
#' @param scope Numero de observaciones e informaciones de warning.
#' @param n.folder Directorio donde iremos almacenar el resultado y guardar el fichero RDS
#' @param seed variable de inicialización
# misips <- getScanIPS()

getScanIPS <- function(scope = 150, n.folder = "datasets", seed = 666) {
   set.seed(seed)
   dir.data <- file.path(getwd(), n.folder)
   if (!dir.exists(dir.data)) {
     dir.create(dir.data)
   }

   print("[*] Load source data sets")
   if (file.exists(file.path(n.folder, "scansio.rds"))) {
     df <- readRDS(file.path(n.folder, "scansio.rds"))
   } else {
     df <- download.ftp.scans.io(n.folder)
   }
   if (file.exists(file.path(n.folder, "geoip.rds"))) {
     df.geoip <- readRDS(file.path(n.folder, "geoip.rds"))
   } else {
     df.geoip <- download.geoip(n.folder)
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

 df <- getScanIPS()

#### Dibujar Grafico ###########
# Instalar package leaflet

# install.packages("leaflet")



getColor <- function(geoips2) {
  sapply(d$src_accuracy_radius, function(accuracy_radius) {
    if(accuracy_radius <= 20) {
      "green"
    } else if(accuracy_radius <= 50) {
      "orange"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(geoips2)
)


leaflet(geoips2) %>% addTiles() %>%
  addAwesomeMarkers(~longitude, ~latitude, icon=icons, label=~as.character(accuracy_radius))







# Funcion Extra ############

#------------------------------------------------------------------

#' Geolocate ip address
#'
#' Función de geolocalización a partir de una dirección IP
#'
#' @param ip dirección IP
#' @param format variable de formato list o dataframe
#' @return objeto "ret"
geolocate <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
  if (1 == length(ip)) {
    # Obtenemos datos de una sola IP
    require(rjson)
    url <- paste(c("http://api.ipstack.com/", ip,"?access_key=0533db13ed22f5c22e17981abcc4696d"), collapse='')
    ret <- fromJSON(readLines(url, warn=FALSE))
    if (format == 'dataframe')
      ret <- data.frame(t(unlist(ret)))
    return(ret)
  }
  else {
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

