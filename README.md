# Proyecto Final - Data Driven Security

Group Assignment base repository for the Data Driven Security subject of the [CyberSecurity Management Msc](https://www.talent.upc.edu/ing/professionals/presentacio/codi/221101/cybersecurity-management/).

## Package

El objetivo del package es permitir el análisis de un dataset TCP Scans para identificar geográficamente los países que reciben mayor número de ataques en el mundo, y también conocer los puntos de geográficos que originan mayor número de ataques, basado en una muestra objetivo.

## CrearDirectorio()

Función para crear directorio de trabajo del proyecto, donde se almacenarán los datasets que se descargarán y analizarán.
  
  - param verbose Variable de inicialización
  - param n.folder Define nombre de folder o carpeta donde se almacenarán datasets
  - Devuelve directorio donde se almacenarán datasets

```  
CrearDirectorio()
```

## downloadScanIO()
Función para descargar, descomprimir y almacenar en directorio específico el dataset con las IPs de origen y destino de ataques.
La función incluye la validación de la creación de la carpeta destino para almacenar dataset.

  - param data.url Introducir la url con el dataset en formato csv que queremos descargar
  - param n.folder Nombre del folder donde se almacenará dataset
  - return Devuelve dataframe descargado con datos en crudo: (df.tcp)

```  
downloadScanIO <- function(n.folder="datasets")
```

## download.geoip()

#### Funcion para descargar y descomprimir el dataset Maxmind para obtener las longitudes y latitudes.
#### La función incluye la validación de la creación de la carpeta destino para almacenar dataset.

```
  - param n.folder Nombre del folder donde se almacenará dataset
  - return Devuelve dataframe descargado con datos en crudo: (df.geoip)
```  

```  
download.geoip <- function(n.folder = "datasets")
```


## generate()

#### Funcion para generar una cantidad N de registros (fileas) del dataset TCP scan. 
```
 param scope valor 50 - número de filas objetivo
 return objeto "df.muestra"
 param verbose valor TRUE
 param scope valor 500 - número de filas objetivo
 author Cristiano Dias / Luiggi Alexis Rodriguez Ruiz
 description Package para generación de dataframe
 return objeto "df.muestra"
```

```
generate <- function(df.osint = df.tcp)
```

## addIPgeolocation

#### Funcion para obtencion de geolocalizacion de las IPs

```
  param ips arreglo de caracteres correspondientes a direcciones IPv4
  param df.geoip dataset descargado con la función download.geoip
  param boost variable por defecto inicializada como FALSE. Si es TRUE usará
  parallel computing using multiple cores.
  return dataset df
  export
 
  examples
  dontrun{
  geoips <- addIPgeolocation(ips = c("8.8.8.8", "147.81.23.1"),
  df.geoip = download.geoip())
```
#### Creamos estos dos dataframe para reproducir sus datos en los graficos.

```
geo.ips <- addIPgeolocation(ips = df.muestra$saddr, df.geoip = download.geoip())
geo.ipd <- addIPgeolocation(ips = df.muestra$daddr, df.geoip = download.geoip())```
```

##  Adiciona.pais()

#### Creación de los graficos , despues de obtener todos los datos y pasearlos creamos los graficos.

```
 Función para adicionar el nombre de país que nos servirá para el posterior análisis
- Se adiciona la columna PAIS al dataset
- @param geo En este parámetro se debe ingresar el resultado de la función  addIPgeolocation
- A partir de ella se puede trabajar con IPs de origen o destino
- @return data set más columna de paises obtenida a partir de longitud, latitud
- @export
- @examples
- \dontrun{
- geo.ips <- Adiciona.pais(geo.ips)
- geo.ipd <- Adiciona.pais(geo.ipd)
```

##  getColor()
####  OBTENER COLORES SEGÚN CRITICIDAD

#### Función para la obtención de colores según criticidad: verde, naranja, rojo
```
- param geo En este parámetro se debe ingresar el resultado de la función addIPgeolocation
- A partir de ella se puede trabajar con IPs de origen o destino
- return Colores a ser empleados por los íconos del gráfico.
- export
- examples
- \dontrun{
-  getColor(geo.ips)
-  getColor(geo.ipd)
```


## iconos()
#### OBTENER ICONOS PARA GRAFICO
#### Función que permite obtener iconos que serán empleados por la función Gráfica

```
- param geo En este parámetro se debe ingresar el resultado de la función addIPgeolocation
- A partir de ella se puede trabajar con IPs de origen o destino
#
- return Iconos a ser empleados en gráfico.
- export
```
#### Hacemos la llamada la función icons con retorno de ips origen y destino

```
- icons <- iconos(geo.ips)
- icons <- iconos(geo.ipd)
```

## grafica()

#### OBTENER UBICACIONES EN MAPA MUNDIAL
#### Función que permite obtener ubicaciones en mapa mundial de Ips Atacantes/Víctimas midiendo
#### la variable precisión que en este caso ha sido llamada ATTACKS.

```
- param geo En este parámetro se debe ingresar el resultado de la función addIPgeolocation
- A partir de ella se puede trabajar con IPs de origen o destino
- return Gráfico de ubicaciones en mapa mundial de Ips Atacantes/Víticmas según caso - origen/destino
- export
```

#### Hacemos la llamada la función grafica con retorno de ips origen y destino

```
Grafica(geo.ips)
Grafica(geo.ipd)
```

## top.paises()

#### Ploteo de paises que originan/reciben mas ataques
#### Función que plotea los países que generan/reciben mayor número de ataques en el mundo
#### de acuerdo a una muestra de 500 IPs

```
 param geo En este parámetro se debe ingresar el resultado de la función addIPgeolocation
 A partir de ella se puede trabajar con IPs de origen o destino
 return Ploteo de paísed que generan/reciben más ataques

  examples
   \dontrun{
  top.paises(geo.ips)
  top.paises(geo.ipd)

top.paises()
```
#### to be continue.... thanks
