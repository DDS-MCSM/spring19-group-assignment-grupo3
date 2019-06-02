# Proyecto Final - Data Driven Security

Group Assignment base repository for the Data Driven Security subject of the [CyberSecurity Management Msc](https://www.talent.upc.edu/ing/professionals/presentacio/codi/221101/cybersecurity-management/).

## Package

El objetivo del package es permitir el análisis de un dataset TCP Scans para identificar geográficamente los países que reciben mayor número de ataques en el mundo, y también conocer los puntos de geográficos que originan mayor número de ataques, basado en una muestra objetivo.

### CrearDirectorio()

Función para crear directorio de trabajo del proyecto, donde se almacenarán los datasets que se descargarán y analizarán.
  
  - @param verbose Variable de inicialización
  - @param n.folder Define nombre de folder o carpeta donde se almacenarán datasets
  - Devuelve directorio donde se almacenarán datasets

```  
CrearDirectorio()
```

### downloadScanIO()
Función para descargar, descomprimir y almacenar en directorio específico el dataset con las IPs de origen y destino de ataques.
La función incluye la validación de la creación de la carpeta destino para almacenar dataset.

  - @param data.url Introducir la url con el dataset en formato csv que queremos descargar
  - @param n.folder Nombre del folder donde se almacenará dataset
  - @return Devuelve dataframe descargado con datos en crudo: (df.tcp)

```  
downloadScanIO <- function(n.folder="datasets")
```

### download.geoip()
Funcion para descargar y descomprimir el dataset Maxmind para obtener las longitudes y latitudes.
La función incluye la validación de la creación de la carpeta destino para almacenar dataset.

  - @param n.folder Nombre del folder donde se almacenará dataset
  - @return Devuelve dataframe descargado con datos en crudo: (df.geoip)
  
```  
download.geoip <- function(n.folder = "datasets")
```

### addIPgeolocation()


### getScanIPS()


### getColor()





