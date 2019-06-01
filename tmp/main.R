# Ejecuta la función sin parametros creara el directorio default.
CrearDirectorio()


# Para ejecutar la descarga del fichero dataset
# downloadScanIO(data.url)

df <- downloadScanIO(data.url)

df.select <- generate(fichero)


geo.new <- geolocate(c('90.69.17.77','90.69.17.87','90.69.17.97','90.69.17.98'))

fichero.selected$newcol <- geo.new


test1 <- geolocate('58.131.182.30')
test2 <- geolocate(c('58.131.182.30','50.58.138.140'))
test2 <- geolocate(c('90.69.17.77','90.69.17.87'))




library(dplyr)
muestra <- sample_n(fichero.csv, 3)
muestra$saddr <- as.character(muestra$saddr)


resolved_source_locations <- geolocate(muestra$saddr)

?fromJSON
?readLines
?rbind

