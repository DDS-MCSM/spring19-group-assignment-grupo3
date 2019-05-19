#-----------------------------------------------------------

#' Descargar archivo .csv desde una url definida
#' @param raw.file igual a función de almacenamiento temporal tempfile
#' @author Cristiano Dias / Luiggi Alexis Rodriguez Ruiz
#' @description Package para descarga de archivos
#' @return objeto "df" con datos leídos de archivo .csv descargado
#' y adición de encabezado/nombre de columna.
#'
downlfile <- function(raw.file = tempfile()){

  url <- "https://osint.bambenekconsulting.com/feeds/c2-masterlist.txt"

  download.file(url = url, destfile = raw.file)

  df <- read.csv(raw.file, comment.char = "#",
                 header = FALSE, stringsAsFactors = FALSE,
                 col.names = c("nsource", "saddr", "domain", "daddr", "description", "file" ))
  return(df)
}
downlfile()









