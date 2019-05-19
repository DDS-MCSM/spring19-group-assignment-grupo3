#------------------------------------------------------------------

#' Generar data frame con 200 primeras filas, convierte direcciones
#' ip a dato numerico y las adiciona como nuevas columnas.
#' @param df.osint
#' @param verbose valor TRUE
#' @param scope valor 200 - número de filas objetivo
#' @author Cristiano Dias / Luiggi Alexis Rodriguez Ruiz
#' @description Package para generación de dataframe
#' @return objeto "gn"
#'
generate <- function(df.osint = df){
  verbose <- TRUE
  scope <- 200
  if (verbose) print("[*] Selección data frame de 200 filas")
  df.osint$saddr.num <- iptools::ip_to_numeric(df.osint$saddr)
  df.osint$daddr.num <- iptools::ip_to_numeric(df.osint$daddr)
  muestra <- sample(1:nrow(df.osint), scope)
  gn <- df.osint[muestra,]
  rm(muestra)
  return(gn)
}

generate(df)
