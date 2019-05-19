#-----------------------------------------------------------------
#
#' Validar Ip, a partir de lectura de datos de columna de datos saddr
#'
#' @param df.valid
#' @author Cristiano Dias / Luiggi Alexis Rodriguez Ruiz
#' @description Package para validar IP.
#' @return objeto "df.valid"
#' @details
#' Se debe instalar previamente el package: install.packages("dplyr")
#' Cargar libreria respextiva: library(dplyr)
#'
validateip <- function(df.valid = gn){
  df.valid <- df.valid %>%
    mutate(Ip_Valida=ifelse(df.valid$saddr.num == 0,
                            "Ip no valida",
                            "Ip OK"))
  return(df.valid)
}

write.csv(val,"validateip.csv")
