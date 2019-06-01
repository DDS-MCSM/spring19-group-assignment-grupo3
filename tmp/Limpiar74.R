# Welcome to the Limpiar74!
#
# This is an función to clean all row with value "NA"
# #
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' @title limpiar74
#' @author Cristiano Dias / Luiggi Alexis Rodriguez Ruiz
#' @param df parametro definido para función buscar un data frame
#' @param n variable que iniciamos a 0 para realizar la busqueda de las lineas
#' @description Función que elimina todas filas que contengan algun valor nulo
#' de un Data Frame
#' @export
#'
limpiar74 <- function(df, n=0) {
  df[rowSums(is.na(df)) <= n,]
}
