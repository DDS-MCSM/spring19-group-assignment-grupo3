% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code.R
\name{getColor}
\alias{getColor}
\title{------------------------------------OBTENER COLORES SEGÚN CRITICIDAD
Función para la obtención de colores según criticidad: verde, naranja, rojo}
\usage{
getColor(geo)
}
\arguments{
\item{geo}{En este parámetro se debe ingresar el resultado de la función addIPgeolocation
A partir de ella se puede trabajar con IPs de origen o destino}
}
\value{
Colores a ser empleados por los íconos del gráfico.
}
\description{
------------------------------------OBTENER COLORES SEGÚN CRITICIDAD
Función para la obtención de colores según criticidad: verde, naranja, rojo
}
\examples{
 \dontrun{
getColor(geo.ips)
getColor(geo.ipd)

}

}
