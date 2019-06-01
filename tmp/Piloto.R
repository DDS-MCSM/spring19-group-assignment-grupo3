############################
#                          #
# Practica DDS - Group     #
#                          #  
#                          # 
#                          #  
############################

# Definicion de variables

verbose <- TRUE
seed <- 666
if (verbose) print("[*] Initial setup")
tini <- Sys.time()
set.seed(666)
dados.url <- "https://opendata.rapid7.com/sonar.tcp/2019-04-20-1555725774-https_get_16993.csv.gz"


Descarga <- function(directorio="data") {
  verbose <- TRUE
  scansio.url <- data.url

  dir.data <- file.path(getwd(), dirdata)
  if (!dir.exists(dir.data)) {
    dir.create(dir.data)
  

    scansio.source <- file.path(dir.data, "https.csv")
    scansio.file.gz <- paste(scansio.source, ".gz", sep = "")
    download.file(url = scansio.url, destfile = scansio.file.gz)
    R.utils::gunzip(scansio.file.gz)
    df.https <- read.csv(scansio.source, stringsAsFactors = FALSE)
    file.remove(scansio.source)
    
    saveRDS(object = df.https, file = file.path(dir.data, "https.rds"))
    
    return(df.https)    
  }
  
