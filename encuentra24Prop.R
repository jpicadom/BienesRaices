setwd("C:/Users/Jonathan/Documents/Jonathan P/Trabajos/Intel/Data Mining & ML/Data Mining Expert/10-Proyecto/")
setwd("C:/Users/Jonathan/Documents/Jonathan P/Trabajos/Intel/Data Mining/Proyecto/")

paquetes <- c("XML","ggplot2","RJSONIO","stringr","jsonlite","plyr","rvest","cluster","FactoMineR",
              "dygraphs","xts","forecast","tm","SnowballC","RCurl","lubridate","randomForest","e1071",
              "stringr","data.table","tidyr","filesstrings","gsubfn","stringr","RCurl")
lapply(paquetes, require, character.only = TRUE) # Carga las librerias necesarias

links <- character()
enlaces<-character()

getCurlOptionsConstants()[["connecttimeout"]]
myOpts <- curlOptions(connecttimeout=200)
ruta <-"http://www.encuentra24.com/costa-rica-es/bienes-raices-venta-de-propiedades" #Ruta principal de la pagina
resultados <- getURL(ruta,.opts = myOpts) #Se obtiene la informacion
resultados_arbol <- htmlParse(resultados,useInternalNodes = TRUE) #Se crea un objeto que la represente en R
class(resultados_arbol)

getCurlOptionsConstants()[["connecttimeout"]]
myOpts <- curlOptions(connecttimeout=200)
urlCSV <- getURL()


#Obtencion de los links de las categorias
enlaces<-ruta %>% read_html() %>% html_nodes(".btn-category") %>% html_attr("href")
enlaces<-str_c("www.encuentra24.com",enlaces) # Asigna el url a los enlaces
enlaces<-enlaces[-c(4,10)] # Elimina propiedades en la playa y en islas
enlaces <-unique(enlaces)

#Obtencion de los links de las propiedades
propiedades <-character()
totalPages <-character()
# Funcion para extraer todas las paginas de una enlace
getPageURLs <- function(url) {
  url <- paste0(ruta,gsub("www.encuentra24.com/costa-rica-es/bienes-raices-venta-de-propiedades", "", enlaces[i]))
  baseurl <- htmlParse(url) 
  paginas <- url %>% read_html() %>% html_nodes('.resultnumber') %>% html_text() #Cantidad de anuncios por enlace
  paginas <- ceiling(NthNumber(paginas, n = 1)/NthNumber(paginas, n = 3)) #Cantidad de pags por enlace
  sigPag <- str_c(".", seq(1,paginas, 1)) # Crea toda la secuencia de páginas
  urls_list <- as.list(str_c(url, sigPag)) # Añade la secuencia a cada url 
  urls_list[length(urls_list) + 1] <- url # Añade la primera página de cada enlace [i]
  return(urls_list)
}
# Entraccion de las paginas de todos los enlaces
for(i in 1:length(enlaces)){
  url <- paste0(ruta,gsub("www.encuentra24.com/costa-rica-es/bienes-raices-venta-de-propiedades", "", enlaces[i]))
  urls_list <- getPageURLs(url)
  totalPages <- c(totalPages,urls_list)
}

#Funcion para extraer todas las propiedades de un pagina
getPageProps <- function(page) {
  resultados <- getURL(page)
  resultados_arbol <- htmlParse(resultados) #Se obtiene el contenido del enlace i
  propiedades <- xpathSApply(resultados_arbol,"//div[@class='ann-box-details']//a",xmlGetAttr, "href")
  propiedades <- unique(propiedades)
  return(propiedades)
}
# Extracccion de todas las propiedades
listaProp <-character()
for(i in 1:length(totalPages)){
  page <- totalPages[i]
  listaN <- getPageProps(page)
  listaProp <- c(listaProp,listaN)
}
length(listaProp)
listaProp <- as.list(unique(listaProp))

# Extraccion de los codigos de cada propiedad para nombras los archivos
# codigoProp <- apply(listaProp,1,function (x) {substr(x,nchar(x)-6,nchar(x))})# cuando lee del archivo
codigoProp <- sapply(listaProp,function (x) {substr(x,nchar(x)-6,nchar(x))})
listaProp <- paste0("www.encuentra24.com", listaProp) # Agrega la direccion web a cada propiedad
write(listaProp, file = "listaProp") # Guarda la lista de propiedades localmente

# Guardar el contenido de los links en la PC
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Con la funcion comentada puede acceder al directorio de trabajo, pero no sirve en el autoreproducible
firmas = system.file("CurlSSL", cainfo = "cacert.pem", package = "RCurl")
if(!dir.exists("Publicaciones"))
  dir.create("Publicaciones") #Se crea la carpeta Publicaciones
#Se descargan los contenidos de cada link
for(i in 1:length(listaProp)){
  setwd <-paste(getwd(),"/Publicaciones")
  if (!file.exists(codigoProp[[i]]))
  url <- listaProp[[i]] 
  tmp <- getURL(url, cainfo = firmas) #Descarga el contenido, utiliza informacion del Cert Autenticidad
  write(tmp, str_c("Publicaciones/", codigoProp[[i]], ".xml")) #Guarda las publicaciones localmente con formato xml
}
length(list.files("Publicaciones"))
list.files("Publicaciones")[1:3] #Revisa los primeros 3 archivos
###########################################################################################################
# list.files produce un vector con los nombres de los archivos dentro del directorio Publicaciones
length(list.files("Publicaciones"))

# Procesamiento de todos los links para extraer la informacion de cada propiedad
i=1
variables<-read_html(str_c("Publicaciones/",codigoProp[[1]],".xml")) %>% html_nodes("[class='info-name']") %>% html_text() # Extrae las categorias
tablaUnida <- data.frame(variables,valores=1:length(variables), row.names = NULL) # Crear tabla en blanco

extraeDatos <- function(docXML) {
  variables<-ejemplo %>% html_nodes("[class='info-name']") %>% html_text() # Extrae las categorias
  tablaUnida <- data.frame(variables,row.names = NULL) # Crear tabla en blanco
  valores <- ejemplo %>% html_nodes("[class='info-value']") %>% html_text() # Extrae los valores
  beneficios<- ejemplo %>% html_nodes("[class='product-features']") %>% html_text()
  beneficios<- gsub("\r?\n|\r", ",", beneficios) # Remueve los cambios de renglon \r\n y los remplaza con comas
  beneficios<- as.String(gsub(",    ", ",", beneficios)) # Remueve espacios en blanco despues de la coma y une todos los beneficios
  detalles<- ejemplo %>% html_nodes("[itemprop='description']") %>% html_text()
  contacto<- ejemplo %>% html_node("[class='user-name']") %>% html_text()
  telefono1<- ejemplo %>% html_nodes("[class='cellphone icon icon-call-mobile']") %>% html_text()
  telefono2<- ejemplo %>% html_nodes("[class='cellphone icon icon-call-mobile mobile-confirmed']") %>% html_text()
  telefono <- as.String(unique(c(telefono1,telefono2)))
  telefono <- unique(as.numeric(gsub("[^0-9]","",telefono))) # Extrae solo numeros de la cadena y remueve duplicados
  tablaTemp <- data.table(variables,valores)
  tablaTemp <- rbind(tablaTemp,data.frame(variables=c("Beneficios:"),valores=c(beneficios)))
  tablaTemp <- rbind(tablaTemp,data.frame(variables=c("Detalles:"),valores=c(detalles)))
  tablaTemp <- rbind(tablaTemp,data.frame(variables=c("Contacto:"),valores=c(contacto)))
  tablaTemp <- rbind(tablaTemp,data.frame(variables=c("Telefono:"),valores=c(telefono)))
  tablaUnida <- merge(tablaUnida,tablaTemp,by.x="variables", by.y = "variables",all.y = TRUE)
  tabla <- spread(data = tablaUnida,key=variables,value = valores, drop = FALSE, fill = NA)
  return(tabla)
}
# Crea la tabla con todos los datos
#listaProp <- read.csv("listaProp",header = FALSE,sep="\t")  Lee la tabla guardada
tablaCompleta <- NULL
for(i in 1:length(list.files("Publicaciones"))){
  ejemplo <- read_html(str_c("Publicaciones/",codigoProp[i],".xml"))
  tablai <- as.data.frame(extraeDatos(ejemplo))
  tablaCompleta <- rbind.fill(tablaCompleta,tablai)
}
# Convierte los nombres de columna a v?lidos
names(tablaCompleta)<-gsub("\\.","_",make.names(gsub(":","",colnames(tablaCompleta)), allow_=TRUE, unique=TRUE))
write.csv2(tablaCompleta,file ="tablaInicial.csv",row.names = FALSE,fileEncoding ="UTF-8")
tablaInicial <- read.csv("tablaInicial.csv",header = TRUE,sep = ";",fileEncoding = "UTF-8")  #Lee la tabla guardada

#######PAGINAS##########
url1 <- "http://www.encuentra24.com/costa-rica-es/bienes-raices-venta-de-propiedades-casas"
htmlpage1 <- read_html(url1)
paginas <- url1 %>% read_html() %>% html_nodes('.resultnumber') %>% html_text()
paginas <- ceiling(NthNumber(paginas, n = 1)/NthNumber(paginas, n = 3))
paginas
########################
write.csv(datos, file = "Datos_Propiedades.csv",row.names = FALSE, fileEncoding = 'UTF-8')
write.csv(variables,file = "categorias.csv",row.names = FALSE, fileEncoding = 'UTF-8')
encoding getOption("1.xml")


