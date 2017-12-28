

# +++++++++++++++++++++++
# GEO API
# +++++++++++++++++++++++

# Construire une fonction qui appelle l'API
get_by_code <- function(query){
  url <- paste("https://geo.api.gouv.fr/communes?codePostal=",query, "&fields=nom,code,codesPostaux,codeDepartement,codeRegion,population&format=json&geometry=centre", sep = "")
  result <- jsonlite::fromJSON(url)
  return(result)
}
#' get_by_code("35200")


# +++++++++++++++++++++++++++++++
# BASE D'ADRESSES NATIONALES
# +++++++++++++++++++++++++++++++

# BASE D'ADRESSES NATIONALES
#https://adresse.data.gouv.fr/api/

#' getGEOAPI('kebab',T)
#' getGEOAPI()
#' getGEOAPI('8 bd du port&lat=48.789&lon=2.789')
#'
# getGEOAPI(query = paste0(
#   paste(df$NumeroVoieEtablissement,df$TypeVoieEtablissement,
#         df$LibelleVoieEtablissement,sep = " ")[1],
#   "&citycode  =",df$CodeCommuneEtablissement[1]))
# getGEOAPI(query = paste0(
#   paste('Pharmacie',df$TypeVoieEtablissement,
#         df$LibelleVoieEtablissement,sep = " ")[1],
#   "&citycode  =",df$CodeCommuneEtablissement[1]),
#   geo.place = T)


getGEOAPI <- function(query = '8 bd du port',
                      geo.place = F){

  url <- if (!geo.place) paste0("https://api-adresse.data.gouv.fr/search/?q=",
                                URLencode(query)) else paste0("http://sirene.addok.xyz/search/?q=",
                                                              URLencode(query))

  query.result <- jsonlite::fromJSON(httr::content(httr::GET(url = url),"text"))

  return(query.result)
}
