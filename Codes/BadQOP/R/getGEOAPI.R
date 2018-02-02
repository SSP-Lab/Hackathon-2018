# +++++++++++++++++++++++
# GEO API
# +++++++++++++++++++++++

#' Query French city informations with GEO API
#'
#' Simple calls to \url{https://api.gouv.fr/api/api-geo.html},
#' French government statistics (from Etalab). \code{get.GEOAPI.byzip}
#' returns the cities associated to the ZIP code requested
#' \code{get.GEOAPI} allows more complex calls
#'
#' @param ZIPcode ZIP code to query
#' @return List returning all cities that match a request.
#'  with this ZIP code with some. For instance, by default,
#' `code` (INSEE city identifier),
#' `codesPostaux` (ZIP code), `codeDepartement` (department code),
#' `codeRegion` (region code), `population`
#'
#' @examples get.GEOAPI.byzip("35220")
#' get.GEOAPI.byzip("75001")

get.GEOAPI.byzip <- function(ZIPcode){
  url <- paste("https://geo.api.gouv.fr/communes?codePostal=",URLencode(ZIPcode),
               "&fields=nom,code,codesPostaux,codeDepartement,codeRegion,population&format=json&geometry=centre", sep = "")
  result <- jsonlite::fromJSON(url)
  return(result)
}

#' @rdname get.GEOAPI.byzip
#' @examples
#' get.GEOAPI("nom=versailles")
#' get.GEOAPI("lon=2.1301&lat=48.8014")
#' get.GEOAPI("nom=toulouse", fields = "nom, code, codesPostaux, population")
get.GEOAPI <- function(query, fields = "nom,code,codesPostaux,codeDepartement,codeRegion,population"){
  url <- paste("https://geo.api.gouv.fr/communes?",URLencode(query),
               "&fields=",stringr::str_replace_all(fields," ",""),
               "&format=json&geometry=centre", sep = "")
  result <- jsonlite::fromJSON(url)
  return(result)
}


# +++++++++++++++++++++++++++++++
# BASE D'ADRESSES NATIONALES
# +++++++++++++++++++++++++++++++

# BASE D'ADRESSES NATIONALES
#

#' Query locations associated with a requested address in France
#'
#' Use French *Base d'Adresses Nationales* (\url{https://adresse.data.gouv.fr/api/})
#' to get coordinates of an address or a place
#'
#' @examples
#' # Search 8 boulevard du Port in all French cities
#' get.banAPI()$features
#' # Search 8 boulevard du Port around a point
#' get.banAPI('8 bd du port&lat=48.789&lon=2.789')$features
#' # Search for Kebab everywhere
#' get.banAPI('kebab',T)$features
#' # Search a specific adress
#' get.banAPI('36 Quai des Orfèvres')$features

get.banAPI <- function(query = '8 bd du port',
                      geo.place = F){

  url <- if (!geo.place) paste0("https://api-adresse.data.gouv.fr/search/?q=",
                                URLencode(query)) else paste0("http://sirene.addok.xyz/search/?q=",
                                                              URLencode(query))

  query.result <- jsonlite::fromJSON(httr::content(httr::GET(url = url),"text"))

  return(query.result)
}
