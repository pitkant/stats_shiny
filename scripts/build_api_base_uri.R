# Function from development version 4.1 of eurostat
build_api_base_uri <- function(agency) {
  agency <- tolower(agency)
  api_base_uri <- switch(
    agency,
    eurostat = "https://ec.europa.eu/eurostat/api/dissemination",
    estat = "https://ec.europa.eu/eurostat/api/dissemination",
    eurostat_comext = "https://ec.europa.eu/eurostat/api/comext/dissemination",
    comext = "https://ec.europa.eu/eurostat/api/comext/dissemination",
    eurostat_prodcom = "https://ec.europa.eu/eurostat/api/comext/dissemination",
    prodcom = "https://ec.europa.eu/eurostat/api/comext/dissemination",
    comp = "https://webgate.ec.europa.eu/comp/redisstat/api/dissemination",
    empl = "https://webgate.ec.europa.eu/empl/redisstat/api/dissemination",
    grow = "https://webgate.ec.europa.eu/grow/redisstat/api/dissemination")

  api_base_uri
}
