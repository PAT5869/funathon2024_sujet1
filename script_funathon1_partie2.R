library(dplyr)
# On définit l'URL des données
STATIONS_DATA_URL <- "https://www.data.gouv.fr/fr/datasets/r/d22ba593-90a4-4725-977c-095d1f654d28"

stations_data <- read.csv2(STATIONS_DATA_URL)

nom_station<-"Lille-Flandres"

coordonnees <- stations_data %>% 
  select(longitude = x_wgs84, latitude = y_wgs84, libelle) %>% 
  filter(libelle==nom_station)

get_station_coordonnees<-function(nom_station,donnees,message=T) {
  if (nom_station != "Strasbourg-Ville") {
    coordonnees <- donnees %>% 
    select(longitude = x_wgs84, latitude = y_wgs84, libelle) %>% 
    filter(libelle==nom_station)}
  else {
    coordonnees <- c(48.584488, 7.735626)
  }
  return (coordonnees)
  }

sallaumines_coordonnees<-get_station_coordonnees("Sallaumines",stations_data,T) 
  
cat(sprintf("(%f, %f)->%s\n",sallaumines_coordonnees[1],sallaumines_coordonnees[2],sallaumines_coordonnees[3]))

# Exercice 3
get_routes_api_json <- function(coords1, coords2) {
  # On créé le JSON pour l'API de routage en se basant sur celui de la sous-partie "Interaction avec l'API de routage de TravelTime"
  request_body <- sprintf('{
    "locations": [
      {
        "id": "point-from",
        "coords": {
          "lat": %f,
          "lng": %f
        }
      },
      {
        "id": "point-to-1",
        "coords": {
          "lat": %f,
          "lng": %f
        }
      }
    ],
    "departure_searches": [
      {
        "id": "departure-search",
        "transportation": {
          "type": "public_transport",
          "walking_time": 900,
          "cycling_time_to_station": 100,
          "parking_time": 0,
          "boarding_time": 0,
          "driving_time_to_station": 1800,
          "pt_change_delay": 0,
          "disable_border_crossing": false
        },
        "departure_location_id": "point-from",
        "arrival_location_ids": [
          "point-to-1"
        ],
        "departure_time": "2024-06-26T18:00:00.000Z",
        "properties": [
          "travel_time",
          "route"
        ],
        "range": {
          "enabled": true,
          "max_results": 5,
          "width": 43200
        }
      }
    ]
  }', coords1[1], coords1[2], coords2[1], coords2[2])
  return(request_body)
}

get_travel_time_between_stations <- function(station1, station2, data, verbose = TRUE) {
  # Si les stations sont identiques aucun trajet nécessaire
  if (station1 == station2) {
    return(NA)
  }
  
  
  # Récupérer les coordonnées pour les deux stations
  coordinates <- lapply(c(station1, station2), get_station_coordinates, data = data, verbose = FALSE)
  
  # Générer le JSON pour l'API de routage
  request_body <- get_routes_api_json(coordinates[[1]], coordinates[[2]])
  
  # Interroger l'API de routage
  response <- get_travel_time_api_response(ROUTES_API_URL, request_body)
  
  # Gérer la limitation du taux d'API
  if (response[[2]] == 429) {
    if (verbose) cat("Trop de requêtes, attente d'une minute...\n")
    Sys.sleep(60)
    return(get_travel_time_between_stations(station1, station2, data, verbose))
  }
  
  # Vérifier l'existence d'un itinéraire valide
  if (length(response[[1]]$results[[1]]$locations) == 0) {
    travel_time <- Inf
  } else {
    # Extraire les données de temps de trajet et trouver le temps de trajet minimum en heures
    travel_times <- sapply(response[[1]]$results[[1]]$locations[[1]]$properties, function(item) item$travel_time)
    travel_time <- min(travel_times) / 3600
  }
  
  # Afficher le temps de trajet si verbose
  if (verbose) {
    message_text <- sprintf("%s -> %s : %s heures\n", station1, station2, ifelse(is.infinite(travel_time), "Aucun itinéraire trouvé", round(travel_time, 2)))
    cat(message_text)
  }
  
  return(travel_time)
}