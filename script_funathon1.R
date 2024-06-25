library(yaml)

secrets <- yaml::read_yaml("secrets.yaml")
X_API_ID <- secrets$travelTime$X_API_ID
X_API_KEY <- secrets$travelTime$X_API_KEY


ROUTES_API_URL <- "https://api.traveltimeapp.com/v4/routes"


request_body <- '
{
  "locations": [
    {
      "id": "point-from",
      "coords": {
        "lat": 51.5119637,
        "lng": -0.1279543
      }
    },
    {
      "id": "point-to-1",
      "coords": {
        "lat": 51.5156177,
        "lng": -0.0919983
      }
    }
  ],
  "departure_searches": [
    {
      "id": "departure-search",
      "transportation": {
        "type": "public_transport"
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
        "width": 900
      }
    }
  ]
}'
  
  
  headers <- httr::add_headers(
    "Content-Type" = "application/json",
    "X-Application-Id" = X_API_ID,
    "X-Api-Key" = X_API_KEY
  )
  
  
  response <- httr::POST(ROUTES_API_URL, body = request_body, encode = "json", headers)
  
  content <- httr::content(response)
  
  # test
  if (httr::status_code(response) == 200) {
    print("La requête a bien été traitée")
    content <- httr::content(response, as = "parsed")
    print(content)
  } else {
    # Affichage d'un message d'erreur si le code de la réponse n'est pas 200
    print(sprintf("Une erreur est survenue. Code de la réponse : %d", httr::status_code(response)))
  }
  
  # Créer une fonction get_travel_time_api_response() 
  get_travel_time_api_response <- function(api_url, request_body) {
    # On prépare les headers
    headers <- httr::add_headers(
      "Content-Type" = "application/json",
      "X-Application-Id" = X_API_ID,
      "X-Api-Key" = X_API_KEY
    )
    ## On envoie la requête avec les headers spécifiés
    response <- httr::POST(api_url, body = request_body, encode = "json", headers)
    
    # On vérifie s'il y a eu une erreur
    if (!httr::http_error(response)) {
      return(list(
        "Content" = httr::content(response, as = "parsed"),
        "Status_code" = httr::status_code(response)
      ))
    } else {
      # On affiche une message d'avertissement lorsque la requête n'a rien renvoyé
      warning("Failed to retrieve data: ", httr::http_status(response)$message)
      return(list(
        "Content" = NA,
        "Status_code" = httr::status_code(response)
      ))
    }
  }
  
  response_from_function <- get_travel_time_api_response(ROUTES_API_URL, request_body)
  
  
  list_itinerary <- response_from_function[[1]]$results[[1]]$locations[[1]]$properties
  print(list_itinerary)