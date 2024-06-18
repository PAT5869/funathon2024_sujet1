# Funathon 2024 - Sujet 1 :star:
pat
Visualiser les émissions de C02 liées à une mesure de restriction de liaison aérienne relativement à des durées de trajets ferroviaires.

## Grandes parties

1.  Interaction avec l'API de routage de TravelTime

2.  Récupération des coordonnées des villes françaises

3.  Obtention du temps de transport entre 2 villes

4.  Téléchargement et traitement des données de trafic aérien entre deux villes

5.  Datavisualisation des données sur une carte avec le package leaflet

## Remarques

Pas d'utilisation des API "isochrone" et "time matrix" de TravelTime car limitées à 4h max tout inclus (trajet à pied du centre à la gare, etc.) donc utilisation de l'API "routes".

Concernant l'API de routage "routes" pour faire des itinéraires point à point :

-   Elle est limitée à quelques requêtes par minute et créer donc de la lenteur dans l'exécution du programme

-   Elle ne permet pas de traverser de frontières.

Voir playground de l'API de routage "routes" de TravelTime [ici](#0).

Voir test de *leaflet* [ici](#0).
