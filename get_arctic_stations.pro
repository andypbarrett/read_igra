;;----------------------------------------------------------------------
;; Gets a list a Arctic rawinsonde stations in the IGRA database
;;
;; 2010-08-23 A.P.Barrett
;;----------------------------------------------------------------------
FUNCTION GET_ARCTIC_STATIONS, FILE, COUNT=NUM_ARCTIC

  READ_IGRA_STATION_LIST, file, stations

  isarctic = WHERE( stations.latitude GE 60., num_arctic )

  arctic_stations = stations[ isarctic ]

  RETURN, arctic_stations

END
