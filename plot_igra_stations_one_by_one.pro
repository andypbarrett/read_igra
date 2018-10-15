;;----------------------------------------------------------------------
;; Plots IGRA stations one at a time
;;
;; 2010-12-10 A.P.Barrett
;;----------------------------------------------------------------------


  station_list_file = 'derived-stations-arctic-subset.txt'

  READ_IGRA_STATION_LIST, station_list_file, station, nrec=nstation
  ;nstation = 1

  WINDOW, 1, XSIZE=700, YSIZE=700
  !P.MULTI=0

  FOR istat=0, nstation-1 DO BEGIN
     MAP_SET, 90., 0, /STEREOGRAPHIC, /ISOTROPIC, $
              LIMIT=[60.,-180.,90.,180], $,
              TITLE=STRTRIM( station[istat].station_name, 2 ) + " (" + $
              STRTRIM( station[istat].station_number, 2 ) + ")"
     MAP_CONTINENTS, /HIRES, /FILL_CONTINENTS, COLOR=150
     MAP_GRID, /BOX, LATDEL=10., LONDEL=45., CHARSIZE=0.75
     PLOTS, station[istat].longitude, station[istat].latitude, PSYM=2, THICK=2
     STOP
  ENDFOR
END
