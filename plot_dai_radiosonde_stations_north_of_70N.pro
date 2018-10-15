;;----------------------------------------------------------------------
;; Makes a map of radiosonde stations north of 70 N.
;;
;; 2011-07-11 A.P.Barrett
;;----------------------------------------------------------------------

  MAKESYM, 10

  station_file = 'dai_radiosonde_polar_stations_qc_subset.txt'
  READ_IGRA_STATION_LIST, station_file, station, NREC=nstat

;  idx = WHERE( station.latitude GE 60. and station.latitude LT 70, nstat60 )
  idx = WHERE( station.latitude GE 70., nstat60 )
  IF ( nstat60 LE 0 ) THEN BEGIN
     PRINT, '% PLOT_DAI_RADIOSONDE_STATIONS_NORTH_OF_70N: No stations exist above 70N'
     RETURN
  ENDIF

  MAP_SET, 90., 0., /ORTHOGRAPHIC, LIMIT=[65.,-180.,90.,180], /ISOTROPIC
  MAP_CONTINENTS, /MEDRES, /FILL, COLOR=150
  MAP_GRID

  ;; Plot symbols for stations
  PLOTS, station[idx].longitude, station[idx].latitude, PSYM=8, SYMSIZE=2

  ;; Plot numbers - uses /normal coordinates
  xx =       [0.45, 0.615, 0.43,  0.37, 0.79, 0.37, 0.38,  0.315, 0.215]
  yy =       [0.21, 0.273, 0.305, 0.22, 0.68, 0.77, 0.445, 0.49,  0.525]
  indexstr = ['1',  '2',   '3',   '4',  '5',  '6',  '7',   '8',   '9']
  XYOUTS, xx, yy, indexstr, CHARSIZE=2, CHARTHICK=3, /NORMAL

  x0 = 0.05
  y0 = 0.85
  dy = 0.017
  FOR istat=0, nstat60-1 DO BEGIN
     XYOUTS, x0, y0-(istat*dy), STRTRIM( istat+1, 2) + ' ' + $
             STRTRIM( station[idx[istat]].station_name ), /NORMAL
  ENDFOR

;  FOR ii = 1, 9 DO PLOTS, [0.,1.], ii*.1, /NORMAL, LINE=1, THICK=2
;  FOR ii = 1, 9 DO PLOTS, ii*.1, [0.,1.], /NORMAL, LINE=1, THICK=2

END
 
