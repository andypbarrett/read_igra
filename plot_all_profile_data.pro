;;----------------------------------------------------------------------
;; Plots all air temperature profile data for a given station
;;
;; 2010-11-23 A.P.Barrett
;;----------------------------------------------------------------------

FUNCTION DIM_MEAN, DATA, DIMENSION, NAN=NAN

  idx = FINITE( data )
  sx  = TOTAL( data, dimension, NAN=NAN )
  sd  = TOTAL( idx, dimension, NAN=NAN )

  result = MAKE_ARRAY( SIZE( sx, /DIMENSIONS ), /FLOAT, $
                       VALUE=!VALUES.F_NAN )
  pos = WHERE( sd GT 0, count )
  IF ( count GT 0 ) THEN result[ pos ] = sx[ pos ] / sd[ pos ]

  RETURN, result

END

PRO PLOT_ALL_DATA, LEVEL, IGRA, OORT_LEVEL, OORT_TAIR, STATION

  dims = SIZE( igra, /DIMENSIONS )
  nlev = dims[ 0 ]
  nrec = dims[ 1 ]
  
  ;; get min, max and mean of oort_tair
  pos = WHERE( oort_tair LE -9999., count )
  IF ( count GT 0 ) THEN oort_tair[pos] = !VALUES.F_NAN
  oort_min = MIN( oort_tair, DIMENSION=2, /NAN ) - 30.
  oort_max = MAX( oort_tair, DIMENSION=2, /NAN ) + 30.
  oort_avg = DIM_MEAN( oort_tair, 2, /NAN )

  noort = N_ELEMENTS( oort_level )
  FOR ilev=0, noort-1 DO PRINT, FORMAT='(f5.0,3(1x,f8.2))', oort_level[ilev], oort_min[ilev], $
                                oort_avg[ ilev ], oort_max[ilev]

  pmin = MAXVAL( level )
  pmax = MINVAL( level[ WHERE( level GT -9999. ) ] )

  tmin = MINVAL( [ MIN( igra[ WHERE( igra GT -9999. ) ] ), oort_min ], /NAN )
  tmax = MAXVAL( [ MAX( igra ), oort_max ], /NAN )

  station_title = STRTRIM(station.station_name,2) + ' - ' + $
                  STRTRIM(station.station_number,2)


  MAKE_PLOT_TEMPLATE, [ pmin, pmax ], [ tmin, tmax ], $
                      XTITLE='Temperature (!Uo!NC)', $
                      MONTH=month, STATION=station_title

  isvalid = WHERE( level GT -9999. AND igra GT -9999., num_isvalid )
  IF ( num_isvalid GT 0 ) THEN PLOTS, igra[ isvalid ], level[ isvalid ], PSYM = 3

  ;; plot Oort profile
  OPLOT, oort_min, oort_level, line=1, thick=2, color=240
  OPLOT, oort_max, oort_level, line=1, thick=2, color=240
  OPLOT, oort_avg, oort_level, line=0, thick=2, color=240

;  FOR ilev = 0, ( nlev - 1 ) DO PLOTS, igra[ *, ilev ], level[ ilev ], PSYM=2
  
  RETURN

END

PRO BATCH_PLOT_ALL_STATION_DATA

  read_igra_station_list, 'igra_stations_arctic.txt', station
  nstation = n_elements(station)
  nstation = 1

  FOR istat = 0, ( nstation-1 ) DO BEGIN

     PLOT_ALL_PROFILE_DATA, station[istat].station_number

  ENDFOR

  RETURN

END

;;**********************************************************************
;; MAIN Routine
;;**********************************************************************
PRO PLOT_ALL_PROFILE_DATA, STATION_NUMBER, MANDATORY=MANDATORY, $
                           LEVEL=LEVEL

  level = N_ELEMENTS( level ) EQ 0 ? [1000., 850., 700., 500., $
                                      400., 300., 200., 150., $
                                      100., 50., 30., 20., 10., $
                                      7., 5., 3., 2., 1. ] : level

  ;; - Oort gridded temperature profile
  oort_file = 'oort1983_air_temperature.nc'
  ;; - Arctic station file
  station_list_file = 'igra_stations_arctic.txt'
  ;; location of data files
  indir = 'data'
  
  ;; Get Arctic stations
  read_igra_station_list, station_list_file, station_list, NREC=nstation

  ;; Get profile data for station
  PRINT, '  - Reading profile data for station ' + STRTRIM( station_number, 2 )
  profile = GET_IGRA_DATA( station_number, INDIR=indir, /NOCLEANUP, /VERBOSE )

  ;; Get station metadata from station_list_file
  istat = WHERE( STRTRIM( station_list.station_number, 2 ) EQ station_number, numstat )
  IF ( numstat EQ 0 ) THEN BEGIN
     PRINT, '% PLOT_ALL_PROFILE_DATA: Unable to find station metadata in ' + station_list_file
     RETURN
  ENDIF

  ;; Import Oort climatology
  PRINT, '  - Getting Oort climatology...'
  oort_tair = READ_OORT_CLIMATOLOGY( oort_file )

  ;; Get oort profile or nearest grid-cell to station
  PRINT, '  - Extract climatological profile for nearest grid cell in Oort...'
  oort_profile = EXTRACT_PROFILE_CLIMO( oort_tair, station_list[istat].latitude, $
                                        station_list[istat].longitude, $
                                        LEVEL=level )

  IF ( KEYWORD_SET( MANDATORY ) EQ 1 ) THEN BEGIN
     ;; Extract mandatory levels
     CREATE_MANDATORY_LEVEL_ARRAY, data, tair, gph, tdew, wdir, wspd, $
                                   LEVEL=level
  ENDIF

  ;; Do plot
  PLOT_ALL_DATA, profile.pressure, profile.temperature, level, oort_profile, station_list[istat]

END  
  
