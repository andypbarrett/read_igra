;;----------------------------------------------------------------------
;; PLOT_ARCTIC_STATIONS - plots the location of Arctic rawinsonde stations
;;
;; 2010-08-23 A.P.Barrett
;;
;; 2010-11-02 A.P.Barrett  refactored to break calculation of number of 
;;                         years of observations after 1979, writing list of
;;                         stations and plotting into separate functions.
;;
;;----------------------------------------------------------------------
;; Compute the number of years after a given date
FUNCTION COUNT_YEARS, STRUCT, BASE_YEAR=BASE_YEAR

  ;; Define BASE_YEAR if not defined
  base_year = ( N_ELEMENTS( base_year ) GT 0 ) ? base_year : 1979

  ;; Get number of station records
  num_rec = N_ELEMENTS( struct )

  ;; Define array to hold number of years of obs
  result = MAKE_ARRAY( num_rec, /INTEGER, VALUE=0)

  ;; Set first_year to latest of 1979 or first year of record  
  first_year = struct.year_begin
  pos = WHERE( struct.year_begin LT base_year, count )
  IF ( count GT 0 ) THEN BEGIN
     first_year[ pos ] = base_year
  ENDIF

  ;; Calculate difference between last year and first year
  pos = WHERE( struct.year_end GT base_year, num_pos )
  IF ( num_pos GT 0 ) THEN BEGIN
     result[pos] = struct[ pos ].year_end - first_year[ pos ]
  ENDIF

  RETURN, result

END  ;; End of COUNT_YEAR

;; - writes station data and results to standard out
PRO WRITE_RESULT, STRUCT, NYEAR, NYEAR_SAT, FILEOUT=FILEOUT

  IF ( N_ELEMENTS( FILEOUT ) GT 0 ) THEN BEGIN
     OPENW, U, FILEOUT, /GET_LUN
  ENDIF

  num_rec = N_ELEMENTS( struct )

  fmt = '(i3,2x,a5,1x,a20,f6.2,1x,f7.2,1x,i4,2(1x,i4),1x,i5,1x,i5)'

  FOR istat = 0, num_rec-1 DO BEGIN

     IF ( N_ELEMENTS( FILEOUT ) GT 0 ) THEN BEGIN
        
        PRINTF, U, FORMAT=fmt, istat, $
                struct[ istat ].station_number, $
                struct[ istat ].station_name, $
                struct[ istat ].latitude, $
                struct[ istat ].longitude, $
                struct[ istat ].elevation, $
                struct[ istat ].year_begin, $
                struct[ istat ].year_end, $
                nyear[istat], $
                nyear_sat[istat]

     ENDIF ELSE BEGIN
        
        PRINT, FORMAT=fmt, istat, $
               struct[ istat ].station_number, $
               struct[ istat ].station_name, $
               struct[ istat ].latitude, $
               struct[ istat ].longitude, $
               struct[ istat ].elevation, $
               struct[ istat ].year_begin, $
               struct[ istat ].year_end, $
               nyear[istat], $
               nyear_sat[istat]

     ENDELSE

  ENDFOR

  IF ( N_ELEMENTS( FILEOUT ) GT 0 ) THEN BEGIN
     CLOSE, U
     FREE_LUN, U
  ENDIF
  
  RETURN

END

;; - Plot map of stations showing record length by proportional circles
PRO PLOT_STATION, STRUCT, NYEAR, NYEAR_SAT

  num_rec = N_ELEMENTS( struct )

  ;; Define a user symbol
  n = 20
  a = FINDGEN(n) * (!PI*2/FLOAT(n-1))  
  USERSYM, COS(A), SIN(A), /FILL  

  ;; Define color - no satellite (post-1979) data blue, post-1979 data red
  color = MAKE_ARRAY( num_rec, /INTEGER, VALUE=60 )
  color[ WHERE( nyear_sat GT 0 ) ] = 240

  ;; Define size of symbols
  max_year = MAX( nyear_sat )
  size_scl = MAKE_ARRAY( num_rec, /INTEGER, VALUE=1 )
  pos = WHERE( nyear_sat GT 0, count )
  IF ( count GT 0 ) THEN BEGIN
     size_scl[ pos ] = 3 * FLOAT( nyear_sat[ pos ] ) / FLOAT( max_year )
  ENDIF

  ;; Make plot
  LOADCT, 39
  MAP_SET, 90., 0., /ORTHOGRAPHIC, /ISOTROPIC, LIMIT=[60.,0.,90.,360.], $
           /NOBORDER, /HORIZON, POSITION=[0.15,0.05,0.95,0.95]
  MAP_CONTINENTS, /HIRES, /FILL
  MAP_GRID, LATDEL=15., LONDEL=45.

  FOR istat = 0, ( num_rec - 1 ) DO BEGIN
     PLOTS, struct[istat].longitude, struct[istat].latitude, PSYM=8, $
            COLOR=color[istat], SYMSIZE=size_scl[istat]
  ENDFOR

  ;; Add legend
  leg_size = 3 * [ 28., 21., 14., 7. ] / FLOAT( max_year )
  XX = 0.05
  YY = [0.95,0.9,0.85,0.8]
  FOR i=0, 3 DO PLOTS, XX, YY[i], PSYM=8, COLOR=240, SYMSIZE=leg_size[i], $
         /NORMAL

  print, min( size_scl ), max( size_scl )

  RETURN

END

;;**********************************************************************
;; MAIN ROUTINE
;;**********************************************************************
PRO PLOT_ARCTIC_STATIONS

  ;; Path and file for arctic stations
  diri = '/raid/IGRA'  ; on arctic6
  file = 'igra-stations.txt'

  station = GET_ARCTIC_STATIONS( diri + '/' + file, count=num_station )

  ;; Compute length of record
  nyear = station.year_end - station.year_begin

  ;; Get number of years of obs after 1979
  nyear_sat = COUNT_YEARS( station, BASE_YEAR=1979 )
  
  WRITE_RESULT, station, nyear, nyear_sat, $
                FILEOUT='igra_stations_arctic_yearcount.txt'

  PLOT_STATION, station, nyear, nyear_sat

  RETURN

END
                        
