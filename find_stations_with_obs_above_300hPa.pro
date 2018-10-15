;;**********************************************************************
;; Find stations with values above 300hPa
;;
;; 2010-11-22 A.P.Barrett
;;**********************************************************************

FUNCTION COUNT_OBS_ABOVE_LEVEL, PROFILE, LEVEL
  
  is_above_300 = MAKE_ARRAY( N_ELEMENTS( profile ), /BYTE, VALUE=0 )
  FOR iprof=0l, (N_ELEMENTS( profile ) -1 ) DO BEGIN
     is_above_300[iprof] =  BYTE( TOTAL( profile[iprof].pressure LT 300. AND $
                                         profile[iprof].pressure GT -9999. ) )
  ENDFOR

  result = TOTAL( is_above_300 )

  RETURN, result

END

PRO FIND_STATIONS_WITH_OBS_ABOVE_300hPA

  ;; - Arctic station file
  station_list_file = 'igra_stations_arctic.txt'
  ;; location of data files
  indir = 'data'

  level = 300.

  ;; Get Arctic stations
  read_igra_station_list, station_list_file, station_list, NREC=nstation
  print, station_list[ 100:112 ]
  STOP
  ;nstation = 4

  obs_above_300 = MAKE_ARRAY( nstation, /integer, VALUE=0 )

  FOR istat = 0, nstation-1 DO BEGIN

     PRINT, '% OBS_ABOVE_300hPa: Currently processing ' + $
            STRTRIM( station_list[ istat ].station_name, 2 ) + '-' + $
            STRTRIM( station_list[ istat ].station_number, 2 ) + ' - ' + $
            STRTRIM( istat+1, 2 ) + ' of ' + STRTRIM( nstation, 2 ) + ' stations'

     ;; Get station profiles
     PRINT, '   - Getting data...'
     profile = GET_IGRA_DATA( station_list[ istat ].station_number, INDIR=indir )

     PRINT, '   - Counting obs...'
     obs_above_300[ istat ] = COUNT_OBS_ABOVE_LEVEL( profile, level )

  ENDFOR

  PRINT, ''
  PRINT, ''
  PRINT, '-------------------------------------------------------------'
  PRINT, '# Observations above 300 hPa'
  PRINT, '-------------------------------------------------------------'
  FOR istat = 0, nstation-1 DO BEGIN
     PRINT, FORMAT='(a20,1x,a10,1x,i6)', station_list[ istat ].station_name, $
            station_list[ istat ].station_number, obs_above_300[ istat ]
  ENDFOR

END
