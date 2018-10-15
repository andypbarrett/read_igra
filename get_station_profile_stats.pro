;;----------------------------------------------------------------------
;; Creates a list of Arctic rawinsonde stations,updates the start and
;; end dates of the station record and writes to a text file.
;;
;; 2010-08-24 A.P.Barrett
;;----------------------------------------------------------------------
PRO WRITE_STATIONS, FILO, DATA, first_year, last_year, num_records, $
                    num_rec1979, max_level

;;  FMT = '(a2,2x,a5,2x,a35,1x,f6.2,1x,f7.2,1x,i4,1x,a1,a1,a1,2x,i4,1x,i4)'
  FMT = '(a2,2x,a5,2x,a35,1x,f6.2,1x,f7.2,1x,i4,1x,a1,a1,a1,2x,i4,1x,i4,' + $
        '" - ",i4,1x,i4,1x,i7," - ",i7," - ",i3)'

  nrec = N_ELEMENTS( data )

  ;; Open file
  OPENW, U, filo, /GET_LUN

  ;; Write records
  FOR irec = 0, ( nrec - 1 ) DO PRINTF, U, FORMAT=FMT, data[ irec ], $
                                       first_year[irec], last_year[irec], $
                                       num_records[irec], num_rec1979[irec], $
                                        max_level[irec]

  ;;Close file
  CLOSE, U
  FREE_LUN, U

  RETURN

END
     
PRO PROFILE_STATS, PROFILE, FirstYear, LastYear, MaxLevel, nProfile, nValid79

  nProfile = N_ELEMENTS( profile )

  nValid    = 0
  nValid79  = 0
  MaxLevel  = 0
  FirstYear = 0
  LastYear  = 0

  FOR iprof = 0l, ( nProfile - 1 ) DO BEGIN

     ;; Find levels below 300 mb
     thislevel = WHERE( profile[ iprof ].pressure GT 300., numLevel )
     
     IF ( numLevel GT 0 ) THEN BEGIN

        ;; Ignore profile if there are no temperature or dewpoint 
        ;; depressions
        nTemperature = TOTAL( profile[ iprof ].temperature[ thisLEVEL ] GT $
                              -9999. )
        nDewpoint    = TOTAL( profile[ iprof ].temperature[ thisLEVEL ] GT $
                              -9999. )

        IF ( nTemperature EQ 0 OR nDewpoint EQ 0 ) THEN CONTINUE

        IF ( iprof EQ 0 ) THEN BEGIN

           FirstYear = profile[ iprof ].year
           LastYear  = profile[ iprof ].year
           MaxLevel  = numLevel

        ENDIF ELSE BEGIN

           IF ( profile[ iprof ].year LT FirstYear ) THEN BEGIN
              FirstYear = profile[ iprof ].year
           ENDIF

           IF ( profile[ iprof ].year GT LastYear ) THEN BEGIN
              LastYear = profile[ iprof ].year
           ENDIF

           IF ( numLevel GT MaxLevel ) THEN BEGIN
              MaxLevel = numLevel
           ENDIF

        ENDELSE

        IF ( profile[ iprof ].year GT 1979 ) THEN nValid79 = nValid79 + 1

        nValid = nValid + 1

     ENDIF

  ENDFOR
  
  nProfile = nValid
  
  RETURN

END
     
PRO GET_STATION_PROFILE_STATS

  diri = '/raid/IGRA'
  fili = 'igra-stations.txt'

  diro = '/raid/IGRA'
  filo = 'igra_stations_arctic.txt'

  data_dir = '/raid/IGRA/data'

  ;; Get list of Arctic stations
  arctic_stations = GET_ARCTIC_STATIONS( diri + '/' + fili, COUNT=num_station )

  first_year  = MAKE_ARRAY( num_station, /INTEGER, VALUE=-9999 )
  last_year   = MAKE_ARRAY( num_station, /INTEGER, VALUE=-9999 )
  num_records = MAKE_ARRAY( num_station, /LONG, VALUE=-9999 )
  num_rec1979 = MAKE_ARRAY( num_station, /LONG, VALUE=-9999 )
  max_level_arr = MAKE_ARRAY( num_station, /LONG, VALUE=-9999 )
  
  ;; Loop through stations and update first and last years of records
  FOR istat = 0, ( num_station - 1 ) DO BEGIN

     dataFile = data_dir + '/' + $
                arctic_stations[ istat ].station_number + '.dat'
     status = FILE_TEST( dataFile + '.gz' )
     IF ( status EQ 1 ) THEN BEGIN

        PRINT, FORMAT='("Getting first and last record dates from ",a)', $
               dataFile

        SPAWN, 'gunzip ' + dataFile + '.gz'

        READ_IGRA_DATA_NEW, dataFile, profile

        PROFILE_STATS, profile, year0, year1, max_level, nProfile, nrec79

        SPAWN, 'gzip ' + dataFile

        first_year[ istat ] = year0
        last_year[ istat ]  = year1
        num_records[ istat ] = nProfile
        num_rec1979[ istat ] = nrec79
        max_level_arr[ istat ] = max_level

        ;;arctic_stations[istat].year_begin = y0
        ;;arctic_stations[istat].year_end = y1
        
     ENDIF ELSE BEGIN

        PRINT, FORMAT='(a," does not exist!")', dataFile
        STOP

     ENDELSE
     
  ENDFOR

  ;; Write stations to file
  WRITE_STATIONS, diro + '/' + filo, arctic_stations, first_year, last_year, $
                  num_records, num_rec1979, max_level_arr

END


  
