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
     
PRO UPDATE_ARCTIC_STATION_LIST

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

        GET_FIRST_AND_LAST_RECORD_DATE, dataFile, y0, m0, d0, h0, $
                                        y1, m1, d1, h1, nrec, nrec79, $
                                        max_level

        SPAWN, 'gzip ' + dataFile

        first_year[ istat ] = y0
        last_year[ istat ]  = y1
        num_records[ istat ] = nrec
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


  
