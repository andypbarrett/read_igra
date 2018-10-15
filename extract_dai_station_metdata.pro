;;----------------------------------------------------------------------
;; Gets the station metadata for the polar stations in the Dai dataset
;;
;; 2010-06-29 A.P.Barrett
;;----------------------------------------------------------------------

DEBUG = 0

;; get list of station ids
FileList   = FINDFILE( '*.month.nc', count=nfile )
station_id = STREGEX( fileList, '[0-9]{5}', /extract )

;; Get metadata for whole dataset
metadata_File = 'igra-stations-withMonthlyDataC.txt'
READ_IGRA_STATION_LIST, metadata_File, station, NREC=nrec

;; Get indices of Dai stations in IGRA station list 
idx = MAKE_ARRAY( nfile, /LONG, VALUE=-1 )
nFound = 0
FOR iFile=0, nfile-1 DO BEGIN
   tmp_idx = WHERE( station_id[iFile] EQ station.station_number, isIdx )
   IF ( isIdx GT 0 ) THEN BEGIN
      idx[nFound] = tmp_idx
      nFound = nFound + 1
   ENDIF
ENDFOR

IF ( DEBUG ) THEN BEGIN
   FOR ii = 0, ( nFound - 1 ) DO BEGIN
      PRINT, FORMAT='(a2,"-",a5,"-",a36,"-",f6.2,"-",f7.2,"-",f6.1,"-",'+$
             'a1,"-",a1,"-",a1,"-",i4,"-",i4)', station[ idx[ii] ]
   ENDFOR
ENDIF

new_station = station[ idx ]

touse = WHERE( new_station.year_begin LE 1979 and new_station.year_end GE 2009, nuse )

WRITE_IGRA_STATION_LIST, 'dai_radiosonde_polar_stations.txt', new_station[ touse ]

END
 
