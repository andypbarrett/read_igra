;;----------------------------------------------------------------------
;; READ_IGRA_STATION_LIST - reads the stations in igra-stations.txt.  
;; Returns a structure.
;;
;; 2010-08-19 Andrew P. Barrett
;;----------------------------------------------------------------------

FUNCTION DEFINE_STRUCT

  struct = {country_code: '', $
            station_number: '', $
            station_name: '', $
            latitude: 0.0, $
            longitude: 0.0, $
            elevation: 0.0, $
            GUAN_code: '', $
            LKS_code: '', $
            composite_code: '', $
            year_begin: 0l, $
            year_end: 0l}

  RETURN, struct

END

FUNCTION PARSE_RECORD, LINE, STRUCT

  IF ( N_PARAMS() NE 2 ) THEN BEGIN
     PRINT, 'USAGE: PARSE_RECORD, LINE, STRUCT'
     RETURN, -1
  ENDIF

  IF ( N_ELEMENTS( line ) EQ 0 ) THEN BEGIN
     PRINT, '% PARSE_RECORD: Argument line is not defined'
     RETURN, -1
  ENDIF

;  IF ( N_ELEMENTS( struct ) EQ 0 ) THEN BEGIN
;     PRINT, '% PARSE_RECORD: Argument struct is not defined'
;     RETURN
;  ENDIF

  struct.country_code   = STRMID( line, 0,  2 )
  struct.station_number = STRMID( line, 4,  5 )
  struct.station_name   = STRMID( line, 11, 36 )
  struct.latitude       = FLOAT( STRMID( line, 47, 6 ) )
  struct.longitude      = FLOAT( STRMID( line, 54, 7 ) )
  struct.elevation      = FLOAT( STRMID( line, 62, 4 ) )
  struct.GUAN_code      = STRMID( line, 67, 1 )
  struct.LKS_code       = STRMID( line, 68, 1 )
  struct.composite_code = STRMID( line, 69, 1 )
  struct.year_begin     = FIX( STRMID( line, 72, 4 ) )
  struct.year_end       = FIX( STRMID( line, 77, 4 ) )
  
  RETURN, struct

END

PRO READ_IGRA_STATION_LIST, FILE, RESULT, NREC=NREC

  DEBUG = 0

  IF ( N_PARAMS() NE 2 ) THEN BEGIN
     PRINT, '% USAGE: READ_IGRA_STATION_LIST, FILE, RESULT'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( FILE ) EQ 0 ) THEN BEGIN
     PRINT, '% READ_IGRA_STATION_LIST: Argument FILE is not defined'
     RETURN
  ENDIF
  
  ;; Define structure to hold data
  MAXREC = 1600
  result = REPLICATE( DEFINE_STRUCT(), MAXREC )

  ;; Open file
  OPENR, U, file, /GET_LUN

  line = ''
  istat = 0l
  WHILE ( NOT EOF(U) ) DO BEGIN

     ;; Read a line
     READF, U, line

     IF ( STREGEX( line, '^[A-Z]{2} +[0-9]{5}', /BOOLEAN ) EQ 1 ) THEN BEGIN
 
        ;; Parse line
        result[ istat ] = PARSE_RECORD( line, result[ istat ] )

        istat = istat + 1
        
        IF ( istat GE MAXREC ) THEN BEGIN
           PRINT, '% READ_IGRA_STATION_LIST: Number of records in ' + $
                  file + ' exceeds MAXREC (' + STRTRIM( MAXREC, 2 ) + ')'
           PRINT, '                          Returning incomplete file'
           BREAK
        ENDIF
        
                                ;IF ( istat EQ 10 ) THEN BREAK
     ENDIF

  ENDWHILE

  CLOSE, U
  FREE_LUN, U

  nrec = istat

  ;; Truncate result
  result = result[ 0 : nrec-1 ]

  IF ( DEBUG ) THEN BEGIN
     FOR ii = 0, ( nrec - 1 ) DO BEGIN
        PRINT, FORMAT='(a2,"-",a5,"-",a36,"-",f6.2,"-",f7.2,"-",f6.1,"-",'+$
               'a1,"-",a1,"-",a1,"-",i4,"-",i4)', result[ ii ]
     ENDFOR
  ENDIF

  RETURN

END
