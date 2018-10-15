;;----------------------------------------------------------------------
;; Tests use of pointers for radiosonde code
;;
;; 2010-08-27 A.P.Barrett
;;----------------------------------------------------------------------

FUNCTION DEFINE_PROFILE_RECORD

  MAXLEV = 265

  data_record = {station_number: '', $
                 year: 0l, $
                 month: 0l, $
                 day: 0l, $
                 obs_hour: 0l, $
                 release_time: 0l, $
                 nlevels: 0l, $
                 major_level_type: MAKE_ARRAY( MAXLEV, /BYTE, VALUE=0 ), $
                 minor_level_type: MAKE_ARRAY( MAXLEV, /BYTE, VALUE=0 ), $
                 pressure:         MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-9999.99 ), $
                 pressure_flag:    MAKE_ARRAY( MAXLEV, /STRING, VALUE='' ), $
                 geopot_hgt:       MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-9999.99 ), $
                 geopot_hgt_flag:  MAKE_ARRAY( MAXLEV, /STRING, VALUE='' ), $
                 temperature:      MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-9999.99 ), $
                 temperature_flag: MAKE_ARRAY( MAXLEV, /STRING, VALUE='' ), $
                 dewpoint:         MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-9999.99 ), $
                 wind_direction:   MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-9999.99 ), $
                 wind_speed:       MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-9999.99 )}

  RETURN, data_record

END

FUNCTION PARSE_HEADER, LINE, STRUCT

  struct.station_number = STRMID( line, 1, 5 )
  struct.year           = LONG( STRMID( line, 6, 4 ) )
  struct.month          = LONG( STRMID( line, 10, 2 ) )
  struct.day            = LONG( STRMID( line, 12, 2 ) )
  struct.obs_hour       = LONG( STRMID( line, 14, 2 ) )
  struct.release_time   = LONG( STRMID( line, 16, 4 ) )
  struct.nlevels        = LONG( STRMID( line, 20, 4 ) )

  RETURN, struct

END

FUNCTION PARSE_DATA_RECORD, LINE, ILEV, STRUCT

  struct.major_level_type = FIX( STRMID( line, 0, 1 ) )
  struct.minor_level_type = FIX( STRMID( line, 1, 1 ) )
  struct.pressure         = FLOAT( STRMID( line, 2, 6 ) ) / 100.
  struct.pressure_flag    = STRMID( line, 8, 1 )
  struct.geopot_hgt       = FLOAT( STRMID( line, 9, 5 ) )
  struct.geopot_hgt_flag  = STRMID( line, 14, 1 )
  struct.temperature      = FLOAT( STRMID( line, 15, 5 ) ) / 10.
  struct.temperature_flag = STRMID( line, 20, 1 )
  struct.dewpoint         = FLOAT( STRMID( line, 21, 5 ) ) / 10.
  struct.wind_direction   = FLOAT( STRMID( line, 26, 5 ) )
  struct.wind_speed       = FLOAT( STRMID( line, 31, 5 ) ) / 10.

  RETURN, struct

END

;;**********************************************************************
;; MAIN CODE
;;**********************************************************************

PRO READ_IGRA_DATA_NEW, FILE, RESULT

  DEBUG = 0

  IF ( N_PARAMS() NE 2 ) THEN BEGIN
     PRINT, '% USAGE: READ_IGRA_DATA, FILE, RESULT'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( FILE ) EQ 0 ) THEN BEGIN
     PRINT, '% READ_IGRA_DATA: Argument FILE is not defined'
     RETURN
  ENDIF

  result = -1

  MAXREC = 60000l
  result = REPLICATE( DEFINE_PROFILE_RECORD(), MAXREC )

  ;; Open file
  OPENR, U, file, /GET_LUN

  irec = -1l
  iheader = 0l
  max_level = 0l
  line = ''
  WHILE ( NOT EOF( U ) ) DO BEGIN

     ;; Read line of file
     READF, U, line

     ;; Check if Header Record
     IF ( STREGEX( line, '^#', /BOOLEAN ) EQ 1 ) THEN BEGIN

        irec = irec + 1

        IF ( irec GE MAXREC ) THEN BREAK

        PRINT, 'Getting header information for record ' + $
               STRTRIM( irec, 2 ) + '...'

        ;; Initialize counter for levels
        ilev = 0

        ;; Read Header information
        result[ irec ] = PARSE_HEADER( line, result[ irec ] )

        IF ( result[ irec ].nlevels GT max_level ) THEN BEGIN
           max_level = header_struct.nlevels
        ENDIF

        IF ( DEBUG EQ 1 ) THEN BEGIN
           PRINT, FORMAT=hdr_fmt, $
                  iheader, result[ irec ].year, result[ irec ].month, $
                  result[ irec ].day, result[ irec ].obs_hour, $
                  result[ irec ].release_time, result[ irec ].nlevels
        ENDIF 

     ENDIF ELSE BEGIN

        PRINT, '   ... getting level information for level ' + $
               STRTRIM( ilev, 2 )

        ;; Assume it is level data
        ;; Parse level record
        ;;print, line
        ;;HELP, header_struct.level_data[ilev]
        ;;HELP, header_struct.level_data[ilev], /STRUCT
        ;;PRINT, N_ELEMENTS( header_struct.level_data[ilev] )
        ;;RETURN

        result[ irec ].level_data[ ilev ] = PARSE_DATA_RECORD( line, $
                                   result[ irec ].level_data[ilev] )

        ilev = ilev + 1

     ENDELSE

  ENDWHILE
  
  nheader = iheader

  HELP, result
  HELP, result, /struct

  RETURN

END

