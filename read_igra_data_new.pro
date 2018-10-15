;;----------------------------------------------------------------------
;; Tests use of pointers for radiosonde code
;;
;; 2010-08-27 A.P.Barrett
;;----------------------------------------------------------------------

FUNCTION DEFINE_PROFILE_RECORD, MAXLEV=MAXLEV

  IF ( N_ELEMENTS( MAXLEV ) EQ 0 ) THEN MAXLEV = 171

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

  MISSING = -8888.
  struct.major_level_type[ ilev ] = FIX( STRMID( line, 0, 1 ) )
  struct.minor_level_type[ ilev ] = FIX( STRMID( line, 1, 1 ) )

  tmp = FLOAT( STRMID( line, 2, 6 ) )
  IF ( tmp GT MISSING ) THEN struct.pressure[ ilev ] = tmp / 100.
  struct.pressure_flag[ ilev ]    = STRMID( line, 8, 1 )

  tmp = FLOAT( STRMID( line, 9, 5 ) )
  IF ( tmp GT MISSING ) THEN struct.geopot_hgt[ ilev ] = tmp
  struct.geopot_hgt_flag[ ilev ]  = STRMID( line, 14, 1 )

  tmp = FLOAT( STRMID( line, 15, 5 ) )
  IF ( tmp GT MISSING ) THEN struct.temperature[ ilev ] = tmp / 10.
  struct.temperature_flag[ ilev ] = STRMID( line, 20, 1 )

  tmp = FLOAT( STRMID( line, 21, 5 ) )
  IF ( tmp GT MISSING ) THEN struct.dewpoint[ ilev ] = tmp / 10.

  tmp = FLOAT( STRMID( line, 26, 5 ) )
  IF ( tmp GT MISSING ) THEN struct.wind_direction[ ilev ] = tmp

  tmp = FLOAT( STRMID( line, 31, 5 ) )
  IF ( tmp GT MISSING ) THEN struct.wind_speed[ ilev ] = tmp / 10.

  RETURN, struct

END

;; Checks if there are any levels above 300 mb
FUNCTION CLEANUP_PROFILE, profile

  ;; Find number of levels above 300 mb level
  indx = WHERE( profile.pressure GT -9999. AND profile.pressure LT 300., numIndx )
  IF ( numIndx GT 0 ) THEN BEGIN
     ;; Set level record to default values
     profile.major_level_type[ indx ] = 0
     profile.minor_level_type[ indx ] = 0
     profile.pressure[ indx ]         = -9999.99
     profile.pressure_flag[ indx ]    = ''
     profile.geopot_hgt[ indx ]       = -9999.99
     profile.geopot_hgt_flag[ indx ]  = ''
     profile.temperature[ indx ]      = -9999.99
     profile.temperature_flag[ indx ] = ''
     profile.dewpoint[ indx ]         = -9999.99
     profile.wind_direction[ indx ]   = -9999.99
     profile.wind_speed[ indx ]       = -9999.99
  ENDIF
  
  ;; Update number of levels greater than 300 mb
  profile.nlevels = TOTAL( profile.pressure GE 300 )
  
  RETURN, profile

END

;;----------------------------------------------------------------------
;; WEED_PROFILES - cleans-up each profile and then removes profiles that have
;;                 no data below 300 mb
;;----------------------------------------------------------------------
FUNCTION WEED_PROFILES, profile

  nprofile = N_ELEMENTS( profile )

  ;; Clean-up profiles
  FOR iprof = 0l, ( nprofile - 1 ) DO BEGIN

     ;; This a work-around to get rid of profiles with measurements from 
     ;; only above 300 mb.
     profile[ iprof ] = CLEANUP_PROFILE( profile[ iprof ] )

  ENDFOR

  indx = WHERE( profile.nlevels GT 0, numIndx )
  IF ( numIndx GT 0 ) THEN BEGIN

     profile = profile[ indx ]

  ENDIF

  RETURN, profile

END

;;**********************************************************************
;; MAIN CODE
;;**********************************************************************

PRO READ_IGRA_DATA_NEW, FILE, RESULT, NOCLEANUP=NOCLEANUP, VERBOSE=VERBOSE, $
                        MaximumLevel=MaximumLevel 

  DEBUG = 0

  IF ( N_PARAMS() NE 2 ) THEN BEGIN
     PRINT, '% USAGE: READ_IGRA_DATA, FILE, RESULT'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( FILE ) EQ 0 ) THEN BEGIN
     PRINT, '% READ_IGRA_DATA: Argument FILE is not defined'
     RETURN
  ENDIF

  ;; Set maximum level if not set - default get everything
  MaximumLevel = N_ELEMENTS( MaximumLevel ) EQ 0 ? 0. : MaximumLevel

  result = -1

  MAXLEV = 100
  MAXREC = 60000l
  result = REPLICATE( DEFINE_PROFILE_RECORD( MAXLEV=MAXLEV ), MAXREC )

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

        IF ( KEYWORD_SET( verbose ) EQ 1 ) THEN BEGIN
           PRINT, 'Getting header information for record ' + $
                  STRTRIM( irec, 2 ) + '...'
        ENDIF

        ;; Initialize counter for levels
        ilev = 0

        ;; Read Header information
        result[ irec ] = PARSE_HEADER( line, result[ irec ] )

        IF ( result[ irec ].nlevels GT max_level ) THEN BEGIN
           max_level = result[irec].nlevels
        ENDIF

        IF ( DEBUG EQ 1 ) THEN BEGIN
           PRINT, FORMAT=hdr_fmt, $
                  iheader, result[ irec ].year, result[ irec ].month, $
                  result[ irec ].day, result[ irec ].obs_hour, $
                  result[ irec ].release_time, result[ irec ].nlevels
        ENDIF 

     ENDIF ELSE BEGIN

        IF ( ilev GE MAXLEV ) THEN BEGIN
           
           IF ( KEYWORD_SET( verbose ) EQ 1 ) THEN BEGIN
              PRINT, '% READ_IGRA_DATA_NEW: Number of levels exceeds MAXLEV'
              PRINT, '                      skipping level record'
           ENDIF
              
           ;IF ( result[ irec ].pressure[ ilev - 1 ] GE MaximumLevel ) THEN BEGIN
           ;   PRINT, '% READ_IGRA_DATA_NEW: WARNING! no 300 hPa level found'
           ;ENDIF

        ENDIF ELSE BEGIN
           
           IF ( KEYWORD_SET( verbose ) EQ 1 ) THEN BEGIN
              PRINT, '   ... getting level information for level ' + $
                     STRTRIM( ilev, 2 )
           ENDIF

           ;; Only get levels below Maximum level - added: APB 2010-09-01
           IF ( ilev GT 0 ) THEN BEGIN
              IF ( result[ irec ].pressure[ ilev - 1 ] GE MaximumLevel ) THEN BEGIN
                 result[ irec ] = PARSE_DATA_RECORD( line, ilev, result[ irec ] )
                 ilev = ilev + 1
              ENDIF
           ENDIF ELSE BEGIN
              result[ irec ] = PARSE_DATA_RECORD( line, ilev, result[ irec ] )
              ilev = ilev + 1
           ENDELSE

           ;;result[ irec ] = PARSE_DATA_RECORD( line, ilev, result[ irec ] )
           ;;ilev = ilev + 1

        ENDELSE

     ENDELSE

  ENDWHILE
  
  CLOSE, U
  FREE_LUN, U

  nrec = irec + 1
  result = result[ 0 : (nrec-1) ]

  ;; Remove profiles with no data
  IF ( KEYWORD_SET( NOCLEANUP ) EQ 0 ) THEN BEGIN
     IF ( DEBUG EQ 1 ) THEN BEGIN
        PRINT, 'Here... but I should not be: NOCLEANUP=' + $
               STRTRIM( KEYWORD_SET( NOCLEANUP ), 2 )
     ENDIF
     result = WEED_PROFILES( result )
  ENDIF

  IF ( DEBUG EQ 1 ) THEN BEGIN
     HELP, result
     HELP, result, /struct
  ENDIF

  RETURN

END

