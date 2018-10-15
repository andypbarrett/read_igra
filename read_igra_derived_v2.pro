;;----------------------------------------------------------------------
;; Tests use of pointers for radiosonde code
;;
;; 2010-08-27 A.P.Barrett
;;----------------------------------------------------------------------

FUNCTION DEFINE_DERIVED_RECORD, MAXLEV=MAXLEV

  IF ( N_ELEMENTS( MAXLEV ) EQ 0 ) THEN MAXLEV = 171

  data_record = {station_number: '', $
                 year:         0l, $
                 month:        0l, $
                 day:          0l, $
                 obs_hour:     0l, $
                 release_time: 0l, $
                 nlevels:      0l, $
                 PW:           -99999.99, $
                 INVPRESS:     -99999.99, $
                 INVHGT:       -99999.99, $
                 INVTEMPDIF:   -99999.99, $
                 MIXPRESS:     -99999.99, $
                 MIXHGT:       -99999.99, $
                 FRZPRESS:     -99999.99, $
                 FRZHGT:       -99999.99, $
                 LCLPRESS:     -99999.99, $
                 LCLHGT:       -99999.99, $
                 LFCPRESS:     -99999.99, $
                 LFCHGT:       -99999.99, $
                 LNBPRESS:     -99999.99, $
                 LNBHGT:       -99999.99, $
                 LI:           -99999.99, $
                 SI:           -99999.99, $
                 KI:           -99999.99, $
                 TTI:          -99999.99, $
                 CAPE:         -99999.99, $
                 CIN:          -99999.99, $
                 PRESS:     MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-99999.99 ), $
                 OBSGPH:    MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-99999.99 ), $
                 CALCGPH:   MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-99999.99 ), $
                 TEMP:      MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-99999.99 ), $
                 TEMPGRAD:  MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-99999.99 ), $
                 PTEMP:     MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-99999.99 ), $
                 PTEMPGRAD: MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-99999.99 ), $
                 VTEMP:     MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-99999.99 ), $
                 VTEMPGRAD: MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-99999.99 ), $
                 VAPPRESS:  MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-99999.99 ), $
                 SATVAP:    MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-99999.99 ), $
                 RH:        MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-99999.99 ), $
                 RHGRAD:    MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-99999.99 ), $
                 UWND:      MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-99999.99 ), $
                 UWNDGRAD:  MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-99999.99 ), $
                 VWND:      MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-99999.99 ), $
                 VWNDGRAD:  MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-99999.99 ), $
                 N:         MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-99999.99 ) }
               
  RETURN, data_record

END

FUNCTION PARSE_DERIVED_HEADER, LINE, STRUCT

  struct.station_number = STRMID( line, 1, 5 )
  struct.year           = LONG( STRMID( line, 6, 4 ) )
  struct.month          = LONG( STRMID( line, 10, 2 ) )
  struct.day            = LONG( STRMID( line, 12, 2 ) )
  struct.obs_hour       = LONG( STRMID( line, 14, 2 ) )
  struct.release_time   = LONG( STRMID( line, 16, 4 ) )
  struct.nlevels        = LONG( STRMID( line, 20, 4 ) )
  struct.PW             = FLOAT( STRMID( line, 24, 6 ) )
  struct.INVPRESS       = FLOAT( STRMID( line, 30, 6 ) )
  struct.INVHGT         = FLOAT( STRMID( line, 36, 6 ) )
  struct.INVTEMPDIF     = FLOAT( STRMID( line, 42, 6 ) )
  struct.MIXPRESS       = FLOAT( STRMID( line, 48, 6 ) )
  struct.MIXHGT         = FLOAT( STRMID( line, 54, 6 ) )
  struct.FRZPRESS       = FLOAT( STRMID( line, 60, 6 ) )
  struct.FRZHGT         = FLOAT( STRMID( line, 66, 6 ) )
  struct.LCLPRESS       = FLOAT( STRMID( line, 72, 6 ) )
  struct.LCLHGT         = FLOAT( STRMID( line, 78, 6 ) )
  struct.LFCPRESS       = FLOAT( STRMID( line, 84, 6 ) )
  struct.LFCHGT         = FLOAT( STRMID( line, 90, 6 ) )
  struct.LNBPRESS       = FLOAT( STRMID( line, 96, 6 ) )
  struct.LNBHGT         = FLOAT( STRMID( line, 102, 6 ) )
  struct.LI             = FLOAT( STRMID( line, 108, 6 ) )
  struct.SI             = FLOAT( STRMID( line, 114, 6 ) )
  struct.KI             = FLOAT( STRMID( line, 120, 6 ) )
  struct.TTI            = FLOAT( STRMID( line, 126, 6 ) )
  struct.CAPE           = FLOAT( STRMID( line, 132, 6 ) )
  struct.CIN            = FLOAT( STRMID( line, 138, 6 ) )

  RETURN, struct

END

FUNCTION PARSE_DERIVED_RECORD, LINE, ILEV, STRUCT

  MISSING = -99999.

  tmp = FLOAT( STRMID( line, 0, 7 ) )
  IF ( tmp GT MISSING ) THEN struct.PRESS[ ilev ] = tmp / 100.

  tmp = FLOAT( STRMID( line, 8, 7 ) )
  IF ( tmp GT MISSING ) THEN struct.OBSGPH[ ilev ] = tmp

  tmp = FLOAT( STRMID( line, 16, 7 ) )
  IF ( tmp GT MISSING ) THEN struct.CALCGPH[ ilev ] = tmp

  tmp = FLOAT( STRMID( line, 24, 7 ) )
  IF ( tmp GT MISSING ) THEN struct.TEMP[ ilev ] = tmp / 10.

  tmp = FLOAT( STRMID( line, 32, 7 ) )
  IF ( tmp GT MISSING ) THEN struct.TEMPGRAD[ ilev ] = tmp / 10.

  tmp = FLOAT( STRMID( line, 40, 7 ) )
  IF ( tmp GT MISSING ) THEN struct.PTEMP[ ilev ] = tmp / 10.

  tmp = FLOAT( STRMID( line, 48, 7 ) )
  IF ( tmp GT MISSING ) THEN struct.PTEMPGRAD[ ilev ] = tmp / 10.

  tmp = FLOAT( STRMID( line, 40, 7 ) )
  IF ( tmp GT MISSING ) THEN struct.VTEMP[ ilev ] = tmp / 10.

  tmp = FLOAT( STRMID( line, 57, 7 ) )
  IF ( tmp GT MISSING ) THEN struct.VTEMPGRAD[ ilev ] = tmp / 10.

  tmp = FLOAT( STRMID( line, 64, 7 ) )
  IF ( tmp GT MISSING ) THEN struct.VAPPRESS[ ilev ] = tmp / 1000.

  tmp = FLOAT( STRMID( line, 72, 7 ) )
  IF ( tmp GT MISSING ) THEN struct.SATVAP[ ilev ] = tmp / 1000.

  tmp = FLOAT( STRMID( line, 80, 7 ) )
  IF ( tmp GT MISSING ) THEN struct.RH[ ilev ] = tmp / 10.

  tmp = FLOAT( STRMID( line, 96, 7 ) )
  IF ( tmp GT MISSING ) THEN struct.RHGRAD[ ilev ] = tmp / 10.

  tmp = FLOAT( STRMID( line, 104, 7 ) )
  IF ( tmp GT MISSING ) THEN struct.UWND[ ilev ] = tmp / 10.

  tmp = FLOAT( STRMID( line, 112, 7 ) )
  IF ( tmp GT MISSING ) THEN struct.UWNDGRAD[ ilev ] = tmp / 10.

  tmp = FLOAT( STRMID( line, 120, 7 ) )
  IF ( tmp GT MISSING ) THEN struct.VWND[ ilev ] = tmp / 10.

  tmp = FLOAT( STRMID( line, 128, 7 ) )
  IF ( tmp GT MISSING ) THEN struct.VWNDGRAD[ ilev ] = tmp / 10.

  tmp = FLOAT( STRMID( line, 136, 7 ) )
  IF ( tmp GT MISSING ) THEN struct.N[ ilev ] = tmp

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

PRO READ_IGRA_DERIVED_V2, FILE, RESULT, NREC=NREC, VERBOSE=VERBOSE

  DEBUG = 0

  IF ( N_PARAMS() NE 2 ) THEN BEGIN
     PRINT, '% USAGE: READ_IGRA_DERIVED_V2, FILE, RESULT'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( FILE ) EQ 0 ) THEN BEGIN
     PRINT, '% READ_IGRA_DERIVED_V2: Argument FILE is not defined'
     RETURN
  ENDIF

  result = -1

  MAXREC = 60000l
  MAXLEV = 171
  result = REPLICATE( DEFINE_DERIVED_RECORD( MAXLEV=MAXLEV ), MAXREC )

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
        result[ irec ] = PARSE_DERIVED_HEADER( line, result[ irec ] )

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

        IF ( KEYWORD_SET( verbose ) EQ 1 ) THEN BEGIN
           PRINT, '   ... getting level information for level ' + $
                  STRTRIM( ilev, 2 )
        ENDIF

        ;; Only get levels below 300 mb - added: APB 2010-09-01
        ;;IF ( ilev GT 0 ) THEN BEGIN
        ;;   IF ( result[ irec ].press[ ilev - 1 ] GT 300. ) THEN BEGIN
        ;;      result[ irec ] = PARSE_DERIVED_RECORD( line, ilev, result[ irec ] )
        ;;      ilev = ilev + 1
        ;;   ENDIF
        ;;ENDIF ELSE BEGIN
        ;;   result[ irec ] = PARSE_DERIVED_RECORD( line, ilev, result[ irec ] )
        ;;   ilev = ilev + 1
        ;;ENDELSE

        IF ( ilev LT MAXLEV ) THEN BEGIN
           result[ irec ] = PARSE_DERIVED_RECORD( line, ilev, result[ irec ] )
           ilev = ilev + 1
        ENDIF ELSE BEGIN
           PRINT, '% READ_IGRA_DERIVED: Number of levels in profile ' + STRTRIM( irec, 2 ) + $ 
                  ' exceed MAXLEV (' + STRTRIM( MAXLEV, 2 ) + ')'
           PRINT, '                     ...Skipping levels until next record'
        ENDELSE

     ENDELSE

  ENDWHILE
  
  CLOSE, U
  FREE_LUN, U

  nrec = irec
  result = result[ 0 : (nrec-1) ]

  ;; Remove profiles with no data
  ;;result = WEED_PROFILES( result )

  IF ( DEBUG EQ 1 ) THEN BEGIN
     HELP, result
     HELP, result, /struct
  ENDIF

  RETURN

END

