;;----------------------------------------------------------------------
;; Tests use of pointers for radiosonde code
;;
;; 2010-08-27 A.P.Barrett
;;----------------------------------------------------------------------

FUNCTION DEFINE_PROFILE_RECORD, MAXLEV=MAXLEV

  IF ( N_ELEMENTS( MAXLEV ) EQ 0 ) THEN MAXLEV = 171

  data_record = {station_number:   '', $
                 year:             0l, $
                 month:            0l, $
                 day:              0l, $
                 obs_hour:         0l, $
                 release_time:     0l, $
                 nlevels:          0l, $
                 pw1:              -9999.99, $  ;; PW sfc-500mb
                 pw2:              -9999.99, $  ;; PW 500-300mb
                 pw3:              -9999.99, $  ;; PW 300-100mb
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
                 wind_speed:       MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-9999.99 ), $
                 specHum:          MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-9999.99 ), $
                 relHum:           MAKE_ARRAY( MAXLEV, /FLOAT, VALUE=-9999.99 ), $
                 dewpoint_flag:    MAKE_ARRAY( MAXLEV, /STRING, VALUE='' ) }

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
  struct.pw1            = FLOAT( STRMID( line, 24, 12 ) )
  struct.pw2            = FLOAT( STRMID( line, 36, 12 ) )
  struct.pw3            = FLOAT( STRMID( line, 48, 12 ) )

  RETURN, struct

END

;; Returns a converted data value or a missing value
;; 2011-06-02 APB Changed to set all missing and replaced values to -9999.99
FUNCTION CONVERT_VALUE, VALUE, FACTOR, REPLACED=REPLACED, $
                        MISSING=MISSING

  replaced = N_ELEMENTS( replaced ) EQ 0 ? -8888. : replaced
  missing = N_ELEMENTS( missing ) EQ 0 ? -9999. : missing

  CASE 1 OF
     ( value GT replaced ): new_value = value / factor
     ( value LE replaced AND value GT missing ): new_value = -9999.99
     ( value LE missing ): new_value = -9999.99
     ELSE: BEGIN
        PRINT, '% CONVERT_VALUE: Unknown value'
     END
  ENDCASE

  RETURN, new_value

END

FUNCTION PARSE_DATA_RECORD, LINE, ILEV, STRUCT

  ON_ERROR, 2

  ; Establish error handler. When errors occur, the index of the  
  ; error is returned in the variable Error_status:  
  CATCH, Error_status  
  
  ;This statement begins the error handler:  
  IF Error_status NE 0 THEN BEGIN  
     PRINT, 'Error index: ', Error_status  
     PRINT, 'Error message: ', !ERROR_STATE.MSG  
     ; Handle the error by extending A:  
     PRINT, line
     STOP
     CATCH, /CANCEL  
  ENDIF  

  fmt = '(2I1,I6,A1,I5,A1,I6,A1,I6,I6,I6,I8,I6,A1)'

  aPf = ''
  aZf = ''
  aTf = ''
  aDf = '' 
  
  READS, line, Majl,Minl,iP,aPf,iZ,aZf,iT,aTf,iD,iWD,iWS,iqq,iRH,aDf, $
         FORMAT=fmt

  struct.major_level_type[ ilev ] = Fix(Majl)
  struct.minor_level_type[ ilev ] = FIX(Minl)
  struct.pressure[ ilev ]         = CONVERT_VALUE( iP, 100. ) 
  struct.pressure_flag[ ilev ]    = aPf
  struct.geopot_hgt[ ilev ]       = CONVERT_VALUE( iZ, 1. ) 
  struct.geopot_hgt_flag[ ilev ]  = aZf
  struct.temperature[ ilev ]      = CONVERT_VALUE( iT, 100. ) 
  struct.temperature_flag[ ilev ] = aTf
  struct.dewpoint[ ilev ]         = CONVERT_VALUE( iD, 100. )
  struct.wind_direction[ ilev ]   = CONVERT_VALUE( iWD, 1. )
  struct.wind_speed[ ilev ]       = CONVERT_VALUE( iWD, 10. )
  struct.specHum[ ilev ]          = CONVERT_VALUE( iqq, 1.E5 )
  struct.relHum[ ilev ]           = CONVERT_VALUE( iRH, 100. )
  struct.dewpoint_flag[ ilev ]    = aDf

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

PRO READ_DAI_RADIOSONDE, FILE, RESULT, NOCLEANUP=NOCLEANUP, VERBOSE=VERBOSE, NREC=NREC

  DEBUG = 0

  IF ( N_PARAMS() NE 2 ) THEN BEGIN
     PRINT, '% USAGE: READ_DAI_RADIOSONDE, FILE, RESULT'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( FILE ) EQ 0 ) THEN BEGIN
     PRINT, '% READ_DAI_RADIOSONDE: Argument FILE is not defined'
     RETURN
  ENDIF

  result = -1

  MAXLEV = 171
  MAXREC = 62000l
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

        IF ( KEYWORD_SET( verbose ) EQ 1 ) THEN BEGIN
           PRINT, '   ... getting level information for level ' + $
                  STRTRIM( ilev, 2 )
        ENDIF

        ;; Only get levels below 300 mb - added: APB 2010-09-01
        ;IF ( ilev GT 0 ) THEN BEGIN
        ;   IF ( result[ irec ].pressure[ ilev - 1 ] GT 300. ) THEN BEGIN
        ;      result[ irec ] = PARSE_DATA_RECORD( line, ilev, result[ irec ] )
        ;      ilev = ilev + 1
        ;   ENDIF
        ;ENDIF ELSE BEGIN
        ;   result[ irec ] = PARSE_DATA_RECORD( line, ilev, result[ irec ] )
        ;   ilev = ilev + 1
        ;ENDELSE

        ;; Some of the data records in Dai's dataset contain '*****'.
        ;; This string cannot be parsed by PARSE_DATA_RECORD.  So these
        ;; strings are replaced by -99999.
        IF ( STREGEX( line, '\*+', /BOOLEAN ) EQ 1 ) THEN BEGIN
           pos = STREGEX( line, '\*+', LENGTH=len )
           IF ( len EQ 5 ) THEN BEGIN
              prefix = STRMID(line,0,pos)
              suffix = STRMID(line,pos+len,strlen(line)-1)
              line = prefix+'-9999'+suffix
           ENDIF ELSE BEGIN
              PRINT, '% READ_DAI_RADIOSONDE: Unexpected *+ string sequence in line:'
              PRINT, ' ' + line
              STOP
           ENDELSE
        ENDIF
        
        result[ irec ] = PARSE_DATA_RECORD( line, ilev, result[ irec ] )
        ilev = ilev + 1

     ENDELSE

     IF ( ilev GE MAXLEV ) THEN BEGIN
        PRINT, '% READ_DAI_RADIOSONDE: Number of levels exceeds MAXLEV'
        PRINT, '                      Returning partial record'
        BREAK
     ENDIF

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

