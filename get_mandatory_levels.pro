;;----------------------------------------------------------------------
;; GET_MANDATORY_LEVELS - gets values for mandatory levels from profile
;;                         data ingested using the READ_IGRA_DATA code.
;;
;; 2010-08-31 A.P.Barrett
;;----------------------------------------------------------------------
PRO GET_MANDATORY_LEVELS, PROFILE, Temperature, Geopotential, DewPoint, $
                          WindDir, WindSpeed, LEVEL=LEVEL, COUNT=LCOUNT, $
                          VERBOSE=VERBOSE

  IF ( N_PARAMS() NE 6 ) THEN BEGIN
     PRINT, '% USAGE: GET_MANDATORY_LEVELS, PROFILE, Temperature, Geopotential, ' + $
            'DewPoint, WindDir, WindSpeed, LEVEL=LEVEL, COUNT=LCOUNT, ' + $
            'VERBOSE=VERBOSE'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( profile ) EQ 0 ) THEN BEGIN
     PRINT, '% GET_MANDATORY_LEVELS: Argument PROFILE is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( profile ) GT 1 ) THEN BEGIN
     PRINT, '% GET_MANDATORY_LEVELS: Only works for single profiles'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( level ) EQ 0 ) THEN BEGIN
     level = [1000., 850., 700., 500., 400., 300., 200., 150., 100., $
               50., 30., 20., 10., 7., 5., 3., 2., 1. ]
  ENDIF

  n_level = N_ELEMENTS( level )

  ;; Array to hold indices
  result       = MAKE_ARRAY( n_level, /LONG, VALUE=-1 )

  ;; Arrays to hold data
  Temperature  = MAKE_ARRAY( n_level, /FLOAT, VALUE=-9999.99 )
  Geopotential = MAKE_ARRAY( n_level, /FLOAT, VALUE=-9999.99 )
  DewPoint     = MAKE_ARRAY( n_level, /FLOAT, VALUE=-9999.99 )
  WindDir      = MAKE_ARRAY( n_level, /FLOAT, VALUE=-9999.99 )
  WindSpeed    = MAKE_ARRAY( n_level, /FLOAT, VALUE=-9999.99 )

  lcount = 0
  FOR ilev = 0, ( n_level - 1 ) DO BEGIN

     pos = WHERE( profile.pressure EQ level[ ilev ], count )

     IF ( count GT 0 ) THEN BEGIN

        result[ ilev ] = pos[0]
        lcount = lcount + 1

     ENDIF ELSE BEGIN

        IF ( KEYWORD_SET( verbose ) EQ 1 ) THEN BEGIN

           PRINT, FORMAT='("% GET_MANDATORY_LEVELS: ",i4," mb level not found in profile")', $
           levels[ ilev ]

        ENDIF

     ENDELSE

  ENDFOR

  pos = WHERE( result NE -1, count )
  IF ( count GT 0 ) THEN BEGIN
     Temperature[ pos ]  = profile.temperature[ result[ pos ] ]
     Geopotential[ pos ] = profile.geopot_hgt[ result[ pos ] ]
     DewPoint[ pos ]     = profile.dewpoint[ result[ pos ] ]
     WindDir[ pos ]      = profile.wind_direction[ result[ pos ] ]
     WindSpeed[ pos ]    = profile.wind_speed[ result[ pos ] ]
  ENDIF

  RETURN

END

  
