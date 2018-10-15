;;----------------------------------------------------------------------
;; CREATE_MANDATORY_LEVEL_ARRAY - generates arrays of temperature, dewpoint
;;                                geopotential and wind speed and direction.
;;
;; 2010-08-31 A.P.Barrett
;;----------------------------------------------------------------------
PRO CREATE_MANDATORY_LEVEL_ARRAY, profile, temperature, geopotential, $
                                  dewpoint, winddir, windspd, LEVEL=LEVEL

  ;; Check arguments
  IF ( N_PARAMS() NE 6 ) THEN BEGIN
     PRINT, '% USAGE: CREATE_MANDATORY_LEVEL_ARRAY, profile, temperature, ' + $
            'geopotential, dewpoint, winddir, windspd'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( profile ) EQ 0 ) THEN BEGIN
     PRINT, '% CREATE_MANDATORY_LEVEL_ARRAY: Argument PROFILE is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( level ) EQ 0 ) THEN BEGIN
     level = [1000., 850., 700., 500., 400., 300., 200., 150., 100., $
               50., 30., 20., 10., 7., 5., 3., 2., 1. ]
  ENDIF

  ;; Define number of mandatory levels
  nlevel = N_ELEMENTS( level )
  nprofile = N_ELEMENTS( profile )

  ;; Define results arrays
  temperature  = MAKE_ARRAY( nprofile, nlevel, /FLOAT, VALUE=-9999.99 )
  geopotential = MAKE_ARRAY( nprofile, nlevel, /FLOAT, VALUE=-9999.99 )
  dewpoint     = MAKE_ARRAY( nprofile, nlevel, /FLOAT, VALUE=-9999.99 )
  winddir      = MAKE_ARRAY( nprofile, nlevel, /FLOAT, VALUE=-9999.99 )
  windspd      = MAKE_ARRAY( nprofile, nlevel, /FLOAT, VALUE=-9999.99 )

  ;; Loop through profiles and extract levels
  FOR iprof = 0l, ( nprofile - 1 ) DO BEGIN

     GET_MANDATORY_LEVELS, profile[ iprof ], t, z, td, wd, ws, LEVEL=level

     temperature[ iprof, * ]  = TRANSPOSE( t )
     geopotential[ iprof, * ] = TRANSPOSE( z )
     dewpoint[ iprof, * ]     = TRANSPOSE( td )
     winddir[ iprof, * ]      = TRANSPOSE( wd )
     windspd[ iprof, * ]      = TRANSPOSE( ws )
  
  ENDFOR

  RETURN

END

