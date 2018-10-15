;;----------------------------------------------------------------------
;; FIND_MANDATORY_LEVELS - gets the indices of mandatory levels from profile
;;                         data ingested using the READ_IGRA_DATA code.
;;
;; 2010-08-31 A.P.Barrett
;;----------------------------------------------------------------------
FUNCTION FIND_MANDATORY_LEVELS, PROFILE, LEVELS=LEVELS, COUNT=LCOUNT, $
                                VERBOSE=VERBOSE

  IF ( N_PARAMS() NE 1 ) THEN BEGIN
     PRINT, '% USAGE: RESULT = FIND_MANDATORY_LEVELS( PROFILE, LEVELS=LEVELS )'
     RETURN, -1
  ENDIF

  IF ( N_ELEMENTS( profile ) EQ 0 ) THEN BEGIN
     PRINT, '% FIND_MANDATORY_LEVELS: Argument PROFILE is not defined'
     RETURN, -1
  ENDIF

  IF ( N_ELEMENTS( levels ) EQ 0 ) THEN BEGIN
     levels = [1000., 850., 700., 500., 400., 300., 200., 150., 100., $
               50., 30., 20., 10., 7., 5., 3., 2., 1. ]
  ENDIF

  n_levels = N_ELEMENTS( levels )

  result = MAKE_ARRAY( n_levels, /LONG, VALUE=-1 )

  lcount = 0
  FOR ilev = 0, ( n_levels - 1 ) DO BEGIN

     pos = WHERE( profile.pressure EQ levels[ ilev ], count )

     IF ( count GT 0 ) THEN BEGIN

        result[ ilev ] = pos[0]
        lcount = lcount + 1

     ENDIF ELSE BEGIN

        IF ( KEYWORD_SET( verbose ) EQ 1 ) THEN BEGIN

           PRINT, FORMAT='("% FIND_MANDATORY_LEVELS: ",i4," mb level not found in profile")', $
           levels[ ilev ]

        ENDIF

     ENDELSE

  ENDFOR

  pos = WHERE( result NE -1, count )
  IF ( count GT 0 ) then result = result[ pos ]

  RETURN, result

END

  
