;;----------------------------------------------------------------------
;; Calculates monthly means of surface and sea level pressure, surface
;; temperature and dewpoint, and mandatory level temperatures and dewpoints
;; from profile observations.  Monthly means are calculated for 00h and 12h
;; separately.
;;
;; 2010-12-02 A.P.Barrett
;;----------------------------------------------------------------------

PRO GET_SURFACE_OBS_DRVD, PROFILE, SFC_PROFILE

  nrec = N_ELEMENTS( profile )
  nlev = 1

  nfield = N_TAGS( profile )

  sfc_profile = REPLICATE( DEFINE_DERIVED_RECORD( MAXLEV = 1 ), nrec )

  FOR ifield = 0, 5 DO sfc_profile.(ifield) = profile.(ifield)
  sfc_profile.nlevels = 1
  FOR ifield = 7, ( nfield-1 ) DO BEGIN
     sfc_profile.(ifield) = REFORM( profile.(ifield)[0], 1, nrec )
  ENDFOR

  RETURN

END

FUNCTION LEVEL_INDEX, PRESSURE, LEVEL, COUNT=COUNT

  nlev = N_ELEMENTS( level )

  index = MAKE_ARRAY( nlev, /LONG, VALUE=-1 )

  FOR ilev = 0, ( nlev - 1 ) DO BEGIN
     index[ ilev ] = WHERE( pressure EQ level[ ilev ] )
  ENDFOR

  count = TOTAL( index GT -1 )

  RETURN, index

END

FUNCTION LEVEL_DATA, DATA, INDEX

  nlev = N_ELEMENTS( index )

  result = MAKE_ARRAY( nlev, /FLOAT, VALUE=-9999.99 )

  pos = WHERE( index GT -1, count )
  IF ( count GT 0 ) THEN result[ pos ] = data[ index[ pos ] ]

  RETURN, result

END

PRO EXTRACT_MANDATORY_LEVELS_DRVD, PROFILE, MND_PROFILE, LEVEL=LEVEL

  mandatory_level = [1000., 850., 700., 500., 400., 300., 200., 150., 100., $
                     50., 30., 20., 10., 7., 5., 3., 2., 1. ]

  level = N_ELEMENTS( level ) EQ 0 ? mandatory_level : level

  nrec = N_ELEMENTS( profile )
  nlev = N_ELEMENTS( level )

  nfield = N_TAGS( profile )

  ;; Define result array of structures
  mnd_profile = REPLICATE( DEFINE_DERIVED_RECORD( MAXLEV=nlev ), nrec )

  ;; Now loop through profiles and extract mandatory levels
  FOR irec = 0l, ( nrec - 1 ) DO BEGIN
     
     ;; Copy station id and time info
     FOR ifield = 0, 5 DO BEGIN
        mnd_profile[irec].( ifield ) = profile[irec].( ifield )
     ENDFOR
     ;; Set nlevels to number of mandatory levels
     mnd_profile.nlevels = nlev

     ;; Get indices of mandatory levels
     index = LEVEL_INDEX( profile[irec].press, level, COUNT=count )
     IF ( count GT 0 ) THEN BEGIN
        FOR ifield=7, ( nfield-1 ) DO BEGIN
           mnd_profile[ irec ].( ifield ) = LEVEL_DATA( profile[irec].(ifield), index )
        ENDFOR
     ENDIF

  ENDFOR

  RETURN

END
     
FUNCTION GET_MEAN_1D_DRVD, VALUE, MIN_NUM_VALUE

  result = -9999.99

  pos = WHERE( value GT -9999., count )
  IF ( count GT min_num_value ) THEN result = MEAN( value[ pos ] )

  RETURN, result

END

FUNCTION GET_MEAN_2D_DRVD, VALUE, MIN_NUM_VALUE

  dims = SIZE( value, /DIMENSIONS )
  nlev = dims[0]

  result = MAKE_ARRAY( nlev, /FLOAT, VALUE=-9999.99 )

  FOR ilev = 0, ( nlev - 1 ) DO BEGIN

     pos = WHERE( value[ ilev, * ] GT -9999.99, count )
     IF ( count GT min_num_value ) THEN result[ ilev ] = MEAN( value[ ilev, pos ] )

  ENDFOR

  RETURN, result

END
  
FUNCTION GET_PROFILE_INDEX, PROFILE, YEAR, MONTH, HOUR, NPROFILE=NPROFILE

  result = -1l
  nprofile = 0l

  IF ( N_ELEMENTS( profile ) EQ 0 ) THEN BEGIN
     PRINT, '% GET_PROFILE_INDEX: Argument PROFILE is undefined'
     RETURN, result
  ENDIF

  IF ( N_ELEMENTS( year ) EQ 0 ) THEN BEGIN
     PRINT, '% GET_PROFILE_INDEX: Argument YEAR is undefined'
     RETURN, result
  ENDIF

  IF ( N_ELEMENTS( month ) EQ 0 ) THEN BEGIN
     PRINT, '% GET_PROFILE_INDEX: Argument MONTH is undefined'
     RETURN, result
  ENDIF

  IF ( N_ELEMENTS( hour ) EQ 0 ) THEN BEGIN
     PRINT, '% GET_PROFILE_INDEX: Argument HOUR is undefined'
     RETURN, result
  ENDIF

  CASE hour OF

     0: BEGIN
        result = WHERE( profile.year EQ year AND $
                        profile.month EQ month AND $
                        ( ( profile.obs_hour GT 21 AND profile.obs_hour LT 24 ) OR $
                          ( profile.obs_hour GE 0 AND profile.obs_hour LT 3 ) ), $
                        nprofile )
     END
     12: BEGIN
        result = WHERE( profile.year EQ year AND $
                        profile.month EQ month AND $
                        ( profile.obs_hour GT 9 AND profile.obs_hour LT 15 ), $
                        nprofile )
     END
     ELSE: BEGIN
        PRINT, '% GET_PROFILE_INDEX: Expects HOUR to be 00h or 12h'
     END

  ENDCASE

  RETURN, result

END

PRO MANDATORY_TO_MONTH, STRUCT_IN, STRUCT_OUT, YEAR_START=YEAR_START, YEAR_END=YEAR_END, $
                  MIN_NUM_VALUE=MIN_NUM_VALUE, OBS_HOUR=OBS_HOUR, DEBUG=DEBUG

  nrec = N_ELEMENTS( struct_in )
  nlev = N_ELEMENTS( struct_in[0].press )

  year_start = N_ELEMENTS( year_start ) EQ 0 ? struct_in[0].year : year_start
  year_end   = N_ELEMENTS( year_end ) EQ 0 ? struct_in[nrec-1].year : year_end
  obs_hour   = N_ELEMENTS( obs_hour ) EQ 0 ? [ 0, 12 ] : obs_hour 
  min_num_value = N_ELEMENTS( min_num_value ) EQ 0 ? 10 : min_num_value

  nyear = year_end - year_start + 1
  nmonth = 12
  ntime = nyear * nmonth

  nobs = N_ELEMENTS( obs_hour )

  level  =  struct_in[ 0 ].press
  time   = MAKE_ARRAY( ntime, /FLOAT, VALUE=-9999.99 )
  obsgph = MAKE_ARRAY( ntime, nobs, nlev, /FLOAT, VALUE=-9999.99 )
  temp   = MAKE_ARRAY( ntime, nobs, nlev, /FLOAT, VALUE=-9999.99 )
  vappress = MAKE_ARRAY( ntime, nobs, nlev, /FLOAT, VALUE=-9999.99 )
  rh     = MAKE_ARRAY( ntime, nobs, nlev, /FLOAT, VALUE=-9999.99 )

  itime = 0
  FOR iyear = year_start, year_end DO BEGIN

     FOR imonth = 1, 12 DO BEGIN
        
        time[ itime ] = JULDAY( imonth, 1, iyear )

        FOR ih = 0, 1 DO BEGIN

           index = GET_PROFILE_INDEX( struct_in, iyear, imonth, obs_hour[ ih ], NPROFILE=nprofile )

           IF ( nprofile GT min_num_value ) THEN BEGIN
              obsgph[ itime, ih, * ]   = GET_MEAN_2D_DRVD( struct_in[ index ].obsgph, min_num_value )
              temp[ itime, ih, * ]     = GET_MEAN_2D_DRVD( struct_in[ index ].temp, min_num_value )
              vappress[ itime, ih, * ] = GET_MEAN_2D_DRVD( struct_in[ index ].vappress, min_num_value )
              rh[ itime, ih, * ]       = GET_MEAN_2D_DRVD( struct_in[ index ].rh, min_num_value )
           ENDIF

           IF ( KEYWORD_SET( DEBUG ) EQ 1 ) THEN BEGIN
              fmt0 = '(i6,1x,i4,1x,i02,1x,i02,"h",2x,i7,2x,i4)'
              fmt1 = '('+STRTRIM(nlev,2)+'(1x,f8.2))'
              PRINT, FORMAT=fmt0, itime, iyear, imonth, obs_hour[ih], time[itime], nprofile
              PRINT, FORMAT=fmt1, obsgph[ itime, ih, * ]
              PRINT, FORMAT=fmt1, temp[ itime, ih, * ]
              PRINT, FORMAT=fmt1, vappress[ itime, ih, * ]
              PRINT, FORMAT=fmt1, rh[ itime, ih, * ]
           ENDIF

        ENDFOR

        itime = itime + 1

     ENDFOR

  ENDFOR

  struct_out = { time: time, $
                 level: level, $
                 obs_hour: obs_hour, $
                 obsgph: obsgph, $
                 temp: temp, $
                 vappress: vappress, $
                 rh: rh }

  RETURN

END

PRO SURFACE_TO_MONTH, STRUCT_IN, STRUCT_OUT, YEAR_START=YEAR_START, YEAR_END=YEAR_END, $
                  MIN_NUM_VALUE=MIN_NUM_VALUE, OBS_HOUR=OBS_HOUR, DEBUG=DEBUG

  nrec = N_ELEMENTS( struct_in )

  year_start = N_ELEMENTS( year_start ) EQ 0 ? struct_in[0].year : year_start
  year_end   = N_ELEMENTS( year_end ) EQ 0 ? struct_in[nrec-1].year : year_end
  obs_hour   = N_ELEMENTS( obs_hour ) EQ 0 ? [ 0, 12 ] : obs_hour 
  min_num_value = N_ELEMENTS( min_num_value ) EQ 0 ? 10 : min_num_value

  nyear = year_end - year_start + 1
  nmonth = 12
  ntime = nyear * nmonth
  
  nobs = N_ELEMENTS( obs_hour )

  time   = MAKE_ARRAY( ntime, /FLOAT, VALUE=-9999.99 )
  obsgph = MAKE_ARRAY( ntime, nobs, /FLOAT, VALUE=-9999.99 )
  temp   = MAKE_ARRAY( ntime, nobs, /FLOAT, VALUE=-9999.99 )
  vappress = MAKE_ARRAY( ntime, nobs, /FLOAT, VALUE=-9999.99 )
  rh     = MAKE_ARRAY( ntime, nobs, /FLOAT, VALUE=-9999.99 )

  itime = 0
  FOR iyear = year_start, year_end DO BEGIN

     FOR imonth = 1, 12 DO BEGIN
        
        time[ itime ] = JULDAY( imonth, 1, iyear )

        FOR ih = 0, 1 DO BEGIN

           index = GET_PROFILE_INDEX( struct_in, iyear, imonth, obs_hour[ ih ], NPROFILE=nprofile )

           IF ( nprofile GT min_num_value ) THEN BEGIN
              obsgph[ itime, ih ]   = GET_MEAN_1D_DRVD( struct_in[ index ].obsgph, min_num_value )
              temp[ itime, ih ]     = GET_MEAN_1D_DRVD( struct_in[ index ].temp, min_num_value )
              vappress[ itime, ih ] = GET_MEAN_1D_DRVD( struct_in[ index ].vappress, min_num_value )
              rh[ itime, ih ]       = GET_MEAN_1D_DRVD( struct_in[ index ].rh, min_num_value )
           ENDIF

           IF ( KEYWORD_SET( DEBUG ) EQ 1 ) THEN BEGIN
              fmt = '(i6,1x,i4,1x,i02,1x,i02,"h",2x,i7,2x,i4,4(1x,f8.2))'
              PRINT, FORMAT=fmt, $
                     itime, iyear, imonth, obs_hour[ih], time[itime], nprofile, $
                     obsgph[ itime, ih ], temp[ itime, ih ], vappress[ itime, ih ], $
                     rh[ itime, ih ]
           ENDIF

        ENDFOR

        itime = itime + 1

     ENDFOR

  ENDFOR

  struct_out = { time: time, $
                 obs_hour: obs_hour, $
                 obsgph: obsgph, $
                 temp: temp, $
                 vappress: vappress, $
                 rh: rh }

  RETURN

END

PRO IGRA_DERIVED_TO_MONTHLY, PROFILE, YEAR_START, YEAR_END, RESULT, $
                             LEVEL=LEVEL, MIN_NUM_PROFILE=MIN_NUM_PROFILE, $
                             OBS_HOUR=OBS_HOUR, DEBUG=DEBUG, VERBOSE=VERBOSE

  IF ( N_PARAMS() NE 4 ) THEN BEGIN
     PRINT, 'USAGE: IGRA_DERIVED_TO_MONTHLY, PROFILE, YEAR_START, YEAR_END, RESULT, ' + $
            'LEVEL=LEVEL, DEBUG=DEBUG'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( profile ) EQ 0 ) THEN BEGIN
     PRINT, '% IGRA_DERIVED_TO_MONTHLY: Argument PROFILE is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( year_start ) EQ 0 ) THEN BEGIN
     PRINT, '% IGRA_DERIVED_TO_MONTHLY: Argument YEAR_START is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( year_end ) EQ 0 ) THEN BEGIN
     PRINT, '% IGRA_DERIVED_TO_MONTHLY: Argument YEAR_END is not defined'
     RETURN
  ENDIF

  mandatory_levels = [1000., 850., 700., 500., 400., 300., 200., 150., 100., 50. ]
  level = N_ELEMENTS( level ) EQ 0 ? mandatory_levels : level

  ;; Observation hours for which monthly means are calculated
  obs_hour = N_ELEMENTS( obs_hour ) EQ 0 ? [ 0, 12 ] : obs_hour

  ;; minimum number of profiles from which means can be calculated
  min_num_profile = N_ELEMENTS( min_num_value ) EQ 0 ? 10 : min_num_value

  ;; Get surface observations
  IF ( KEYWORD_SET( VERBOSE ) EQ 1 ) THEN BEGIN
     PRINT, '% IGRA_DERIVED_TO_MONTHLY: Getting surface obs...' 
  ENDIF
  GET_SURFACE_OBS_DRVD, profile, surface

  ;; Get mandatory levels
  IF ( KEYWORD_SET( VERBOSE ) EQ 1 ) THEN BEGIN
     PRINT, '% IGRA_DERIVED_TO_MONTHLY: Getting mandatory levels...' 
  ENDIF
  EXTRACT_MANDATORY_LEVELS_DRVD, profile, mandatory, LEVEL=level

  IF ( KEYWORD_SET( VERBOSE ) EQ 1 ) THEN BEGIN
     PRINT, '% IGRA_DERIVED_TO_MONTHLY: Calculating monthly means for surface...' 
  ENDIF
  SURFACE_TO_MONTH, surface, surface_month, YEAR_START=year_start, $
                    YEAR_END=year_end, MIN_NUM_VALUE=min_num_profile, $
                    OBS_HOUR=obs_hour, DEBUG=DEBUG

  IF ( KEYWORD_SET( VERBOSE ) EQ 1 ) THEN BEGIN
     PRINT, '% IGRA_DERIVED_TO_MONTHLY: Calculating monthly means for mandatory levels...' 
  ENDIF
  MANDATORY_TO_MONTH, mandatory, mandatory_month, YEAR_START=year_start, $
                      YEAR_END=year_end, MIN_NUM_VALUE=min_num_profile, $
                      OBS_HOUR=obs_hour, DEBUG=DEBUG

  result = { surface: surface_month, $
             mandatory: mandatory_month }

  RETURN

END           
     
