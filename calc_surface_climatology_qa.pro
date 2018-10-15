;;----------------------------------------------------------------------
;; Calculates the climatology of surface temperature.  This routine is 
;; used by PROCESS_PROFILE_CLIMATOLOGY.
;;
;; 2010-12-01 A.P.Barrett
;;----------------------------------------------------------------------

;; Extracts profiles for a given month
FUNCTION GET_MONTH_PROFILE, PROFILE, MONTH, NPROFILE=NPROFILE

  result = -1
  nprofile = 0

  IF ( N_ELEMENTS( profile ) EQ 0 ) THEN BEGIN
     PRINT, '% GET_MONTH_PROFILE: Argument PROFILE is not defined'
     RETURN, result
  ENDIF

  IF ( N_ELEMENTS( month ) EQ 0 ) THEN BEGIN
     PRINT, '% GET_MONTH_PROFILE: Argument MONTH is not defined'
     RETURN, result
  ENDIF

  index = WHERE( profile.month EQ month, nprofile )
  IF ( nprofile EQ 0 ) THEN RETURN, result

  result = profile[ index ]

  RETURN, result

END

;; Gets profiles with observation hours within a defined time-span of a given
;; given hour; e.g. +/-3 h of 00h.  Default time span is 3h.
FUNCTION GET_HOUR_PROFILE, PROFILE, HOUR, NPROFILE=NPROFILE

  result = -1
  nprofile = 0

  IF ( N_ELEMENTS( Profile ) EQ 0 ) THEN BEGIN
     PRINT, '% GET_HOUR_PROFILE: Argument PROFILE is not defined'
     RETURN, result
  ENDIF

  IF ( N_ELEMENTS( Hour ) EQ 0 ) THEN BEGIN
     PRINT, '% GET_HOUR_PROFILE: Argument HOUR is not defined'
     RETURN, result
  ENDIF
     
  CASE 1 OF
     ( Hour EQ 0 ): BEGIN
        h0 = 21
        h1 = 3
        index = WHERE( ( Profile.Obs_Hour GT h0 AND Profile.Obs_Hour LT 24 ) OR $
                       ( Profile.Obs_Hour GE 0 AND Profile.Obs_Hour LT h1 ), nprofile )
     END
     ( Hour EQ 12 ): BEGIN
        h0 = 9
        h1 = 15
        index = WHERE( Profile.Obs_Hour GT h0 AND Profile.Obs_Hour LT h1, nprofile )
     END
     ELSE: BEGIN
        PRINT, '% GET_HOUR_PROFILE: Expects Hour to be 00h or 12h'
        RETURN, result
     END
  ENDCASE

  IF ( nprofile GT 0 ) THEN result = Profile[ index ]
     
  RETURN, result

END

;; Get only valid surface temperatures.  Surface values have
;; minor_level_type=1.  
FUNCTION GET_SURFACE_TEMP, PROFILE, NVALUE=NVALUE, DEBUG=DEBUG

  result = -1
  nvalue = 0

  IF ( N_ELEMENTS( Profile ) EQ 0 ) THEN BEGIN
     PRINT, '% GET_MONTH_PROFILE: Argument PROFILE is not defined'
     RETURN, result
  ENDIF

  ;; Surface temperatures are always first in temp array
  Tsurf = Profile.Temperature[ 0 ]
  ;; ... but not all of these temperatures are surface
  index = WHERE( Profile.Minor_Level_Type[ 0 ] EQ 1, nvalue )
  IF ( nvalue GT 0 ) THEN result = Tsurf[ index ]

  RETURN, result

END

;; Sets temperaures outside of first-cut plausible range check to
;; missing
PRO IMPLAUSIBLE_TEMPERATURE_CHECK, TSURF, NEXCLUDE

  missing = -9999.
  lowest_plausible = -110.
  highest_plausible = 70.

  implausible = WHERE( ( tsurf GT missing AND tsurf LT lowest_plausible ) OR $
                       tsurf GT highest_plausible, nexclude )
  IF ( nexclude GT 0 ) THEN tsurf[ implausible ] = -9999.99

  RETURN

END

;; Calculates average and standard deviations of temperatures
PRO CALC_TSURF_STATISTICS, TSURF, TAVG, TSTD, NEXCLUDE, VERBOSE=VERBOSE

  IF ( N_ELEMENTS( tsurf ) EQ 0 ) THEN BEGIN
     PRINT, '% CALC_TSURF_STATISTICS: Argument TSURF is not defined'
     RETURN
  ENDIF

  TAVG = -9999.99
  TSTD = -9999.99
  NEXCLUDE = 0

  ;; cehck for implausible values
  IMPLAUSIBLE_TEMPERATURE_CHECK, tsurf, nexclude

  ;; Remove missing values
  isvalid = WHERE( tsurf GT -9999., num_valid )
  IF ( num_valid EQ 0 ) THEN RETURN
  
  IF ( num_valid LT 150 ) THEN BEGIN
     IF ( KEYWORD_SET( VERBOSE ) EQ 1 ) THEN BEGIN
        PRINT, '% CALC_TSURF_STATISTICS: Insufficient valid surface temperature values'
        PRINT, '                         Need more than 150 valid values'
     ENDIF
     RETURN
  ENDIF

  tavg = MEAN( tsurf[ isvalid ] )
  tstd = STDDEV( tsurf[ isvalid ] )

  RETURN

END

;:**********************************************************************
;; MAIN ROUTINE
;;**********************************************************************
PRO CALC_SURFACE_CLIMATOLOGY_QA, PROFILE, TAVERAGE, TSTDDEV, NEXCLUDE, $
                                 DOPLOT=DOPLOT, DEBUG=DEBUG, DOHOUR=DOHOUR 

  taverage     = MAKE_ARRAY( 12, /FLOAT, VALUE=-9999.99 )
  tstddev      = MAKE_ARRAY( 12, /FLOAT, VALUE=-9999.99 )
  nexclude     = MAKE_ARRAY( 12, /FLOAT, VALUE=0 )

  FOR imon = 0, 11 DO BEGIN

     thismonth = imon + 1
     
     ;; Get profiles for a given month
     mon_profile = GET_MONTH_PROFILE( profile, thismonth, NPROFILE=num_thismonth )
     
     ;; Get surface temperatures
     Tsurf = GET_SURFACE_TEMP( mon_profile, NVALUE=num_surface )
     
     ;; Get surface temperature statistics
     CALC_TSURF_STATISTICS, Tsurf, Tavg, Tstd, nexclude_, VERBOSE=0
     
     IF ( KEYWORD_SET( DEBUG ) EQ 1 ) THEN BEGIN
        PRINT, '%  THISMONTH=' + STRTRIM( thismonth, 2 ) + $
               '   NUM_THISMONTH='   + STRTRIM( num_thismonth, 2 ) + $
               '   NUM_SURFACE='     + STRTRIM( num_surface, 2 ) + $
               '   NEXCLUDE='        + STRTRIM( nexclude_, 2 ) + $
               '  - Taverage='      + STRTRIM( tavg, 2 ) + $
               '  - Tstddev='        + STRTRIM( tstd, 2 )
     ENDIF
     
     taverage[ imon ] = tavg
     tstddev[ imon ]  = tstd
     nexclude[ imon ] = nexclude_
     
  ENDFOR ;; month
  
  RETURN

END

