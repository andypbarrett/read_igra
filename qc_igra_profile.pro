;;----------------------------------------------------------------------
;; Performs quality control of profile data following the QC procedure 
;; of Ross and Elliot.
;;
;; 2010-11-24 A.P.Barrett
;;----------------------------------------------------------------------

;; Check mandatory levels are within range
PRO QC_TAIR_MANDATORY, PROFILE, TAVG, TSTD, NEXCLUDE, $
                       LEVEL=LEVEL
  
  ;; # standard deviations allowed for tair 
  nstd = 4

  ;; minimum value of non-missing data
  missing = -9999.

  ;; default mandatory levels
  default_level = [1000., 850., 700., 500., 400., 300., 200., 150., $
                   100., 50., 30., 20., 10., 7., 5., 3., 2., 1. ]

  ;; Check if levels keyword is set
  level = N_ELEMENTS( level ) EQ 0 ? default_level : level

  ;; initialize counter for excluded values
  nexclude = 0l

  ;; get number of profiles
  nprofile = N_ELEMENTS( profile )

  ;; Define upper and lower temperature limits
  lower = tavg - ( nstd * tstd )
  upper = tavg + ( nstd * tstd )

  ;; Loop through profiles
  FOR iprof = 0, ( nprofile - 1 ) DO BEGIN
 
     monthid = profile[ iprof ].month - 1

     ;; Get indices for mandatory levels
     index = FIND_MANDATORY_LEVELS( profile, COUNT=lcount, LEVELS=level )
     IF ( lcount LE 0 ) THEN BEGIN
        PRINT, '% QC_TAIR_MANDATORY: Unable to find mandatory ' + $
               'levels in profile ' + STRTRIM( iprof, 2 )
     ENDIF

     tmp = profile[ iprof ].temperature[ index ]
     test = ( ( tmp GT missing AND tmp LT lower[ *, monthid ] ) OR $
              tmp GT upper[ *, monthid ] )

     reject = WHERE( test EQ 1, num_reject )
     IF ( num_reject GT 0 ) THEN BEGIN
        tmp[ reject ] = -9999.99
        profile[ iprof ].temperature[ index ] = tmp
        nexclude = nexclude + num_reject
     ENDIF

  ENDFOR

  RETURN

END ;; end of QC_TAIR_MANDATORY

FUNCTION TEMPERATURE_RANGE, PRESSURE, LEVEL, UPPER, LOWER

  isnearest = VALUE_LOCATE( level, pressure )

  CASE isnearest OF

     ;; Below lowest level
     -1: BEGIN
        

  RETURN, limit

END
;; Check significant levels are within range
PRO QC_TAIR_SIGNIFICANT, PROFILE, TAVG, TSTD, NEXCLUDE, $
                       LEVEL=LEVEL
  
  ;; initialize counter for excluded values
  nexclude = 0l

  return

  ;; # standard deviations allowed for tair 
  nstd = 4

  ;; minimum value of non-missing data
  missing = -9999.

  ;; default mandatory levels
  default_level = [1000., 850., 700., 500., 400., 300., 200., 150., $
                   100., 50., 30., 20., 10., 7., 5., 3., 2., 1. ]
  ;; Check if levels keyword is set
  level = N_ELEMENTS( level ) EQ 0 ? default_level : level
  nlevel = N_ELEMENTS( level )

  ;; get number of profiles
  nprofile = N_ELEMENTS( profile )

  ;; Define upper and lower temperature limits
  lower = tavg - ( nstd * tstd )
  upper = tavg + ( nstd * tstd )

  ;; Loop through profiles
  FOR iprof = 0, ( nprofile - 1 ) DO BEGIN
 
     monthid = profile[ iprof ].month - 1

     ;; Find significant levels
     islevel = WHERE( profile[iprof].major_level_type EQ 2, num_level )
     isnearest = VALUE_LOCATE( level, profile[iprof].pressure[ islevel ] )

     FOR ilev = 0, ( num_level - 1 ) DO BEGIN

        CASE isnearest[ ilev ] of
           -1: BEGIN
              llimit = -9999.99
              ulimit = upper[0, monthid]
           END
           nlevel-1: BEGIN
              llimit = lower[ nlevel-1, monthid ]
              ulimit = -9999.99
           END
           ELSE: BEGIN
              llimit = lower[ isnearest[ilev], monthid ]
              ulimit = upper[ isnearest[ilev] + 1, monthid ]
           END
        ENDCASE

        PRINT, FORMAT='(i4,3(1x,f8.2))', profile[iprof].pressure[ islevel[ilev] ], $
               profile[ iprof ].temperature[ islevel[ilev] ], llimit, ulimit
     ENDFOR
     ;STOP

  ENDFOR

  RETURN

END ;; end of QC_TAIR_SIGNIFICANT

;; Checks that dewpoint temperatures are within acceptable limits
PRO QC_DEWPOINT, PROFILE, NEXCLUDE

  ;; initialize counter for excluded values
  nexclude = 0l

  missing = -9999.
  tdew_min = 0.
  tdew_max = 49.

  index = WHERE( ( profile.dewpoint GT missing AND $
                   profile.dewpoint LT tdew_min ) OR $
                   profile.dewpoint GT tdew_max, nexclude )
  IF ( nexclude GT 0 ) THEN profile.dewpoint[index] = -9999.99

  RETURN

END ;; end of QC_DEWPOINT

;; Checks that surface pressure is within acceptable range
PRO QC_SURFACE_PRESSURE, PROFILE, ELEVATION, NEXCLUDE

  missing = -9999.
  ps_min =  960.
  ps_max = 1060.

  ;; initialize counter for excluded values
  nexclude = 0l

  ;; Find non-missing surface pressures
  is_surface = WHERE( profile.minor_level_type EQ 1 AND $
                      profile.pressure GT missing, num_surface )
  IF ( num_surface GT 0 ) THEN BEGIN

     tmp = profile.pressure[ is_surface ]
     tmp_ps = pressure2sealevel( tmp, elevation )
     isreject = WHERE( tmp_ps LT ps_min OR $
                       tmp_ps GT ps_max, nexclude )
     IF ( nexclude GT 0 ) THEN tmp[ isreject ] = -9999.99

     profile.pressure[ is_surface ] = tmp

  ENDIF

  RETURN

END ;; end of QC_SURFACE_PRESSURE

PRO REMOVE_SOUNDING, PROFILE, NREMOVED

  nremoved = 0l

  RETURN

END ;; end of REMOVE_SOUNDING

PRO TRUNCATE_SOUNDING, PROFILE, NTRUNCATED

  ntruncated = 0l

  RETURN

END ;; end of TRUNCATE_SOUNDING

PRO TEST_QC_IGRA_PROFILE

  infile = 'test.dat'
  clmfile = 'profile_climatology.nc'
  ;; - Arctic station file
  station_list_file = 'igra_stations_arctic.txt'

  ;; get station list and extract first station
  READ_IGRA_STATION_LIST, station_list_file, station_list, NREC=nstation
  station = station_list[ 0 ]

  ;; Get station temperature climatology
  tavg = ncdf_readv( clmfile, varname='TAVG' )
  tstd = ncdf_readv( clmfile, varname='TSTD' )
  level = ncdf_readv( clmfile, varname='level' )
  tavg = tavg[ *, *, 0 ]
  tstd = tstd[ *, *, 0 ]

  ;; Read profile
  READ_IGRA_DATA_NEW, infile, profile, /NOCLEANUP

  PRINT, ''
  PRINT, '***** BEFORE QC *****'
  WRITE_IGRA_DATA, profile, /STDOUT

  QC_IGRA_PROFILE, profile, station, tavg, tstd, statistics, LEVEL=level, $
                   /DEBUG

  PRINT, ''
  PRINT, '***** After QC *****'
  WRITE_IGRA_DATA, profile, /STDOUT

  RETURN

END

;;**********************************************************************
;; MAIN ROUTINE
;;**********************************************************************
PRO QC_IGRA_PROFILE, PROFILE, STATION_INFO, TAVG, TSTD, TSURF_AVG, $
                     TSURF_STD, QC_STATISTICS, $
                     LEVEL=LEVEL, DEBUG=DEBUG

  IF ( N_ELEMENTS( profile ) EQ 0 ) THEN BEGIN
     PRINT, '% QC_IGRA_PROFILE: Argument PROFILE is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( tavg ) EQ 0 ) THEN BEGIN
     PRINT, '% QC_IGRA_PROFILE: Argument TAVG is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( tstd ) EQ 0 ) THEN BEGIN
     PRINT, '% QC_IGRA_PROFILE: Argument tstd is not defined'
     RETURN
  ENDIF

  default_level = [1000., 850., 700., 500., 400., 300., 200., 150., $
                   100., 50. ]

  level = N_ELEMENTS( level ) EQ 0 ? default_level : level

  nprofile = N_ELEMENTS( profile )

  ;; QC air temperature
  QC_TAIR_MANDATORY, profile, tavg, tstd, nexclude_ml, LEVEL=level

  QC_TSURF, profile, tsurf_avg, tsurf_std, nexclude_surf

  QC_TAIR_SIGNIFICANT, profile, tavg, tstd, nexclude_sl, LEVEL=level
  
  ;; QC dewpoint
  QC_DEWPOINT, profile, nexclude_dp
  
  ;; Check surface pressure is in acceptable limits
  QC_SURFACE_PRESSURE, profile, station_info.elevation, nexclude_ps

  ;; Weed out sounding if no surface, 850 hPa or 700 hPa
  REMOVE_SOUNDING, profile, nremoved

  ;; Truncate profile above highest valid mandatory level or 300 hPa
  TRUNCATE_SOUNDING, profile, ntruncated

  IF ( KEYWORD_SET( DEBUG ) EQ 1 ) THEN BEGIN
     PRINT, ''
     PRINT, '************* QC SUMMARY STATISTICS **************'
     PRINT, FORMAT='("Mandatory Level Temperatures Rejected:   ", i6)', nexclude_ml
     PRINT, FORMAT='("Surface Temperatures Rejected:           ", i6)', nexclude_surf
     PRINT, FORMAT='("Significant Level Temperatures Rejected: ", i6)', nexclude_sl
     PRINT, FORMAT='("Dewpoint Temperatures Rejected:          ", i6)', nexclude_dp
     PRINT, FORMAT='("Surface Rejected:                        ", i6)', nexclude_ps
     PRINT, FORMAT='("Profiles removed:                        ", i6)', nremoved
     PRINT, FORMAT='("Profiles truncated:                      ", i6)', ntruncated
     PRINT, ''
     PRINT, FORMAT='("Profiles processed:                      ", i6)', nprofile
     PRINT, FORMAT='("Profiles returned:                       ", i6)', N_ELEMENTS( profile )
  ENDIF

;  qc_statistics = {excluded_tair_ml: nexclude_ml, $
;                   excluded_tair_sl: nexlcude_sl, $
;                   excluded_tdew:    nexlcude_dp, $
;                   excluded_ps:      nexclude_ps, $
;                   removed:          nremove, $
;                   truncated:        ntruncate}

  RETURN

END

  
