;;----------------------------------------------------------------------
;; Generates a file containing mean, standard deviation, min, max and
;; quartiles of RH for zonal bands 60N to 70N, and 70N to 80N.
;;
;; 2011-07-08 A.P.Barrett
;;----------------------------------------------------------------------

FUNCTION STATION_LEVEL_MEAN, DATA

  nlevel = 6

  result = MAKE_ARRAY( nlevel, /FLOAT, VALUE=!VALUES.F_NAN )

  FOR ilev = 0, ( nlevel - 1 ) DO BEGIN

     result[ ilev ] = MEAN( data[ *, *, ilev, * ], /NAN )

  ENDFOR

  RETURN, result

END

FUNCTION LOC_DIM_STDDEV, X, MX

  dimsizes = SIZE( x, /DIMENSION )
  
  n0 = dimsizes[ 0 ]

  dx = MAKE_ARRAY( dimsizes, /FLOAT )
  FOR i0 = 0, n0-1 DO dx[ i0, *, *, * ] = x[ i0, *, *, * ] - mx

  nx = TOTAL( FINITE( dx ), 1, /NAN )
  sdx = TOTAL( dx, 1, /NAN )
  sdx2 = sdx * sdx

  dimsizes_r = SIZE( nx, /DIMENSION )
  sddev_x = MAKE_ARRAY( dimsizes_r, /FLOAT, VALUE=!VALUES.F_NAN )

  isNotZero = WHERE( nx GT 0, numNotZero )
  
  IF ( numNotZero GT 0 ) THEN BEGIN
     sddev_x[ isNotZero ] = float( sdx2[ isNotZero ] ) / float( nx[ isNotZero ] - 1. )
  ENDIF ELSE BEGIN
     PRINT, 'LOC_DIM_STDDEV: Denominator array is all zeros'
  ENDELSE

  return, sddev_x

END

FUNCTION LEVEL_MEAN, DATA

  nlevel = 6

  result = MAKE_ARRAY( nlevel, /FLOAT, VALUE=!VALUES.F_NAN )

  FOR ilev = 0, ( nlevel - 1 ) DO BEGIN

     result[ ilev ] = MEAN( data[ *, *, ilev, * ], /NAN )

  ENDFOR

  RETURN, result

END

FUNCTION LEVEL_STDDEV, DATA

  nlevel = 6

  result = MAKE_ARRAY( nlevel, /FLOAT, VALUE=!VALUES.F_NAN )

  FOR ilev = 0, ( nlevel - 1 ) DO BEGIN

     result[ ilev ] = STDDEV( data[ *, *, ilev, * ], /NAN )

  ENDFOR

  RETURN, result

END

FUNCTION MIDDLEQ, X

  num = N_ELEMENTS( x )

  IF ( ( num MOD 2 ) GT 0 ) THEN BEGIN
     idx = FIX( .5 * ( num + 1 ) ) - 1
     medval = x[ idx ]
  ENDIF ELSE BEGIN
     idx0 = FIX( 0.5 * num ) - 1
     idx1 = idx0 + 1
     medval = ( x[idx0] + x[idx1] ) / 2.
  ENDELSE

  RETURN, medval

END

FUNCTION LOWERQ, X

  num = N_ELEMENTS( x )

  idx = FLOAT( num ) * 0.25

  ;; Check if idx is integer value
  IF ( idx NE FLOOR( idx ) ) THEN BEGIN
     idx0 = FLOOR( idx )
     idx1 = idx0 + 1
     result = ( x[ idx0 ] + x[ idx1 ] ) / 2.
  ENDIF ELSE BEGIN
     result = x[ FLOOR( idx ) ]
  ENDELSE

  RETURN, result

END

FUNCTION UPPERQ, X

  num = N_ELEMENTS( x )

  idx = FLOAT( num ) * 0.75

  ;; Check if idx is integer value
  IF ( idx NE FLOOR( idx ) ) THEN BEGIN
     idx0 = FLOOR( idx )
     idx1 = idx0 + 1
     result = ( x[ idx0 ] + x[ idx1 ] ) / 2.
  ENDIF ELSE BEGIN
     result = x[ FLOOR( idx ) ]
  ENDELSE

  RETURN, result

END

     
PRO LEVEL_QUARTILES, DATA, LOWER, UPPER, MEDVAL

  nlevel = 6

  lower = MAKE_ARRAY( nlevel, /FLOAT, VALUE=!VALUES.F_NAN )
  upper = MAKE_ARRAY( nlevel, /FLOAT, VALUE=!VALUES.F_NAN )
  medval = MAKE_ARRAY( nlevel, /FLOAT, VALUE=!VALUES.F_NAN )

  FOR ilev = 0, ( nlevel - 1 ) DO BEGIN

     ;; Grab data for level
     tmp = data[ *, *, ilev, * ]
     ;; Extract non NaN data
     isfinite = WHERE( FINITE( tmp ) EQ 1, num_finite )
     IF ( num_finite GT 0 ) THEN tmp = tmp[ isfinite ]
     ;; Sort data
     tmp_srt = tmp[ SORT( tmp ) ]
     
     lower[ ilev ]  = LOWERQ( tmp_srt )
     upper[ ilev ]  = UPPERQ( tmp_srt )
     medval[ ilev ] = MIDDLEQ( tmp_srt )
     
  ENDFOR

  RETURN

END

PRO PLOT_HISTOGRAMS, DATA, mean60, stddev60, lower60, upper60, median60

  !P.MULTI=[0,2,3]
  FOR ilev = 0, 5 DO BEGIN
     HIST_PLOT, data[ *, *, ilev, * ]
     PLOTS, mean60[ilev], !Y.CRANGE, THICK=2, COLOR=240
     PLOTS, mean60[ilev]+stddev60[0], !Y.CRANGE, LINE=1, COLOR=240
     PLOTS, mean60[ilev]-stddev60[0], !Y.CRANGE, LINE=1, COLOR=240
     PLOTS, median60[ilev], !Y.CRANGE, THICK=2, COLOR=160
     PLOTS, lower60[ilev], !Y.CRANGE, LINE=1, COLOR=160
     PLOTS, upper60[ilev], !Y.CRANGE, LINE=1, COLOR=160
     PLOTS, mean60[ilev]+(2.*stddev60[0]), !Y.CRANGE, LINE=1, COLOR=200
     PLOTS, mean60[ilev]-(2.*stddev60[0]), !Y.CRANGE, LINE=1, COLOR=200
  ENDFOR

  RETURN

END

PRO PLOT_RESULTS, MEAN_MON, STDDEV_MON
 
  LOADCT, 0

  xmin = 210
  xmax = 285.

  level = [1000.,850.,700.,500.,400.,300.]
  !P.Multi=[0,3,4]
  FOR imon=0, 11 DO BEGIN

     x0 = mean_mon[imon,*]-stddev_mon[imon,*]
     x1 = mean_mon[imon,*]+stddev_mon[imon,*]
     xx=[TRANSPOSE(x0),REVERSE(TRANSPOSE(x1))]
     yy=[level,REVERSE(level)]

     PLOT, mean_mon[imon,*], level, XRANGE=[xmin,xmax], XSTYLE=1, $
           YRANGE=[1000.,300.], YSTYLE=1, $
           YTITLE='Level (hPa)', $
           XTITLE='%', /NODATA
     POLYFILL, xx, yy, /FILL, /DATA, COLOR=200
     OPLOT, mean_mon[imon,*], level, THICK=2

  ENDFOR

  LOADCT, 39

END

PRO WRITE_RESULTS, MEAN_MON, STDDEV_MON, STATION_MEAN, OUTFILE

  nmon  = 12
  nlev  = 6
  nstat = 9

  month = INDGEN( 12 ) + 1
  level = [1000.,850.,700.,500.,400.,300.]

  ;; Open file for writing
  cdfid = NCDF_CREATE( outfile, /CLOBBER )

  ;; Define coordinate variables
  mon_dimID = NCDF_DIMDEF( cdfid, 'month', nmon )
  lev_dimID = NCDF_DIMDEF( cdfid, 'level', nlev )
  sta_dimID = NCDF_DIMDEF( cdfid, 'station', nstat )

  ;; Define variables
  mon_varID = NCDF_VARDEF( cdfid, 'month', mon_dimID, /FLOAT )
  lev_varID = NCDF_VARDEF( cdfid, 'level', lev_dimID, /FLOAT )
  avg_varID = NCDF_VARDEF( cdfid, 'average', [mon_dimID,lev_dimID], /FLOAT )
  sdv_varID = NCDF_VARDEF( cdfid, 'stddev', [mon_dimID,lev_dimID], /FLOAT )
  sta_varID = NCDF_VARDEF( cdfid, 'station', [sta_dimID,mon_dimID,lev_dimID], /FLOAT )

  ;; Define attributes
  NCDF_ATTPUT, cdfid, mon_varID, 'long_name', 'month'
  NCDF_ATTPUT, cdfid, mon_varID, 'units', 'month of year'
  
  NCDF_ATTPUT, cdfid, lev_varID, 'long_name', 'level'
  NCDF_ATTPUT, cdfid, lev_varID, 'units', 'hPa'
  
  NCDF_ATTPUT, cdfid, avg_varID, 'long_name', 'monthly average T'
  NCDF_ATTPUT, cdfid, avg_varID, 'units', 'K'
  
  NCDF_ATTPUT, cdfid, sdv_varID, 'long_name', 'monthly standard deviation T'
  NCDF_ATTPUT, cdfid, sdv_varID, 'units', 'K'
  
  NCDF_ATTPUT, cdfid, sta_varID, 'long_name', 'monthly station average'
  NCDF_ATTPUT, cdfid, sta_varID, 'units', 'K'
  
  ;; Define global attributes
  NCDF_ATTPUT, cdfid, /GLOBAL, 'created', SYSTIME()
  NCDF_ATTPUT, cdfid, /GLOBAL, 'created_by', 'Andrew P. Barrett apbarret@nsidc.org'
  NCDF_ATTPUT, cdfid, /GLOBAL, 'description', 'statistics for radiosonde profiles for 60n to 70N'
  NCDF_ATTPUT, cdfid, /GLOBAL, 'source', 'Aiguo Dais homogenized radiosonde data set'

  NCDF_CONTROL, cdfid, /ENDEF

  ;; Write variables
  NCDF_VARPUT, cdfid, mon_varID, month
  NCDF_VARPUT, cdfid, lev_varID, level
  NCDF_VARPUT, cdfid, avg_varID, mean_mon
  NCDF_VARPUT, cdfid, sdv_varID, stddev_mon
  NCDF_VARPUT, cdfid, sta_varID, station_mean

  ;; Close file
  NCDF_CLOSE, cdfid

END

;;**********************************************************************
;; Main Routine
;;**********************************************************************

  ybeg = 1979
  yend = 2001

  timespan_string = STRING( ybeg, yend, FORMAT='(i4,"to",i4)' )

  station_file = 'dai_radiosonde_polar_stations_shum_qc_subset.txt'
  READ_IGRA_STATION_LIST, station_file, station, NREC=nstat

;  idx = WHERE( station.latitude GE 60. and station.latitude LT 70, nstat60 )
  idx = WHERE( station.latitude GE 70., nstat60 )
  IF ( nstat60 LE 0 ) THEN BEGIN
     PRINT, '% MAKE_ZONAL_PROFILE_STATISTICS: No stations exist north of 70N'
     RETURN
  ENDIF
  theseStation = idx

  ;; calculate size of time dimension
  ntim = ( yend - ybeg + 1 ) * 12

  ;; Define array to hold all station data
  all_data = MAKE_ARRAY( nstat60, 2, 6, ntim, /FLOAT, VALUE=-9999.99 )
  station_mean = MAKE_ARRAY( nstat60, 12, 6, /FLOAT, VALUE=-9999.99 )

  ;; Read data into all data array
  FOR istat = 0, nstat60 - 1 DO BEGIN

     ;; Read data from station file
     station_file = station[ idx[istat] ].station_number + '.month.nc'
     data = NCDF_READV( station_file, VARNAME='ta' )
     time = NCDF_READV( station_file, VARNAME='time' )

     ;; Convert data to kelvin
     isvalid = WHERE( data GT -9999., count )
     IF ( count GT 0 ) THEN BEGIN
        data[ isvalid ] = data[ isvalid ] + 273.15
     ENDIF

     ;; Find subset of data between 1989 and 2001
     year = UT_CALENDAR( time, 5 )
     thisyear = WHERE( year GE ybeg AND year LE yend, num_thisyear )

     ;; Put data in all_data array
     IF ( num_thisyear EQ ntim ) THEN BEGIN 
        all_data[ istat, *, *, * ] = data[ *, *, thisyear ]
     ENDIF ELSE BEGIN
        PRINT, "% MAKE_ZONAL_PROFILE_STATISTICS: Cannot find full year of data"
     ENDELSE

;     print, station[ idx[istat] ].station_number, station[ idx[istat] ].station_name
 
  ENDFOR

  ;; Get dimensions
  dims = SIZE( all_data, /DIMENSION )
  ntime = dims[3]

  ;; Set missing data to NaN
  idx = WHERE( all_data LE -9999., count )
  IF ( count GT 0 ) THEN all_data[ idx ] = !VALUES.F_NAN

  ;; Get statistics for each month
  ;; - define arrays
  mean_mon   = MAKE_ARRAY( 12, 6, /FLOAT, VALUE=!VALUES.F_NAN )
  stddev_mon = MAKE_ARRAY( 12, 6, /FLOAT, VALUE=!VALUES.F_NAN )
  lower_mon  = MAKE_ARRAY( 12, 6, /FLOAT, VALUE=!VALUES.F_NAN )
  upper_mon  = MAKE_ARRAY( 12, 6, /FLOAT, VALUE=!VALUES.F_NAN )
  median_mon = MAKE_ARRAY( 12, 6, /FLOAT, VALUE=!VALUES.F_NAN )
  ;; Get statistics
  nyear = ntime / 12
  idx = LINDGEN( nyear ) * 12
  FOR imon = 0, 11 DO BEGIN
     mean_mon[imon,*]   = LEVEL_MEAN( all_data[*,*,*,(idx+imon)] )
     stddev_mon[imon,*] = LEVEL_STDDEV( all_data[*,*,*,(idx+imon)] )
     LEVEL_QUARTILES, all_data[*,*,*,(idx+imon)], tmp_lower, tmp_upper, tmp_median
     lower_mon[imon,*]  = tmp_lower
     upper_mon[imon,*]  = tmp_upper
     median_mon[imon,*] = tmp_median
  ENDFOR

  FOR istat = 0, 8 DO BEGIN
     FOR imon = 0, 11 DO BEGIN
        station_mean[ istat, imon, * ] = STATION_LEVEL_MEAN( all_data[ istat, *, *, (idx+imon) ] )
     ENDFOR
  ENDFOR


  ;; For testing
;;  FOR imon = 0, 11 DO BEGIN
;;     PLOT_HISTOGRAMS, all_data[*,*,*,(idx+imon)], mean_mon[imon,*], stddev_mon[imon,*], $
;;                   lower_mon[imon,*], upper_mon[imon,*], median_mon[imon,*]
;;     STOP
;;  ENDFOR

  print, min( mean_mon - stddev_mon )
  print, max( mean_mon + stddev_mon )

  PLOT_RESULTS, mean_mon, stddev_mon

  WRITE_RESULTS, mean_mon, stddev_mon, station_mean, 'dai_radiosonde.ta.clm.month.profiles.70Nto90N.' + $
                                       timespan_string + '.nc'

END
