;;----------------------------------------------------------------------
;; Makes plots of time series of monthly data from IGRA derived data sets
;;
;; 2010-12-06 A.P.Barrett
;;----------------------------------------------------------------------
;; Adds a regression line to the plot
FUNCTION SUMSQR, X

  result = TOTAL( ( x - MEAN( x ) )^2 )

  RETURN, result

END

FUNCTION CORRCOEF, b1, xx, yy

  ssx = SUMSQR( xx )
  ssy = SUMSQR( yy )

  r = b1 * SQRT( ssx / ssy )

  RETURN, r

END

FUNCTION STUDENTS_REG, CORR, N

  df = n - 2
  sdf = SQRT( df )

  denom = SQRT( 1 - ( corr * corr ) )

  result = corr * sdf / denom

  RETURN, result

END

PRO ADD_REGRESSION, X, Y, MONTH, MIN_COUNT=MIN_COUNT, $
                    YBEG=YBEG, YEND=YEND, DEBUG=DEBUG, $
                    SLOPE=SLOPE, PROB=PROB
  
  min_count = N_ELEMENTS( min_count ) EQ 0 ? 20 : min_count
  ybeg      = N_ELEMENTS( ybeg ) EQ 0 ? 1979 : ybeg
  yend      = N_ELEMENTS( yend ) EQ 0 ? 2008 : yend

  slope = -99.99
  prob = -99.99

  imon = month - 1
  
  xx = x[ imon:*:12 ]
  yy = y[ imon:*:12 ]

  ;; Subset data to ybeg to yend
  CALDAT, xx, mon, day, year
  pos = WHERE( year GE ybeg AND year LE yend, count )
  IF ( count GT 0 ) THEN BEGIN
     xx = xx[ pos ]
     yy = yy[ pos ]
  ENDIF

  xfit = TIMEGEN( START=xx[ 0 ], FINAL=xx[ count - 1 ], UNITS="Years" )
  
  pos = WHERE( yy GT -9999., count )
  IF ( count LT min_count ) THEN BEGIN
     PRINT, '% ADD_REGRESSION: Not enough valid values to compute regression'
     RETURN
  ENDIF
  xx = xx[ pos ]
  yy = yy[ pos ]

  result = REGRESS( xx, yy, SIGMA=sigma, CONST=const, CORRELATION=corr )
  yfit = const + ( xfit * result[0] )

  OPLOT, xfit, yfit, THICK=2, COLOR=240

  slope = result[ 0 ]
  prob  = 1 - ( 2 * ( 1 - T_PDF( ABS( STUDENTS_REG( corr, count ) ), count - 2 ) ) )

  IF ( KEYWORD_SET( DEBUG ) EQ 1 ) THEN BEGIN 
     PRINT, '  Constant:        ', const
     PRINT, '  Coeficients:     ', result
     PRINT, '  Standard Errors: ', sigma
     PRINT, '  Test H0         '
     PRINT, '     T-crit:       ', T_CVF( 0.025, count - 2 )
     PRINT, '          T:       ', ABS( STUDENTS_REG( corr, count ) )
     PRINT, '   P(b1=/0):       ', 1 - ( 2 * ( 1 - T_PDF( ABS( STUDENTS_REG( corr, count ) ), count - 2 ) ) )
     ;;FOR iy = 0, ( N_ELEMENTS( yfit ) - 1 ) DO PRINT, FORMAT='(C(CYI,"-",CMOI,"-",CDI),1x,f8.2)', xfit[iy], yfit[iy]
  ENDIF

  RETURN

END

;;**********************************************************************
;; MAIN ROUTINE
;;**********************************************************************
PRO MAKE_IGRA_DERIVED_PLOTS

  station_list_file = 'derived-stations-arctic-subset.txt'
  this_level = 850.
  this_hour = 12
  
  READ_IGRA_STATION_LIST, station_list_file, station, nrec=nstation
  ;nstation = 5

  ;; Open file for output
  OPENW, U, 'igra_arctic_qv_trends.txt', /GET_LUN

  FOR istat = 0, ( nstation - 1 ) DO BEGIN

     PRINT, '% Making plots for ' + STRTRIM( station[istat].station_number, 2 ) + $
            ' (' + STRTRIM( station[istat].station_name, 2 ) + ')'

     file = STRTRIM( station[istat].station_number, 2 ) + '.month.nc'
     
     time     = NCDF_READV( file, VARNAME='time' ) + julday( 1, 1, 1900 ) - 1
     level    = NCDF_READV( file, VARNAME='level' )
     obs_hour = NCDF_READV( file, VARNAME='obs_hour' )
     hurs     = NCDF_READV( file, VARNAME='hurs' )
     rh       = NCDF_READV( file, VARNAME='rh' )
     
     ilevel = WHERE( this_level EQ level, found_level )
     IF ( found_level EQ 0 ) THEN BEGIN
        PRINT, 'MAKE_IGRA_DERIVED_PLOTS: Unable to find ' + $
               STRTRIM( this_level, 2 ) + ' hPa level in ' + $
               'record for station ' + $
               STRTRIM( station[ istat].station_number, 2 ) + $
               ' (' + STRTRIM( station[ istat].station_name, 2 ) + $
               ')'
        PRINT, '...Skipping station!'
        CONTINUE
     ENDIF
     
     ihor = WHERE( this_hour EQ obs_hour, found_hour )
     IF ( found_hour EQ 0 ) THEN BEGIN
        PRINT, 'MAKE_IGRA_DERIVED_PLOTS: Unable to find ' + $
               STRTRIM( this_hour, 2 ) + 'h observation in ' + $
               'record for station ' + $
               STRTRIM( station[ istat].station_number, 2 ) + $
               ' (' + STRTRIM( station[ istat].station_name, 2 ) + $
               ')'
        PRINT, '...Skipping station!'
        CONTINUE
     ENDIF
     
     value0 = rh[ *, ihor, ilevel ]
     
     WINDOW, 0, XSIZE=1700, YSIZE=1050
     !P.MULTI=[0,3,4]
     
     print_array = MAKE_ARRAY( 24, /FLOAT, VALUE=-99.99 )

     ik = 0
     FOR month=1,12 DO BEGIN
        
        PLOT_IGRA_DERIVED_MONTHLY, time, value0, month, VARNAME='RH', $
                                   LEVEL=this_level, HOUR=this_hour, $
                                   YBEG=1960, YEND=2008, STATION=station[istat]

        ADD_REGRESSION, time, value0, month, /DEBUG, SLOPE=slope, PROB=prob

        print_array[ ik ] = slope
        print_array[ ik + 1 ] = prob
        ik = ik + 2
        
     ENDFOR
    
     PRINTF, U, FORMAT='(a5,1x,12(1x,f5.2,1x,"(",f5.2,")"))', STRTRIM( station[istat].station_number, 2 ), print_array

     outfile = STRTRIM( station[istat].station_number, 2 ) + '.month.' + $
               STRING( FORMAT='(i04)', this_level) + 'hPa.' + $
               STRING( FORMAT='(i02)', this_hour) + 'h.png'
     SAVEIMAGE, outfile, /PNG
     ;STOP

  ENDFOR

  CLOSE, U
  FREE_LUN, U

END

