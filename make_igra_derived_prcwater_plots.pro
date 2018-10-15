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
     xx = year[ pos ]
     yy = yy[ pos ]
  ENDIF

;;  xfit = TIMEGEN( START=xx[ 0 ], FINAL=xx[ count - 1 ], UNITS="Years" )
  xfit = FINDGEN( count ) + ybeg
  
  pos = WHERE( yy GT -9999., count )
  IF ( count LT min_count ) THEN BEGIN
     PRINT, '% ADD_REGRESSION: Not enough valid values to compute regression'
     RETURN
  ENDIF
  xx = xx[ pos ]
  yy = yy[ pos ]

  result = REGRESS( xx, yy, SIGMA=sigma, CONST=const, CORRELATION=corr )
  yfit = const + ( xfit * result[0] )

  tt = JULDAY( 6, 15, xfit )
  OPLOT, tt, yfit, THICK=2, COLOR=240

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

PRO WRITE_TO_NETCDF, STATION, SLOPE, PROB, FILEOUT

  IF ( N_PARAMS() NE 4 ) THEN BEGIN
     PRINT, 'USAGE: WRITE_MONTHLY_IGRA, DATA, PROFILE, FILEOUT'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( STATION ) EQ 0 ) THEN BEGIN
     PRINT, '% WRITE_MONTHLY_IGRA: Argument STATION is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( SLOPE ) EQ 0 ) THEN BEGIN
     PRINT, '% WRITE_MONTHLY_IGRA: Argument SLOPE is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( PROB ) EQ 0 ) THEN BEGIN
     PRINT, '% WRITE_MONTHLY_IGRA: Argument PROB is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( FILEOUT ) EQ 0 ) THEN BEGIN
     PRINT, '% WRITE_MONTHLY_IGRA: Argument FILEOUT is not defined'
     RETURN
  ENDIF

  ;; Get dimension sizes
  nmonth = 12
  nstat  = N_ELEMENTS( station )

  ;; Define netCDF file
  cdfid = NCDF_CREATE( fileout, /clobber )
  tim_dimid = NCDF_DIMDEF( cdfid, 'month', nmonth )
  obs_dimid = NCDF_DIMDEF( cdfid, 'station', nstat )

  tim_varid = NCDF_VARDEF( cdfid, 'month', [tim_dimid], /FLOAT )
  obs_varid = NCDF_VARDEF( cdfid, 'station', [obs_dimid], /FLOAT )

  lat_varid = NCDF_VARDEF( cdfid, 'lat', [ obs_dimid ], /FLOAT )
  lon_varid = NCDF_VARDEF( cdfid, 'lon', [ obs_dimid ], /FLOAT )
  slp_varid = NCDF_VARDEF( cdfid, 'slope', $
                          [ obs_dimid, tim_dimid ], /FLOAT )
  prb_varid = NCDF_VARDEF( cdfid, 'prob', $
                          [ obs_dimid, tim_dimid ], /FLOAT )

  ;; Write variable attributes
  NCDF_ATTPUT, cdfid, tim_varid, 'long_name', 'month'
  NCDF_ATTPUT, cdfid, tim_varid, 'units', 'month number 1..12'
  NCDF_ATTPUT, cdfid, obs_varid, 'long_name', 'station'
  NCDF_ATTPUT, cdfid, obs_varid, 'units', '1'

  NCDF_ATTPUT, cdfid, lat_varid,  'long_name', 'latitude'
  NCDF_ATTPUT, cdfid, lat_varid,  'units', 'degrees_north'

  NCDF_ATTPUT, cdfid, lon_varid,  'long_name', 'longitude'
  NCDF_ATTPUT, cdfid, lon_varid,  'units', 'degrees_east'

  NCDF_ATTPUT, cdfid, slp_varid,  'long_name', 'slope of precipitable water trend line'
  NCDF_ATTPUT, cdfid, slp_varid,  'units', 'mm/year'
  NCDF_ATTPUT, cdfid, slp_varid,  'standard_name', 'trend_precipitable_water'
  NCDF_ATTPUT, cdfid, slp_varid, '_FillValue', -99.99

  NCDF_ATTPUT, cdfid, prb_varid,  'long_name', 'probability of trend'
  NCDF_ATTPUT, cdfid, prb_varid,  'units', '1'
  NCDF_ATTPUT, cdfid, prb_varid,  'standard_name', 'probability_trend_precipitable_water'
  NCDF_ATTPUT, cdfid, prb_varid, '_FillValue', -99.99
  
  ;; Write global attributes
  NCDF_ATTPUT, cdfid, 'description', 'Trends of precipitable water for IGRA stations', /GLOBAL
  NCDF_ATTPUT, cdfid, 'created_by', 'Andrew P. Barrett <apbarret@nsidc.org', /GLOBAL
  NCDF_ATTPUT, cdfid, 'creation_date', systime(), /GLOBAL

  NCDF_CONTROL, cdfid, /ENDEF

  ;; Write variables
  NCDF_VARPUT, cdfid, tim_varid,  INDGEN(12)+1
  NCDF_VARPUT, cdfid, obs_varid,  INDGEN(nstat)
  NCDF_VARPUT, cdfid, lat_varid, station.latitude
  NCDF_VARPUT, cdfid, lon_varid, station.longitude
  NCDF_VARPUT, cdfid, slp_varid, slope
  NCDF_VARPUT, cdfid, prb_varid, prob

  NCDF_CLOSE, cdfid

  RETURN
  
END

;;**********************************************************************
;; MAIN ROUTINE
;;**********************************************************************
PRO MAKE_IGRA_DERIVED_PLOTS, PW300=PW300, PW850=PW850, THIS_HOUR=THIS_HOUR, $
                             POSTSCRIPT=POSTSCRIPT, STDOUT=STDOUT

  loadct, 39

  station_list_file = 'derived-stations-arctic-subset.txt'

  this_hour = N_ELEMENTS( this_hour ) EQ 0 ? 12 : this_hour
  this_level = KEYWORD_SET( pw300 ) EQ 1 ? 300 : 850
 
  READ_IGRA_STATION_LIST, station_list_file, station, nrec=nstation
  ;nstation = 2

  nmonth = 12
  slope_sav = MAKE_ARRAY( nstation, nmonth, /FLOAT, VALUE=-99.99 )
  prob_sav = MAKE_ARRAY( nstation, nmonth, /FLOAT, VALUE=-99.99 )

  ;; Open file for output
  IF ( KEYWORD_SET( STDOUT ) EQ 1 ) THEN BEGIN
     U = -1
  ENDIF ELSE BEGIN
     OPENW, U, 'igra_arctic_prcwater_trends.'  + $
            STRING( FORMAT='(i04)', this_level) + 'hPa.' + $
            STRING( FORMAT='(i02)', this_hour) + 'h.txt', /GET_LUN
  ENDELSE

  WINDOW, 0, XSIZE=1700, YSIZE=1050

  FOR istat = 0, ( nstation - 1 ) DO BEGIN

     PRINT, '% Making plots for ' + STRTRIM( station[istat].station_number, 2 ) + $
            ' (' + STRTRIM( station[istat].station_name, 2 ) + ')'

     file = STRTRIM( station[istat].station_number, 2 ) + '.prcwater.month.nc'
     
     time     = NCDF_READV( file, VARNAME='time' ) + julday( 1, 1, 1900 ) - 1
     obs_hour = NCDF_READV( file, VARNAME='obs_hour' )
     IF ( KEYWORD_SET( PW300 ) EQ 1 ) THEN BEGIN
        pw = NCDF_READV( file, VARNAME='pw300' )
     ENDIF ELSE BEGIN
        pw = NCDF_READV( file, VARNAME='pw850' )
     ENDELSE

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
     
     value0 = pw[ *, ihor ]

     !P.MULTI=[0,3,4]
     
;     print_array = MAKE_ARRAY( 24, /FLOAT, VALUE=-99.99 )
     slope_array = MAKE_ARRAY( 12, /FLOAT, VALUE=-99.99 )
     prob_array  = MAKE_ARRAY( 12, /FLOAT, VALUE=-99.99 )

     IF ( KEYWORD_SET( postscript ) EQ 1 ) THEN BEGIN
        outfile = STRTRIM( station[istat].station_number, 2 ) + '.prcwater.month.' + $
                  STRING( FORMAT='(i04)', this_level) + 'hPa.' + $
                  STRING( FORMAT='(i02)', this_hour) + 'h.eps'
        PSON, FILE=outfile
        DEVICE, /encapsulated
     ENDIF ELSE BEGIN
        outfile = STRTRIM( station[istat].station_number, 2 ) + '.prcwater.month.' + $
                  STRING( FORMAT='(i04)', this_level) + 'hPa.' + $
                  STRING( FORMAT='(i02)', this_hour) + 'h.png'
     ENDELSE

     ik = 0
     FOR month=1,12 DO BEGIN
        
        PLOT_IGRA_DERIVED_MONTHLY, time, value0, month, VARNAME='PW', $
                                   LEVEL=this_level, HOUR=this_hour, $
                                   YBEG=1970, YEND=2008, STATION=station[istat]

        ADD_REGRESSION, time, value0, month, SLOPE=slope, PROB=prob  ;, /DEBUG

        ;; For netCDF file
        slope_sav[ istat, month-1 ] = slope
        prob_sav[ istat, month-1 ] = prob

        ;; Array for printing
;        print_array[ ik ] = slope GT -99. ? slope*1e4 : -99.99
;        print_array[ ik + 1 ] = prob
        slope_array[ month-1 ] = slope GT -99. ? slope*10. : -99.99
        prob_array[ month-1 ]  = prob

        ik = ik + 1
        
     ENDFOR
    
     PRINTF, U, FORMAT='(a5,1x,12(1x,f8.4))', STRTRIM( station[istat].station_number, 2 ), slope_array
     PRINTF, U, FORMAT='(6x,12(1x,f8.4))', prob_array

     IF ( KEYWORD_SET( postscript ) EQ 1 ) THEN BEGIN
        PSOFF
     ENDIF ELSE BEGIN
        SAVEIMAGE, outfile, /PNG
        ;;STOP
     ENDELSE

  ENDFOR

  IF ( KEYWORD_SET( STDOUT ) EQ 0 ) THEN BEGIN
     CLOSE, U
     FREE_LUN, U
  ENDIF

  ;; Write to netCDF file
;  ncfile = 'igra.prcwater.month.trend.' + $
;           STRING( FORMAT='(i04)', this_level) + 'hPa.' + $
;           STRING( FORMAT='(i02)', this_hour) + 'h.nc'
;  WRITE_TO_NETCDF, station, slope_sav, prob_sav, ncfile

END

