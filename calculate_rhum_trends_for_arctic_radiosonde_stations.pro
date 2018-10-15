;;----------------------------------------------------------------------
;; Calculate linear trends in specific humidity for Arctic radiosonde stations
;; using least squares regression.  Arctic stations are those north of 70 N. 
;;
;; 2011-07-08 A.P.Barrett
;;----------------------------------------------------------------------

FUNCTION REGRESS_TSERIES, X, Y, SIGNIFICANCE=SIGNIFICANCE, YFIT=YFIT

  ;; Make sure no default values are included
  pos = WHERE( y GT -9999., count )
  IF ( count gt 0 ) THEN BEGIN
     xx = x[pos]
     yy = y[pos]
  ENDIF

  IF ( N_ELEMENTS( yy )  LT 10. ) THEN RETURN, -9999.99

  result = REGRESS( xx, yy, CONST=const, SIGMA=sigma, YFIT=yfit, CORRELATION=corr )

  ;; Calculate significance of slope
  n = N_ELEMENTS( xx )
  r = CORRELATE( xx, yy )
  r2 = r * r
  t = r * SQRT( n - 2 ) / SQRT( 1 - r2 )
  p = t_pdf( t, n-2 )

  significance = p

  RETURN, result

END

PRO PLOT_TSERIES, X, Y, YFIT, TREND, PROB, NAME, MONTH, HOUR, LEVEL

  tbeg = 1978
  tend = 2009

  name = STRTRIM( name, 2 )
  len = STRLEN( name )

  fmt = '(a'+STRTRIM(len,2)+',": ",a9,": ",a3,": Level ",i4,' + $
        '"hPa : Trend=",f6.3," (P=",f4.2,")")'

  title = STRING( name, month, hour, level, trend, prob, FORMAT=fmt )

  PLOT, x, y, XRANGE=[tbeg,tend], XSTYLE=1, XTITLE='time', PSYM=-2, $
        TITLE=title, YTITLE='g/kg'
  OPLOT, x, yfit
  
  RETURN

END

PRO WRITE_RESULTS, TREND, SIGNIFICANCE, OUTFILE

  nmon  = 12
  nlev  = 6
  nobs  = 2
  nstat = 9

  month = INDGEN( 12 ) + 1
  level = [1000.,850.,700.,600.,500.,300.]
  obs_hour = [0,12]

  ;; Open file for writing
  cdfid = NCDF_CREATE( outfile, /CLOBBER )

  ;; Define coordinate variables
  mon_dimID = NCDF_DIMDEF( cdfid, 'month', nmon )
  lev_dimID = NCDF_DIMDEF( cdfid, 'level', nlev )
  hor_dimId = NCDF_DIMDEF( cdfid, 'hour', nlev )
  sta_dimID = NCDF_DIMDEF( cdfid, 'station', nlev )

  ;; Define variables
  mon_varID = NCDF_VARDEF( cdfid, 'month', mon_dimID, /FLOAT )
  lev_varID = NCDF_VARDEF( cdfid, 'level', lev_dimID, /FLOAT )
  hor_varID = NCDF_VARDEF( cdfid, 'hour', lev_dimID, /FLOAT )
  sta_varID = NCDF_VARDEF( cdfid, 'station', lev_dimID, /FLOAT )

  trd_varID = NCDF_VARDEF( cdfid, 'trend', [mon_dimID,lev_dimID], /FLOAT )
  sig_varID = NCDF_VARDEF( cdfid, 'significance', [mon_dimID,lev_dimID], /FLOAT )

  ;; Define attributes
  NCDF_ATTPUT, cdfid, mon_varID, 'long_name', 'month'
  NCDF_ATTPUT, cdfid, mon_varID, 'units', 'month of year'
  
  NCDF_ATTPUT, cdfid, lev_varID, 'long_name', 'level'
  NCDF_ATTPUT, cdfid, lev_varID, 'units', 'hPa'
  
  NCDF_ATTPUT, cdfid, hor_varID, 'long_name', 'observation hour'
  NCDF_ATTPUT, cdfid, hor_varID, 'units', 'hour'

  NCDF_ATTPUT, cdfid, sta_varID, 'long_name', 'station number'
  NCDF_ATTPUT, cdfid, sta_varID, 'units', 'none'

  NCDF_ATTPUT, cdfid, trd_varID, 'long_name', 'trend'
  NCDF_ATTPUT, cdfid, trd_varID, 'units', 'g/kg/a'
  
  NCDF_ATTPUT, cdfid, sig_varID, 'long_name', 'probability trend is different from zero'
  NCDF_ATTPUT, cdfid, sig_varID, 'units', 'fraction'
  
  ;; Define global attributes
  NCDF_ATTPUT, cdfid, /GLOBAL, 'created', SYSTIME()
  NCDF_ATTPUT, cdfid, /GLOBAL, 'created_by', 'Andrew P. Barrett apbarret@nsidc.org'
  NCDF_ATTPUT, cdfid, /GLOBAL, 'description', 'Trend and significance of specific ' + $
                                              'humidity for Arctic radiosonde ' + $
                                              'stations north of 70 N'
  NCDF_ATTPUT, cdfid, /GLOBAL, 'source', 'Aiguo Dais homogenized radiosonde data set'

  NCDF_CONTROL, cdfid, /ENDEF

  ;; Write variables
  NCDF_VARPUT, cdfid, mon_varID, month
  NCDF_VARPUT, cdfid, lev_varID, level
  NCDF_VARPUT, cdfid, hor_varID, obs_hour
  NCDF_VARPUT, cdfid, sta_varID, station
  NCDF_VARPUT, cdfid, trd_varID, trend
  NCDF_VARPUT, cdfid, sig_varID, significance

  ;; Close file
  NCDF_CLOSE, cdfid

END

PRO PLOT_RESULTS, TREND, PROB

  idx = WHERE( trend LE -9999., count )
  IF ( count GT 0 ) THEN trend[idx]=!VALUES.F_NAN

  dims = SIZE( trend, /DIMENSIONS )
  nstat = dims[0]
  nobs  = dims[1]
  nmon  = dims[2]
  nlev  = dims[3]

  xtickv = INDGEN(12)+1
  xtickname = GET_MONTH_STRING(/FIRST_LETTER)

  level = [1000.,850.,700.,500.,400.,300.]

  !P.MULTI = [0,2,3]

  FOR ilev = 0, nlev-1 DO BEGIN

     ymin = MIN( trend[ *, *, *, ilev ] )
     ymax = MAX( trend[ *, *, *, ilev ] )

     PLOT, RANDOMU(seed,10), RANDOMU(seed,10), $
           XRANGE=[0,13], XSTYLE=5, $
           YRANGE=[ymin,ymax], YSTYLE=1, YTITLE='Trend (%/yr)', $
           TITLE=STRTRIM( level[ilev],2)+" hPa", /NODATA
     AXIS, XAXIS=0, XTICKS=11, XTICKV=xtickv, XTICKNAME=xtickname
     AXIS, XAXIS=1, XTICKS=11, XTICKV=xtickv, XTICKNAME=REPLICATE(' ',12)
     IF ( ymin LT 0 ) THEN PLOTS, !X.CRANGE, 0, LINE=0

     FOR imon = 0, nmon-1 DO BEGIN
        FOR istat=0, nstat-1 DO BEGIN
           IF ( prob[istat,0,imon,ilev] GT 0.9 ) THEN symbol = 2 ELSE symbol = 3
           PLOTS, imon+0.8, trend[istat,0,imon,ilev], PSYM=symbol
           IF ( prob[istat,1,imon,ilev] GT 0.9 ) THEN symbol = 2 ELSE symbol = 3
           PLOTS, imon+1.2, trend[istat,1,imon,ilev], PSYM=symbol
        ENDFOR
     ENDFOR

  ENDFOR

  RETURN

END

        
;;**********************************************************************
;; Main Routine
;;**********************************************************************
;; Arctic stations should be the following
;; JAN MAYEN                           01001
;; BJORNOYA                            01028
;; DANMARKSHAVN                        04320
;; SCORESBYSUND                        04339
;; BUKHTA TIKSI                        21824
;; BARROW                              70026
;; ALERT                               71082
;; EUREKA                              71917
;; RESOLUTE BAY                        71924

  month_str = STRUPCASE( GET_MONTH_STRING() )
  hour_str = ['00h','12h']

  ybeg = 1979
  yend = 2008

  ;; Define dimensions
  nobs = 2  ; # observation hours 00h, 12h
  nmon = 12 ; # months
  nlev = 6  ; # levels

  timespan_string = STRING( ybeg, yend, FORMAT='(i4,"to",i4)' )

  station_file = 'dai_radiosonde_polar_stations_qc_subset.txt'
  READ_IGRA_STATION_LIST, station_file, station, NREC=nstat

  theseStations = WHERE( station.latitude GE 70., numStation )
  IF ( numStation LE 0 ) THEN BEGIN
     PRINT, '% MAKE_ZONAL_PROFILE_STATISTICS: No stations exist north of 70N'
     RETURN
  ENDIF

  ;; calculate size of time dimension
  ntim = ( yend - ybeg + 1 ) * nmon
  
  ;; Define array to hold all station data
  trend        = MAKE_ARRAY( numStation, nobs, nmon, nlev, /FLOAT, VALUE=-9999.99 )
  significance = MAKE_ARRAY( numStation, nobs, nmon, nlev, /FLOAT, VALUE=-9999.99 )

  ;; Read data into all data array
  ;numStation = 1
  FOR istat = 0, numStation - 1 DO BEGIN

     ;; Read data from station file
     station_file = station[ theseStations[istat] ].station_number + '.month.rhum.qc.nc'
     data = NCDF_READV( station_file, VARNAME='rhum' )
     time = NCDF_READV( station_file, VARNAME='time' )
     level = NCDF_READV( station_file, VARNAME='level' )

     ;; Get month and year from time
     date = UT_CALENDAR( time, 0 )
     year  = date[ 0, * ]
     month = date[ 1, * ]

     ;; Find subset of data between 1979 and 2008
     yearIdx = WHERE( year GE ybeg AND year LE yend, numYear )
     IF ( numYear GT 0 ) THEN BEGIN
        data = data[ *, *, yearIdx ]
        year = year[ yearIdx ]
        month = month[ yearIdx ]
     ENDIF ELSE BEGIN
        PRINT, '% CALCULATE SHUM_TRENDS: no years found for ' + STRTRIM( ybeg, 2 ) + $
               ' to ' + STRTRIM( yend, 2 )
     ENDELSE

     ;; Loop through months and levels
     FOR imon = 0, 11 DO BEGIN

        thisMonth = imon + 1

        ;; Find indices for data in thisMonth
        monIdx = WHERE( month EQ thisMonth, numMon )
        IF ( numMon LE 0 ) THEN BEGIN
           PRINT, '% CALCULATE SHUM_TRENDS: no data for month ' + $
                  STRTRIM( thisMonth, 2 )
           STOP
        ENDIF
        
        ;; Define x
        x = year[ monIdx ]

        FOR ilev = 0, nlev - 1 DO BEGIN

           FOR iobs = 0, 1 DO BEGIN

              y = REFORM( data[ iobs, ilev, monIdx ], numMon )

              ;; Calculate trend and significance
              dydt = REGRESS_TSERIES( x, y, SIGNIFICANCE=p, YFIT=yfit )
              trend[ istat, iobs, imon, ilev ] = dydt 
              significance[ istat, iobs, imon, ilev ] = p

              ;; Plot trend and significance
              ;PLOT_TSERIES, x, y, yfit, dydt, p, station[ idx[istat] ].station_name, $
              ;              month_str[imon], hour_str[iobs], level[ilev]

              ;stop

           ENDFOR ; obs hours loop

        ENDFOR  ; level loop

     ENDFOR     ; month loop

  ENDFOR

  PLOT_RESULTS, trend, significance

  ;WRITE_RESULTS, trend, significance, LONG( station[theseStations].station_number ), $
  ;               'dai_radiosonde.shum.trend.' + timespan_string + '.nc'

END
