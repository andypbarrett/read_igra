;;----------------------------------------------------------------------
;; Plots monthly time series of water vapour variables.
;;
;; 2011-06-29 A.P.Barrett
;;----------------------------------------------------------------------

FUNCTION REGRESS_TSERIES, X, Y

  IF ( TOTAL( Y GT -9999. ) LT 10. ) THEN RETURN, -9999.99

  ;; Make sure no default values are included
  pos = WHERE( x GE JULDAY(1,1,1979,0) AND $
               y GT -9999., count )
  IF ( count gt 0 ) THEN BEGIN
     xx = x[pos]
     yy = y[pos]
  ENDIF

  result = REGRESS( xx, yy, CONST=const, SIGMA=sigma, YFIT=yfit, CORRELATION=corr )

  ;PRINT, 'Constant: ',const
  ;PRINT, 'Coeficient: ',result[*]
  ;PRINT, 'Standard Errors: ',sigma[*]
  ;PRINT, 'Correlation: ', corr

  ;; Calculate significance of slope
  n = N_ELEMENTS( xx )
  r = CORRELATE( xx, yy )
  r2 = r * r
  t = r * SQRT( n - 2 ) / SQRT( 1 - r2 )
  p = t_pdf( t, n-2 )

  ;; String to write result
  result_str = STRING( result[0]*365, p, FORMAT='("b1=",f7.4," %/yr (P=",f4.2,")")')

  ;; Plot trend line
  vmin = MIN( [MIN(yy),MIN(yfit)] )
  vmax = MAX( [MAX(yy),MAX(yfit)] )
  OPLOT, xx, yfit, COLOR=240

  ;; Write result string on plot
  XYOUTS, JULDAY(1,1,1980), 10., result_str

  RETURN, result

END
  
PRO PLOT_TSERIES, X, Y, LEVEL, RHUM=RHUM, SHUM=SHUM, $
                NOTITLE=NOTITLE, DOREGRESS=DOREGRESS

  CASE 1 OF
     N_ELEMENTS( RHUM ) GT 0: BEGIN
        title = 'Relative Humidity'
        units = '%'
        ymin = 0.
        ymax = 100.
     END
     N_ELEMENTS( SHUM ) GT 0: BEGIN
        title = 'Specific Humidity'
        units = 'g/kg'
        ymin = 0.
        ymax = MAX( y )
     END
     ELSE: BEGIN
        title = ''
        units = ''
        ymin = min(y)
        ymax = max(y)
     END
  ENDCASE

  ;; calculate mean and standard deviation of y
  idx = WHERE( y GT -9999., count )
  IF ( count GT 0 ) THEN BEGIN
     yp = y[ idx ]
     ymean = MEAN( yp )
     ystdv = STDEV( yp )
     yuppr = ymean+(4*ystdv) GT 100 ? 100. : ymean+(4*ystdv)
     ylowr = ymean-(4*ystdv) LT 0 ? 0. : ymean-(4*ystdv)
  ENDIF ELSE BEGIN
     ymean = -9999.99
     ystdv = -9999.99
     yuppr = -9999.99
     ylowr = -9999.99
  ENDELSE

  dummy = LABEL_DATE( DATE_FORMAT=['%Y'] )

  IF ( KEYWORD_SET(NOTITLE) EQ 1 ) THEN BEGIN
     
     PLOT, x, y, MIN_VALUE=-9999., $
           XTICKFORMAT='LABEL_DATE', $
           XRANGE=[MIN(x)-365,MAX(x)+365], XSTYLE=1, $
           YRANGE=[ymin,ymax], YSTYLE=1, $
           YTITLE=units, XTICKS=6, XMINOR=5

  ENDIF ELSE BEGIN

     title = title + ': ' + STRTRIM( level, 2 ) + ' hPa'
     
     PLOT, x, y, MIN_VALUE=-9999., $
           XTICKFORMAT='LABEL_DATE', $
           XRANGE=[MIN(x)-365,MAX(x)+365], XSTYLE=1, $
           YRANGE=[ymin,ymax], YSTYLE=1, $
           YTITLE=units, TITLE=title, XTICKS=6, XMINOR=5

  ENDELSE

  PLOTS, JULDAY(1,1,1979,0), !Y.CRANGE, LINE=1, THICK=2

  IF ( ymean GT -9999. ) THEN PLOTS, !X.CRANGE, ymean, LINE=1, COLOR=240, THICK=2
  IF ( yuppr GT -9999. ) THEN PLOTS, !X.CRANGE, yuppr, LINE=1, COLOR=240, THICK=2
  IF ( ylowr GT -9999. ) THEN PLOTS, !X.CRANGE, ylowr, LINE=1, COLOR=240, THICK=2

  IF ( KEYWORD_SET( DOREGRESS ) EQ 1 ) THEN BEGIN
     result = REGRESS_TSERIES( x, y )
  ENDIF

  RETURN

END

PRO PLOT_DAI_RHUM_QC_PROFILES_TSERIES_BATCH

  station_file = 'dai_radiosonde_polar_stations_qc_subset.txt'
  READ_IGRA_STATION_LIST, station_file, station, NREC=nstat

  level = [1000,850,700,500,400,300]

  ;;nstat = 1

  suffix = '.rhum.12h.qc.tseries.png'

  FOR istat=0, nstat-1 DO BEGIN

     PLOT_DAI_RHUM_QC_PROFILES_TSERIES, station[istat].station_number, OBS_HOUR_ID=1
        
     SAVEIMAGE, station[istat].station_number + suffix, /PNG

     ERASE
        
  ENDFOR

  RETURN

END

PRO PLOT_DAI_RHUM_QC_PROFILES_TSERIES, STATION, OBS_HOUR_ID=OBS_HOUR_ID

  obs_hour_id = N_ELEMENTS( OBS_HOUR_ID ) EQ 0 ? 0 : obs_hour_id

  fileName = station+'.month.rhum.qc.nc'
  
  time = NCDF_READV( fileName, VARNAME='time' )
  level = NCDF_READV( filename, VARNAME='level' )

  data = NCDF_READV( fileName, VARNAME='rhum' )

  level = [1000,850,700,600,500,300]

  CALDAT, time, month, day, year

  !P.MULTI = [0,2,3]

  FOR ilevel = 0, 5 DO BEGIN

     thisLevel = level[ilevel]

     levIDX = WHERE( level EQ thislevel, isLevel )

     IF ( isLevel EQ 0 ) THEN BEGIN
        PRINT, '% PLOT_DAI_PROFILES_TSERIES: Unable to find level in ' + filename 
        RETURN
     ENDIF

     PLOT_TSERIES, time, data[ obs_hour_id, levIdx, * ], $
                   thislevel, /RHUM, /DOREGRESS

  ENDFOR

END

                 
