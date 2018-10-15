;;----------------------------------------------------------------------
;; Plots monthly time series of water vapour variables.
;;
;; 2011-06-29 A.P.Barrett
;;----------------------------------------------------------------------

FUNCTION REGRESS_TSERIES, X, Y, RESULT_STRING=RESULT_STRING

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
  result_string = STRING( result[0]*365, p, FORMAT='("b1=",f7.4," %/yr (P=",f4.2,")")')

  ;; Plot trend line
  vmin = MIN( [MIN(yy),MIN(yfit)] )
  vmax = MAX( [MAX(yy),MAX(yfit)] )
  OPLOT, xx, yfit, COLOR=240

  ;; Write result string on plot
;  xyy = !Y.CRANGE[0] + ( (!Y.CRANGE[1] - !Y.CRANGE[0]) * 0.1 )
;  xyx = JULDAY(1,1,1980)
;  XYOUTS, xyx, xyy, result_str

  RETURN, result

END
  
PRO PLOT_TSERIES, X, Y, LEVEL, RHUM=RHUM, SHUM=SHUM, $
                NOTITLE=NOTITLE, DOREGRESS=DOREGRESS

  CASE 1 OF
     KEYWORD_SET( RHUM ): BEGIN
        title = 'Relative Humidity'
        units = '%'
        ymin = 0.
        ymax = 100.
     END
     KEYWORD_SET( SHUM ): BEGIN
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

  dummy = LABEL_DATE( DATE_FORMAT=['%Y'] )

  IF ( KEYWORD_SET( DOREGRESS ) EQ 1 ) THEN BEGIN
     result = REGRESS_TSERIES( x, y, result_string=result_string )
  ENDIF

  IF ( KEYWORD_SET(NOTITLE) EQ 1 ) THEN BEGIN
     
     title = result_string

     PLOT, x, y, MIN_VALUE=-9999., $
           XTICKFORMAT='LABEL_DATE', $
           XRANGE=[MIN(x)-365,MAX(x)+365], XSTYLE=1, $
           YRANGE=[ymin,ymax], YSTYLE=1, $
           YTITLE=units, TITLE=result_string

  ENDIF ELSE BEGIN

     title = title + ': ' + STRTRIM( level, 2 ) + ' hPa' + ' : ' + $
             result_string

     PLOT, x, y, MIN_VALUE=-9999., $
           XTICKFORMAT='LABEL_DATE', $
           XRANGE=[MIN(x)-365,MAX(x)+365], XSTYLE=1, $
           YRANGE=[ymin,ymax], YSTYLE=1, $
           YTITLE=units, TITLE=title

  ENDELSE

  PLOTS, JULDAY(1,1,1979,0), !Y.CRANGE, LINE=1, THICK=2

  RETURN

END

PRO PLOT_DAI_PROFILES_TSERIES_BATCH

  rhum = 0
  shum = 1

  station_file = 'dai_radiosonde_polar_stations.txt'
  READ_IGRA_STATION_LIST, station_file, station, NREC=nstat

  level = [1000,850,700,500,400,300]

  nstat = 1

  CASE 1 OF
     rhum EQ 1: suffix = '.rhum.tseries.png'
     shum EQ 1: suffix = '.shum.tseries.png'
     ELSE: BEGIN
        PRINT, '% PLOT_DAI_PROFILES_TSERIES_BATCH: neither SHUM or RHUM switches are set'
        RETURN
     END
  ENDCASE

  FOR istat=0, nstat-1 DO BEGIN

     PLOT_DAI_PROFILES_TSERIES, station[istat].station_number, $
                                RHUM=rhum, SHUM=shum
        
     SAVEIMAGE, station[istat].station_number + suffix, /PNG

     ERASE
        
  ENDFOR

  RETURN

END

PRO PLOT_DAI_PROFILES_TSERIES, STATION, RHUM=RHUM, SHUM=SHUM 

  fileName = station+'.month.nc'
  
  time = NCDF_READV( fileName, VARNAME='time' )
  level = NCDF_READV( filename, VARNAME='level' )

  CASE 1 OF
     KEYWORD_SET( SHUM ) EQ 1: BEGIN
        data = NCDF_READV( fileName, VARNAME='shum' )
        SHUM=1
        RHUM=0
     END
     ELSE: BEGIN
        data = NCDF_READV( fileName, VARNAME='rhum' )
        RHUM=1
        SHUM=0
     END
  ENDCASE

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

     PLOT_TSERIES, time, data[ 0, levIdx, * ], $
                   thislevel, RHUM=rhum, SHUM=shum, $
                   /DOREGRESS

  ENDFOR

END

                 
