;;----------------------------------------------------------------------
;; Plots monthly time series of water vapour variables.
;;
;; 2011-06-29 A.P.Barrett
;;----------------------------------------------------------------------

PRO PLOT_MONTH, X, Y, MONTH_STR, LEVEL, RHUM=RHUM, SHUM=SHUM, $
                NOTITLE=NOTITLE

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

  dummy = LABEL_DATE( DATE_FORMAT=['%Y'] )

  IF ( KEYWORD_SET(NOTITLE) EQ 1 ) THEN BEGIN
     
     PLOT, x, y, MIN_VALUE=-9999., $
           XTICKFORMAT='LABEL_DATE', $
           XRANGE=[MIN(x)-365,MAX(x)+365], XSTYLE=1, $
           YRANGE=[0,100], YSTYLE=1, $
           YTITLE=units

  ENDIF ELSE BEGIN

     title = title + ': ' + month_str + ' at ' + $
             STRTRIM( level, 2 ) + ' hPa'
     
     PLOT, x, y, MIN_VALUE=-9999., $
           XTICKFORMAT='LABEL_DATE', $
           XRANGE=[MIN(x)-365,MAX(x)+365], XSTYLE=1, $
           YRANGE=[0,100], YSTYLE=1, $
           YTITLE=units, TITLE=title

  ENDELSE

  RETURN

END

PRO PLOT_DAI_PROFILES_MONTH_BATCH

  station_file = 'dai_radiosonde_polar_stations.txt'
  READ_IGRA_STATION_LIST, station_file, station, NREC=nstat

  level = [1000,850,700,500,400,300]

  nstat=2

  FOR istat=0, nstat-1 DO BEGIN

     FOR ilevel=0, 5 DO BEGIN

        PLOT_DAI_PROFILES_MONTH, station[istat].station_number, level[ilevel], /RHUM
        
        SAVEIMAGE, station[istat].station_number + '.' + $
                   STRTRIM(level[ilevel],2) + 'hPa.month.png', /PNG

        ERASE
        
     ENDFOR

  ENDFOR

  RETURN

END

PRO PLOT_DAI_PROFILES_MONTH, STATION, THISLEVEL, RHUM=RHUM, SHUM=SHUM 

  fileName = station+'.month.nc'
  
  time = NCDF_READV( fileName, VARNAME='time' )
  level = NCDF_READV( filename, VARNAME='level' )

  CASE 1 OF
     N_ELEMENTS( SHUM ) EQ 1: BEGIN
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

  CALDAT, time, month, day, year

  levIDX = WHERE( level EQ thislevel, isLevel )
  IF ( isLevel EQ 0 ) THEN BEGIN
     PRINT, '% PLOT_DAI_PROFILES_MONTH: Unable to find level in ' + filename 
     RETURN
  ENDIF

  month_str = ['January', 'February', 'March', 'April', $
               'May', 'June', 'July', 'August', $
               'September', 'October', 'November', 'December']

  !P.MULTI = [0,3,4]

  FOR imon = 0, 11 DO BEGIN

     timIdx = WHERE( month EQ imon+1, istime )
     
     PLOT_MONTH, time[ timIdx ], data[ 0, levIdx, timIdx ], $
                 month_str[imon], thislevel, RHUM=rhum, SHUM=shum

  ENDFOR

END

                 
