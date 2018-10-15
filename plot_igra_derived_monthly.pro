;;----------------------------------------------------------------------
;; Plots time series of monthly parameters for one or more mandatory 
;; levels for a given station
;;
;; 2010-12-03 A.P.Barrett
;;----------------------------------------------------------------------
PRO PLOT_IGRA_DERIVED_MONTHLY, TIME, VALUE, MONTH, VARNAME=VARNAME, $
                               LEVEL=LEVEL, HOUR=HOUR, YBEG=YBEG, $
                               YEND=YEND, STATION=STATION
  
  IF ( N_ELEMENTS( time ) EQ 0 ) THEN BEGIN
     PRINT, '% PLOT_IGRA_DERIVED_MONTHLY: Argument TIME not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( value ) EQ 0 ) THEN BEGIN
     PRINT, '% PLOT_IGRA_DERIVED_MONTHLY: Argument VALUE not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( month ) EQ 0 ) THEN BEGIN
     PRINT, '% PLOT_IGRA_DERIVED_MONTHLY: Argument MONTH not defined'
     RETURN
  ENDIF

  monthstr = [ 'January', 'February', 'March', 'April', $
               'May', 'June', 'July', 'August', $
               'September', 'October', 'November', 'December' ]

  imon = month - 1

  title = monthstr[imon]
  IF ( N_ELEMENTS( hour ) GT 0 ) THEN title = STRING(FORMAT='(i02)', hour) + ' ' + title 
  IF ( N_ELEMENTS( level ) GT 0 ) THEN title = title + ' ' + STRTRIM( FIX(level), 2 ) + ' hPa ' 
  IF ( N_ELEMENTS( varname ) GT 0 ) THEN title = title + varname
  IF ( N_ELEMENTS( station ) GT 0 ) THEN BEGIN
     title = title + ' - ' + STRTRIM( station.station_name, 2 ) + ' (' + $
             STRTRIM( station.station_number, 2 ) + ')'
  ENDIF

  x = time[ imon:*:12 ]
  y = value[ imon:*:12 ]

  nrec = N_ELEMENTS( x )

  CALDAT, x, mon, day, year
  ybeg = N_ELEMENTS( ybeg ) EQ 0 ? year[0] : ybeg
  yend = N_ELEMENTS( yend ) EQ 0 ? year[nrec-1] : yend

  tbeg = JULDAY( 1, 1, ybeg, 0 )
  tend = JULDAY( 12, 31, yend, 23 )
  
  not_valid = WHERE( y LE -9999., num_not_valid )
  IF ( num_not_valid GT 0 ) THEN y[ not_valid ] = !VALUES.F_NAN

  ymin = MINVAL( y, /NAN )
  ymax = MAXVAL( y, /NAN )

  ;print, ymin, ymax 
  dummy = LABEL_DATE( DATE_FORMAT=['%y'])

  PLOT, x, y, XRANGE=[tbeg,tend], XSTYLE=1, XTITLE='time', $
        YRANGE=[ymin,ymax], YSTYLE=1, YTITLE='mm', $
        XTICKUNITS = ['Time'], $  
        XTICKFORMAT='LABEL_DATE', PSYM=-2, THICK=2, $
        TITLE=title, FONT=1, CHARSIZE=1.75 

  RETURN

END  
  

  
