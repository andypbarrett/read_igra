;;----------------------------------------------------------------------
;; PLOT_IGRA_PROFILE - plots an IGRA profile
;;
;; 2010-11-09 A.P.Barrett
;;----------------------------------------------------------------------
FUNCTION MAXVAL, array

  result = CEIL( MAX( array ) / 10. ) * 10.

  RETURN, result

END

FUNCTION MINVAL, array

  result = FLOOR( MIN( array ) / 10. ) * 10.

  RETURN, result

END

PRO MAKE_PLOT_TEMPLATE, PRESSURE_RANGE, XRANGE, XTITLE=XTITLE, STATION=STATION, $
                        YEAR=YEAR, MONTH=MONTH, DAY=DAY, HOUR=HOUR

  IF ( N_ELEMENTS( XTITLE ) EQ 0 ) THEN XTITLE = ' '

  ;; Make title
  IF ( N_ELEMENTS( STATION ) GT 0 ) THEN TITLE='STATION: '+STATION+' '
  IF ( N_ELEMENTS( YEAR ) GT 0 ) THEN  TITLE = TITLE + STRING(YEAR,FORMAT='(i4)')
  IF ( N_ELEMENTS( MONTH ) GT 0 ) THEN TITLE = TITLE + STRING(MONTH,FORMAT='(i02)')
  IF ( N_ELEMENTS( DAY ) GT 0 ) THEN   TITLE = TITLE + STRING(DAY,FORMAT='(i02)')
  IF ( N_ELEMENTS( HOUR ) GT 0 ) THEN  TITLE = TITLE + STRING(HOUR,FORMAT='(1x,i02,"h")')
  TITLE=TITLE+'!C!C'

  MANDATORY_LEVEL = [ 1000., 850., 700., 400., 300. ]

  PLOT, RANDOMU( SEED, 10 ), RANDOMU( SEED, 10 ), $
        XRANGE=XRANGE, XSTYLE=9, XTITLE=XTITLE, $
        YSTYLE=5, TITLE=TITLE, YMARGIN=[5,5], /NODATA
  AXIS, YAXIS=0, YRANGE=PRESSURE_RANGE, YTICKS=5, YTICKV=MANDATORY_LEVEL, $
        YTITLE='Pressure (hPa)', /SAVE
  AXIS, YAXIS=1, YRANGE=PRESSURE_RANGE, YTICKS=5, YTICKV=MANDATORY_LEVEL, $
        YTICKNAME=REPLICATE( ' ', 5 )

END

PRO PLOT_TAIR_AND_GEOPOT, PRESSURE, TAIR, DEWPOINT, GEOPOT_HGT, STATION=STATION, $
                          YEAR=YEAR, MONTH=MONTH, DAY=DAY, HOUR=HOUR 

  ;; Find valid values
  ;; - temperature
  tvalid = WHERE( tair GT -9999. AND pressure GT -9999., num_tvalid )
  ;; - dewpoint
  dvalid = WHERE( dewpoint GT -9999. AND pressure GT -9999., num_dvalid )
  ;; - geopotential height
  zvalid = WHERE( geopot_hgt GT -9999. AND pressure GT -9999., num_zvalid )

  IF ( num_tvalid LE 0 ) THEN BEGIN
     PRINT, '% PLOT_TAIR_AND_GEOPOT: No valid air temperature values - aborting plot'
     RETURN
  ENDIF

  ;; Find max and min of temperature and dewpoint
  tmin = MINVAL( tair[ tvalid ] )
  tmax = MAXVAL( tair[ tvalid ] )

  ;; Find max and min dewpoint values
  IF ( num_dvalid GT 0 ) THEN BEGIN
     dmin = MINVAL( dewpoint[ dvalid ] ] )
     dmax = MAXVAL( dewpoint[ dvalid ] ] )
     tmin = MIN( [ tmin, dmin ] )
     tmax = MAX( [ tmax, dmax ] )
  ENDIF

  ;; Find max and min geopotential height values
  IF ( num_dvalid GT 0 ) THEN BEGIN
     zmin = MINVAL( geopot_hgt[ dvalid ] ] )
     zmax = MAXVAL( geopot_hgt[ dvalid ] ] )
  ENDIF

  ;; Find max value of pressure.  Minimum is set to 300 hPa
  pmin = MAXVAL( pressure )
  pmax = 300.
  
  MAKE_PLOT_TEMPLATE, [ pmin, pmax ], [ tmin, tmax ], STATION=data.station_number, $
                      YEAR=data.year, MONTH=data.month, DAY=data.day, HOUR=data.obs_hour, $
                      XTITLE='Temperature (!Uo!NC)'
  
  OPLOT, tair[ tvalid ], pressure[ tvalid ], $
         PSYM=-2, THICK=2

  IF ( num_dvalid GT 0 ) THEN BEGIN
     OPLOT, dewpoint[ dvalid ], pressure[ dvalid ], PSYM=-4, THICK=2, COLOR=60
  ENDIF ELSE BEGIN
     PRINT, '% PLOT_TAIR_AND_GEOPOT: No valid dewpoint values found'
  ENDELSE

  IF ( num_zvalid GT 0 ) THEN BEGIN
     AXIS, XAXIS=1, XRANGE=[ zmin, zmax ], XSTYLE=1, XTITLE='Geopotential height (m)', /SAVE
     OPLOT, geopot_hgt[ zvalid ], pressure[ zvalid ], PSYM=-5, THICK=2, COLOR=240
  ENDIF ELSE BEGIN
     PRINT, '% PLOT_TAIR_AND_GEOPOT: No valid geopotential values found'
  ENDELSE

  RETURN

END


PRO PLOT_IGRA_PROFILE, DATA

  IF ( N_PARAMS() NE 1 ) THEN BEGIN
     PRINT, 'USAGE: PLOT_IGRA_PROFILE, DATA'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( DATA ) EQ 0 ) THEN BEGIN
     PRINT, 'PLOT_IGRA_PROFILE: Argument DATA is not defined'
     RETURN
  ENDIF

  ;; Plot temperature profile
  isvalid = WHERE( data.temperature GT -9999. AND data.pressure GT -9999., num_valid )

  IF ( num_valid GT 0 ) THEN BEGIN

     ;; Get max and min temperature
     tmax = MAXVAL( data.temperature[ isvalid ] )
     tmin = MINVAL( data.temperature[ isvalid ] )
     pmin = MAXVAL( data.pressure[ isvalid ] )
     pmax = 300.

     time_string=STRING(data.year, data.month, data.day, data.obs_hour, $
            FORMAT='(i4,"-",i02,"-",i02,": ",i02)' )

;     PLOT, data.temperature[ isvalid ], data.pressure[ isvalid ], $
;           XRANGE=[tmin, tmax ], XSTYLE=1, $
;           XTITLE='Temperature (!Uo!NC)', $
;           YRANGE=[pmin,pmax], YSTYLE=1, $
;           YTITLE='Pressure (hPA)', $
;           PSYM=-2, THICK=2, $
;           TITLE='STATION: '+data.station_number+"!C!C"+time_string

     MAKE_PLOT_TEMPLATE, [ pmin, pmax ], [ tmin, tmax ], STATION=data.station_number, $
                         YEAR=data.year, MONTH=data.month, DAY=data.day, HOUR=data.obs_hour, $
                         XTITLE='Temperature (!Uo!NC)'

     OPLOT, data.temperature[ isvalid ], data.pressure[ isvalid ], $
            PSYM=-2, THICK=2

  ENDIF ELSE BEGIN
     PRINT, 'No valid temperature values in DATA'
  ENDELSE

  ;; Plot dewpoint profile
  isvalid = WHERE( data.dewpoint GT -9999. AND data.pressure GT -9999., num_valid )
  IF ( num_valid GT 0 ) THEN BEGIN

     dmax = MAXVAL( data.dewpoint[ isvalid ] )
     dmin = MINVAL( data.dewpoint[ isvalid ] )

     print, 'dewpoint: MIN: ' + STRTRIM(dmin,2) + ' MAX: ' + STRTRIM(dmax,2) 
     print, data.dewpoint[ isvalid ]
     return

     OPLOT, data.dewpoint[ isvalid ], data.pressure[ isvalid ], $
            PSYM=-4, COLOR=60, THICK=2

  ENDIF ELSE BEGIN
     PRINT, 'No valid dewpoint values in DATA'
  ENDELSE

  ;; PLOT Geopotential height profile
  isvalid = WHERE( data.geopot_hgt GT -9999. AND data.pressure GT -9999., num_valid )
  IF ( num_valid GT 0 ) THEN BEGIN

     ;; Do nothing at the moment
     PRINT, 'Valid Geopotential Height values: ' + STRTRIM( num_valid, 2 )

  ENDIF ELSE BEGIN
     PRINT, 'No valid Geopotential height values in DATA'
  ENDELSE


  ;; Plot wind speed
  isvalid = WHERE( data.wind_speed GT -9999. AND data.pressure GT -9999., num_valid )
  IF ( num_valid GT -9999 ) THEN BEGIN
     
     ;; Do nothing at the moment
     PRINT, 'Valid Wind Speed values: ' + STRTRIM( num_valid, 2 )
     
  ENDIF ELSE BEGIN
     PRINT, 'No valid wind speed values in DATA'
  ENDELSE

  ;; Plot wind direction
  isvalid = WHERE( data.wind_direction GT -9999. AND data.pressure GT -9999., num_valid )
  IF ( num_valid GT -9999 ) THEN BEGIN
     
     ;; Do nothing at the moment
     PRINT, 'Valid Wind Direction values: ' + STRTRIM( num_valid, 2 )
     
  ENDIF ELSE BEGIN
     PRINT, 'No valid wind direction values in DATA'
  ENDELSE


     
END

