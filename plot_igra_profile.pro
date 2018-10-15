;;----------------------------------------------------------------------
;; PLOT_IGRA_PROFILE - plots an IGRA profile
;;
;; 2010-11-09 A.P.Barrett
;;----------------------------------------------------------------------
FUNCTION MAXVAL, array, NAN=NAN

  result = CEIL( MAX( array, NAN=NAN ) / 10. ) * 10.

  RETURN, result

END

FUNCTION MINVAL, array, NAN=NAN

  result = FLOOR( MIN( array, NAN=NAN ) / 10. ) * 10.

  RETURN, result

END

PRO MAKE_PLOT_TEMPLATE, PRESSURE_RANGE, XRANGE, XTITLE=XTITLE, STATION=STATION, $
                        YEAR=YEAR, MONTH=MONTH, DAY=DAY, HOUR=HOUR

  IF ( N_ELEMENTS( XTITLE ) EQ 0 ) THEN XTITLE = ' '

  ;; Make title
  TITLE=''
  IF ( N_ELEMENTS( STATION ) GT 0 ) THEN TITLE='STATION: '+STATION+' '
  IF ( N_ELEMENTS( YEAR ) GT 0 ) THEN  TITLE = TITLE + STRING(YEAR,FORMAT='(i4)')
  IF ( N_ELEMENTS( MONTH ) GT 0 ) THEN TITLE = TITLE + STRING(MONTH,FORMAT='(i02)')
  IF ( N_ELEMENTS( DAY ) GT 0 ) THEN   TITLE = TITLE + STRING(DAY,FORMAT='(i02)')
  IF ( N_ELEMENTS( HOUR ) GT 0 ) THEN  TITLE = TITLE + STRING(HOUR,FORMAT='(1x,i02,"h")')
  TITLE=TITLE+'!C!C'

  MANDATORY_LEVEL = [ 1000., 850., 700., 500., 400., 300., 200., 100., 50. ]

  PLOT, RANDOMU( SEED, 10 ), RANDOMU( SEED, 10 ), $
        XRANGE=XRANGE, XSTYLE=9, XTITLE=XTITLE, $
        YSTYLE=5, TITLE=TITLE, YMARGIN=[5,5], /NODATA
  AXIS, YAXIS=0, YRANGE=PRESSURE_RANGE, YTICKS=N_ELEMENTS( MANDATORY_LEVEL ), $
        YTICKV=MANDATORY_LEVEL, YTITLE='Pressure (hPa)', /SAVE
  AXIS, YAXIS=1, YRANGE=PRESSURE_RANGE, YTICKS=N_ELEMENTS( MANDATORY_LEVEL ), $
        YTICKV=MANDATORY_LEVEL, YTICKNAME=REPLICATE( ' ', N_ELEMENTS( MANDATORY_LEVEL ) )

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
     dmin = MINVAL( dewpoint[ dvalid ] )
     dmax = MAXVAL( dewpoint[ dvalid ] )
     tmin = MIN( [ tmin, dmin ] )
     tmax = MAX( [ tmax, dmax ] )
  ENDIF

  ;; Find max and min geopotential height values
  IF ( num_dvalid GT 0 ) THEN BEGIN
     zmin = MINVAL( geopot_hgt[ zvalid ] )
     zmax = MAXVAL( geopot_hgt[ zvalid ] )
  ENDIF

  ;; Find max value of pressure.  Minimum is set to 300 hPa
  pmin = MAXVAL( pressure )
  pmax = 300.
  
  MAKE_PLOT_TEMPLATE, [ pmin, pmax ], [ tmin, tmax ], STATION=station, $
                      YEAR=year, MONTH=month, DAY=day, HOUR=hour, $
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


PRO PLOT_WIND_SPEED_AND_DIRECTION, PRESSURE, SPEED, DIRECTION, STATION=STATION, $
                          YEAR=YEAR, MONTH=MONTH, DAY=DAY, HOUR=HOUR 

  ;; Find valid values
  ;; - temperature
  svalid = WHERE( speed GT -9999. AND pressure GT -9999., num_svalid )
  ;; - dewpoint
  dvalid = WHERE( direction GT -9999. AND pressure GT -9999., num_dvalid )

  IF ( num_svalid LE 0 ) THEN BEGIN
     PRINT, '% PLOT_WIND_SPEED_AND_DIRECTION: No valid wind speed values - aborting plot'
     RETURN
  ENDIF

  ;; Find max and min of temperature and dewpoint
  smin = MINVAL( speed[ svalid ] )
  smax = MAXVAL( speed[ svalid ] )

  ;; Set max and min wind direction values
  dmin = 0.
  dmax = 359.

  ;; Find max value of pressure.  Minimum is set to 300 hPa
  pmin = MAXVAL( pressure )
  pmax = 300.
  
  MAKE_PLOT_TEMPLATE, [ pmin, pmax ], [ smin, smax ], STATION=station, $
                      YEAR=year, MONTH=month, DAY=day, HOUR=hour, $
                      XTITLE='Wind speed (m s!U-1!N)'
  
  OPLOT, speed[ svalid ], pressure[ svalid ], $
         PSYM=-2, THICK=2

  IF ( num_dvalid GT 0 ) THEN BEGIN
     AXIS, XAXIS=1, XRANGE=[ dmin, dmax ], XSTYLE=1, XTITLE='Direction (deg)', /SAVE
     OPLOT, direction[ dvalid ], pressure[ dvalid ], PSYM=-5, THICK=2, COLOR=240
  ENDIF ELSE BEGIN
     PRINT, '% PLOT_WIND_SPEED_AND_DIRECTION: No valid wind direction found'
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

  !P.MULTI = [ 0, 2, 1 ]

  nval = N_ELEMENTS( data.dewpoint )
  print, nval
  dewpoint = MAKE_ARRAY( nval, /FLOAT, VALUE=-9999.99 )
  pos = WHERE( data.dewpoint GT -9999. AND data.temperature GT -9999., count )
  IF ( COUNT GT 0 ) THEN BEGIN
     dewpoint[ pos ] = data.temperature[ pos ] - data.dewpoint[ pos ]
  ENDIF

  PLOT_TAIR_AND_GEOPOT, data.pressure, data.temperature, dewpoint, data.geopot_hgt, $
                        STATION=data.station_number, YEAR=data.year, MONTH=data.month, $
                        DAY=data.day, HOUR=data.obs_hour

  PLOT_WIND_SPEED_AND_DIRECTION, data.pressure, data.wind_speed, data.wind_direction, $
                        STATION=data.station_number, YEAR=data.year, MONTH=data.month, $
                        DAY=data.day, HOUR=data.obs_hour
     
END

