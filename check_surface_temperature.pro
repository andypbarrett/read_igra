;;----------------------------------------------------------------------
;; An exploratory routine to check surface temperatures
;;----------------------------------------------------------------------
PRO BATCH_CHECK_SURFACE_TEMPERATURE

  station_list_file='igra_stations_arctic.txt'
  
  READ_IGRA_STATION_LIST, station_list_file, station_list, NREC=nstation
;;  nstation = 2

  tavg_array = MAKE_ARRAY( 12, nstation, /FLOAT, VALUE=-9999.99 )
  tstd_array = MAKE_ARRAY( 12, nstation, /FLOAT, VALUE=-9999.99 )

  FOR istat = 0, ( nstation - 1 ) DO BEGIN
     PRINT, '% CHECK_SURFACE_TEMPERATURE: currently processing station: ' + $
            station_list[istat].STATION_NUMBER + ' - ' + STRTRIM( istat+1, 2 ) + $
            ' of ' + STRTRIM( nstation, 2 )
     CHECK_SURFACE_TEMPERATURE, station_list[istat].STATION_NUMBER, TAVG, TSTD
     tavg_array[ *, istat ] = tavg
     tstd_array[ *, istat ] = tstd
  ENDFOR

  fileout='surface_temperature_statistics.txt'
  OPENW, U, fileout, /GET_LUN 
  FOR istat = 0, ( nstation - 1 ) DO BEGIN
     PRINTF, U, FORMAT='(a5,24(1x,f8.2))', station_list[istat].station_number, $
            tavg_array[ *, istat ], tstd_array[ *, istat ]
  ENDFOR
  CLOSE, U
  FREE_LUN, U

  RETURN

END

PRO PLOT_SURFACE_TEMPERATURE, TIME, tsurf, tavg, tstd, XRANGE=XRANGE, $
                              YRANGE=YRANGE, TITLE=TITLE

  title = N_ELEMENTS( title ) EQ 0 ? ' ' : title

  ;; Get first and last dates
  xmin = time[0]
  xmax = time[ N_ELEMENTS( time ) - 1 ]

  ;; Calculate temperature ranges
  ;; - 1 stddev
  tlower = tavg - tstd
  tupper = tavg + tstd
  ;; - 4 stddev
  tex_lower = tavg - ( 4 * tstd )
  tex_upper = tavg + ( 4 * tstd )
  
  tmin = MINVAL( [ MINVAL( tsurf, /NAN ), tlower, tex_lower ] )
  tmax = MAXVAL( [ MAXVAL( tsurf, /NAN ), tupper, tex_upper ] )

  xrange = N_ELEMENTS( xrange ) EQ 0 ? [ xmin, xmax ] : xrange
  yrange = N_ELEMENTS( yrange ) EQ 0 ? [ tmin, tmax ] : yrange

  dummy = LABEL_DATE( DATE_FORMAT=['%Y'] )

  PLOT, RANDOMU( SEED, 10 ), RANDOMU( SEED, 10 ), $
        YRANGE=yrange, YSTYLE=1, $
        XRANGE=xrange, XSTYLE=1, XTITLE='time', $
        TITLE=title, XTICKFORMAT='LABEL_DATE'
  
  PLOTS, JULDAY( 1, 1, 1979), !Y.CRANGE, LINE=0, THICK=2, COLOR=160
  PLOTS, JULDAY( 1, 1, 2007), !Y.CRANGE, LINE=0, THICK=2, COLOR=160

  OPLOT, time, tsurf, PSYM=2

  IF ( tlower GE yrange[0] ) THEN PLOTS, !X.CRANGE, tlower, LINE=2, THICK=2, COLOR=240
  IF ( tupper LE yrange[1] ) THEN PLOTS, !X.CRANGE, tupper, LINE=2, THICK=2, COLOR=240
  IF ( tavg GE yrange[0] AND tavg LE yrange[1] ) THEN $
     PLOTS, !X.CRANGE, tavg, LINE=0, THICK=2, COLOR=240
  IF ( tex_lower GE yrange[0] ) THEN PLOTS, !X.CRANGE, tex_lower, LINE=2, THICK=2, COLOR=90
  IF ( tex_upper LE yrange[1] ) THEN PLOTS, !X.CRANGE, tex_upper, LINE=2, THICK=2, COLOR=90
  
END

PRO CHECK_SURFACE_TEMPERATURE, STATION_NUMBER, TAVG_ARRAY, TSTD_ARRAY

profile = GET_IGRA_DATA( station_number, INDIR='data',  $
                         /VERBOSE )

;; Extract month time stamp and surface temperature
month = profile.month

time = julday( profile.month, profile.day, profile.year, profile.obs_hour )
tbeg = julday(  1,  1, 1938, 00. )
tend = julday( 12, 31, 2010, 12. )

;; Surface temperatures are only stored in first level of profile
;; but not all first levels contain surface profiles so
tsurf = profile.temperature[ 0 ]
not_surface = WHERE( profile.minor_level_type[ 0 ] NE 1, num_not_surface )
IF ( num_not_surface GT 0 ) THEN tsurf[ not_surface ] = !VALUES.F_NAN
not_valid = WHERE( tsurf LE -9999., num_not_valid )
IF ( num_not_valid GT 0 ) THEN tsurf[ not_valid ] = !VALUES.F_NAN

tavg_array = MAKE_ARRAY( 12, /FLOAT, VALUE=-9999.99 )
tstd_array = MAKE_ARRAY( 12, /FLOAT, VALUE=-9999.99 )

WINDOW, 1, XS=1850, YS=1100
!P.MULTI = [0,3,4]

monthstr = ['January','February','March','April','May','June', $
            'July','August','September','October','November','December' ]

;; Loop through each month
FOR imon=0, 11 DO BEGIN

   thismonth = imon + 1

   is_month = WHERE( month EQ thismonth, num_month )
   IF ( num_month LT 20 ) THEN BEGIN
      PRINT, '% CHECK_SURFACE_TEMPERATURE: too few values found for month ' + $
             STRTRIM( thismonth, 2 ) + ': N='+STRTRIM(num_month,2) 
      CONTINUE
   ENDIF

   tavg = MEAN( tsurf[is_month], /NAN )
   tstd = STDDEV( tsurf[is_month], /NAN )

   tavg_array[ imon ] = tavg
   tstd_array[ imon ] = tstd

   PLOT_SURFACE_TEMPERATURE, time[ is_month ], tsurf[ is_month ], tavg, tstd, $
                             XRANGE=[ tbeg, tend ], TITLE=monthstr[imon]

ENDFOR

;; Write plot to image
image_file = 'surface_temperature_check.'+station_number+'.png'
SAVEIMAGE, image_file, /PNG

!P.MULTI=0

END



  
