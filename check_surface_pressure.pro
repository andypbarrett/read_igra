;;----------------------------------------------------------------------
;; An exploratory routine to check surface pressures
;;----------------------------------------------------------------------
PRO BATCH_CHECK_SURFACE_PRESSURE

  station_list_file='igra_stations_arctic.txt'
  
  READ_IGRA_STATION_LIST, station_list_file, station_list, NREC=nstation
;;  nstation = 2

  pavg_array = MAKE_ARRAY( 12, nstation, /FLOAT, VALUE=-9999.99 )
  pstd_array = MAKE_ARRAY( 12, nstation, /FLOAT, VALUE=-9999.99 )

  FOR istat = 0, ( nstation - 1 ) DO BEGIN
     PRINT, '% CHECK_SURFACE_PRESSURE: currently processing station: ' + $
            station_list[istat].STATION_NUMBER + ' - ' + STRTRIM( istat+1, 2 ) + $
            ' of ' + STRTRIM( nstation, 2 )
     CHECK_SURFACE_PRESSURE, station_list[istat].STATION_NUMBER, $
                             station_list[istat].elevation, PAVG, PSTD
     pavg_array[ *, istat ] = pavg
     pstd_array[ *, istat ] = pstd
  ENDFOR

  fileout='surface_pressure_statistics.txt'
  OPENW, U, fileout, /GET_LUN 
  FOR istat = 0, ( nstation - 1 ) DO BEGIN
     PRINTF, U, FORMAT='(a5,24(1x,f8.2))', station_list[istat].station_number, $
            pavg_array[ *, istat ], pstd_array[ *, istat ]
  ENDFOR
  CLOSE, U
  FREE_LUN, U

  RETURN

END

PRO PLOT_SURFACE_PRESSURE, TIME, psurf, pavg, pstd, XRANGE=XRANGE, $
                              YRANGE=YRANGE, TITLE=TITLE

  title = N_ELEMENTS( title ) EQ 0 ? ' ' : title

  ;; Get first and last dates
  xmin = time[0]
  xmax = time[ N_ELEMENTS( time ) - 1 ]

  ;; Calculate pressure ranges
  ;; - 1 stddev
  plower = pavg - pstd
  pupper = pavg + pstd
  ;; - 4 stddev
  pex_lower = pavg - ( 4 * pstd )
  pex_upper = pavg + ( 4 * pstd )
  
  pmin = MINVAL( [ MINVAL( psurf, /NAN ), plower, 950. ] )
  pmax = MAXVAL( [ MAXVAL( psurf, /NAN ), pupper, 1070. ] )

  xrange = N_ELEMENTS( xrange ) EQ 0 ? [ xmin, xmax ] : xrange
  yrange = N_ELEMENTS( yrange ) EQ 0 ? [ pmin, pmax ] : yrange

  dummy = LABEL_DATE( DATE_FORMAT=['%Y'] )

  PLOT, RANDOMU( SEED, 10 ), RANDOMU( SEED, 10 ), $
        YRANGE=yrange, YSTYLE=1, $
        XRANGE=xrange, XSTYLE=1, XTITLE='time', $
        TITLE=title, XTICKFORMAT='LABEL_DATE'
  
  PLOTS, JULDAY( 1, 1, 1979), !Y.CRANGE, LINE=0, THICK=2, COLOR=160
  PLOTS, JULDAY( 1, 1, 2007), !Y.CRANGE, LINE=0, THICK=2, COLOR=160

  OPLOT, time, psurf, PSYM=2

  IF ( plower GE yrange[0] ) THEN PLOTS, !X.CRANGE, plower, LINE=2, THICK=2, COLOR=240
  IF ( pupper LE yrange[1] ) THEN PLOTS, !X.CRANGE, pupper, LINE=2, THICK=2, COLOR=240
  IF ( pavg GE yrange[0] AND pavg LE yrange[1] ) THEN $
     PLOTS, !X.CRANGE, pavg, LINE=0, THICK=2, COLOR=240
  ;IF ( pex_lower GE yrange[0] ) THEN PLOTS, !X.CRANGE, pex_lower, LINE=2, THICK=2, COLOR=90
  ;IF ( pex_upper LE yrange[1] ) THEN PLOTS, !X.CRANGE, pex_upper, LINE=2, THICK=2, COLOR=90
  PLOTS, !X.CRANGE,  960., LINE=2, THICK=2, COLOR=90
  PLOTS, !X.CRANGE, 1060., LINE=2, THICK=2, COLOR=90

END

PRO CHECK_SURFACE_PRESSURE, STATION_NUMBER, ELEVATION, PAVG_ARRAY, PSTD_ARRAY

profile = GET_IGRA_DATA( station_number, INDIR='data',  $
                         /VERBOSE )

;; Extract month time stamp and surface pressure
month = profile.month

time = julday( profile.month, profile.day, profile.year, profile.obs_hour )
tbeg = julday(  1,  1, 1938, 00. )
tend = julday( 12, 31, 2010, 12. )

;; Surface pressures are only stored in first level of profile
;; but not all first levels contain surface profiles so
psurf = profile.pressure[ 0 ]
not_surface = WHERE( profile.minor_level_type[ 0 ] NE 1, num_not_surface )
IF ( num_not_surface GT 0 ) THEN psurf[ not_surface ] = !VALUES.F_NAN
not_valid = WHERE( psurf LE -9999., num_not_valid )
IF ( num_not_valid GT 0 ) THEN psurf[ not_valid ] = !VALUES.F_NAN

;; Convert to sea level pressure
psl = pressure2sealevel( psurf, elevation )

pavg_array = MAKE_ARRAY( 12, /FLOAT, VALUE=-9999.99 )
pstd_array = MAKE_ARRAY( 12, /FLOAT, VALUE=-9999.99 )

WINDOW, 1, XS=1850, YS=1100
!P.MULTI = [0,3,4]

monthstr = ['January','February','March','April','May','June', $
            'July','August','September','October','November','December' ]

;; Loop through each month
FOR imon=0, 11 DO BEGIN

   thismonth = imon + 1

   is_month = WHERE( month EQ thismonth, num_month )
   IF ( num_month LT 20 ) THEN BEGIN
      PRINT, '% CHECK_SURFACE_PRESSURE: too few values found for month ' + $
             STRTRIM( thismonth, 2 ) + ': N='+STRTRIM(num_month,2) 
      CONTINUE
   ENDIF

   pavg = MEAN( psl[is_month], /NAN )
   pstd = STDDEV( psl[is_month], /NAN )

   pavg_array[ imon ] = pavg
   pstd_array[ imon ] = pstd

   PLOT_SURFACE_PRESSURE, time[ is_month ], psl[ is_month ], pavg, pstd, $
                             XRANGE=[ tbeg, tend ], TITLE=monthstr[imon]

ENDFOR

;; Write plot to image
image_file = 'surface_pressure_check.'+station_number+'.png'
SAVEIMAGE, image_file, /PNG

!P.MULTI=0

END



  
