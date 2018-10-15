;;----------------------------------------------------------------------
;; Calculates a monthly climatology of temperature profiles for each 
;; station.  Climatologies are calculated for mandatory levels up to 50 hPa
;; using the method described in Ross and Elliot.
;;
;; 2010-11-18 A.P.Barrett
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; Procedures and Functions
;;----------------------------------------------------------------------

;; Extract climatological air temperature profile from
;; Oort's data set for a given lat and lon cell
FUNCTION GET_OORT_TAIR_CLIMO, FILI, LAT, LON, LEVEL=LEVEL

  oort_level = [ 1000.0, 950.0, 900.0, 850.0, 700.0, $
                 500.0, 400.0, 300.0, 200.0, 100.0, 50.0 ]
  level = ( N_ELEMENTS( level ) EQ 0 ) ? oort_level : level
 
  tair = NCDF_READV( fili, VARNAME='tair' )
  y    = NCDF_READV( fili, VARNAME='Y' )
  x    = NCDF_READV( fili, VARNAME='X' )

  nlev = N_ELEMENTS( level )

  result = MAKE_ARRAY( [ nlev, 12 ], /FLOAT, VALUE=-9999.99 )
  levid  = MAKE_ARRAY( nlev, /INTEGER, VALUE=-1 )

  ;; Find index of nearest cell by latitude 
  yid = VALUE_LOCATE( y, lat )
  xid = VALUE_LOCATE( x, lon )
  ;; get indices of pressure levels
  FOR ilev=0, ( nlev - 1 ) DO BEGIN
     levid[ ilev ] = WHERE( oort_level EQ level[ ilev ] )
  ENDFOR

  idx = WHERE( levid GT -1, nfound )
  IF ( nfound GT 0 ) THEN result[ idx, * ] = tair[ xid, yid, levid[ idx ], * ]

  IF ( nfound NE nlev ) THEN BEGIN
     PRINT, '% GET_OORT_TAIR_CLIMO: Warning - could not find all requested levels'
  ENDIF

  RETURN, result
  
END  ;; end of GET_OORT_TAIR_CLIMO

;; Extracts data for a given month from station profile data
FUNCTION GET_MONTH_DATA, DATA, TIME, MONTH, NDATA=NDATA

  pos = WHERE( time EQ month, ndata )
  IF ( ndata GT 0 ) THEN BEGIN
     result = data[ pos, * ]
  ENDIF ELSE BEGIN
     result = -1
  ENDELSE

  RETURN, result

END ;; end of GET_MONTH_DATA

;; Performs a first pass QC on temperature data - temperatures that exceed
;; +/- 30C of Oort temperatures are removed
;;
;; THRESHOLD - the deviation in degrees C beyond which data is excluded
;;             ( Default = 30C)
;; NEXCLUDE  - a n-level array containing the number oftemperatures
;;             excluded and removed by QC procedure
FUNCTION FIRST_PASS_QC, IGRA, OORT, THRESHOLD=THRESHOLD, $
                        NEXCLUDE=NEXCLUDE, DEBUG=DEBUG

  debug = N_ELEMENTS( debug ) EQ 0 ? 0 : debug

  threshold = N_ELEMENTS( threshold ) EQ 0 ? 30. : threshold

  ;; Get size of IGRA
  dims = SIZE( igra, /DIMENSIONS )
  nrec = dims[ 0 ]
  nlev = dims[ 1 ]

  ;; define arrays to return
  result = igra
  nexclude = MAKE_ARRAY( nlev, /INTEGER, VALUE=0 )

  fmt = '(i2,7(1x,f8.2))'
  FOR irec = 0, ( nrec - 1 ) DO BEGIN

     test = ( igra[ irec, * ] LT -9999. ) OR $
            ( ( igra[ irec, * ] GT ( oort - 30. ) ) AND $
              ( igra[ irec, * ] LT ( oort + 30. ) ) )
            

     isextreme = WHERE( test EQ 0, num_extreme )
     IF ( num_extreme GT 0 ) THEN BEGIN
        result[ irec, isextreme ] = -9999.99
        nexclude[ isextreme ] = nexclude[ isextreme ] + 1
     ENDIF

     IF ( DEBUG ) THEN BEGIN
        FOR ilev = 0, ( nlev - 1 ) DO BEGIN
           PRINT, FORMAT=fmt, ilev, igra[ irec, ilev ], oort[ilev], $
                  oort[ilev] + [ -30., 30. ], test[ ilev ], $
                  result[ irec, ilev ], nexclude[ ilev ]
        ENDFOR
        STOP
     ENDIF

  ENDFOR

  RETURN, result

END ;; end of FIRST_PASS_QC

;; Calculates mean and standard deviation of data
PRO GET_CLIMATOLOGY, DATA, TAVG, TSTD, DEBUG=DEBUG 

  dims = SIZE( data, /DIMENSIONS )
  nrec = dims[0]
  nlev = dims[1]

  tmp = data

  ;; Set values < -9999 to NAN
  notvalid = WHERE( data LT -9999., num_notvalid )
  IF ( num_notvalid GT 0 ) THEN BEGIN
     tmp[ notvalid ] = !VALUES.F_NAN
  ENDIF

  tavg = MAKE_ARRAY( nlev, /FLOAT, VALUE=-9999.99 )
  tstd = MAKE_ARRAY( nlev, /FLOAT, VALUE=-9999.99 )

  FOR ilev = 0, ( nlev - 1 ) DO BEGIN

     IF ( KEYWORD_SET( DEBUG ) EQ 1 ) THEN BEGIN
        PRINT, '% GET_CLIMATOLOGY:    ilev=' + STRTRIM( ilev, 2 )
        PRINT, '                     # Obs=' + $
               STRTRIM( N_ELEMENTS( tmp[ *, ilev ] ), 2 )
        PRINT, '                   # valid=' + $
               STRTRIM( TOTAL( FINITE( tmp[ *, ilev ] ) ), 2 )
     ENDIF

     ;; Check that there are enough values to calculate mean
     IF ( TOTAL( FINITE( tmp[ *, ilev ] ) ) LE 20 ) THEN BEGIN
        IF ( KEYWORD_SET( VERBOSE ) EQ 1 ) THEN BEGIN
           PRINT, '% GET_CLIMATOLOGY: Less than 20 values at level ' + STRTRIM( ilev, 2 )
           PRINT, '                   Unable to calculate stats'
        ENDIF
        CONTINUE
     ENDIF

     tavg[ ilev ] = MEAN( tmp[ *, ilev ], /NAN )
     tstd[ ilev ] = STDDEV( tmp[ *, ilev ], /NAN )

  ENDFOR

  RETURN

END ;; end of GET_CLIMATOLOGY

;; Performs a second pass QC on temperature - temperatures are excluded if
;; they exceed +/- 5 standard deviations of the average temperature
FUNCTION SECOND_PASS_QC, IGRA, CLIMAVG, CLIMSTD, NSTD=NSTD, $
                         NEXCLUDE=NEXCLUDE, DEBUG=DEBUG

  nstd = N_ELEMENTS( nstd ) EQ 0 ? 5 : nstd
  
  ;; Get size of IGRA
  dims = SIZE( igra, /DIMENSIONS )
  nrec = dims[ 0 ]
  nlev = dims[ 1 ]

  ;; define arrays to return
  result = igra
  nexclude = MAKE_ARRAY( nlev, /INTEGER, VALUE=0 )

  ;; Set limits
  clm_lower = climavg - ( nstd * climstd )
  clm_upper = climavg + ( nstd * climstd )

  FOR irec = 0, ( nrec - 1 ) DO BEGIN

     test = ( igra[ irec, * ] LT -9999. ) OR $
            ( ( igra[ irec, * ] GT clm_lower ) AND $
              ( igra[ irec, * ] LT clm_upper ) )
            

     isextreme = WHERE( test EQ 0, num_extreme )
     IF ( num_extreme GT 0 ) THEN BEGIN
        result[ irec, isextreme ] = -9999.99
        nexclude[ isextreme ] = nexclude[ isextreme ] + 1
     ENDIF

     IF ( KEYWORD_SET( DEBUG ) ) THEN BEGIN
        FOR ilev = 0, ( nlev - 1 ) DO BEGIN
           PRINT, FORMAT=fmt, ilev, igra[ irec, ilev ], oort[ilev], $
                  oort[ilev] + [ -30., 30. ], test[ ilev ], $
                  result[ irec, ilev ], nexclude[ ilev ]
        ENDFOR
        STOP
     ENDIF

  ENDFOR

  RETURN, result

END ;; end of SECOND_PASS_QC

PRO PLOT_CLIMO_BOUNDS, LEVEL, AVG, STD, NSTD, COLOR=COLOR

  color = N_ELEMENTS( color ) EQ 0 ? 255 : color

  y = level
  x0 = avg
  xp = std

  pos = WHERE( y LT -9999., count )
  IF ( count GT 0 ) THEN y[ pos ] = !VALUES.F_NAN
  pos = WHERE( x0 LT -9999., count )
  IF ( count GT 0 ) THEN x0[ pos ] = !VALUES.F_NAN
  pos = WHERE( xp LT -9999., count )
  IF ( count GT 0 ) THEN xp[ pos ] = !VALUES.F_NAN

  OPLOT, x0, y, THICK=2, COLOR=color
  OPLOT, x0-(nstd * xp), y, THICK=2, LINE=1, COLOR=color
  OPLOT, x0+(nstd * xp), y, THICK=2, LINE=1, COLOR=color
  
  RETURN

END

PRO MAKE_PLOT, LEVEL, IGRA, OORT, TAVG0, TSTD0, TAVG1, TSTD1, MONTH, STATION

  dims = SIZE( igra, /DIMENSIONS )
  nrec = dims[ 0 ]
  nlev = dims[ 1 ]

  pmin = 1013.
  pmax = 40.
  
  tmin = MINVAL( [ MIN( igra[ WHERE( igra GT -9999. ) ] ), $
                   MIN( oort-30. ) ] )
  tmax = MAXVAL( [ MAX( igra ), MAX( oort+30. ) ] )

  MAKE_PLOT_TEMPLATE, [ pmin, pmax ], [ tmin, tmax ], $
                      XTITLE='Temperature (!Uo!NC)', $
                      MONTH=month, STATION=station

  FOR ilev = 0, ( nlev - 1 ) DO PLOTS, igra[ *, ilev ], level[ ilev ], PSYM=2
  OPLOT, oort, level, THICK=2
  OPLOT, oort-30., level, THICK=2, LINE=1
  OPLOT, oort+30., level, THICK=2, LINE=1

  PLOT_CLIMO_BOUNDS, level, tavg0, tstd0, 5, COLOR=240
;  OPLOT, tavg0, level, THICK=2, COLOR=240, MIN_VALUE=-9999.
;  OPLOT, tavg0-( 5 * tstd0 ), level, THICK=2, COLOR=240, LINE=1, MIN_VALUE=-9999.
;  OPLOT, tavg0+( 5 * tstd0 ), level, THICK=2, COLOR=240, LINE=1, MIN_VALUE=-9999.
  
  PLOT_CLIMO_BOUNDS, level, tavg1, tstd1, 4, COLOR=190
;  OPLOT, tavg1, level, THICK=2, COLOR=190, MIN_VALUE=-9999.
;  OPLOT, tavg1-( 4 * tstd1 ), level, THICK=2, COLOR=190, LINE=1, MIN_VALUE=-9999.
;  OPLOT, tavg1+( 4 * tstd1 ), level, THICK=2, COLOR=190, LINE=1, MIN_VALUE=-9999.
  
  RETURN

END

PRO WRITE_RESULTS, TAVG, TSTD, NEXCLUDE, LEVEL

  dims = SIZE( tavg, /DIMENSIONS )
  nlev = dims[ 0 ]
  nmon = dims[ 1 ]

  fmt0 = '(i2,1x,f6.1,12(1x,f8.2))'
  fmt1 = '(i2,1x,f6.1,12(1x,i8))'
  monthstr = ['Jan','Feb','Mar','Apr','May','Jun', $
              'Jul','Aug','Sep','Oct','Nov','Dec']

  PRINT, ''
  PRINT, 'Average Temperature ( deg C )'
  PRINT, '-----------------------------'
  PRINT, FORMAT='(9x,12(3x,a3,3x))', monthstr
  FOR ilev = 0, ( nlev - 1 ) DO PRINT, FORMAT=fmt0, ilev, level[ ilev ], tavg[ ilev, * ]

  PRINT, ''
  PRINT, 'Stdev Temperature ( deg C )'
  PRINT, '-----------------------------'
  PRINT, FORMAT='(9x,12(3x,a3,3x))', monthstr
  FOR ilev = 0, ( nlev - 1 ) DO PRINT, FORMAT=fmt0, ilev, level[ ilev ], tstd[ ilev, * ]

  PRINT, ''
  PRINT, 'Values Excluded'
  PRINT, '-----------------------------'
  PRINT, FORMAT='(9x,12(3x,a3,3x))', monthstr
  FOR ilev = 0, ( nlev - 1 ) DO PRINT, FORMAT=fmt1, ilev, level[ ilev ], nexclude[ ilev, * ]

  RETURN

END

;;----------------------------------------------------------------------
;; Main Routine
;;----------------------------------------------------------------------
PRO CALC_PROFILE_CLIMATOLOGY_QA, DATA, OORT_TAIR, TAVG_FINAL, TSTD_FINAL, $
                                 NEXCL_FIN, LEVEL=LEVEL, DEBUG=DEBUG, $
                                 STATION=STATION, DOPLOT=DOPLOT

  IF ( N_PARAMS() NE 5 ) THEN BEGIN
     PRINT, 'USAGE: CALC_PROFILE_CLIMATOLOGY_QA, DATA, OORT_TAIR, ' + $
            'TAVG_FINAL, TSTD_FINAL, NEXCL_FIN, LEVEL=LEVEL, DEBUG=DEBUG'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( DATA ) EQ 0 ) THEN BEGIN
     PRINT, '% CALC_PROFILE_CLIMATOLOGY_QA: Argument DATA is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( OORT_TAIR ) EQ 0 ) THEN BEGIN
     PRINT, '% CALC_PROFILE_CLIMATOLOGY_QA: Argument OORT_TAIR is not defined'
     RETURN
  ENDIF

  level = N_ELEMENTS( level ) EQ 0 ? [1000.0, 850.0, 700.0, 500.0, $
                                      400.0, 300.0, 200.0, 100.0, 50.0 ] : $
          level
  nlev = N_ELEMENTS( level )

  station = N_ELEMENTS( station ) EQ 0 ? STRTRIM( data[0].station_number, 2 ) : $
            STRTRIM( station, 2 ) + ' (' + STRTRIM( data[0].station_number, 2 ) + ')'

  image_file_pref = 'station_climatology.' + STRTRIM( data[0].station_number, 2 )

  ;; Extract mandatory levels
  CREATE_MANDATORY_LEVEL_ARRAY, data, tair, gph, tdew, wdir, wspd, $
                                LEVEL=level

  tavg_final = MAKE_ARRAY( nlev, 12, /FLOAT, VALUE=-9999.99 )
  tstd_final = MAKE_ARRAY( nlev, 12, /FLOAT, VALUE=-9999.99 )
  nexcl_fin  = MAKE_ARRAY( nlev, 12, /FLOAT, VALUE=-9999.99 )

  ;; Set up plotting
  WINDOW, 1, XS=1300, YS=1100
  !P.MULTI=[0,3,4]

  ;; Loop through months
  FOR imon = 0, 11 DO BEGIN

     thismonth = imon + 1

     tair_month = GET_MONTH_DATA( tair, data.month, thismonth, NDATA=nprofile )

     PRINT, FORMAT='("Month: ",i2," N-data=",i4)', thismonth, nprofile

     month_data0 = FIRST_PASS_QC( tair_month, oort_tair[*,imon], NEXCLUDE=nexclude0 )

     GET_CLIMATOLOGY, month_data0, tavg0, tstd0, DEBUG=DEBUG

     month_data1 = SECOND_PASS_QC( month_data0, tavg0, tstd0, NEXCLUDE=nexclude1 )

     GET_CLIMATOLOGY, month_data1, tavg1, tstd1, DEBUG=DEBUG

     IF ( KEYWORD_SET( DEBUG ) EQ 1 ) THEN BEGIN
        ;FOR ilev = 0, ( nlev - 1 ) DO BEGIN
        ;   PRINT, FORMAT=fmt, ilev, level[ ilev ], oort_tair[ ilev, imon ], $
        ;          nexclude0[ ilev ], tavg0[ ilev ], tstd0[ ilev ], $
        ;          nexclude1[ ilev ], tavg1[ ilev ], tstd1[ ilev ]
        ;ENDFOR
        ;STOP
     ENDIF

     IF ( KEYWORD_SET( DOPLOT ) EQ 1 ) THEN BEGIN 
        MAKE_PLOT, level, tair_month, oort_tair[ *, imon ], $
                   tavg0, tstd0, tavg1, tstd1, thismonth, station
     ENDIF

     tavg_final[ *, imon ] = tavg1
     tstd_final[ *, imon ] = tstd1
     nexcl_fin[ *, imon ]  = nexclude0 + nexclude1

  ENDFOR

  ;; Save image
  image_file = STRING( FORMAT='(a,".png")', image_file_pref )
  PRINT, '   - Writing image to ' + image_file
  SAVEIMAGE, image_file, /PNG
  !P.MULTI=0

  IF ( KEYWORD_SET( DEBUG ) EQ 1 ) THEN STOP

  ;; Change NAN to -9999.99
  notvalid = WHERE( FINITE( tavg_final ) EQ 0, num_notvalid )
  IF ( num_notvalid GT 0 ) THEN tavg_final[ notvalid ] = -9999.99
  notvalid = WHERE( FINITE( tstd_final ) EQ 0, num_notvalid )
  IF ( num_notvalid GT 0 ) THEN tstd_final[ notvalid ] = -9999.99

  IF ( KEYWORD_SET( DEBUG ) EQ 1 ) THEN BEGIN
     WRITE_RESULTS, tavg_final, tstd_final, nexcl_fin, level
     STOP
  ENDIF

END
