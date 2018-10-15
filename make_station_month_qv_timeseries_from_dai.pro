
FUNCTION SUBSET_BY_YEAR, data, time, ybeg, yend

  ;; Get date info from time
  CALDAT, time, month, day, year, hour

  idx = WHERE( year GE ybeg AND year LE yend, count )
  IF ( count LE 0 ) THEN BEGIN
     PRINT, '% SUBSET_BY_YEAR: Unable to find dates for ' + $
            STRTRIM( ybeg, 2 ) + ' to ' + STRTRIM( yend, 2 ) + ' period'
     RETURN, -1
  ENDIF

  ;; Check if number of time steps found is factor of 12
  IF ( ( count MOD 12 ) GT 0 ) THEN BEGIN
     PRINT, '% SUBSET_BY_YEAR: Partial years being returned!'
     RETURN, -1
  ENDIF

  result = data[ *, *, idx ]

  RETURN, result
  
END 

FUNCTION CALC_AVG, data

  idx = WHERE( data LT -9999., count )
  IF ( count GT 0 ) THEN data[idx] = !VALUES.F_NAN

  numer = TOTAL( data, 1, /NAN )
  denom = TOTAL( FINITE( data ), 1 )

  dims = SIZE( numer, /DIMENSION )
  result = MAKE_ARRAY( dims, /FLOAT, VALUE=-9999.99 )

  isvalid = WHERE( denom GT 0, numvalid )
  IF ( numvalid GT 0 ) THEN result[ isvalid ] = numer[ isvalid ] / denom[ isvalid ]

  RETURN, result

END

FUNCTION GET_LEVEL_IDX, lev0, lev1

  nlev1 = N_ELEMENTS( lev1 )
  
  idx = MAKE_ARRAY( nlev1, /LONG, VALUE=-1 )

  FOR il=0, nlev1-1 DO BEGIN

     idx[il] = WHERE( lev1[ il ] EQ lev0, count )
     
  ENDFOR

  RETURN, idx

END

FUNCTION DIM_CLM_MEAN, x

  dims = SIZE( x, /DIMENSION )
  nstat = dims[0]
  ntime = dims[1]

  nmonth = 12
  nyear  = ntime / nmonth

  result = MAKE_ARRAY( nstat, nmonth, /FLOAT, VALUE=!VALUES.F_NAN )

  idx = WHERE( x LE -9999., count )
  IF ( count GT 0 ) THEN x[ idx ] = !VALUES.F_NAN

  itx = LINDGEN( nyear )*12

  FOR im = 0, nmonth-1 DO BEGIN
  
     tmp = x[ *, itx+im ]

     numer = TOTAL( tmp, 2, /NAN )
     denom = TOTAL( FINITE( tmp ), 2 )

     result[ *, im ] = numer / denom

  ENDFOR

  RETURN, result

END

FUNCTION DIM_CLM_ANOM, x

  dims = SIZE( x, /DIMENSION )
  nstat = dims[0]
  ntime = dims[1]

  nmonth = 12
  nyear  = ntime / nmonth

  xAnom = MAKE_ARRAY( nstat, ntime, /FLOAT, VALUE=!VALUES.F_NAN )

  idx = WHERE( x LE -9999., count )
  IF ( count GT 0 ) THEN x[ idx ] = !VALUES.F_NAN

  ;; get average
  xMonAvg = DIM_CLM_MEAN( x )

  itx = LINDGEN( nyear )*12

  FOR im = 0, nmonth-1 DO BEGIN
  
     xAnom[ *, itx+im ] = x[ *, itx+im ] - $
                           REBIN( xMonAvg[ *, im ], nstat, nyear, /SAMPLE ) 

  ENDFOR

  RETURN, xAnom

END
  
FUNCTION DIM_CLM_STDDEV, x

  dims = SIZE( x, /DIMENSION )
  nstat = dims[0]
  ntime = dims[1]

  nmonth = 12
  nyear  = ntime / nmonth

  xStd = MAKE_ARRAY( nstat, nmonth, /FLOAT, VALUE=!VALUES.F_NAN )

  idx = WHERE( x LE -9999., count )
  IF ( count GT 0 ) THEN x[ idx ] = !VALUES.F_NAN

  ;; get average
  xMonAvg = DIM_CLM_MEAN( x )

  itx = LINDGEN( nyear )*12

  FOR im = 0, nmonth-1 DO BEGIN
  
     dx = x[ *, itx+im ] - REBIN( xMonAvg[ *, im ], nstat, nyear, /SAMPLE )
     dx2 = dx * dx

     sdx2 = TOTAL( dx2, 2, /NAN )
     nx   = TOTAL( FINITE( dx ), 2 )

     xStd[ *, im ] = sdx2 / ( nx - 1 )

  ENDFOR

  RETURN, xStd

END

FUNCTION GET_STDANOM, x

  dims   = SIZE( x, /DIMENSIONS )
  nlevel = dims[0]
  ntime  = dims[1]

  nmonth = 12
  nyear  = ntime / nmonth

  xStdAnom = MAKE_ARRAY( nlevel, ntime, /FLOAT, VALUE=!VALUES.F_NAN )

  idx = WHERE( x LE -9999., count )
  IF ( count GT 0 ) THEN x[ idx ] = !VALUES.F_NAN

;  xMonStd = DIM_CLM_STDDEV( x )
;  xAnom = DIM_CLM_ANOM( x )

;  itx = LINDGEN( nyear )*12

;  FOR im = 0, nmonth-1 DO BEGIN
  
;     xStdAnom[ *, itx+im ] = xAnom[ *, itx+im ] / $
;                             REBIN( xMonStd[ *, im ], nstat, nyear, /SAMPLE )

;  ENDFOR

  itx = LINDGEN( nyear )*12

  FOR il = 0, nlevel-1 DO BEGIN
     FOR im = 0, nmonth-1 DO BEGIN
        tmp = x[ il, itx+im ]
        xStdAnom[ il, itx+im ] = ( tmp - MEAN( tmp, /NAN ) ) / STDDEV( tmp, /NAN )
     ENDFOR
  ENDFOR
        
  RETURN, xStdAnom

END

PRO WRITE_TO_NETCDF, data, Anom, time, level, station_id, filo

  dims = SIZE( data, /DIMENSIONS )
  nstation = dims[0]
  nlevel   = dims[1]
  ntime    = dims[2]

  statvar = FLOAT( station_id )
  ftime   = time - JULDAY( 1, 1, 1900, 0, 0 , 0 )
 
  cdfid = NCDF_CREATE( filo, /CLOBBER )
  stat_dimid = NCDF_DIMDEF( cdfid, 'station', nstation )
  levl_dimid = NCDF_DIMDEF( cdfid, 'level', nlevel )
  time_dimid = NCDF_DIMDEF( cdfid, 'time', ntime )

  statid     = NCDF_VARDEF( cdfid, 'station', stat_dimid, /FLOAT   )
  levlid     = NCDF_VARDEF( cdfid, 'level', levl_dimid, /FLOAT   )
  timeid     = NCDF_VARDEF( cdfid, 'time', time_dimid, /DOUBLE )
  shumid     = NCDF_VARDEF( cdfid, 'shum', $
                            [ stat_dimid, levl_dimid, time_dimid ], /FLOAT )
  zscrid     = NCDF_VARDEF( cdfid, 'zscore', $
                            [ stat_dimid, levl_dimid, time_dimid ], /FLOAT )

  NCDF_ATTPUT, cdfid, statid, 'long_name', 'station id'
  NCDF_ATTPUT, cdfid, statid, 'units', 'none'

  NCDF_ATTPUT, cdfid, timeid, 'long_name', 'time'
  NCDF_ATTPUT, cdfid, timeid, 'units', 'days since 1900-01-01 00:00:00'

  NCDF_ATTPUT, cdfid, levlid, 'long_name', 'level'
  NCDF_ATTPUT, cdfid, levlid, 'units', 'hPa'

  NCDF_ATTPUT, cdfid, shumid, 'long_name', 'specific humidity'
  NCDF_ATTPUT, cdfid, shumid, 'units', 'kg kg**-1'

  NCDF_ATTPUT, cdfid, zscrid, 'long_name', 'z-score'
  NCDF_ATTPUT, cdfid, zscrid, 'units', 'none'

  NCDF_ATTPUT, cdfid, 'created_on', SYSTIME(), /GLOBAL
  NCDF_ATTPUT, cdfid, 'created_by', 'Andrew P. Barrett <apbarret@nsidc.org>', /GLOBAL
  
  NCDF_CONTROL, cdfid, /ENDEF

  NCDF_VARPUT, cdfid, statid, statvar
  NCDF_VARPUT, cdfid, levlid, level
  NCDF_VARPUT, cdfid, timeid, ftime
  NCDF_VARPUT, cdfid, shumid, data
  NCDF_VARPUT, cdfid, zscrid, Anom

  NCDF_CLOSE, cdfid

  RETURN

END

;;----------------------------------------------------------------------
;; Main routine
;;----------------------------------------------------------------------

DEBUG = 0

station_name = [ 'JAN MAYEN', $
                 'BJORNOYA', $
                 'DANMARKSHAVN', $
                 'SCORESBYSUND', $
                 'BUKHTA TIKSI', $
                 'BARROW', $
                 'ALERT', $
                 'EUREKA', $
                 'RESOLUTE BAY' ]

station_id = [ '01001', $
               '01028', $
               '04320', $
               '04339', $
               '21824', $
               '70026', $
               '71082', $
               '71917', $
               '71924' ]

ybeg   = 1979
yend   = 2001
nyear  = yend - ybeg + 1
nmonth = 12

nstation = N_ELEMENTS( station_id )
nlevel   = 8
ntime    = nyear * nmonth

;; Make array to hold all stations
data = MAKE_ARRAY( nstation, nlevel, ntime, /FLOAT, VALUE=-9999.99 )
stdAnom = MAKE_ARRAY( nstation, nlevel, ntime, /FLOAT, VALUE=-9999.99 )
time = TIMEGEN( START=JULDAY( 1, 1, ybeg, 0 ), FINAL=JULDAY( 12, 1, yend, 0 ), UNITS='Month' )
level = [ 300, 400, 500, 600, 700, 850, 925, 1000 ] ; hPa

!p.multi=[0, 1, 6 ]

FOR istat = 0, nstation-1 DO BEGIN

   ;; Make station file name
   fili = station_id[istat] + ".month.nc"

   ;; Get data
   shum   = NCDF_READV( fili, VARNAME='shum' )
   ftime  = NCDF_READV( fili, VARNAME='time' )
   flevel = NCDF_READV( fili, VARNAME='level' )

   ;; Subset by year
   shum_new = SUBSET_BY_YEAR( shum, ftime, ybeg, yend )

   ;; Calculate mean for both hours
   shum_avg = CALC_AVG( shum_new )

   levid = GET_LEVEL_IDX( level, flevel )

   data[ istat, levid, * ] = shum_avg

   stdAnom[ istat, levid, * ] = GET_STDANOM( shum_avg )

;;----------------------------------------------------------------------
;; For debugging
;;----------------------------------------------------------------------
   IF ( DEBUG EQ 1 ) THEN BEGIN
      FOR ilev=0, N_ELEMENTS(flevel)-1 DO BEGIN

;      PLOT, time, shum_new[ 0, ilev, * ], MIN_VALUE=-9999.
;      OPLOT, time, shum_new[ 1, ilev, * ], MIN_VALUE=-9999., COLOR=240
         PLOT, time, stdAnom[ istat, levid[ilev], * ], MIN_VALUE=-9999.
         PLOTS, !X.CRANGE, 0.

;      OPLOT, time, data[ istat, levid[ilev], * ], MIN_VALUE=-9999., COLOR=240
      ENDFOR
   ;FOR it = 0, ntime-1 DO PRINT, time[it], shum_avg[*,it], FORMAT='(C(CYI4,CMOI02,CDI02," ",CHI02,"h"),1x,6(1x,f9.2))
      STOP
   ENDIF
;;----------------------------------------------------------------------

ENDFOR

filo = 'radiosonde.station.month.timeseries.QV.nc'
WRITE_TO_NETCDF, data, stdAnom, time, level, station_id, filo

!P.MULTI=[0,3,3]
itx = LINDGEN( 23 ) + 11
idx = WHERE( stdAnom LE -9999., count )
IF ( count GT 0 ) THEN stdAnom[idx] = !VALUES.F_NAN 
FOR il = 0, N_ELEMENTS(flevel)-2 DO BEGIN
   FOR istat=0,8 DO BEGIN
      x = stdAnom[istat,levid[il],itx]
      y = stdAnom[istat,levid[il+1],itx]
      PLOT, x, y, XRANGE=[-5,5], XSTYLE=1, YRANGE=[-5,5], YSTYLE=1, $
            XTITLE=STRTRIM(flevel[il],2)+' hPa', YTITLE=STRTRIM(flevel[il+1],2)+' hPa', $
            PSYM=3, TITLE=station_id[istat]
      PLOTS, !X.CRANGE, !Y.CRANGE, COLOR=240
   ENDFOR
   STOP
ENDFOR

END

