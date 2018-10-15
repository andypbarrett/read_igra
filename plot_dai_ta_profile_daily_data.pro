;;----------------------------------------------------------------------
;; Plots monthly profiles using daily data.  Also plots mean profiles.
;;
;; 2011-08-05 A.P.Barrett
;;----------------------------------------------------------------------

FUNCTION GET_PROFILE_STATS, Q

  ;; Calculate initial average and standard deviation
  monAvg = MEAN( q )
  monStd = STDDEV( q )
  monMin = MIN( q )
  monMax = MAX( q )

  ;; Calculate upper and lower limits (5*std)
  lower = monAvg - (4 * monStd)
  isneg = WHERE( lower LT 0, numNeg )
  IF ( numNeg GT 0 ) THEN lower = 0.0
  upper = monAvg + (4 * monStd)
  
  isValid = WHERE( q GT lower AND q LT upper, numValid )
  IF ( numValid GT 0 ) THEN BEGIN
     monAvg_qc = MEAN( q[ isValid ] )
     monStd_qc = STDDEV( q[ isValid ] )
     monMin_qc = MIN( q[ isValid ] )
     monMax_qc = MAX( q[ isValid ] )
  ENDIF ELSE BEGIN
     monAvg_qc = -9999.99
     monStd_qc = -9999.99
     monMin_qc = -9999.99
     monMax_qc = -9999.99
  ENDELSE

  result = {monAvg: monAvg, $
            monStd: monStd, $
            monMin: monMin, $
            monMax: monMax, $
            lower: lower, $
            upper: upper, $
            monAvg_qc: monAvg_qc, $
            monStd_qc: monStd_qc, $
            monMin_qc: monMin_qc, $
            monMax_qc: monMax_qc}

  RETURN, result

END

PRO WRITE_RESULTS, MEAN_MON, STDDEV_MON, STATION_MEAN, STATION_MIN, STATION_MAX, OUTFILE

  nmon  = 12
  nlev  = 6
  nstat = 9

  month = INDGEN( 12 ) + 1
  level = [1000.,850.,700.,500.,400.,300.]
  sid   = INDGEN( nstat ) + 1

  ;; Open file for writing
  cdfid = NCDF_CREATE( outfile, /CLOBBER )

  ;; Define coordinate variables
  mon_dimID = NCDF_DIMDEF( cdfid, 'month', nmon )
  lev_dimID = NCDF_DIMDEF( cdfid, 'level', nlev )
  sta_dimID = NCDF_DIMDEF( cdfid, 'station_id', nstat )

  ;; Define variables
  mon_varID = NCDF_VARDEF( cdfid, 'month', mon_dimID, /FLOAT )
  lev_varID = NCDF_VARDEF( cdfid, 'level', lev_dimID, /FLOAT )
  sid_varID = NCDF_VARDEF( cdfid, 'station_id', sta_dimID, /FLOAT )
  avg_varID = NCDF_VARDEF( cdfid, 'average', [mon_dimID,lev_dimID], /FLOAT )
  sdv_varID = NCDF_VARDEF( cdfid, 'stddev', [mon_dimID,lev_dimID], /FLOAT )
  sta_varID = NCDF_VARDEF( cdfid, 'station', [sta_dimID,mon_dimID,lev_dimID], /FLOAT )
  stn_varID = NCDF_VARDEF( cdfid, 'station_min', [sta_dimID,mon_dimID,lev_dimID], /FLOAT )
  stx_varID = NCDF_VARDEF( cdfid, 'station_max', [sta_dimID,mon_dimID,lev_dimID], /FLOAT )

  ;; Define attributes
  NCDF_ATTPUT, cdfid, mon_varID, 'long_name', 'month'
  NCDF_ATTPUT, cdfid, mon_varID, 'units', 'month of year'
  
  NCDF_ATTPUT, cdfid, lev_varID, 'long_name', 'level'
  NCDF_ATTPUT, cdfid, lev_varID, 'units', 'hPa'
  
  NCDF_ATTPUT, cdfid, sid_varID, 'long_name', 'station id'
  NCDF_ATTPUT, cdfid, sid_varID, 'units', 'none'
  
  NCDF_ATTPUT, cdfid, avg_varID, 'long_name', 'monthly average T'
  NCDF_ATTPUT, cdfid, avg_varID, 'units', 'K'
  
  NCDF_ATTPUT, cdfid, sdv_varID, 'long_name', 'monthly standard deviation T'
  NCDF_ATTPUT, cdfid, sdv_varID, 'units', 'K'
  
  NCDF_ATTPUT, cdfid, sta_varID, 'long_name', 'monthly station average'
  NCDF_ATTPUT, cdfid, sta_varID, 'units', 'K'
  
  NCDF_ATTPUT, cdfid, stn_varID, 'long_name', 'monthly station minimum'
  NCDF_ATTPUT, cdfid, stn_varID, 'units', 'K'
  
  NCDF_ATTPUT, cdfid, stx_varID, 'long_name', 'monthly station maximum'
  NCDF_ATTPUT, cdfid, stx_varID, 'units', 'K'
  
  ;; Define global attributes
  NCDF_ATTPUT, cdfid, /GLOBAL, 'created', SYSTIME()
  NCDF_ATTPUT, cdfid, /GLOBAL, 'created_by', 'Andrew P. Barrett apbarret@nsidc.org'
  NCDF_ATTPUT, cdfid, /GLOBAL, 'description', 'statistics for radiosonde profiles for 60n to 70N'
  NCDF_ATTPUT, cdfid, /GLOBAL, 'source', 'Aiguo Dais homogenized radiosonde data set'

  NCDF_CONTROL, cdfid, /ENDEF

  ;; Write variables
  NCDF_VARPUT, cdfid, mon_varID, month
  NCDF_VARPUT, cdfid, lev_varID, level
  NCDF_VARPUT, cdfid, sid_varID, sid
  NCDF_VARPUT, cdfid, avg_varID, mean_mon
  NCDF_VARPUT, cdfid, sdv_varID, stddev_mon
  NCDF_VARPUT, cdfid, sta_varID, station_mean
  NCDF_VARPUT, cdfid, stn_varID, station_min
  NCDF_VARPUT, cdfid, stx_varID, station_max

  ;; Close file
  NCDF_CLOSE, cdfid

END

station_id = ['01001', $
              '01028', $
              '04320', $
              '04339', $
              '21824', $
              '70026', $
              '71082', $
              '71917', $
              '71924' ]
fili = station_id + '.txt'
nstat = N_ELEMENTS( fili )

level = [1000.,850.,700.,500.,400.,300.]
nlevel = N_ELEMENTS( level )
   
month_str = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
nmonth = 12

ybeg = 2003
yend = 2008

monAvg = MAKE_ARRAY( nstat, nmonth, nlevel, /FLOAT, VALUE=-9999.99 )
monStd = MAKE_ARRAY( nstat, nmonth, nlevel, /FLOAT, VALUE=-9999.99 )
monAvg_qc = MAKE_ARRAY( nstat, nmonth, nlevel, /FLOAT, VALUE=-9999.99 )
monStd_qc = MAKE_ARRAY( nstat, nmonth, nlevel, /FLOAT, VALUE=-9999.99 )
upper = MAKE_ARRAY( nstat, nmonth, nlevel, /FLOAT, VALUE=-9999.99 )
lower = MAKE_ARRAY( nstat, nmonth, nlevel, /FLOAT, VALUE=-9999.99 )
monMin = MAKE_ARRAY( nstat, nmonth, nlevel, /FLOAT, VALUE=-9999.99 )
monMax = MAKE_ARRAY( nstat, nmonth, nlevel, /FLOAT, VALUE=-9999.99 )
monMin_qc = MAKE_ARRAY( nstat, nmonth, nlevel, /FLOAT, VALUE=-9999.99 )
monMax_qc = MAKE_ARRAY( nstat, nmonth, nlevel, /FLOAT, VALUE=-9999.99 )

;nstat = 1
FOR istat = 0, nstat-1 DO BEGIN

   PRINT, '% PLOT_DAI_PROFILE_DAILY_DATA: Processing data for station ' + station_id[istat]

   PRINT, '   - unzipping ' + fili[istat] + '.gz'
   SPAWN, 'gunzip ' + fili[istat] + '.gz'

   ;; get data
   PRINT, '   - reading data from ' + fili[istat]
   read_dai_radiosonde, fili[istat], profile

   ;; Subset for 1989 to 2001 period
   thisYear = WHERE( profile.year GE ybeg AND profile.year LE yend, numThisYear )
   IF ( numThisYear EQ 0 ) THEN BEGIN
      PRINT, '% PLOT_DAI_PROFILE_DAILY_DATA: unable to find data for selected year-span'
      STOP
   ENDIF
   profileSub = profile[ thisYear ]
   
   ymin = 1050.
   ymax = 299.
   xmin = 0.
   
   PRINT, '   - plotting data'

   !P.Multi = [0,3,4]
   
   FOR imon = 0, 11 DO BEGIN
      
      thisMonth = imon + 1
      
      idxMonth = WHERE( profileSub.month EQ thisMonth, numMonth )
      IF ( numMonth EQ 0 ) THEN BEGIN
         PRINT, '% PLOT_DAI_PROFILE_DAILY_DATA: no data for month: ' + STRTRIM(thisMonth,2)
         CONTINUE
      ENDIF
      
      p = profileSub[ idxMonth ].pressure
      q = profileSub[ idxMonth ].temperature

      isValid = WHERE( p GT -9999. AND q GT -9999., numValid )
      IF ( numValid EQ 0 ) THEN BEGIN
         PRINT, '% PLOT_DAI_PROFILE_DAILY_DATA: no valid data for month: ' + STRTRIM(thisMonth,2)
         CONTINUE
      ENDIF
      
      pv = p[ isValid ]
      qv = q[ isValid ]
      qv = qv + 273.15
      
      ;; Calculate and plot mean q for each mandatory level to 300.
      FOR ilev = 0, (nlevel -1 ) DO BEGIN
         isLevel = WHERE( pv EQ level[ilev], numLevel )
         IF ( numLevel GT 0 ) THEN BEGIN
            ;; Calculate initial average and standard deviation
            ;;monAvg[ istat, imon, ilev ] = MEAN( qv[ isLevel ] )
            ;;monStd[ istat, imon, ilev ] = STDDEV( qv[ isLevel ] )
            ;; Calculate upper and lower limits (5*std)
            ;;lower[ istat, imon, ilev ] = monAvg[ istat, imon, ilev ] - (4 * monStd[ istat, imon, ilev ])
            ;;isneg = WHERE( lower LT 0, numNeg )
            ;;IF ( numNeg GT 0 ) THEN lower[ istat, imon, ilev ] = 0.0
            ;;upper[ istat, imon, ilev ] = monAvg[ istat, imon, ilev ] + (4 * monStd[ istat, imon, ilev ])
            struct = GET_PROFILE_STATS( qv[ isLevel ] )
            monAvg[ istat, imon, ilev ] = struct.monAvg
            monStd[ istat, imon, ilev ] = struct.monStd
            monMin[ istat, imon, ilev ] = struct.monMin
            monMax[ istat, imon, ilev ] = struct.monMax
            lower[ istat, imon, ilev ]  = struct.lower
            upper[ istat, imon, ilev ]  = struct.upper
            monAvg_qc[ istat, imon, ilev ] = struct.monAvg_qc
            monStd_qc[ istat, imon, ilev ] = struct.monStd_qc
            monMin_qc[ istat, imon, ilev ] = struct.monMin_qc
            monMax_qc[ istat, imon, ilev ] = struct.monMin_qc
         ENDIF
      ENDFOR
      
      ;; Set limits to 4 standard deviations

      xmin = FLOOR( MIN( qv ) / 5. ) * 5.
      xmax = CEIL( MAX( qv ) / 5. ) * 5. 
      
      PLOT, qv, pv, XRANGE=[xmin,xmax], XSTYLE=1, XTITLE='K', $
            YRANGE=[ymin,ymax], YSTYLE=1, /YLOG, YTITLE='Pressure (hPa)', $
            PSYM=3, YTICKV=ALOG(level), TITLE=month_str[imon]
      
      OPLOT, monAvg[ istat, imon, * ], level, PSYM=-2, COLOR=240
      OPLOT, lower[ istat, imon, * ], level, PSYM=-2, COLOR=90
      OPLOT, upper[ istat, imon, * ], level, PSYM=-2, COLOR=90

      OPLOT, monAvg_qc[ istat, imon, * ], level, PSYM=-4, COLOR=240
      OPLOT, monAvg_qc[ istat, imon, * ] + monStd_qc[ istat, imon, * ], level, PSYM=-4, COLOR=160
      OPLOT, monAvg_qc[ istat, imon, * ] + (4*monStd_qc[ istat, imon, * ]), level, PSYM=-4, COLOR=160
;      OPLOT, monAvg_qc[ istat, imon, * ], level, PSYM=-4, COLOR=90
      
   ENDFOR

   PRINT, '   - saving image to ' + station_id[istat] + '.png'
   SAVEIMAGE, station_id[istat] + '.qv.profile.png', /PNG

   PRINT, '   - zipping ' + fili[istat]
   SPAWN, 'gzip ' + fili[istat]

ENDFOR

zonalAvg = MAKE_ARRAY( nmonth, nlevel, /FLOAT, VALUE=-9999.99 )
zonalStd = MAKE_ARRAY( nmonth, nlevel, /FLOAT, VALUE=-9999.99 )
zonalMin = MAKE_ARRAY( nmonth, nlevel, /FLOAT, VALUE=-9999.99 )
zonalMax = MAKE_ARRAY( nmonth, nlevel, /FLOAT, VALUE=-9999.99 )

FOR imon = 0, nmonth-1 DO BEGIN
   FOR ilev = 0, nlevel-1 DO BEGIN
      tmp = monAvg_qc[ *, imon, ilev ]
      isValid = WHERE( tmp GT -9999., numValid )
      IF ( numValid GT 1 ) THEN BEGIN
         zonalAvg[ imon, ilev ] = MEAN( tmp[ isValid ] )
         zonalStd[ imon, ilev ] = STDDEV( tmp[ isValid ] )
         zonalMin[ imon, ilev ] = MIN( tmp[ isValid ] )
         zonalMax[ imon, ilev ] = MAX( tmp[ isValid ] )
      ENDIF ELSE BEGIN
         zonalAvg[ imon, ilev ] = -9999.99
         zonalStd[ imon, ilev ] = -9999.99
         zonalMin[ imon, ilev ] = -9999.99
         zonalMax[ imon, ilev ] = -9999.99
      ENDELSE
   ENDFOR
ENDFOR

fmt = '(i4,12(1x,f8.2))'
PRINT, ''
PRINT, 'Zonal Average' 
FOR ilev = 0, nlevel-1 DO PRINT, level[ilev], zonalAvg[ *,ilev ], FORMAT=fmt
PRINT, ''
PRINT, 'Zonal Minimum' 
FOR ilev = 0, nlevel-1 DO PRINT, level[ilev], zonalMin[ *,ilev ], FORMAT=fmt
PRINT, ''
PRINT, 'Zonal Maximum' 
FOR ilev = 0, nlevel-1 DO PRINT, level[ilev], zonalMax[ *,ilev ], FORMAT=fmt

;; Write to netCDF
outFile = 'dai_radiosonde.ta.clm.month.profiles.70Nto90N.' + $
          STRTRIM(ybeg,2) + 'to' + STRTRIM(yend,2) + '.nc'
WRITE_RESULTS, zonalAvg, zonalStd, monAvg_qc, monMin_qc, monMax_qc, outFile

END


   
