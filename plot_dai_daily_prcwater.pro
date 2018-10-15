;;----------------------------------------------------------------------
;; Plots daily PRCWATER to 500 hPa from the Dai dataset and calculates monthly
;; mean PRCWATER

PRO WRITE_RESULTS, MEAN_MON, STDDEV_MON, STATION_MEAN, STATION_MIN, STATION_MAX, OUTFILE

  nmon  = 12
  nlev  = 6
  nstat = 9

  month = INDGEN( 12 ) + 1
  sid   = INDGEN( nstat ) + 1

  ;; Open file for writing
  cdfid = NCDF_CREATE( outfile, /CLOBBER )

  ;; Define coordinate variables
  mon_dimID = NCDF_DIMDEF( cdfid, 'month', nmon )
  sta_dimID = NCDF_DIMDEF( cdfid, 'station_id', nstat )

  ;; Define variables
  mon_varID = NCDF_VARDEF( cdfid, 'month', mon_dimID, /FLOAT )
  sid_varID = NCDF_VARDEF( cdfid, 'station_id', sta_dimID, /FLOAT )
  avg_varID = NCDF_VARDEF( cdfid, 'average', [mon_dimID], /FLOAT )
  sdv_varID = NCDF_VARDEF( cdfid, 'stddev', [mon_dimID], /FLOAT )
  sta_varID = NCDF_VARDEF( cdfid, 'station', [sta_dimID,mon_dimID], /FLOAT )
  stn_varID = NCDF_VARDEF( cdfid, 'station_min', [sta_dimID,mon_dimID], /FLOAT )
  stx_varID = NCDF_VARDEF( cdfid, 'station_max', [sta_dimID,mon_dimID], /FLOAT )

  ;; Define attributes
  NCDF_ATTPUT, cdfid, mon_varID, 'long_name', 'month'
  NCDF_ATTPUT, cdfid, mon_varID, 'units', 'month of year'
  
  NCDF_ATTPUT, cdfid, sid_varID, 'long_name', 'station id'
  NCDF_ATTPUT, cdfid, sid_varID, 'units', 'none'
  
  NCDF_ATTPUT, cdfid, avg_varID, 'long_name', 'monthly average P. Water'
  NCDF_ATTPUT, cdfid, avg_varID, 'units', 'K'
  
  NCDF_ATTPUT, cdfid, sdv_varID, 'long_name', 'monthly standard deviation P. Water'
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

ybeg = 1979
yend = 2008

nmonth = 12

;; Define arrays to hold min, mean, max and std
monAvg = MAKE_ARRAY( nstat, nmonth, /FLOAT, VALUE=-9999.99 )
monStd = MAKE_ARRAY( nstat, nmonth, /FLOAT, VALUE=-9999.99 )
monMin = MAKE_ARRAY( nstat, nmonth, /FLOAT, VALUE=-9999.99 )
monMax = MAKE_ARRAY( nstat, nmonth, /FLOAT, VALUE=-9999.99 )

;nstat = 2
FOR istat = 0, nstat - 1 DO BEGIN

   PRINT, 'Processing data for station ' + station_id[istat] 

   PRINT, '   - unzipping ' + fili[istat] + '.gz'
   SPAWN, 'gunzip ' + fili[istat] + '.gz'

   ;; Read Data
   PRINT, '  - Getting data'
   read_dai_radiosonde, fili[istat], data
   
   ;; Subset data
   in_range = WHERE( data.year GE ybeg AND data.year LE yend, num_in_range )
   IF ( num_in_range GT 0 ) THEN BEGIN
      dataSub = data[ in_range ];
   ENDIF ELSE BEGIN
      PRINT, '% PLOT_DAI_DAILY_PRCWATER: No data within year range ' + $
             STRTRIM( ybeg, 2 ) + ' to ' + STRTRIM( yend, 2 )
   ENDELSE

   ;; Find maximum PRCWATER
   ymax = MAX( dataSub.PW1 )
   ymin = 0
 
   ;; Make Plot
   plot, randomu(seed,10), randomu(seed,10), XRANGE=[0,13], XSTYLE=1, $
         YRANGE=[ymin,ymax], YSTYLE=1, YTITLE='PRCWATER (mm)', /NODATA, $
      TITLE=station_id[istat]

   PRINT, '   - Plotting data'

   FOR imon = 0, nmonth-1 DO BEGIN

      thismonth = imon + 1
      
      ;; Find data for given month
      in_month = WHERE( dataSub.month EQ thismonth, num_in_month )
      IF ( num_in_month LE 0 ) THEN BEGIN
         PRINT, '% PLOT_DAI_DAILY_PRCWATER: no data found for month ' + $
                STRTRIM( thismonth, 2 )
         CONTINUE
      ENDIF
      pw = dataSub[ in_month ].PW1
      
      ;; Find valid data
      isValid = WHERE( pw GT -9999., numValid )
      IF ( numValid LE 0 ) THEN BEGIN
         PRINT, '% PLOT_DAI_DAILY_PRCWATER: no valid data found for month ' + $
                STRTRIM( thismonth, 2 )
         CONTINUE
      ENDIF
      pw = pw[ isValid ]
      
      ;; Make color histogram index for plotting
      h = HISTOGRAM( pw, OMIN=om, REVERSE_INDICES=ri )
      nh = N_ELEMENTS( h )
      perc = h * 100. / TOTAL(h)
      color = BYTSCL( perc, MIN=0.00001,TOP=230 ) + 20
      
      FOR ih = 0, (nh-1) DO BEGIN
         IF ( h[ih] GT 0 ) THEN BEGIN
            PLOTS, REPLICATE( thismonth, h[ih] ), $
                   pw[ ri[ ri[ih]:ri[ih+1]-1] ], COLOR=color[ih]
         ENDIF
      ENDFOR
      
      ;;PRINT, 'Month: ' + STRTRIM(thismonth,2) + ': Min=' + STRTRIM( Min(pw), 2 ) + ' Max=' + STRTRIM( Max(pw), 2 )
      
      ;; Calculate mean PW
      monAvg( istat, imon ) = MEAN( pw )
      monStd( istat, imon ) = STDDEV( pw )
      monMin( istat, imon ) = MIN( pw )
      monMax( istat, imon ) = MAX( pw )
      
      PLOTS, thismonth, monAvg(istat,imon), PSYM=2, COLOR=255
      PLOTS, thismonth, monAvg(istat,imon)-monAvg(istat,imon), PSYM=4, COLOR=255
      PLOTS, thismonth, monAvg(istat,imon)+monAvg(istat,imon), PSYM=4, COLOR=255
      PLOTS, thismonth, monMax(istat,imon), PSYM=5, COLOR=255

   ENDFOR

   pngFile = station_id[istat] + '.dai.prcwater.month.png'
   SAVEIMAGE, pngFile, /PNG

   PRINT, '   - zipping ' + fili[istat]
   SPAWN, 'gzip ' + fili[istat]

ENDFOR

;; Calculate zonal means and max and min
zonalAvg = MAKE_ARRAY( 12, /FLOAT, VALUE=-9999.99 )
zonalStd = MAKE_ARRAY( 12, /FLOAT, VALUE=-9999.99 )
zonalMin = MAKE_ARRAY( 12, /FLOAT, VALUE=-9999.99 )
zonalMax = MAKE_ARRAY( 12, /FLOAT, VALUE=-9999.99 )
FOR imon=0,nmonth-1 DO BEGIN
   zonalAvg[ imon ] = MEAN( monAvg[ *, imon ] )
   zonalStd[ imon ] = STDDEV( monAvg[ *, imon ] )
   zonalMin[ imon ] = MIN( monAvg[ *, imon ] )
   zonalMax[ imon ] = MAX( monAvg[ *, imon ] )
ENDFOR

fmt = '(1x,a6,1x,12(1x,f8.2))'
PRINT, ''
PRINT, 'Mean'
FOR istat = 0, nstat-1 DO PRINT, station_id[istat], monAvg[ istat, * ], FORMAT=fmt
PRINT, ''
PRINT, 'Standard Deviation'
FOR istat = 0, nstat-1 DO PRINT, station_id[istat], monStd[ istat, * ], FORMAT=fmt
PRINT, ''
PRINT, 'Minimum'
FOR istat = 0, nstat-1 DO PRINT, station_id[istat], monMin[ istat, * ], FORMAT=fmt
PRINT, ''
PRINT, 'Maximum'
FOR istat = 0, nstat-1 DO PRINT, station_id[istat], monMax[ istat, * ], FORMAT=fmt
PRINT, ''
PRINT, '------------------------------------------'
PRINT, 'Zonal Statistics'
PRINT, ''
PRINT, 'Zonal Mean'
fmt = '(8x,12(1x,f8.2))'
PRINT, zonalAvg, FORMAT=fmt
PRINT, ''
PRINT, 'Zonal Minimum'
PRINT, zonalMin, FORMAT=fmt
PRINT, ''
PRINT, 'Zonal Maximum'
PRINT, zonalMax, FORMAT=fmt

outFile = 'dai_radiosonde.PW500.clm.month.70Nto90N.' + $
          STRTRIM(ybeg,2) + 'to' + STRTRIM(yend,2) + '.nc'
WRITE_RESULTS, zonalAvg, zonalStd, monAvg, monMin, monMax, outFile

END

   
 
