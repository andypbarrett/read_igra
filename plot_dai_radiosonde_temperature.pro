
obs_hour = 0
level = 0

month_string = ['January','February','March','April', $
                'May','June','July','August', $
                'September','October','November','December']

!p.multi=[0,3,4]

FOR imon = 0, 11 DO BEGIN

   thismonth = imon + 1

   CASE 1 OF
      ( obs_hour EQ 0 ): pos = WHERE( data.month EQ thismonth AND $
                                      (data.obs_hour GT 22 OR data.obs_hour LT 2 ), $
                                      num_obs )
      ( obs_hour EQ 12 ): pos = WHERE( data.month EQ thismonth AND $
                                      (data.obs_hour GT 10 OR data.obs_hour LT 14 ), $
                                      num_obs )
      ELSE: PRINT, 'Unknown observation hour'
   ENDCASE

   IF ( num_obs GT 0 ) THEN BEGIN

      nstd = 5

      obs   = data[pos].temperature[0]
      level = data[pos].minor_level_type[0]
      year  = data[pos].year
      month = data[pos].month
      day   = data[pos].day
      idx   = WHERE( obs GT -9999. AND level EQ 1, numidx )
      IF ( numidx GT 0 ) THEN BEGIN
         mean_obs = MEAN( obs[idx] )
         stdev_obs = STDDEV( obs[idx] )
         time = JULDAY( month[idx], day[idx], year[idx] )
      ENDIF ELSE BEGIN
         mean_obs = -9999.
         stdev_obs = -9999.
      ENDELSE

      upper_limit = mean_obs + ( nstd * stdev_obs )
      lower_limit = mean_obs - ( nstd * stdev_obs )

      ymin = FLOOR((mean_obs - ( (nstd+1) * stdev_obs )) / 5.) * 5.
      ymax = CEIL((mean_obs + ( (nstd+1) * stdev_obs )) / 5.) * 5.

      dummy = LABEL_DATE(DATE_FORMAT=['%Y'])

      PLOT, time, obs, MIN_VALUE=-9998, PSYM=3, $
            YRANGE=[ymin,ymax], YSTYLE=1, $
            XTICKFORMAT='LABEL_DATE', TITLE=month_string[imon]
      
      PLOTS, !X.CRANGE, mean_obs, LINE=0, COLOR=240
      PLOTS, !X.CRANGE, upper_limit, LINE=0, COLOR=90
      PLOTS, !X.CRANGE, lower_limit, LINE=0, COLOR=90

   ENDIF

ENDFOR

END
