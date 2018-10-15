;;----------------------------------------------------------------------
;; Plots a time series of geopotential height for the lowest level (we assume
;; surface ) for IGRA stations.
;;
;; 2010-12-02 A.P.Barrett
;;----------------------------------------------------------------------
PRO PLOT_IGRA_DERIVED_GPH, PROFILE, STATION

  dummy = LABEL_DATE( DATE_FORMAT=['%y'] )

  time = JULDAY( profile.month, profile.day, $
                 profile.year, profile.obs_hour )

  title = STRTRIM( station.station_name, 2 ) + ' - ' + $
          STRTRIM( station.station_number, 2 )

  tbeg = JULDAY( 1, 1, 1945, 0 )
  tend = JULDAY( 12, 31, 2010, 23 )
  ymin = -10
  ymax = MAXVAL( profile.obsgph[0] ) + 10.

  PLOT, time, profile.obsgph[0], PSYM=3, $
        XRANGE=[ tbeg, tend ], XSTYLE=1, XTITLE='time', $
        YRANGE=[ ymin, ymax ], YSTYLE=1, YTITLE='GPH (m)', $
        TITLE= title, /NODATA, XTICKUNITS = ['Time'], $  
        XTICKFORMAT='LABEL_DATE'

  PLOTS, !X.CRANGE, station.elevation, COLOR=240
  PLOTS, JULDAY( 1, 1, 1979 ), !Y.CRANGE, COLOR=160
  PLOTS, JULDAY( 12, 31, 2008 ), !Y.CRANGE, COLOR=160

  OPLOT, time, profile.obsgph[0], MIN_VALUE=-9999., PSYM=2

END

