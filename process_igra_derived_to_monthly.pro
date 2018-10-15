PRO PROCESS_IGRA_DERIVED_TO_MONTHLY

start_time = SYSTIME( /SECONDS )

station_list = 'derived-stations-arctic.txt'

level = [ 1000., 850., 700., 500., 400., 300. ]
min_num_profile = 10
obs_hour = [0,12]

year_start = 1945
year_end   = 2008

READ_IGRA_STATION_LIST, station_list, station, NREC=nstation
;nstation = 1

FOR istat = 0, ( nstation - 1 ) DO BEGIN

   PRINT, '% PROCESS_IGRA_DERIVED_TO_MONTHLY: Getting data for station: ' + $
          STRTRIM( station[ istat ].station_number, 2 )
   profile = GET_IGRA_DERIVED( station[ istat ].station_number, /VERBOSE )

   IGRA_DERIVED_TO_MONTHLY, profile, year_start, year_end, result, LEVEL=LEVEL, $
                            MIN_NUM_PROFILE=min_num_profile, obs_hour=obs_hour, $
                            /VERBOSE

   ncfile = STRTRIM( station[istat].station_number, 2 ) + '.month.nc'
   WRITE_MONTHLY_IGRA_DERIVED, result, station[istat], ncfile

ENDFOR

PRINT, FORMAT='("% PROCESS_IGRA_DERIVED_TO_MONTHLY: ELAPSED TIME = ",f8.2)', $
       ( SYSTIME( /SECONDS ) - start_time ) / 60.

END
