;;----------------------------------------------------------------------
;; Performs a QC of the Dai profiles.
;;
;; The tests are as follows
;;
;; 1) Records have to be 90% complete for the 1979 period.  This test is
;;    performed on a level and obs hours basis.
;; 
;; 2011-07-06 A.P.Barrett
;;----------------------------------------------------------------------

PRO SUBSET_BY_YEAR, DATA, TIME, YBEG, YEND

  CALDAT, time, month, day, year

  pos = WHERE( year GE ybeg AND year LE yend, count )

  data = data[ *, *, pos ]
  time = time[ pos ]

  RETURN

END

FUNCTION IS_FULL_TIMESPAN, DATA, TIME, TIME_BEGIN, TIME_END, $
                           FIRST_DATA=FIRST_DATA, LAST_DATA=LAST_DATA

  idx = WHERE( data GT -9999., count )
  
  first_data = UT_CALENDAR( time[ idx[0] ], 1 )
  last_data = UT_CALENDAR( time[ idx[count-1] ], 1 )

  IF ( first_data EQ time_begin AND $
       last_data EQ time_end ) THEN BEGIN
     is_full_timespan = 1
  ENDIF ELSE BEGIN
     is_full_timespan = 0
  ENDELSE

  RETURN, is_full_timespan

END

FUNCTION PERCENTAGE_MISSING, DATA, THRESHOLD=THRESHOLD, NUM_VALID=NUM_VALID

  threshold = N_ELEMENTS( threshold ) EQ 0 ? 90. : threshold

  num_valid = FLOAT( TOTAL( data GT -9999. ) ) * 100. / FLOAT( N_ELEMENTS( data ) )

  IF ( num_valid LT threshold ) THEN BEGIN
     isgood = 0
  ENDIF ELSE BEGIN
     isgood = 1
  ENDELSE

  RETURN, isgood

END

PRO QC_DAI_PROFILES_BATCH

  station_file = 'dai_radiosonde_polar_stations_qc_subset.txt'
  READ_IGRA_STATION_LIST, station_file, station, NREC=nstat

  result_file = 'dai_radiosonde_polar_stations_qc_info_subset.txt'
  flag_file = 'dai_radiosonde_polar_stations_qc_flag_subset.txt'

  OPENW, U, result_file, /GET_LUN
  OPENW, V, flag_file, /GET_LUN

  ;;nstat = 1

  FOR istat=0, nstat-1 DO BEGIN

     filename = station[istat].station_number + '.month.nc'

     PRINTF, U, station[istat].country_code, station[istat].station_number, $
            station[istat].station_name, station[istat].latitude, station[istat].longitude, $
            station[istat].elevation, FORMAT='(a2,2x,a5,2x,a20,2(1x,f9.4),1x,f6.1)'
     QC_DAI_PROFILES, filename, LUN=U, FLAG=flag, /REPLACE

     IF ( flag EQ 1 ) THEN BEGIN
        PRINTF, V, station[istat].country_code, station[istat].station_number, $
            station[istat].station_name, station[istat].latitude, station[istat].longitude, $
            station[istat].elevation, FORMAT='(a2,2x,a5,2x,a20,2(1x,f9.4),1x,f6.1," ***")'
     ENDIF

     ;; reset flag
     flag = 0

  ENDFOR

  CLOSE, U
  FREE_LUN, U

  CLOSE, V
  FREE_LUN, V

  RETURN


END

PRO QC_DAI_PROFILES_SHUM_BATCH

  station_file = 'dai_radiosonde_polar_stations_shum_qc_subset.txt'
  READ_IGRA_STATION_LIST, station_file, station, NREC=nstat

  result_file = 'dai_radiosonde_polar_stations_shum_qc_info_subset.txt'
  flag_file = 'dai_radiosonde_polar_stations_shum_qc_flag_subset.txt'

  OPENW, U, result_file, /GET_LUN
  OPENW, V, flag_file, /GET_LUN

  ;;nstat = 1

  FOR istat=0, nstat-1 DO BEGIN

     filename = station[istat].station_number + '.month.nc'

     dummy = FILE_SEARCH( filename, count=isfile )
     IF ( isfile EQ 0 ) THEN BEGIN
        PRINT, '% QC_DAI_PROFILES_SHUM_BATCH: unable to find file ' + filename
        PRINT, '%    getting next file'
        CONTINUE
     ENDIF

     PRINTF, U, station[istat].country_code, station[istat].station_number, $
            station[istat].station_name, station[istat].latitude, station[istat].longitude, $
            station[istat].elevation, FORMAT='(a2,2x,a5,2x,a20,2(1x,f9.4),1x,f6.1)'
     QC_DAI_PROFILES, filename, VARNAME='shum', LUN=U, FLAG=flag, /REPLACE

     print, station[istat].station_number, " ", flag

     IF ( flag EQ 12 ) THEN BEGIN
        PRINTF, V, station[istat].country_code, station[istat].station_number, $
            station[istat].station_name, station[istat].latitude, station[istat].longitude, $
            station[istat].elevation, FORMAT='(a2,2x,a5,2x,a20,2(1x,f9.4),1x,f6.1," ***")'
     ENDIF

     ;; reset flag
     flag = 0

  ENDFOR

  CLOSE, U
  FREE_LUN, U

  CLOSE, V
  FREE_LUN, V

  RETURN

END


PRO WRITE_QC_TO_NETCDF, DATA, TIME, OUTFILE, STATION_NUMBER

;  start_time = SYSTIME(/Julian)

  level = [1000.,850.,700.,500.,400.,300.]
  obs_hour = [0,12]

  ntime  = N_ELEMENTS( time )
  nlevel = N_ELEMENTS( level )
  nobs   = N_ELEMENTS( obs_hour )

  ;; Set missing value to 1.0e+20 to conform with NCL
  FillValue = -9999.99
  
  ;; Open file for writing
  cdfid = NCDF_CREATE( outfile, /clobber )
  
  ;; Define dimensions
  tim_dimid = NCDF_DIMDEF( cdfid, 'time', ntime )
  lev_dimid = NCDF_DIMDEF( cdfid, 'level', nlevel )
  obs_dimid = NCDF_DIMDEF( cdfid, 'hour', nobs )

  ;; variables
  tim_varid     = NCDF_VARDEF( cdfid, 'time', [tim_dimid], /DOUBLE )
  lev_varid     = NCDF_VARDEF( cdfid, 'level', [lev_dimid], /FLOAT )
  obs_varid     = NCDF_VARDEF( cdfid, 'hour', [obs_dimid], /FLOAT )

  shum_varid    = NCDF_VARDEF( cdfid, 'shum',    [obs_dimid, lev_dimid, tim_dimid], /FLOAT )
  
  NCDF_ATTPUT, cdfid, tim_varid, 'long_name', 'time'
  NCDF_ATTPUT, cdfid, tim_varid, 'units', 'days since 1900-01-01 00:00:0.0'
  NCDF_ATTPUT, cdfid, tim_varid, '_FillValue', DOUBLE(FillValue), /DOUBLE

  NCDF_ATTPUT, cdfid, lev_varid, 'long_name', 'mandatory levels'
  NCDF_ATTPUT, cdfid, lev_varid, 'units', 'hPa'
  NCDF_ATTPUT, cdfid, lev_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, obs_varid, 'long_name', 'observation hour'
  NCDF_ATTPUT, cdfid, obs_varid, 'units', 'hour'
  NCDF_ATTPUT, cdfid, obs_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, shum_varid, 'long_name', 'specific humidity'
  NCDF_ATTPUT, cdfid, shum_varid, 'units', 'g/kg'
  NCDF_ATTPUT, cdfid, shum_varid, '_FillValue', FillValue

  ;; Add global variables
  NCDF_ATTPUT, cdfid, 'created_by', 'A.P.Barrett', /GLOBAL
  NCDF_ATTPUT, cdfid, 'created', SYSTIME(), /GLOBAL
  NCDF_ATTPUT, cdfid, 'station_number', station_number, /GLOBAL
  NCDF_ATTPUT, cdfid, 'source', 'Dai homogenized radiosonde data', /GLOBAL

  NCDF_CONTROL, cdfid, /ENDEF

  NCDF_VARPUT, cdfid, tim_varid,      time
  NCDF_VARPUT, cdfid, lev_varid,      level
  NCDF_VARPUT, cdfid, obs_varid,      obs_hour

  NCDF_VARPUT, cdfid, shum_varid,     data
  
  NCDF_CLOSE, cdfid

;  time_elapsed = SYSTIME(/Julian) - start_time
;  PRINT, '% WRITE_PROFILES_TO_NETCDF: Time Elapsed: '+STRTRIM(time_elapsed,2)

  RETURN

END


PRO QC_DAI_PROFILES, FILENAME, VARNAME=VARNAME, VERBOSE=VERBOSE, LUN=LUN, FLAG=FLAG, $
                     REPLACE=REPLACE

  IF ( N_ELEMENTS( VARNAME ) EQ 0 ) THEN BEGIN
     IF ( KEYWORD_SET( VERBOSE ) EQ 1 ) THEN $
        PRINT, '% QC_DAI_PROFILE: running QC for rhum'
     varname = 'rhum'
  ENDIF ELSE BEGIN
     IF ( KEYWORD_SET( VERBOSE ) EQ 1 ) THEN $
        PRINT, '% QC_DAI_PROFILE: running QC for ' + varname
  ENDELSE

  IF ( N_ELEMENTS( LUN ) EQ 0 ) THEN lun = -1 ELSE lun=lun

  station = STRMID( filename, 0, 5 )

  ybeg = 1979
  yend = 2008
  time_begin = 197901
  time_end = 200812
  missing_threshold = 90.0

  level = [1000,850,700,500,400,300]
  nlevel = N_ELEMENTS( level )
  obs_hour = ['00','12']

  flag = 0

  ;; Get data
  data = NCDF_READV( filename, VARNAME=varname )
  time = NCDF_READV( filename, VARNAME='time' )
  
  ;; Subset data to 1979 to 2009 period
  SUBSET_BY_YEAR, data, time, ybeg, yend

  fmt = '(" Hour=",a2," Level=",i4," First=",i6," Last=",i6," Percentage Complete=",f5.1,"%",' + $
        '" IS_FULL_TIMESPAN=",i1," MISSING_CHECK=",i1)'

  flag = 0

  ;; Loop through observation hours and levels
  FOR ihour = 0, 1 DO BEGIN

     FOR ilev = 0, nlevel-1 DO BEGIN

        ;; Check data spans 1979 to 2009 period
        timespan_check = IS_FULL_TIMESPAN( data[ihour,ilev,*], time, time_begin, time_end, $
                                           FIRST_DATA=first_data, LAST_DATA=last_data )

        ;; Check that data has no less than 90% complete record
        missing_check = PERCENTAGE_MISSING( data[ihour,ilev,*], THRESHOLD = missing_threshold, NUM_VALID=num_valid )
        
        PRINTF, lun, obs_hour[ihour], level[ilev], first_data, last_data, $
               num_valid, timespan_check, missing_check, $
               FORMAT=fmt
        
        IF ( timespan_check EQ 0 OR missing_check EQ 0 ) THEN BEGIN
           flag = flag + 1
        ENDIF

        IF ( KEYWORD_SET( REPLACE ) EQ 1 ) THEN BEGIN

           IF ( timespan_check EQ 0 OR missing_check EQ 0 ) THEN BEGIN
              data[ ihour, ilev, * ] = -9999.99
           ENDIF

        ENDIF

     ENDFOR ;; End of level loop

  ENDFOR ;; end of hour loop

  ;; Write QC'd data to outfile
  IF ( KEYWORD_SET( REPLACE ) EQ 1 ) THEN BEGIN
     ncoutfile = station + ".month."+varname+".qc.nc"
     WRITE_QC_TO_NETCDF, data, time, ncoutfile, station
  ENDIF 

  RETURN

END
