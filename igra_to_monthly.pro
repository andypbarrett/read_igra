;;----------------------------------------------------------------------
;; Calculates monthly means of surface and sea level pressure, surface
;; temperature and dewpoint, and mandatory level temperatures and dewpoints
;; from profile observations.  Monthly means are calculated for 00h and 12h
;; separately.
;;
;; 2010-12-02 A.P.Barrett
;;----------------------------------------------------------------------

PRO GET_SURFACE_OBS, PROFILE, PRESSURE, TEMPERATURE, DEWPOINT, $
                     NSURFACE=NSURFACE

  pressure    = profile.pressure[0]
  temperature = profile.temperature[0]
  dewpoint    = profile.dewpoint[0]

  not_surface = WHERE( profile.minor_level_type[0] NE 1, num_not_surface )
  IF ( num_not_surface GT 0 ) THEN BEGIN
     pressure[ not_surface ]    = -9999.99
     temperature[ not_surface ] = -9999.99
     dewpoint[ not_surface ]    = -9999.99
  ENDIF

  nsurface = TOTAL( profile.minor_level_type[0] EQ 1 )

  RETURN

END

FUNCTION GET_SEA_LEVEL_PRESSURE, SURFACE_PRESSURE, ELEVATION

  psl = MAKE_ARRAY( N_ELEMENTS( surface_pressure ) , /FLOAT, VALUE=-9999.99 )

  pos = WHERE( surface_pressure GT -9999., count )
  IF ( count GT 0 ) THEN BEGIN
     psl[ pos ] = PRESSURE2SEALEVEL( surface_pressure[ pos ], elevation )
  ENDIF

  RETURN, psl

END

FUNCTION GET_MEAN_1D, VALUE, MIN_NUM_VALUE

  result = -9999.99

  pos = WHERE( value GT -9999., count )
  IF ( count GT min_num_value ) THEN result = MEAN( value[ pos ] )

  RETURN, result

END

FUNCTION GET_MEAN_2D, VALUE, MIN_NUM_VALUE

  dims = SIZE( value, /DIMENSIONS )
  nlev = dims[1]

  result = MAKE_ARRAY( nlev, /FLOAT, VALUE=-9999.99 )

  FOR ilev = 0, ( nlev - 1 ) DO BEGIN

     pos = WHERE( value[ *, ilev ] GT -9999.99, count )
     IF ( count GT min_num_value ) THEN result[ ilev ] = MEAN( value[ pos, ilev ] )

  ENDFOR

  RETURN, result

END
  
FUNCTION GET_DEWPOINT, TAIR, TDEW

  dims = SIZE( tair, /DIMENSIONS )

  result = MAKE_ARRAY( dims, /FLOAT, VALUE=-9999.99 )

  pos = WHERE( tair GT -9999.99 AND tdew GT -9999.99, count )
  IF ( count GT 0 ) THEN BEGIN
     result[ pos ] = tair[ pos ] - tdew[ pos ]
  ENDIF

  RETURN, result

END

FUNCTION GET_PROFILE_INDEX, PROFILE, YEAR, MONTH, HOUR, NPROFILE=NPROFILE

  result = -1l
  nprofile = 0l

  IF ( N_ELEMENTS( profile ) EQ 0 ) THEN BEGIN
     PRINT, '% GET_PROFILE_INDEX: Argument PROFILE is undefined'
     RETURN, result
  ENDIF

  IF ( N_ELEMENTS( year ) EQ 0 ) THEN BEGIN
     PRINT, '% GET_PROFILE_INDEX: Argument YEAR is undefined'
     RETURN, result
  ENDIF

  IF ( N_ELEMENTS( month ) EQ 0 ) THEN BEGIN
     PRINT, '% GET_PROFILE_INDEX: Argument MONTH is undefined'
     RETURN, result
  ENDIF

  IF ( N_ELEMENTS( hour ) EQ 0 ) THEN BEGIN
     PRINT, '% GET_PROFILE_INDEX: Argument HOUR is undefined'
     RETURN, result
  ENDIF

  CASE hour OF

     0: BEGIN
        result = WHERE( profile.year EQ year AND $
                        profile.month EQ month AND $
                        ( ( profile.obs_hour GT 21 AND profile.obs_hour LT 24 ) OR $
                          ( profile.obs_hour GE 0 AND profile.obs_hour LT 3 ) ), $
                        nprofile )
     END
     12: BEGIN
        result = WHERE( profile.year EQ year AND $
                        profile.month EQ month AND $
                        ( profile.obs_hour GT 9 AND profile.obs_hour LT 15 ), $
                        nprofile )
     END
     ELSE: BEGIN
        PRINT, '% GET_PROFILE_INDEX: Expects HOUR to be 00h or 12h'
     END

  ENDCASE

  RETURN, result

END

PRO MONTH_PROFILES_TO_NETCDF, DATA, STATION_NUMBER, OUTFILE

;  start_time = SYSTIME(/Julian)

  level    = data.level
  obs_hour = data.obs_hour

  ntime  = N_ELEMENTS( data.time )
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
  tim_varid     = NCDF_VARDEF( cdfid, 'time', [tim_dimid], /FLOAT )
  lev_varid     = NCDF_VARDEF( cdfid, 'level', [lev_dimid], /FLOAT )
  obs_varid     = NCDF_VARDEF( cdfid, 'hour', [obs_dimid], /FLOAT )

;  pw1_varid     = NCDF_VARDEF( cdfid, 'pwat500', [obs_dimid, tim_dimid], /FLOAT )
;  pw2_varid     = NCDF_VARDEF( cdfid, 'pwat300', [obs_dimid, tim_dimid], /FLOAT )
;  pw3_varid     = NCDF_VARDEF( cdfid, 'pwat100', [obs_dimid, tim_dimid], /FLOAT )
  psfc_varid    = NCDF_VARDEF( cdfid, 'psfc',    [tim_dimid, obs_dimid], /FLOAT )
  tsfc_varid    = NCDF_VARDEF( cdfid, 'tsfc',    [tim_dimid, obs_dimid], /FLOAT )
  tdsfc_varid   = NCDF_VARDEF( cdfid, 'tdsfc',   [tim_dimid, obs_dimid], /FLOAT )
;  shum_sfc_varid = NCDF_VARDEF( cdfid, 'shum_sfc', [obs_dimid, tim_dimid], /FLOAT )
;  rhum_sfc_varid = NCDF_VARDEF( cdfid, 'rhum_sfc', [obs_dimid, tim_dimid], /FLOAT )
  ta_varid      = NCDF_VARDEF( cdfid, 'ta',      [tim_dimid, obs_dimid, lev_dimid], /FLOAT )
  td_varid      = NCDF_VARDEF( cdfid, 'td',      [tim_dimid, obs_dimid, lev_dimid], /FLOAT )
  shum_varid    = NCDF_VARDEF( cdfid, 'shum',    [tim_dimid, obs_dimid, lev_dimid], /FLOAT )
;  rhum_varid    = NCDF_VARDEF( cdfid, 'rhum',    [obs_dimid, lev_dimid, tim_dimid], /FLOAT )
  
  NCDF_ATTPUT, cdfid, tim_varid, 'long_name', 'time'
  NCDF_ATTPUT, cdfid, tim_varid, 'units', 'days since 1900-01-01 00:00:0.0'
  NCDF_ATTPUT, cdfid, tim_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, lev_varid, 'long_name', 'mandatory levels'
  NCDF_ATTPUT, cdfid, lev_varid, 'units', 'hPa'
  NCDF_ATTPUT, cdfid, lev_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, obs_varid, 'long_name', 'observation hour'
  NCDF_ATTPUT, cdfid, obs_varid, 'units', 'hour'
  NCDF_ATTPUT, cdfid, obs_varid, '_FillValue', FillValue

;  NCDF_ATTPUT, cdfid, pw1_varid, 'long_name', 'precipitable water surface to 500mb'
;  NCDF_ATTPUT, cdfid, pw1_varid, 'units', 'mm'
;  NCDF_ATTPUT, cdfid, pw1_varid, '_FillValue', FillValue

;  NCDF_ATTPUT, cdfid, pw2_varid, 'long_name', 'precipitable water 500mb to 300mb'
;  NCDF_ATTPUT, cdfid, pw2_varid, 'units', 'mm'
;  NCDF_ATTPUT, cdfid, pw2_varid, '_FillValue', FillValue

;  NCDF_ATTPUT, cdfid, pw3_varid, 'long_name', 'precipitable water 300mb to 100mb'
;  NCDF_ATTPUT, cdfid, pw3_varid, 'units', 'mm'
;  NCDF_ATTPUT, cdfid, pw3_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, psfc_varid, 'long_name', 'surface pressure'
  NCDF_ATTPUT, cdfid, psfc_varid, 'units', 'hPa'
  NCDF_ATTPUT, cdfid, psfc_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, tsfc_varid, 'long_name', 'surface temperature'
  NCDF_ATTPUT, cdfid, tsfc_varid, 'units', 'deg C'
  NCDF_ATTPUT, cdfid, tsfc_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, tdsfc_varid, 'long_name', 'surface dewpoint temperature depression'
  NCDF_ATTPUT, cdfid, tdsfc_varid, 'units', 'deg C'
  NCDF_ATTPUT, cdfid, tdsfc_varid, '_FillValue', FillValue

;  NCDF_ATTPUT, cdfid, shum_sfc_varid, 'long_name', 'surface specific humidity'
;  NCDF_ATTPUT, cdfid, shum_sfc_varid, 'units', 'g/kg'
;  NCDF_ATTPUT, cdfid, shum_sfc_varid, '_FillValue', FillValue

;  NCDF_ATTPUT, cdfid, rhum_sfc_varid, 'long_name', 'surface relative humidity'
;  NCDF_ATTPUT, cdfid, rhum_sfc_varid, 'units', '%'
;  NCDF_ATTPUT, cdfid, rhum_sfc_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, ta_varid, 'long_name', 'temperature'
  NCDF_ATTPUT, cdfid, ta_varid, 'units', 'deg C'
  NCDF_ATTPUT, cdfid, ta_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, td_varid, 'long_name', 'dewpoint temperature depression'
  NCDF_ATTPUT, cdfid, td_varid, 'units', 'deg C'
  NCDF_ATTPUT, cdfid, td_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, shum_varid, 'long_name', 'specfic humidity'
  NCDF_ATTPUT, cdfid, shum_varid, 'units', 'g/kg'
  NCDF_ATTPUT, cdfid, shum_varid, '_FillValue', FillValue

;  NCDF_ATTPUT, cdfid, rhum_varid, 'long_name', 'relative humidity'
;  NCDF_ATTPUT, cdfid, rhum_varid, 'units', '%'
;  NCDF_ATTPUT, cdfid, rhum_varid, '_FillValue', FillValue

  ;; Add global variables
  NCDF_ATTPUT, cdfid, 'created_by', 'A.P.Barrett', /GLOBAL
  NCDF_ATTPUT, cdfid, 'created', SYSTIME(), /GLOBAL
  NCDF_ATTPUT, cdfid, 'station_number', station_number, /GLOBAL
  NCDF_ATTPUT, cdfid, 'source', 'IGRA radiosonde data', /GLOBAL

  NCDF_CONTROL, cdfid, /ENDEF

  NCDF_VARPUT, cdfid, tim_varid,      data.time
  NCDF_VARPUT, cdfid, lev_varid,      level
  NCDF_VARPUT, cdfid, obs_varid,      obs_hour

;  NCDF_VARPUT, cdfid, pw1_varid,      data.pw1
;  NCDF_VARPUT, cdfid, pw2_varid,      data.pw2
;  NCDF_VARPUT, cdfid, pw3_varid,      data.pw3
  NCDF_VARPUT, cdfid, psfc_varid,     data.psurf
  NCDF_VARPUT, cdfid, tsfc_varid,     data.tair_surf
  NCDF_VARPUT, cdfid, tdsfc_varid,    data.tdew_surf
;  NCDF_VARPUT, cdfid, shum_sfc_varid, data.spechum
;  NCDF_VARPUT, cdfid, rhum_sfc_varid, data.relhum
  NCDF_VARPUT, cdfid, ta_varid,       data.tair
  NCDF_VARPUT, cdfid, td_varid,       data.tdew
  NCDF_VARPUT, cdfid, shum_varid,     data.spfh
;  NCDF_VARPUT, cdfid, rhum_varid,     data.relhum
  
  NCDF_CLOSE, cdfid

;  time_elapsed = SYSTIME(/Julian) - start_time
;  PRINT, '% WRITE_PROFILES_TO_NETCDF: Time Elapsed: '+STRTRIM(time_elapsed,2)

  RETURN

END

PRO IGRA_TO_MONTHLY_BATCH

  fileList = [ '01001.dat.gz', $
               '01028.dat.gz', $
               '04320.dat.gz', $
               '04339.dat.gz', $
               '21824.dat.gz', $
               '70026.dat.gz', $
               '71082.dat.gz', $
               '71917.dat.gz', $
               '71924.dat.gz' ]
  nFile = N_ELEMENTS( fileList )

  elevation = [ 9., 18., 14., 69., 8., 12., 66., 10., 40. ]

  level = [1000., 850., 700., 500., 400., 300]

  yrStart = 1979
  yrEnd   = 2009

  FOR ifile = 0, nFile-1 DO BEGIN

     IF ( STREGEX( fileList[ifile], '.gz$', /BOOLEAN ) EQ 1 ) THEN BEGIN
        PRINT, '% IGRA_TO_MONTHLY_BATCH: unzipping ' + fileList[ ifile ]
        SPAWN, 'gunzip ' + fileList[ ifile ]
        newFile = STRJOIN( (STRSPLIT( fileList[ ifile ], '.', /EXTRACT ))[0:1], '.' )
     ENDIF ELSE BEGIN
        newFile = fileList[ ifile ]
     ENDELSE

     PRINT, '% IGRA_TO_MONTHLY_BATCH: reading data from ' + newFile
     READ_IGRA_DATA_NEW, newFile, data

     PRINT, ' - calculating monthly means'
     IGRA_TO_MONTHLY, data, yrStart, yrEnd, mon_data, elevation[ifile], $
                      LEVEL=level

     station_id = (STRSPLIT( fileList[ ifile ], '.', /EXTRACT ))[0]
     outFile =  station_id + '.month.nc'
     PRINT, ' - writing modnthly means to ' + outFile
     MONTH_PROFILES_TO_NETCDF, mon_data, station_id, outFile

     PRINT, ' - zipping ' + newFile
     SPAWN, 'gzip ' + newFile

  ENDFOR

  RETURN

END
     

PRO IGRA_TO_MONTHLY, PROFILE, YEAR_START, YEAR_END, RESULT, $
                     ELEVATION, LEVEL=LEVEL, DEBUG=DEBUG

  mandatory_levels = [1000., 850., 700., 500., 400., 300., 200., 150., 100., 50. ]

  obs_hour = [ 0, 12 ]  ;; Observation hours for which monthly means are calculated

  min_num_profile = 10  ;; minimum number of profiles from which means can be calculated

  level = N_ELEMENTS( level ) EQ 0 ? mandatory_levels : level

  nyear = year_end - year_start + 1
  ntime = nyear * 12 ;; number of monthly time steps

  nobs_hour = 2  ;; number of observation hours 
  nlevel = N_ELEMENTS( level )

  ;; Define result arrays
  time  = MAKE_ARRAY( ntime, /FLOAT, VALUE=-9999.99 )
  psurf = MAKE_ARRAY( ntime, nobs_hour, /FLOAT, VALUE=-9999.99 )
  psl   = MAKE_ARRAY( ntime, nobs_hour, /FLOAT, VALUE=-9999.99 )
  tair_surf = MAKE_ARRAY( ntime, nobs_hour, /FLOAT, VALUE=-9999.99 )
  tdew_surf = MAKE_ARRAY( ntime, nobs_hour, /FLOAT, VALUE=-9999.99 )

  tair  = MAKE_ARRAY( ntime, nobs_hour, nlevel, /FLOAT, VALUE=-9999.99 )
  tdew  = MAKE_ARRAY( ntime, nobs_hour, nlevel, /FLOAT, VALUE=-9999.99 )
  spfh  = MAKE_ARRAY( ntime, nobs_hour, nlevel, /FLOAT, VALUE=-9999.99 )

  ;; Get surface observations
  GET_SURFACE_OBS, profile, psurf_prf, tair_surf_prf, tdew_dep_surf_prf

  ;; Calculate sea level pressure
  psl_prf = GET_SEA_LEVEL_PRESSURE( psurf_prf, elevation )

  ;; Calculate dewpoint temperature from dewpoint depression
  tdew_surf_prf = GET_DEWPOINT( tair_surf_prf, tdew_dep_surf_prf )

  ;; Get mandatory levels
  CREATE_MANDATORY_LEVEL_ARRAY, profile, tair_prf, gph, tdew_dep_prf, wdir, wspd, $
                                LEVEL=level

  ;; calculate dewpoints from dewpoint depression for mandatory levels
  tdew_prf = GET_DEWPOINT( tair_prf, tdew_dep_prf )

  ;; Calculate specific humidity from dew points
  ;; Create array to hold specific humidity
  dims = SIZE( tair_prf, /DIMENSIONS )
  spfh_prf = MAKE_ARRAY( dims, /FLOAT, VALUE=-9999.99 )
  ;; - create pressure array
  press_prf = REBIN( TRANSPOSE(level), dims[0], dims[1], /SAMPLE )
  ;; Find valid tdew values
  isValid = WHERE( tdew_prf GT -9999., numValid )
  ;; Calculate specific humidity
  spfh_prf[ isValid ] = TDEW2SPECHUM( tdew_prf[ isValid ], press_prf[ isValid ] ) 
  
  itime = 0
  FOR iyear = year_start, year_end DO BEGIN
  
     FOR imonth = 1, 12 DO BEGIN

        time[ itime ] = JULDAY( imonth, 1, iyear )

        FOR iobs = 0, 1 DO BEGIN
           
           thisprofile = GET_PROFILE_INDEX( profile, iyear, imonth, obs_hour[ iobs ], NPROFILE=num_profile )

           IF ( num_profile GE min_num_profile ) THEN BEGIN

              ;; Calculate mean surface pressure
              psurf[ itime, iobs ] = GET_MEAN_1D( psurf_prf[ thisprofile ], min_num_profile )

              ;; Calculate mean sea level pressure
              psl[ itime, iobs ] = GET_MEAN_1D( psl_prf[ thisprofile ], min_num_profile )

              ;; Calculate mean surface temperature
              tair_surf[ itime, iobs ] = GET_MEAN_1D( tair_surf_prf[ thisprofile ], min_num_profile )

              ;; Calculate mean surface dewpoint temperature
              tdew_surf[ itime, iobs ] = GET_MEAN_1D( tdew_surf_prf[ thisprofile ], min_num_profile )

              ;; Calculate mean temperature at mandatory levels
              tair[ itime, iobs, * ] = GET_MEAN_2D( tair_prf[ thisprofile, * ], min_num_profile )

              ;; Calculate mean dewpoint temperature for mandatory levels
              tdew[ itime, iobs, * ] = GET_MEAN_2D( tdew_prf[ thisprofile, * ], min_num_profile )

              ;; Calculate mean specific humidity
              spfh[ itime, iobs, * ] = GET_MEAN_2D( spfh_prf[ thisprofile, * ], min_num_profile )

           ENDIF
           
           IF ( KEYWORD_SET( DEBUG ) EQ 1 ) THEN BEGIN
              fmt = '(i6,1x,i4,1x,i02,1x,i02,"h",2x,i7,2x,i4,4(1x,f8.2)," - ",' + $
                    STRTRIM( nlevel, 2 ) + '(1x,f8.2))'
              PRINT, FORMAT=fmt, $
                     itime, iyear, imonth, obs_hour[iobs], time[itime], num_profile, $
                     psurf[ itime, iobs ], psl[ itime, iobs ], tair_surf[ itime, iobs ], $
                     tdew_surf[ itime, iobs ], tair[ itime, iobs, * ]
           ENDIF

        ENDFOR
           
        itime = itime + 1

     ENDFOR

  ENDFOR

  result = {time: time, $
            obs_hour: obs_hour, $
            level: level, $
            psurf: psurf, $
            psl: psl, $
            tair_surf: tair_surf, $
            tdew_surf: tdew_surf, $
            tair: tair, $
            tdew: tdew, $
            spfh: spfh}

  RETURN

END           
     
