;;----------------------------------------------------------------------
;; Calculates monthly averages for period of record for 00h and 12h
;; observations.
;;
;; 2011-06-07 A.P.Barrett
;;----------------------------------------------------------------------

FUNCTION DEFINE_MONTH_STRUCT, MAXLEV=MAXLEV, MAXOBS=MAXOBS

  MAXLEV = N_ELEMENTS( MAXLEV ) EQ 0 ? 7 : MAXLEV
  MAXOBS = N_ELEMENTS( MAXOBS ) EQ 0 ? 2 : MAXOBS

  data_record = { time:           -9999.99, $
                  pw1:            MAKE_ARRAY( MAXOBS, /FLOAT, VALUE=-9999.99) , $
                  pw2:            MAKE_ARRAY( MAXOBS, /FLOAT, VALUE=-9999.99) , $
                  pw3:            MAKE_ARRAY( MAXOBS, /FLOAT, VALUE=-9999.99) , $
                  pressure:       MAKE_ARRAY( MAXOBS, MAXLEV, /FLOAT, VALUE=-9999.99 ), $
                  geopot_hgt:     MAKE_ARRAY( MAXOBS, MAXLEV, /FLOAT, VALUE=-9999.99 ), $
                  temperature:    MAKE_ARRAY( MAXOBS, MAXLEV, /FLOAT, VALUE=-9999.99 ), $
                  dewpoint:       MAKE_ARRAY( MAXOBS, MAXLEV, /FLOAT, VALUE=-9999.99 ), $
                  wind_direction: MAKE_ARRAY( MAXOBS, MAXLEV, /FLOAT, VALUE=-9999.99 ), $
                  wind_speed:     MAKE_ARRAY( MAXOBS, MAXLEV, /FLOAT, VALUE=-9999.99 ), $
                  specHum:        MAKE_ARRAY( MAXOBS, MAXLEV, /FLOAT, VALUE=-9999.99 ), $
                  relHum:         MAKE_ARRAY( MAXOBS, MAXLEV, /FLOAT, VALUE=-9999.99 ) }

  RETURN, data_record

END

FUNCTION DIM_AVG, DATA, DIMID, DEFAULT=DEFAULT, MINNUM=MINNUM

  IF ( N_PARAMS() NE 2 ) THEN BEGIN
     PRINT, '% DIM_AVG: USAGE: RESULT = DIM_AVG(DATA, DIMID, DEFAULT=DEFAULT, MINNUM=MINNUM)'
     RETURN, -1
  ENDIF

  DEFAULT = N_ELEMENTS( DEFAULT ) EQ 0 ? -9999. : DEFAULT
  MINNUM  = N_ELEMENTS( minnum ) EQ 0 ? 10 : minnum

  isdefault = WHERE( data LE default, numdefault )
  IF ( numdefault GT 0 ) THEN data[ isdefault ] = !VALUES.F_NAN

  sum = TOTAL( data, 2, /NAN )
  num = TOTAL( FINITE( data ), 2, /NAN )

  result = MAKE_ARRAY( N_ELEMENTS( sum ), /FLOAT, VALUE=-9999.99 )

  isvalid = WHERE( num GE minnum, numvalid )
  IF ( numvalid GT 0 ) THEN result[isvalid] = sum[isvalid] / num[isvalid]

  RETURN, result

END

FUNCTION ONED_AVG, DATA, DEFAULT=DEFAULT, MINNUM=MINNUM

  DEFAULT = N_ELEMENTS( DEFAULT ) EQ 0 ? -9999. : DEFAULT
  MINNUM  = N_ELEMENTS( minnum ) EQ 0 ? 10 : minnum

  isdefault = WHERE( data LE default, numdefault )
  IF ( numdefault GT 0 ) THEN data[ isdefault ] = !VALUES.F_NAN

  result = MEAN( data, /NAN )

  IF ( FINITE( result ) EQ 0 ) THEN result = -9999.99

  RETURN, result

END


FUNCTION AVERAGE_STRUCT, DSTRUCT, MSTRUCT, IOBS

  thisField = [7,8,9,12,14,16,18,19,20,21,22]
  nfield = N_ELEMENTS( thisField )

  help, dstruct
  help, dstruct, /struct
  stop

  FOR ifield = 0, nfield - 1 DO BEGIN

     IF ( ifield LT 3 ) THEN BEGIN

        mstruct.(ifield+1)[iobs,*] = ONED_AVG( dstruct.(thisField[ifield])  )

     ENDIF ELSE BEGIN

        mstruct.(ifield+1)[iobs,*] = DIM_AVG( dstruct.(thisField[ifield])  )
        
     ENDELSE

  ENDFOR

  RETURN, mstruct
        
END


PRO MONTH_PROFILES_TO_NETCDF, DATA, STATION_NUMBER, OUTFILE

;  start_time = SYSTIME(/Julian)

  level = [1000.,850.,700.,500.,400.,300.]
  obs_hour = [0,12]

  ntime  = N_ELEMENTS( DATA )
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

  pw1_varid     = NCDF_VARDEF( cdfid, 'pwat500', [obs_dimid, tim_dimid], /FLOAT )
  pw2_varid     = NCDF_VARDEF( cdfid, 'pwat300', [obs_dimid, tim_dimid], /FLOAT )
  pw3_varid     = NCDF_VARDEF( cdfid, 'pwat100', [obs_dimid, tim_dimid], /FLOAT )
  psfc_varid    = NCDF_VARDEF( cdfid, 'psfc',    [obs_dimid, tim_dimid], /FLOAT )
  zsfc_varid    = NCDF_VARDEF( cdfid, 'zsfc',    [obs_dimid, tim_dimid], /FLOAT )
  tsfc_varid    = NCDF_VARDEF( cdfid, 'tsfc',    [obs_dimid, tim_dimid], /FLOAT )
  tdsfc_varid   = NCDF_VARDEF( cdfid, 'tdsfc',   [obs_dimid, tim_dimid], /FLOAT )
  wdsfc_varid   = NCDF_VARDEF( cdfid, 'wdsfc',   [obs_dimid, tim_dimid], /FLOAT )
  wssfc_varid   = NCDF_VARDEF( cdfid, 'wssfc',   [obs_dimid, tim_dimid], /FLOAT )
  shum_sfc_varid = NCDF_VARDEF( cdfid, 'shum_sfc', [obs_dimid, tim_dimid], /FLOAT )
  rhum_sfc_varid = NCDF_VARDEF( cdfid, 'rhum_sfc', [obs_dimid, tim_dimid], /FLOAT )
  pres_varid    = NCDF_VARDEF( cdfid, 'pres',    [obs_dimid, lev_dimid, tim_dimid], /FLOAT )
  z_varid       = NCDF_VARDEF( cdfid, 'z',       [obs_dimid, lev_dimid, tim_dimid], /FLOAT )
  ta_varid      = NCDF_VARDEF( cdfid, 'ta',      [obs_dimid, lev_dimid, tim_dimid], /FLOAT )
  td_varid      = NCDF_VARDEF( cdfid, 'td',      [obs_dimid, lev_dimid, tim_dimid], /FLOAT )
  wd_varid      = NCDF_VARDEF( cdfid, 'wd',      [obs_dimid, lev_dimid, tim_dimid], /FLOAT )
  ws_varid      = NCDF_VARDEF( cdfid, 'ws',      [obs_dimid, lev_dimid, tim_dimid], /FLOAT )
  shum_varid    = NCDF_VARDEF( cdfid, 'shum',    [obs_dimid, lev_dimid, tim_dimid], /FLOAT )
  rhum_varid    = NCDF_VARDEF( cdfid, 'rhum',    [obs_dimid, lev_dimid, tim_dimid], /FLOAT )
  
  NCDF_ATTPUT, cdfid, tim_varid, 'long_name', 'time'
  NCDF_ATTPUT, cdfid, tim_varid, 'units', 'days since 1900-01-01 00:00:0.0'
  NCDF_ATTPUT, cdfid, tim_varid, '_FillValue', DOUBLE(FillValue), /DOUBLE

  NCDF_ATTPUT, cdfid, lev_varid, 'long_name', 'mandatory levels'
  NCDF_ATTPUT, cdfid, lev_varid, 'units', 'hPa'
  NCDF_ATTPUT, cdfid, lev_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, obs_varid, 'long_name', 'observation hour'
  NCDF_ATTPUT, cdfid, obs_varid, 'units', 'hour'
  NCDF_ATTPUT, cdfid, obs_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, pw1_varid, 'long_name', 'precipitable water surface to 500mb'
  NCDF_ATTPUT, cdfid, pw1_varid, 'units', 'mm'
  NCDF_ATTPUT, cdfid, pw1_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, pw2_varid, 'long_name', 'precipitable water 500mb to 300mb'
  NCDF_ATTPUT, cdfid, pw2_varid, 'units', 'mm'
  NCDF_ATTPUT, cdfid, pw2_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, pw3_varid, 'long_name', 'precipitable water 300mb to 100mb'
  NCDF_ATTPUT, cdfid, pw3_varid, 'units', 'mm'
  NCDF_ATTPUT, cdfid, pw3_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, psfc_varid, 'long_name', 'surface pressure'
  NCDF_ATTPUT, cdfid, psfc_varid, 'units', 'hPa'
  NCDF_ATTPUT, cdfid, psfc_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, zsfc_varid, 'long_name', 'surface geopotential height'
  NCDF_ATTPUT, cdfid, zsfc_varid, 'units', 'm'
  NCDF_ATTPUT, cdfid, zsfc_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, tsfc_varid, 'long_name', 'surface temperature'
  NCDF_ATTPUT, cdfid, tsfc_varid, 'units', 'deg C'
  NCDF_ATTPUT, cdfid, tsfc_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, tdsfc_varid, 'long_name', 'surface dewpoint temperature depression'
  NCDF_ATTPUT, cdfid, tdsfc_varid, 'units', 'deg C'
  NCDF_ATTPUT, cdfid, tdsfc_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, wdsfc_varid, 'long_name', 'surface wind direction'
  NCDF_ATTPUT, cdfid, wdsfc_varid, 'units', 'degrees of arc'
  NCDF_ATTPUT, cdfid, wdsfc_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, wssfc_varid, 'long_name', 'surface wind speed'
  NCDF_ATTPUT, cdfid, wssfc_varid, 'units', 'degrees of arc'
  NCDF_ATTPUT, cdfid, wssfc_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, shum_sfc_varid, 'long_name', 'surface specific humidity'
  NCDF_ATTPUT, cdfid, shum_sfc_varid, 'units', 'g/kg'
  NCDF_ATTPUT, cdfid, shum_sfc_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, rhum_sfc_varid, 'long_name', 'surface relative humidity'
  NCDF_ATTPUT, cdfid, rhum_sfc_varid, 'units', '%'
  NCDF_ATTPUT, cdfid, rhum_sfc_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, pres_varid, 'long_name', 'pressure'
  NCDF_ATTPUT, cdfid, pres_varid, 'units', 'hPa'
  NCDF_ATTPUT, cdfid, pres_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, z_varid, 'long_name', 'geopotential height'
  NCDF_ATTPUT, cdfid, z_varid, 'units', 'm'
  NCDF_ATTPUT, cdfid, z_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, ta_varid, 'long_name', 'temperature'
  NCDF_ATTPUT, cdfid, ta_varid, 'units', 'deg C'
  NCDF_ATTPUT, cdfid, ta_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, td_varid, 'long_name', 'dewpoint temperature depression'
  NCDF_ATTPUT, cdfid, td_varid, 'units', 'deg C'
  NCDF_ATTPUT, cdfid, td_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, td_varid, 'long_name', 'wind direction'
  NCDF_ATTPUT, cdfid, td_varid, 'units', 'degrees of arc'
  NCDF_ATTPUT, cdfid, td_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, wd_varid, 'long_name', 'wind direction'
  NCDF_ATTPUT, cdfid, wd_varid, 'units', 'degrees'
  NCDF_ATTPUT, cdfid, wd_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, ws_varid, 'long_name', 'wind speed'
  NCDF_ATTPUT, cdfid, ws_varid, 'units', 'm/s'
  NCDF_ATTPUT, cdfid, ws_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, shum_varid, 'long_name', 'specfic humidity'
  NCDF_ATTPUT, cdfid, shum_varid, 'units', 'g/kg'
  NCDF_ATTPUT, cdfid, shum_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, rhum_varid, 'long_name', 'relative humidity'
  NCDF_ATTPUT, cdfid, rhum_varid, 'units', '%'
  NCDF_ATTPUT, cdfid, rhum_varid, '_FillValue', FillValue

  ;; Add global variables
  NCDF_ATTPUT, cdfid, 'created_by', 'A.P.Barrett', /GLOBAL
  NCDF_ATTPUT, cdfid, 'created', SYSTIME(), /GLOBAL
  NCDF_ATTPUT, cdfid, 'station_number', station_number, /GLOBAL
  NCDF_ATTPUT, cdfid, 'source', 'Dai homogenized radiosonde data', /GLOBAL

  NCDF_CONTROL, cdfid, /ENDEF

  NCDF_VARPUT, cdfid, tim_varid,      data.time
  NCDF_VARPUT, cdfid, lev_varid,      level
  NCDF_VARPUT, cdfid, obs_varid,      obs_hour

  NCDF_VARPUT, cdfid, pw1_varid,      data.pw1[*,0]
  NCDF_VARPUT, cdfid, pw2_varid,      data.pw2[*,0]
  NCDF_VARPUT, cdfid, pw3_varid,      data.pw3[*,0]
  NCDF_VARPUT, cdfid, psfc_varid,     data.pressure[*,0]
  NCDF_VARPUT, cdfid, zsfc_varid,     data.geopot_hgt[*,0]
  NCDF_VARPUT, cdfid, tsfc_varid,     data.temperature[*,0]
  NCDF_VARPUT, cdfid, tdsfc_varid,    data.dewpoint[*,0]
  NCDF_VARPUT, cdfid, wdsfc_varid,    data.wind_direction[*,0]
  NCDF_VARPUT, cdfid, wssfc_varid,    data.wind_speed[*,0]
  NCDF_VARPUT, cdfid, shum_sfc_varid, data.spechum[*,0]
  NCDF_VARPUT, cdfid, rhum_sfc_varid, data.relhum[*,0]
  NCDF_VARPUT, cdfid, psfc_varid,     data.pressure[*,0]
  NCDF_VARPUT, cdfid, z_varid,        data.geopot_hgt[*,1:6,*]
  NCDF_VARPUT, cdfid, ta_varid,       data.temperature[*,1:6,*]
  NCDF_VARPUT, cdfid, td_varid,       data.dewpoint[*,1:6,*]
  NCDF_VARPUT, cdfid, wd_varid,       data.wind_direction[*,1:6,*]
  NCDF_VARPUT, cdfid, ws_varid,       data.wind_speed[*,1:6,*]
  NCDF_VARPUT, cdfid, shum_varid,     data.spechum[*,1:6,*]
  NCDF_VARPUT, cdfid, rhum_varid,     data.relhum[*,1:6,*]
  
  NCDF_CLOSE, cdfid

;  time_elapsed = SYSTIME(/Julian) - start_time
;  PRINT, '% WRITE_PROFILES_TO_NETCDF: Time Elapsed: '+STRTRIM(time_elapsed,2)

  RETURN

END


PRO DAI_PROFILES_TO_MONTH, DAY_STRUCT, MON_STRUCT, minRec=minRec

  nmonth = 12

  minrec = N_ELEMENTS( minRec ) EQ 0 ? 10 : minRec

  ;; Find start and end year, and calculate number of months
  ybeg = MIN( day_struct.year )
  yend = MAX( day_struct.year )

  ;; Find number of years in record
  nyear = yend - ybeg + 1

  ;; calculate number monthly time steps
  ntime = nyear * nmonth

  ;; Define array of structures to hold monthly means
  tmpStruct = DEFINE_MONTH_STRUCT( )
  mon_struct = REPLICATE( tmpStruct, ntime )

  ;; Calculate averages for 00h
  iobs = 0
  k = 0
  FOR iy = 0, nyear - 1 DO BEGIN

     thisYear = ybeg + iy

     FOR im = 0, nmonth - 1 DO BEGIN

        thisMonth = im + 1

        ;; Define time
        time = JULDAY( thisMonth, 1, thisYear, 0 ) - JULDAY(1,1,1900,0)
        mon_struct[k].time = time

        ;; Find indices for given month and year and observation hour
        idx = WHERE( day_struct.year EQ thisYear AND $
                     day_struct.month EQ thisMonth AND $
                     ( day_struct.obs_hour GE 22 OR $
                       day_struct.obs_hour LE 2 ), numIdx )

        thisData = 0
        IF ( numIdx GE minRec ) THEN BEGIN

           ;; Calculate means of data
           thisData = day_struct[ idx ]
           mon_struct[k] = AVERAGE_STRUCT( thisData, mon_struct[k], iobs )

        ENDIF

        ;; Increment counter for time
        k = k + 1

     ENDFOR

  ENDFOR
  
  ;; Calculate averages for 12h
  iobs = 1
  k = 0
  FOR iy = 0, nyear - 1 DO BEGIN

     thisYear = ybeg + iy

     FOR im = 0, nmonth - 1 DO BEGIN

        thisMonth = im + 1

        ;; Time is already assigned in loops for 00h obs

        ;; Find indices for given month and year and observation hour
        idx = WHERE( day_struct.year EQ thisYear AND $
                     day_struct.month EQ thisMonth AND $
                     ( day_struct.obs_hour GE 10 AND $
                       day_struct.obs_hour LE 14 ), numIdx )

        thisData = 0
        IF ( numIdx GE minRec ) THEN BEGIN

           ;; Calculate means of data
           thisData = day_struct[ idx ]
           mon_struct[k] = AVERAGE_STRUCT( thisData, mon_struct[k], iobs )

        ENDIF

        ;; Increment counter for time
        k = k + 1
        
     ENDFOR

  ENDFOR

  RETURN
  
END
