;;----------------------------------------------------------------------
;; Reformats radiosonde profiles from the Dai data set so that pressure levels
;; are organized by column, such that the first column is surface data, second
;; column is 1000mb data, third column is 850mb, and so on for each of the
;; mandatory levels (1000, 850, 700, 500, 400, 300) up to 300mb.  the data is
;; then written to a netCDF file.
;;
;; 2011-05-31 A.P.Barrett
;;----------------------------------------------------------------------

;; Create new profile with only 7 column arrays


;; Gets the array indices for mandatory levels and returns them as an array in
;; the correct order
FUNCTION GET_LEVEL_INDEX, level, major, minor, nlevel=nlevel

  mandat_level = [1000.,850.,700.,500.,400.,300.]
  nlev = N_ELEMENTS( mandat_level )

  index = MAKE_ARRAY( nlev+1, /LONG, VALUE = -1 )

  nlevel = 0

  ;; Find surface level if it exists
  pos = WHERE( major EQ 2 AND minor EQ 1, count )
  IF ( count GT 0 ) THEN BEGIN
     index[0] = pos
     nlevel = nlevel + 1
  ENDIF

  ;; Find the mandatory levels
  FOR ilev=0, nlev-1 DO BEGIN
     pos = WHERE( level EQ mandat_level[ilev], count )
     IF ( count GT 0 ) THEN BEGIN
        index[ilev+1] = pos
        nlevel = nlevel + 1
     ENDIF
  ENDFOR

  RETURN, index

END


;; Reformat a profile
FUNCTION REFORMAT_PROFILES, STRUCT

  new_struct = struct

  numtag = N_TAGS( struct )

  ;; Define mandatory levels
  level = [-9999.,1000.,850.,700.,500.,400.,300.]
  numlev = N_ELEMENTS(level)
 
  ;; Find rows in profiles that are not surface
  ;; - assumes any obs that is a mandatory level is not a surface level
  ;;   The only non-mandatory levels in the Dai data set are surface levels 
  pos = WHERE( new_struct.major_level_type[0] EQ 1, count )
  IF ( count GT 0 ) THEN BEGIN
     ;; Shift non-surface rows to the right by one 
     FOR itag = 10, 23 DO BEGIN
        new_struct[pos].(itag) = SHIFT( TEMPORARY(new_struct[pos].(itag)), 1, 0 )
     ENDFOR
  ENDIF

  ;; ...next loop through mandatory levels up to 300mb and perform the same
  ;; operations as done for the surface.
  FOR ilev=1, (numlev-1) DO BEGIN
     pos = 0
     pos = WHERE( new_struct.pressure[ilev] LT level[ilev] AND $
                  new_struct.pressure[ilev] GT -9999., count )
     IF ( count GT 0 ) THEN BEGIN
        ;; Shift arrays for mandatory levels to the right by 1
        FOR itag = 10, 23 DO BEGIN 
           tmparr = 0  ;; deletes old memory allocation for tmparr 
           tmparr = new_struct[pos].(itag)[ilev:numlev-1]
           IF ( SIZE( tmparr, /N_DIMENSIONS ) GT 1 ) THEN BEGIN
              tmparr = SHIFT( TEMPORARY(tmparr), 1, 0 )
           ENDIF ELSE BEGIN
              tmparr = SHIFT( TEMPORARY(tmparr), 1 )
           ENDELSE
           new_struct[pos].(itag)[ilev:numlev-1] = tmparr
        ENDFOR
     ENDIF
  ENDFOR

  RETURN, new_struct

END
        
;; Subset the radiosone data structure
PRO SUBSET_STRUCT, OLD_STRUCT, NEW_STRUCT, COL0, COL1

  num_lev = col1 - col0 + 1

  new_rec = DEFINE_PROFILE_RECORD( MAXLEV = num_lev )
  num_rec = N_ELEMENTS( old_struct )
  new_struct = REPLICATE( new_rec, num_rec )

  num_tags = N_TAGS( old_struct )

  FOR itag = 1, num_tags-1 DO BEGIN
     
     IF ( SIZE( old_struct.(itag), /N_DIMENSIONS ) GT 1 ) THEN BEGIN
        new_struct.(itag) = old_struct.(itag)[col0:col1]
     ENDIF ELSE BEGIN
        new_struct.(itag) = old_struct.(itag)
     ENDELSE
     
  ENDFOR

  RETURN

END

PRO WRITE_PROFILES_TO_NETCDF, DATA, OUTFILE

  start_time = SYSTIME()

  level = [1000.,850.,700.,500.,400.,300.]

  ntime  = N_ELEMENTS( DATA )
  nlevel = N_ELEMENTS( level )

  ;; calculate time for netCDF - wrt 1-1-1900:00:00
  time = JULDAY( data.month, data.day, data.year, data.obs_hour )
  time = time - JULDAY( 1, 1, 1900, 0 )

  ;; Set missing value to 1.0e+20 to conform with NCL
  FillValue = -9999.99
  
  ;; Open file for writing
  cdfid = NCDF_CREATE( outfile, /clobber )
  
  ;; Define dimensions
  tim_dimid = NCDF_DIMDEF( cdfid, 'time', ntime )
  lev_dimid = NCDF_DIMDEF( cdfid, 'level', nlevel )

  ;; variables
  tim_varid     = NCDF_VARDEF( cdfid, 'time', [tim_dimid], /DOUBLE )
  lev_varid     = NCDF_VARDEF( cdfid, 'level', [lev_dimid], /FLOAT )
  obshr_varid   = NCDF_VARDEF( cdfid, 'obs_hour', [tim_dimid], /FLOAT )
  reltime_varid = NCDF_VARDEF( cdfid, 'reltime', [tim_dimid], /FLOAT )
  nlevel_varid  = NCDF_VARDEF( cdfid, 'nlevels', [tim_dimid], /FLOAT )
  pw1_varid     = NCDF_VARDEF( cdfid, 'pwat500', [tim_dimid], /FLOAT )
  pw2_varid     = NCDF_VARDEF( cdfid, 'pwat300', [tim_dimid], /FLOAT )
  pw3_varid     = NCDF_VARDEF( cdfid, 'pwat100', [tim_dimid], /FLOAT )
  psfc_varid    = NCDF_VARDEF( cdfid, 'psfc', [tim_dimid], /FLOAT )
  zsfc_varid    = NCDF_VARDEF( cdfid, 'zsfc', [tim_dimid], /FLOAT )
  tsfc_varid    = NCDF_VARDEF( cdfid, 'tsfc', [tim_dimid], /FLOAT )
  tdsfc_varid   = NCDF_VARDEF( cdfid, 'tdsfc', [tim_dimid], /FLOAT )
  wdsfc_varid   = NCDF_VARDEF( cdfid, 'wdsfc', [tim_dimid], /FLOAT )
  wssfc_varid   = NCDF_VARDEF( cdfid, 'wssfc', [tim_dimid], /FLOAT )
  shum_sfc_varid = NCDF_VARDEF( cdfid, 'shum_sfc', [tim_dimid], /FLOAT )
  rhum_sfc_varid = NCDF_VARDEF( cdfid, 'rhum_sfc', [tim_dimid], /FLOAT )
  pres_varid    = NCDF_VARDEF( cdfid, 'pres', [tim_dimid, lev_dimid], /FLOAT )
  z_varid       = NCDF_VARDEF( cdfid, 'z', [tim_dimid, lev_dimid], /FLOAT )
  ta_varid      = NCDF_VARDEF( cdfid, 'ta', [tim_dimid, lev_dimid], /FLOAT )
  td_varid      = NCDF_VARDEF( cdfid, 'td', [tim_dimid, lev_dimid], /FLOAT )
  wd_varid      = NCDF_VARDEF( cdfid, 'wd', [tim_dimid, lev_dimid], /FLOAT )
  ws_varid      = NCDF_VARDEF( cdfid, 'ws', [tim_dimid, lev_dimid], /FLOAT )
  shum_varid    = NCDF_VARDEF( cdfid, 'shum', [tim_dimid, lev_dimid], /FLOAT )
  rhum_varid    = NCDF_VARDEF( cdfid, 'rhum', [tim_dimid, lev_dimid], /FLOAT )
  
  NCDF_ATTPUT, cdfid, tim_varid, 'long_name', 'time'
  NCDF_ATTPUT, cdfid, tim_varid, 'units', 'days since 1900-01-01 00:00:0.0'
  NCDF_ATTPUT, cdfid, tim_varid, '_FillValue', DOUBLE(FillValue), /DOUBLE

  NCDF_ATTPUT, cdfid, lev_varid, 'long_name', 'mandatory levels'
  NCDF_ATTPUT, cdfid, lev_varid, 'units', 'hPa'
  NCDF_ATTPUT, cdfid, lev_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, reltime_varid, 'long_name', 'observation hour'
  NCDF_ATTPUT, cdfid, reltime_varid, 'units', 'hour'
  NCDF_ATTPUT, cdfid, reltime_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, reltime_varid, 'long_name', 'release time'
  NCDF_ATTPUT, cdfid, reltime_varid, 'units', 'hours'
  NCDF_ATTPUT, cdfid, reltime_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, nlevel_varid, 'long_name', 'number of observed levels'
  NCDF_ATTPUT, cdfid, nlevel_varid, 'units', 'none'
  NCDF_ATTPUT, cdfid, nlevel_varid, '_FillValue', FillValue

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

  NCDF_ATTPUT, cdfid, ws_varid, 'long_name', 'wind speed'
  NCDF_ATTPUT, cdfid, ws_varid, 'units', 'm/s'
  NCDF_ATTPUT, cdfid, ws_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, shum_varid, 'long_name', 'specfic humidity'
  NCDF_ATTPUT, cdfid, shum_varid, 'units', 'g/kg'
  NCDF_ATTPUT, cdfid, shum_varid, '_FillValue', FillValue

  NCDF_ATTPUT, cdfid, rhum_varid, 'long_name', 'relative humidity'
  NCDF_ATTPUT, cdfid, rhum_varid, 'units', '%'
  NCDF_ATTPUT, cdfid, rhum_varid, '_FillValue', FillValue

  NCDF_CONTROL, cdfid, /ENDEF

  NCDF_VARPUT, cdfid, tim_varid,      time
  NCDF_VARPUT, cdfid, lev_varid,      level
  NCDF_VARPUT, cdfid, obshr_varid,    FLOAT( data.obs_hour )
  NCDF_VARPUT, cdfid, reltime_varid,  data.release_time
  NCDF_VARPUT, cdfid, nlevel_varid,   data.nlevels
  NCDF_VARPUT, cdfid, pw1_varid,      data.pw1
  NCDF_VARPUT, cdfid, pw2_varid,      data.pw2
  NCDF_VARPUT, cdfid, pw3_varid,      data.pw3
  NCDF_VARPUT, cdfid, psfc_varid,     REFORM( data.pressure[0,*], ntime )
  NCDF_VARPUT, cdfid, zsfc_varid,     REFORM( data.geopot_hgt[0,*], ntime )
  NCDF_VARPUT, cdfid, tsfc_varid,     REFORM( data.temperature[0,*], ntime )
  NCDF_VARPUT, cdfid, tdsfc_varid,    REFORM( data.dewpoint[0,*], ntime )
  NCDF_VARPUT, cdfid, wdsfc_varid,    REFORM( data.wind_direction[0,*], ntime )
  NCDF_VARPUT, cdfid, wssfc_varid,    REFORM( data.wind_speed[0,*], ntime )
  NCDF_VARPUT, cdfid, shum_sfc_varid, REFORM( data.spechum[0,*], ntime )
  NCDF_VARPUT, cdfid, rhum_sfc_varid, REFORM( data.relhum[0,*], ntime )
  NCDF_VARPUT, cdfid, psfc_varid,     data.pressure[1:6,*]
  NCDF_VARPUT, cdfid, z_varid,        data.geopot_hgt[1:6,*]
  NCDF_VARPUT, cdfid, ta_varid,       data.temperature[1:6,*]
  NCDF_VARPUT, cdfid, td_varid,       data.dewpoint[1:6,*]
  NCDF_VARPUT, cdfid, wd_varid,       data.wind_direction[1:6,*]
  NCDF_VARPUT, cdfid, ws_varid,       data.wind_speed[1:6,*]
  NCDF_VARPUT, cdfid, shum_varid,     data.spechum[1:6,*]
  NCDF_VARPUT, cdfid, rhum_varid,     data.relhum[1:6,*]
  
  NCDF_CLOSE, cdfid

  time_elapsed = SYSTIME() - start_time
  PRINT, '% WRITE_PROFILES_TO_NETCDF: Time Elapsed: '+STRTRIM(time_elapsed,2)

  RETURN

END
