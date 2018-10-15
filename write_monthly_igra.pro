;;----------------------------------------------------------------------
;; Writes the monthly mean values of IGRA surface and sea level pressure,
;; surface temperature and dewpoint, and temperature and dewpoint at mandatory
;; levels to a NetCDF file.
;;
;; 2010-12-02 A.P.Barrett
;;----------------------------------------------------------------------
PRO WRITE_MONTHLY_IGRA, DATA, STATION, FILEOUT

  IF ( N_PARAMS() NE 3 ) THEN BEGIN
     PRINT, 'USAGE: WRITE_MONTHLY_IGRA, DATA, PROFILE, FILEOUT'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( DATA ) EQ 0 ) THEN BEGIN
     PRINT, '% WRITE_MONTHLY_IGRA: Argument DATA is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( STATION ) EQ 0 ) THEN BEGIN
     PRINT, '% WRITE_MONTHLY_IGRA: Argument STATION is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( FILEOUT ) EQ 0 ) THEN BEGIN
     PRINT, '% WRITE_MONTHLY_IGRA: Argument FILEOUT is not defined'
     RETURN
  ENDIF

  ;; Get dimension sizes
  ntim = N_ELEMENTS( data.time )
  nobs = N_ELEMENTS( data.obs_hour )
  nlev = N_ELEMENTS( data.level )

  ;; Define netCDF file
  cdfid = NCDF_CREATE( fileout, /clobber )
  tim_dimid = NCDF_DIMDEF( cdfid, 'time', ntim )
  obs_dimid = NCDF_DIMDEF( cdfid, 'obs_hour', nobs )
  lev_dimid = NCDF_DIMDEF( cdfid, 'level', nlev )

  tim_varid = NCDF_VARDEF( cdfid, 'time', [tim_dimid], /FLOAT )
  obs_varid = NCDF_VARDEF( cdfid, 'obs_hour', [obs_dimid], /FLOAT )
  lev_varid = NCDF_VARDEF( cdfid, 'level', [lev_dimid], /FLOAT )

  ps_varid = NCDF_VARDEF( cdfid, 'ps', $
                          [ tim_dimid, obs_dimid ], /FLOAT )
  psl_varid = NCDF_VARDEF( cdfid, 'psl', $
                          [ tim_dimid, obs_dimid ], /FLOAT )
  ts_varid = NCDF_VARDEF( cdfid, 'ts', $
                          [ tim_dimid, obs_dimid ], /FLOAT )
  tdews_varid = NCDF_VARDEF( cdfid, 'dpts', $
                          [ tim_dimid, obs_dimid ], /FLOAT )
  ta_varid = NCDF_VARDEF( cdfid, 'ta', $
                          [ tim_dimid, obs_dimid, lev_dimid ], /FLOAT )
  tdew_varid = NCDF_VARDEF( cdfid, 'dpt', $
                          [ tim_dimid, obs_dimid, lev_dimid ], /FLOAT )

  ;; Write variable attributes
  NCDF_ATTPUT, cdfid, tim_varid, 'long_name', 'time'
  NCDF_ATTPUT, cdfid, tim_varid, 'units', 'days since 1-1-1900 00:00'
  NCDF_ATTPUT, cdfid, lev_varid, 'long_name', 'Pressure Level'
  NCDF_ATTPUT, cdfid, lev_varid, 'units', 'Pa'
  NCDF_ATTPUT, cdfid, obs_varid, 'long_name', 'Observation Hour'
  NCDF_ATTPUT, cdfid, obs_varid, 'units', 'hour'

  NCDF_ATTPUT, cdfid, ps_varid,  'long_name', 'Pressure at the surface '
  NCDF_ATTPUT, cdfid, ps_varid,  'units', 'Pa'
  NCDF_ATTPUT, cdfid, ps_varid,  'standard_name', 'surface_air_pressure'
  NCDF_ATTPUT, cdfid, ps_varid, '_FillValue', -9999.99

  NCDF_ATTPUT, cdfid, psl_varid, 'long_name', 'Sea level pressure'
  NCDF_ATTPUT, cdfid, psl_varid, 'units', 'Pa'
  NCDF_ATTPUT, cdfid, psl_varid, 'standard_name', 'air_pressure_at_sea_level'
  NCDF_ATTPUT, cdfid, psl_varid, '_FillValue', -9999.99

  NCDF_ATTPUT, cdfid, ts_varid,  'long_name', 'Surface Temperature'
  NCDF_ATTPUT, cdfid, ts_varid,  'units', 'K'
  NCDF_ATTPUT, cdfid, ts_varid,  '_FillValue', -9999.99
  NCDF_ATTPUT, cdfid, ts_varid,  'standard_name', 'surface_temperature'

  NCDF_ATTPUT, cdfid, tdews_varid, 'long_name', 'Surface Dew Point Temperature'
  NCDF_ATTPUT, cdfid, tdews_varid, 'units', 'K'
  NCDF_ATTPUT, cdfid, tdews_varid, '_FillValue', -9999.99
  NCDF_ATTPUT, cdfid, tdews_varid, 'standard_name', 'surface_dew_point_temperature'

  NCDF_ATTPUT, cdfid, ta_varid, 'long_name', 'Air Temperature'
  NCDF_ATTPUT, cdfid, ta_varid, 'units', 'K'
  NCDF_ATTPUT, cdfid, ta_varid, '_FillValue', -9999.99
  NCDF_ATTPUT, cdfid, ta_varid, 'standard_name', 'air_temperature'

  NCDF_ATTPUT, cdfid, tdew_varid, 'long_name', 'Dew Point Temperature'
  NCDF_ATTPUT, cdfid, tdew_varid, 'units', 'K'
  NCDF_ATTPUT, cdfid, tdew_varid, '_FillValue', -9999.99
  NCDF_ATTPUT, cdfid, tdew_varid, 'standard_name', 'dew_point_temperature'

  
  ;; Write global attributes
  NCDF_ATTPUT, cdfid, 'description', 'Monthly means for IGRA profiles for station ' + station.station_number, /GLOBAL
  NCDF_ATTPUT, cdfid, 'station_name', station.station_name, /GLOBAL
  NCDF_ATTPUT, cdfid, 'station_number', station.station_number, /GLOBAL
  NCDF_ATTPUT, cdfid, 'station_latitude', station.latitude, /GLOBAL
  NCDF_ATTPUT, cdfid, 'station_longitude', station.longitude, /GLOBAL
  NCDF_ATTPUT, cdfid, 'station_elevation', station.elevation, /GLOBAL
  NCDF_ATTPUT, cdfid, 'created_by', 'Andrew P. Barrett <apbarret@nsidc.org', /GLOBAL
  NCDF_ATTPUT, cdfid, 'creation_date', systime(), /GLOBAL

  NCDF_CONTROL, cdfid, /ENDEF

  ;; Write variables
  NCDF_VARPUT, cdfid, tim_varid,  data.time - JULDAY( 1, 1, 1900, 0 )
  NCDF_VARPUT, cdfid, obs_varid,  data.obs_hour
  NCDF_VARPUT, cdfid, lev_varid,  data.level
  NCDF_VARPUT, cdfid, ps_varid,   data.psurf * 100.
  NCDF_VARPUT, cdfid, psl_varid,  data.psl * 100.
  NCDF_VARPUT, cdfid, ts_varid,   data.tair_surf + 273.15
  NCDF_VARPUT, cdfid, tdews_varid, data.tdew_surf + 273.15
  NCDF_VARPUT, cdfid, ta_varid,   data.tair + 273.15
  NCDF_VARPUT, cdfid, tdew_varid,  data.tdew + 273.15

  NCDF_CLOSE, cdfid

  RETURN

END
