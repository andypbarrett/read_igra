;;----------------------------------------------------------------------
;; Writes the monthly mean values of IGRA surface and sea level pressure,
;; surface temperature and dewpoint, and temperature and dewpoint at mandatory
;; levels to a NetCDF file.
;;
;; 2010-12-02 A.P.Barrett
;;----------------------------------------------------------------------
PRO WRITE_MONTHLY_IGRA_DERIVED, DATA, STATION, FILEOUT

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

  surface = data.surface
  mandatory = data.mandatory

  ;; Convert variables to CF-units; e.g. Pa, K
  pos = WHERE( surface.temp GT -9999.99, count )
  IF ( count GT 0 ) THEN surface.temp[ pos ] = surface.temp[ pos ] + 273.15
  pos = WHERE( mandatory.temp GT -9999.99, count )
  IF ( count GT 0 ) THEN mandatory.temp[ pos ] = mandatory.temp[ pos ] + 273.15
  pos = WHERE( surface.vappress GT -9999.99, count )
  IF ( count GT 0 ) THEN surface.vappress[ pos ] = surface.vappress[ pos ] * 100.
  pos = WHERE( mandatory.vappress GT -9999.99, count )
  IF ( count GT 0 ) THEN mandatory.vappress[ pos ] = mandatory.vappress[ pos ] * 100.

  ;; Get dimension sizes
  ntim = N_ELEMENTS( mandatory.time )
  nobs = N_ELEMENTS( mandatory.obs_hour )
  nlev = N_ELEMENTS( mandatory.level )

  ;; Define netCDF file
  cdfid = NCDF_CREATE( fileout, /clobber )
  tim_dimid = NCDF_DIMDEF( cdfid, 'time', ntim )
  obs_dimid = NCDF_DIMDEF( cdfid, 'obs_hour', nobs )
  lev_dimid = NCDF_DIMDEF( cdfid, 'level', nlev )

  tim_varid = NCDF_VARDEF( cdfid, 'time', [tim_dimid], /FLOAT )
  obs_varid = NCDF_VARDEF( cdfid, 'obs_hour', [obs_dimid], /FLOAT )
  lev_varid = NCDF_VARDEF( cdfid, 'level', [lev_dimid], /FLOAT )

  gphs_varid = NCDF_VARDEF( cdfid, 'orog', $
                          [ tim_dimid, obs_dimid ], /FLOAT )
  ts_varid = NCDF_VARDEF( cdfid, 'ts', $
                          [ tim_dimid, obs_dimid ], /FLOAT )
  vaps_varid = NCDF_VARDEF( cdfid, 'vaps', $
                          [ tim_dimid, obs_dimid ], /FLOAT )
  rhs_varid = NCDF_VARDEF( cdfid, 'hurs', $
                          [ tim_dimid, obs_dimid ], /FLOAT )
  zg_varid = NCDF_VARDEF( cdfid, 'zg', $
                          [ tim_dimid, obs_dimid, lev_dimid ], /FLOAT )
  ta_varid = NCDF_VARDEF( cdfid, 'ta', $
                          [ tim_dimid, obs_dimid, lev_dimid ], /FLOAT )
  vap_varid = NCDF_VARDEF( cdfid, 'vap', $
                          [ tim_dimid, obs_dimid, lev_dimid ], /FLOAT )
  rh_varid = NCDF_VARDEF( cdfid, 'rh', $
                          [ tim_dimid, obs_dimid, lev_dimid ], /FLOAT )

  ;; Write variable attributes
  NCDF_ATTPUT, cdfid, tim_varid, 'long_name', 'time'
  NCDF_ATTPUT, cdfid, tim_varid, 'units', 'days since 1-1-1900 00:00'
  NCDF_ATTPUT, cdfid, lev_varid, 'long_name', 'Pressure Level'
  NCDF_ATTPUT, cdfid, lev_varid, 'units', 'Pa'
  NCDF_ATTPUT, cdfid, obs_varid, 'long_name', 'Observation Hour'
  NCDF_ATTPUT, cdfid, obs_varid, 'units', 'hour'

  NCDF_ATTPUT, cdfid, gphs_varid,  'long_name', 'geopotential height at the surface '
  NCDF_ATTPUT, cdfid, gphs_varid,  'units', 'm'
  NCDF_ATTPUT, cdfid, gphs_varid,  'standard_name', 'surface_geopotential_height'
  NCDF_ATTPUT, cdfid, gphs_varid, '_FillValue', -9999.99

  NCDF_ATTPUT, cdfid, ts_varid,  'long_name', 'Surface Temperature'
  NCDF_ATTPUT, cdfid, ts_varid,  'units', 'K'
  NCDF_ATTPUT, cdfid, ts_varid,  '_FillValue', -9999.99
  NCDF_ATTPUT, cdfid, ts_varid,  'standard_name', 'surface_temperature'

  NCDF_ATTPUT, cdfid, vaps_varid, 'long_name', 'Surface vapour pressure'
  NCDF_ATTPUT, cdfid, vaps_varid, 'units', 'Pa'
  NCDF_ATTPUT, cdfid, vaps_varid, 'standard_name', 'surface_vapour_pressure'
  NCDF_ATTPUT, cdfid, vaps_varid, '_FillValue', -9999.99

  NCDF_ATTPUT, cdfid, rhs_varid, 'long_name', 'Surface relative humidity'
  NCDF_ATTPUT, cdfid, rhs_varid, 'units', '1'
  NCDF_ATTPUT, cdfid, rhs_varid, '_FillValue', -9999.99
  NCDF_ATTPUT, cdfid, rhs_varid, 'standard_name', 'surface_relative_humidity'

  NCDF_ATTPUT, cdfid, ta_varid, 'long_name', 'Air Temperature'
  NCDF_ATTPUT, cdfid, ta_varid, 'units', 'K'
  NCDF_ATTPUT, cdfid, ta_varid, '_FillValue', -9999.99
  NCDF_ATTPUT, cdfid, ta_varid, 'standard_name', 'air_temperature'

  NCDF_ATTPUT, cdfid, zg_varid, 'long_name', 'Geopotential height'
  NCDF_ATTPUT, cdfid, zg_varid, 'units', 'm'
  NCDF_ATTPUT, cdfid, zg_varid, '_FillValue', -9999.99
  NCDF_ATTPUT, cdfid, zg_varid, 'standard_name', 'geopotential_height'

  NCDF_ATTPUT, cdfid, vap_varid, 'long_name', 'Water vapour pressure'
  NCDF_ATTPUT, cdfid, vap_varid, 'units', 'Pa'
  NCDF_ATTPUT, cdfid, vap_varid, '_FillValue', -9999.99
  NCDF_ATTPUT, cdfid, vap_varid, 'standard_name', 'water_vapour_pressure'

  NCDF_ATTPUT, cdfid, rh_varid, 'long_name', 'relative humidity'
  NCDF_ATTPUT, cdfid, rh_varid, 'units', '1'
  NCDF_ATTPUT, cdfid, rh_varid, '_FillValue', -9999.99
  NCDF_ATTPUT, cdfid, rh_varid, 'standard_name', 'relative_humidity'

  
  ;; Write global attributes
  NCDF_ATTPUT, cdfid, 'description', 'Monthly means for IGRA derived variables for station ' + station.station_number, /GLOBAL
  NCDF_ATTPUT, cdfid, 'station_name', station.station_name, /GLOBAL
  NCDF_ATTPUT, cdfid, 'station_number', station.station_number, /GLOBAL
  NCDF_ATTPUT, cdfid, 'station_latitude', station.latitude, /GLOBAL
  NCDF_ATTPUT, cdfid, 'station_longitude', station.longitude, /GLOBAL
  NCDF_ATTPUT, cdfid, 'station_elevation', station.elevation, /GLOBAL
  NCDF_ATTPUT, cdfid, 'created_by', 'Andrew P. Barrett <apbarret@nsidc.org', /GLOBAL
  NCDF_ATTPUT, cdfid, 'creation_date', systime(), /GLOBAL

  NCDF_CONTROL, cdfid, /ENDEF

  ;; Write variables
  NCDF_VARPUT, cdfid, tim_varid,  mandatory.time - JULDAY( 1, 1, 1900, 0 )
  NCDF_VARPUT, cdfid, obs_varid,  mandatory.obs_hour
  NCDF_VARPUT, cdfid, lev_varid,  mandatory.level
  NCDF_VARPUT, cdfid, gphs_varid, surface.obsgph
  NCDF_VARPUT, cdfid, ts_varid,   surface.temp
  NCDF_VARPUT, cdfid, vaps_varid, surface.vappress
  NCDF_VARPUT, cdfid, rhs_varid,  surface.rh

  NCDF_VARPUT, cdfid, zg_varid,   mandatory.obsgph
  NCDF_VARPUT, cdfid, ta_varid,   mandatory.temp
  NCDF_VARPUT, cdfid, vap_varid,  mandatory.vappress
  NCDF_VARPUT, cdfid, rh_varid,   mandatory.rh

  NCDF_CLOSE, cdfid

  RETURN

END
