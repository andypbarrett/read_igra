;;----------------------------------------------------------------------
;; Get profile climatologies for Arctic stations
;;
;; 2010-11-22 A.P.Barrett
;;----------------------------------------------------------------------

FUNCTION READ_OORT_CLIMATOLOGY, FILI

  tair = NCDF_READV( fili, VARNAME='tair' )
  y    = NCDF_READV( fili, VARNAME='Y' )
  x    = NCDF_READV( fili, VARNAME='X' )
  p    = NCDF_READV( fili, VARNAME='P' )
  t    = NCDF_READV( fili, VARNAME='T' )

  result = {lat: y, $
            lon: x, $
            lev: p, $
            time: t, $
            tair: tair}

  RETURN, result
  
END  ;; end of GET_OORT_TAIR_CLIMO

FUNCTION EXTRACT_PROFILE_CLIMO, DATA, LAT, LON, LEVEL=LEVEL

  oort_level = [ 1000.0, 950.0, 900.0, 850.0, 700.0, $
                 500.0, 400.0, 300.0, 200.0, 100.0, 50.0 ]
  level = ( N_ELEMENTS( level ) EQ 0 ) ? oort_level : level
 
  ;; Find index of nearest cell by latitude 
  yid = VALUE_LOCATE( data.lat, lat )
  xid = VALUE_LOCATE( data.lon, lon )

  nlev = N_ELEMENTS( level )

  result = MAKE_ARRAY( [ nlev, 12 ], /FLOAT, VALUE=-9999.99 )
  levid  = MAKE_ARRAY( nlev, /INTEGER, VALUE=-1 )

  ;; get indices of pressure levels
  FOR ilev=0, ( nlev - 1 ) DO BEGIN
     levid[ ilev ] = WHERE( data.lev EQ level[ ilev ] )
  ENDFOR

  idx = WHERE( levid GT -1, nfound )
  IF ( nfound GT 0 ) THEN result[ idx, * ] = data.tair[ xid, yid, levid[ idx ], * ]

  IF ( nfound NE nlev ) THEN BEGIN
     PRINT, '% GET_OORT_TAIR_CLIMO: Warning - could not find all requested levels'
  ENDIF

  RETURN, result

END

FUNCTION GET_IGRA_DATA, station_number, INDIR=INDIR, $
                        SUFFIX=SUFFIX, STATUS=STATUS, $
                        NOCLEANUP=NOCLEANUP, VERBOSE=VERBOSE

  status = 0

  indir = N_ELEMENTS( indir ) EQ 0 ? '.' : indir
  suffix = N_ELEMENTS( suffix ) EQ 0 ? 'dat' : suffix

  fili = STRTRIM( station_number, 2 ) + '.' + suffix
  
  IF ( indir NE '.' ) THEN BEGIN
     CD, indir, CURRENT=old_dir
  ENDIF

  isfile = FILE_SEARCH( fili + '.gz', COUNT=nfile )
  IF ( nfile EQ 0 ) THEN BEGIN
     IF ( indir NE '.' ) THEN CD, old_dir
     result = -1
     status = -1
     RETURN, result
  ENDIF

  IF ( KEYWORD_SET( VERBOSE ) ) THEN BEGIN
     PRINT, '% GET_IGRA_DATA: Unzipping ' + fili + '.gz'
  ENDIF
  SPAWN, 'gunzip ' + fili + '.gz'

  IF ( KEYWORD_SET( VERBOSE ) ) THEN BEGIN
     PRINT, '% GET_IGRA_DATA: Reading data...'
  ENDIF
  READ_IGRA_DATA_NEW, fili, result, NOCLEANUP=NOCLEANUP

  IF ( KEYWORD_SET( VERBOSE ) ) THEN BEGIN
     PRINT, '% GET_IGRA_DATA: Zipping ' + fili
  ENDIF
  SPAWN, 'gzip ' + fili

  IF ( indir NE '.' ) THEN BEGIN
     CD, old_dir
  ENDIF

  RETURN, result

END

PRO WRITE_TO_NETCDF, FILEOUT, STATION_NUMBER, LATITUDE, LONGITUDE, $
                     LEVEL, TAVG, TSTD, NEXCLUDE

  ;; Check everthing is defined
  IF ( N_ELEMENTS( FILEOUT ) EQ 0 ) THEN BEGIN
     PRINT, '% WRITE_TO_NETCDF: Argument FILEOUT is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( STATION_NUMBER ) EQ 0 ) THEN BEGIN
     PRINT, '% WRITE_TO_NETCDF: Argument STATION_NUMBER is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( LATITUDE ) EQ 0 ) THEN BEGIN
     PRINT, '% WRITE_TO_NETCDF: Argument LATITUDE is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( LONGITUDE ) EQ 0 ) THEN BEGIN
     PRINT, '% WRITE_TO_NETCDF: Argument LONGITUDE is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( LEVEL ) EQ 0 ) THEN BEGIN
     PRINT, '% WRITE_TO_NETCDF: Argument FILEOUT is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( TAVG ) EQ 0 ) THEN BEGIN
     PRINT, '% WRITE_TO_NETCDF: Argument TAVG is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( TSTD ) EQ 0 ) THEN BEGIN
     PRINT, '% WRITE_TO_NETCDF: Argument TSTD is not defined'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( NEXCLUDE ) EQ 0 ) THEN BEGIN
     PRINT, '% WRITE_TO_NETCDF: Argument NEXCLUDE is not defined'
     RETURN
  ENDIF

  ;; Get dimension sizes
  nstat = N_ELEMENTS( station_number )
  nlev  = N_ELEMENTS( level )
  nmonth = 12

  ;; Station id is used because string coords are not very useful
  station_id = INDGEN( nstat )
  month      = INDGEN(nmonth) + 1

  ;; Define netCDF file
  cdfid = NCDF_CREATE( fileout, /clobber )
  stat_dimid = NCDF_DIMDEF( cdfid, 'station_id', nstat )
  lev_dimid  = NCDF_DIMDEF( cdfid, 'level', nlev )
  mon_dimid  = NCDF_DIMDEF( cdfid, 'month', nmonth )

  stat_varid = NCDF_VARDEF( cdfid, 'station_id', [stat_dimid], /FLOAT )
  lev_varid  = NCDF_VARDEF( cdfid, 'level', [lev_dimid], /FLOAT )
  mon_varid  = NCDF_VARDEF( cdfid, 'month', [mon_dimid], /FLOAT )
  tavg_varid = NCDF_VARDEF( cdfid, 'TAVG', $
                            [lev_dimid, mon_dimid, stat_dimid], /FLOAT )
  tstd_varid = NCDF_VARDEF( cdfid, 'TSTD', $
                            [lev_dimid, mon_dimid, stat_dimid], /FLOAT )
  nexc_varid = NCDF_VARDEF( cdfid, 'NEXCLUDE', $
                            [lev_dimid, mon_dimid, stat_dimid], /FLOAT )
  lat_varid  = NCDF_VARDEF( cdfid, 'lat', [stat_dimid], /FLOAT )
  lon_varid  = NCDF_VARDEF( cdfid, 'lon', [stat_dimid], /FLOAT )
  ;statn_varid = NCDF_VARDEF( cdfid, 'STATION_NUMBER', [stat_dimid], /STRING )
  
  ;; Write variable attributes
  NCDF_ATTPUT, cdfid, stat_varid, 'long_name', 'Numeric station id'
  NCDF_ATTPUT, cdfid, stat_varid, 'units', 'none'
  NCDF_ATTPUT, cdfid, lev_varid, 'long_name', 'Pressure level'
  NCDF_ATTPUT, cdfid, lev_varid, 'units', 'hPa'
  NCDF_ATTPUT, cdfid, mon_varid, 'long_name', 'month'
  NCDF_ATTPUT, cdfid, mon_varid, 'units', 'none'
  NCDF_ATTPUT, cdfid, tavg_varid, 'long_name', 'Monthly Average Temperature'
  NCDF_ATTPUT, cdfid, tavg_varid, 'units', 'C'
  NCDF_ATTPUT, cdfid, tavg_varid, '_FillValue', -9999.99
  NCDF_ATTPUT, cdfid, tstd_varid, 'long_name', 'Monthly Standard Deviation of Temperature'
  NCDF_ATTPUT, cdfid, tstd_varid, 'units', 'C'
  NCDF_ATTPUT, cdfid, tstd_varid, '_FillValue', -9999.99
  NCDF_ATTPUT, cdfid, nexc_varid, 'long_name', 'Number data points excluded by QA'
  NCDF_ATTPUT, cdfid, nexc_varid, 'units', 'none'
  NCDF_ATTPUT, cdfid, nexc_varid, '_FillValue', -9999.99
  ;NCDF_ATTPUT, cdfid, statn_varid, 'long_name', 'IGRA station number'
  ;NCDF_ATTPUT, cdfid, statn_varid, 'units', 'none'
  NCDF_ATTPUT, cdfid, lat_varid, 'long_name', 'latitude'
  NCDF_ATTPUT, cdfid, lat_varid, 'units', 'degrees north'
  NCDF_ATTPUT, cdfid, lon_varid, 'long_name', 'longitude'
  NCDF_ATTPUT, cdfid, lon_varid, 'units', 'degrees east'

  ;; Write global attributes
  NCDF_ATTPUT, cdfid, 'description', 'Station climatologies for IGRA profiles', /GLOBAL
  NCDF_ATTPUT, cdfid, 'created_by', 'Andrew P. Barrett <apbarret@nsidc.org', /GLOBAL
  NCDF_ATTPUT, cdfid, 'creation_date', systime(), /GLOBAL

  NCDF_CONTROL, cdfid, /ENDEF

  ;; Write variables
  NCDF_VARPUT, cdfid, stat_varid, station_id
  NCDF_VARPUT, cdfid, lev_varid, level
  NCDF_VARPUT, cdfid, mon_varid, month
  NCDF_VARPUT, cdfid, tavg_varid, tavg
  NCDF_VARPUT, cdfid, tstd_varid, tstd
  NCDF_VARPUT, cdfid, nexc_varid, nexclude
  ;NCDF_VARPUT, cdfid, statn_varid, station_number
  NCDF_VARPUT, cdfid, lat_varid, latitude
  NCDF_VARPUT, cdfid, lon_varid, longitude

  NCDF_CLOSE, cdfid

END

FUNCTION READ_IGRA_CLIMATOLOGY, FILI, NREC=NREC

  station_id = NCDF_READV( fili, VARNAME='station_id' )
  level      = NCDF_READV( fili, VARNAME='level' )
  month      = NCDF_READV( fili, VARNAME='month' )
  tavg       = NCDF_READV( fili, VARNAME='TAVG' )
  tstd       = NCDF_READV( fili, VARNAME='TSTD' )
  nexclude   = NCDF_READV( fili, VARNAME='NEXCLUDE' )
  lat        = NCDF_READV( fili, VARNAME='lat' )
  lon        = NCDF_READV( fili, VARNAME='lon' )

  nrec = N_ELEMENTS( station_id )

  result = {station_id: station_id, $
            level:      level, $
            month:      month, $
            tavg:       tavg, $
            tstd:       tstd, $
            nexclude:   nexclude, $
            lat:        lat, $
            lon:        lon}

  RETURN, result

END

PRO PROCESS_PROFILE_CLIMATOLOGY, FILEOUT=FILEOUT, DEBUG=DEBUG

  fileout = N_ELEMENTS( fileout ) EQ 0 ? 'profile_climatology.nc' : fileout
 
  ;; Define files
  ;; - Oort gridded temperature profile
  oort_file = 'oort1983_air_temperature.nc'
  ;; - Arctic station file
  station_list_file = 'igra_stations_arctic.txt'
  ;; location of data files
  indir = 'data'

  ;; Define levels
  level = [1000.0, 850.0, 700.0, 500.0, $
           400.0, 300.0, 200.0, 100.0, 50.0 ]
  nlevel = N_ELEMENTS( level )
  nmonth = 12

  ;; Get Arctic stations
  read_igra_station_list, station_list_file, station_list, NREC=nstation
;;  nstation = 2

  ;; Import Oort climatology
  oort_tair = READ_OORT_CLIMATOLOGY( oort_file )

  ;; Define result arrays
  tavg = MAKE_ARRAY( nlevel, nmonth, nstation, /FLOAT, VALUE=-9999.99 )
  tstd = MAKE_ARRAY( nlevel, nmonth, nstation, /FLOAT, VALUE=-9999.99 )
  nexclude = MAKE_ARRAY( nlevel, nmonth, nstation, /FLOAT, VALUE=-9999.99 )


  ;; Process station by station 
  FOR istat = 0, ( nstation - 1 ) DO BEGIN

     PRINT, '% PROCESS_PROFILE_CLIMATOLOGY: Calculating climatology for ' + $
            STRTRIM( station_list[istat].station_name, 2 ) + '-' + $
            STRTRIM( station_list[istat].station_number, 2 ) + ' - ' + $
            STRTRIM( istat+1, 2 ) + ' of ' + STRTRIM( nstation, 2 ) + $
            ' stations'

     ;; Get oort profile or nearest grid-cell to station
     PRINT, '  - Extract climatological profile for nearest grid cell in Oort...'
     oort_profile = EXTRACT_PROFILE_CLIMO( oort_tair, station_list[istat].latitude, $
                                           station_list[istat].longitude, $
                                           LEVEL=level )

     ;; Get profile data for station
     PRINT, '  - Reading profile data for station...'
     profile = GET_IGRA_DATA( station_list[ istat ].station_number, INDIR=indir, /NOCLEANUP )
     
     ;; Calculate climatology
     PRINT, '  - Processing profile and calculating station temperature profile climatology...'
     CALC_PROFILE_CLIMATOLOGY_QA, PROFILE, OORT_PROFILE, TAVG_, TSTD_, $
                                  NEXCLUDE_, LEVEL=level, $
                                  STATION=station_list[ istat ].station_name, $
                                  /DOPLOT, DEBUG=DEBUG

     ;PRINT, '  - Processing profile and calculating station surface temperature climatology...'
     ;CALC_SURFACE_CLIMATOLOGY_QA, PROFILE, TSURF_AVG_, TSURF_STD_, TSURF_NEXCLUDE_, $
     ;                             STATION=station_list[ istat ].station_name, $
     ;                             /DOPLOT, DEBUG=DEBUG

     ;PRINT, '  - Performing QC...'
     ;;QC_IGRA_PROFILE, Profile, Station_List, tavg, tstd, tsurf_avg,
     ;;                 tsurf_std, qc_statistics, /DEBUG

     ;PRINT, '  - Writing QC'd profile to file
     ;;WRITE_IGRA_DATA, Profile, qc_fileout

     ;PRINT, '  - Calculating vapour pressures...'
     ;;

     ;PRINT, '  - Calculating precipitable water...'


     ;PRINT, '  - Calculating monthly mean vapour pressures at mandatory levels...'


     ;PRINT, '  - Calculating monthly mean of precipitable water...'


     ;PRINT, '  - Writing vapour pressures and precipitable waters to ' + results_out 


     tavg[ *, *, istat ] = tavg_
     tstd[ *, *, istat ] = tstd_
     nexclude[ *, *, istat ] = nexclude_


  ENDFOR

  PRINT, '  - Writing climatologies to netCDF file: ' + fileout
  WRITE_TO_NETCDF, fileout, station_list.station_number, station_list.latitude, $
                      station_list.longitude, level, tavg, tstd, nexclude

END
