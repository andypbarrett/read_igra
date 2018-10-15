;;----------------------------------------------------------------------
;; Calculates precipitable water for each profile for the surface to 850 
;; hPa and surface to 300 hPa.  The procedure and calculations follow that of
;; Ross and Elliot.  The routine conducts the following QC procedures before
;; calculating precipitable water.
;;
;;   1) vapour pressures are rejected if Tair < -40C
;;   2) a sounding is rejected if there is no valid surface pressure,
;;      or temperature and vapour pressure at the surface, 850 hPa (if this is
;;      above the surface) and 700 hPa levels.
;;   3) a sounding is truncated at the last mandatory level with temperature
;;      and vapour pressure.
;;   4) significant levels are set to missing if there are missing pressures, 
;;      temperatures or vapour pressures.
;;
;; 2010-12-08 A.P.Barrett
;;----------------------------------------------------------------------

FUNCTION GET_MEAN_1D, VALUE, MIN_NUM_VALUE

  result = -9999.99

  pos = WHERE( value GT -9999., count )
  IF ( count GT min_num_value ) THEN result = MEAN( value[ pos ] )

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

;; Find profiles with no valid vapour pressure
FUNCTION VALID_VAPPRESS, PROFILE, COUNT

  isvap = profile.vappress GT -9999.
  numvap = TOTAL( isvap, 1 )

  result = WHERE( numvap GT 0, count )

  RETURN, result

END

;; Checks that temperatures are greater than -40C (or the set threshold).
;; This is done because humidity measurements are difficult to make at the low
;; temperatures
PRO CHECK_TEMPERATURE, PROFILE, THRESHOLD=THRESHOLD, COUNT=COUNT, STATUS=STATUS, $
                       VERBOSE=VERBOSE

  threshold = N_ELEMENTS( threshold ) EQ 0 ? 233.15 : threshold

  status = 0

  pos = WHERE( profile.temp GT -9999. AND $
               profile.temp LT threshold, count )
  IF ( count GT 0 ) THEN BEGIN
     profile.vappress[ pos ] = -9999.99
     status = 1
  ENDIF

  IF ( KEYWORD_SET( VERBOSE ) EQ 1 ) THEN BEGIN
     PRINT, FORMAT='("% CHECK_TEMPERATURE: VAPPRESS set to missing: ",i8)', count
  ENDIF

  RETURN

END

;; Sets all profile values to missing (-9999.99) if no surface, 850 hPa or
;; 700 hPa levels with pressure, temperature or vapour pressure
FUNCTION CHECK_VALID_LOWER_LEVELS, PROFILE

  status = 1

  ;; Assumes first level is surface
  IF ( profile.press[0] LT -9999. OR profile.temp[0] LT -9999. OR $
       profile.vappress[0] LT -9999. ) THEN status = 0

  ;; Check 850 hPa and 700 hPa levels
  pos850 = WHERE( profile.press EQ 850., is850 )
  IF ( is850 EQ 0 ) THEN BEGIN 
     status = 0
  ENDIF ELSE BEGIN
     IF ( profile.temp[ pos850 ] LT -9999. ) THEN status = 0
     IF ( profile.vappress[ pos850 ] LT -9999. ) THEN status = 0
  ENDELSE

  pos700 = WHERE( profile.press EQ 700., is700 )
  IF ( is700 EQ 0 ) THEN BEGIN
     status = 0
  ENDIF ELSE BEGIN
     IF ( profile.temp[ pos700 ] LT -9999. ) THEN status = 0
     IF ( profile.vappress[ pos700 ] LT -9999. ) THEN status = 0
  ENDELSE
     
  RETURN, status

END

FUNCTION REMOVE_INVALID_LOWER_LEVELS, PROFILE, VERBOSE=VERBOSE, COUNT=COUNT

  num_profile = N_ELEMENTS( profile )

  isvalid = MAKE_ARRAY( num_profile, /LONG, VALUE=1 )

  FOR ip=0, ( num_profile - 1 ) DO BEGIN
     isvalid[ ip ] = CHECK_VALID_LOWER_LEVELS( profile[ ip ] )
  ENDFOR

  pos = WHERE( isvalid EQ 1, count )
  IF ( count GT 0 ) THEN BEGIN
     new_profile = profile[ pos ]
  ENDIF ELSE BEGIN
     PRINT, '% REMOVE_INVALID_LOWER_LEVELS: WARNING! No valid profile retained'
     STOP
  ENDELSE

  IF ( KEYWORD_SET( VERBOSE ) EQ 1 ) THEN BEGIN
     PRINT, '% REMOVE_PROFILES_NOVAP: Profiles retained: ' + STRTRIM( count, 2 )
     PRINT, '% REMOVE_PROFILES_NOVAP: Profiles removed:  ' + STRTRIM( num_profile - count, 2 )
  ENDIF

  RETURN, new_profile

END

FUNCTION REMOVE_PROFILES_WITH_NOVAP, PROFILE, VERBOSE=VERBOSE, COUNT=COUNT

  num_profile = N_ELEMENTS( profile )

  isvalid = VALID_VAPPRESS( profile, num_isvalid )
  IF ( num_isvalid GT 0 ) THEN BEGIN
     new_profile = profile[ isvalid ]
  ENDIF ELSE BEGIN
     PRINT, '% REMOVE_PROFILES_WITH_NOVAP: WARNING! No valid profile retained'
     STOP
  ENDELSE
  
  IF ( KEYWORD_SET( VERBOSE ) ) THEN BEGIN
     PRINT, '% REMOVE_PROFILES_NOVAP: Profiles retained: ' + STRTRIM( num_isvalid, 2 )
     PRINT, '% REMOVE_PROFILES_NOVAP: Profiles removed:  ' + STRTRIM( num_profile - num_isvalid, 2 )
  ENDIF

  RETURN, new_profile

END

;; Truncate levels above 700 at lowest mandatory level with data   
PRO TRUNCATE_LEVEL, PROFILE, STATUS

  level = [ 700., 500., 400., 300. ]

  status = 0

  FOR ilev = 1, 3 DO BEGIN

     pos0 = WHERE( profile.press EQ level[ ilev-1 ], islevel0 )
     pos1 = WHERE( profile.press EQ level[ ilev ], islevel1 )

     IF ( islevel1 EQ 1 ) THEN BEGIN
        IF ( profile.temp[ pos1 ] LE -9999. OR $
             profile.vappress[ pos1 ] LE -9999. ) THEN BEGIN
           profile.nlevels = pos0+1
           FOR ifield = 7, 21 DO profile.(ifield)[pos0+1:170] = -9999.99
           STATUS = 1
           RETURN
        ENDIF
     ENDIF

  ENDFOR

  RETURN

END

PRO TRUNCATE_PROFILES, PROFILE, VERBOSE=VERBOSE, COUNT=COUNT

  num_profile = N_ELEMENTS( profile )

  count = 0

  FOR ip = 0, ( num_profile - 1 ) DO BEGIN
     TRUNCATE_LEVEL, profile[ ip ], status
     count = count + status
  ENDFOR

  IF ( KEYWORD_SET( VERBOSE ) EQ 1 ) THEN BEGIN
     PRINT, '% TRUNCATE_PROFILES: Truncations: ' + STRTRIM( count, 2 )
  ENDIF

  RETURN

END

;; Calculate specific humidity from vapour pressure
FUNCTION VP2HUS, vp, press

  a0 = 0.622
  a1 = 0.378

  b0 = a1 * vp
  b1 = press - b0

  q = a0 * vp / b1

  RETURN, q

END

FUNCTION PRCWATER, p, q

  g = 9.8

  nq = N_ELEMENTS( q )
  np = N_ELEMENTS( p )

  IF ( np NE nq ) THEN BEGIN
     PRINT, '% PRCWATER: P and Q must have same number of elements'
     RETURN, -1
  ENDIF

  mq = ( q[0:nq-2] + q[1:nq-1] ) / 2
  dp = p[0:nq-2] - p[1:nq-1]

  pw = TOTAL( mq * dp ) / g

  RETURN, pw

END

FUNCTION PROFILE_PRCWATER, PROFILE, PTOP

  ptop = N_ELEMENTS( ptop ) EQ 0 ? 300. : ptop

  pw = -9999.99

  ;; Get pressure and vapour pressure up to ptop
  pos = WHERE( profile.press GE ptop, count )
  IF ( count EQ 0 ) THEN BEGIN
     PRINT, '% PROFILE_PRCWATER: No valid pressure found'
     RETURN, pw
  ENDIF
  p = profile.press[ pos ]
  vp = profile.vappress[ pos ]

  ;; Get rid of missing values - these should only be at
  ;; significant levels
  pos = WHERE( p GT -9999. AND vp GT -9999., count )
  IF ( count EQ 0 ) THEN BEGIN
     PRINT, '% PROFILE_PRCWATER: No valid vp or q found'
     RETURN, pw
  ENDIF
  ;; extract valid values and convert to pascals
  p = profile.press[ pos ] * 100.
  vp = profile.vappress[ pos ] * 100.

  ;; Calculate specific humidity
  q = VP2HUS( vp, p )

  ;; Calculate precipitable water
  pw = PRCWATER( p, q )

  RETURN, pw

END

FUNCTION CALC_PRCWATER, PROFILE, PTOP

  num_profile = N_ELEMENTS( profile )

  pw = MAKE_ARRAY( num_profile, /FLOAT, VALUE=-9999.99 )

  FOR ip = 0, ( num_profile - 1 ) DO BEGIN
     pw[ ip ] = PROFILE_PRCWATER( profile[ ip ], ptop )
  ENDFOR

  RETURN, pw

END

FUNCTION MONTHLY_MEAN_PRCWATER, pw, profile, YBEG=YBEG, YEND=YEND, DEBUG=DEBUG

  nprof = N_ELEMENTS( profile )

  ybeg = N_ELEMENTS( ybeg ) EQ 0 ? profile[0].year : ybeg
  yend = N_ELEMENTS( yend ) EQ 0 ? profile[nprof-1].year : yend

  nyear = yend - ybeg + 1
  ntime = nyear * 12

  obs_hour = [ 0, 12 ]
  min_num_profile = 10

  time = MAKE_ARRAY( ntime, /FLOAT, VALUE=-1 )
  result = MAKE_ARRAY( ntime, 2, /FLOAT, VALUE=-9999.99 )

  itime = 0
  FOR iyear = ybeg, yend DO BEGIN

     FOR imonth = 1, 12 DO BEGIN

        time[ itime ] = JULDAY( imonth, 1, iyear )

        FOR iobs = 0, 1 DO BEGIN
           
           thisprofile = GET_PROFILE_INDEX( profile, iyear, imonth, obs_hour[ iobs ], NPROFILE=num_profile )
           
           IF ( num_profile GE min_num_profile ) THEN BEGIN
              result[ itime, iobs ] = GET_MEAN_1D( pw[ thisprofile ], min_num_profile )
           ENDIF

           IF ( KEYWORD_SET( DEBUG ) EQ 1 ) THEN BEGIN
              fmt = '(i6,1x,i4,1x,i02,1x,i02,"h",2x,i7,2x,i4,1x,f8.2)'
              PRINT, FORMAT=fmt, $
                     itime, iyear, imonth, obs_hour[iobs], time[itime], num_profile, $
                     result[ itime, iobs ]
           ENDIF

        ENDFOR
           
        itime = itime + 1

     ENDFOR

  ENDFOR

  struct = {time: time, $
            obs_hour: obs_hour, $
            pw: result }

  RETURN, struct

           
END

;;----------------------------------------------------------------------
;; Writes the monthly mean values of IGRA surface and sea level pressure,
;; surface temperature and dewpoint, and temperature and dewpoint at mandatory
;; levels to a NetCDF file.
;;
;; 2010-12-02 A.P.Barrett
;;----------------------------------------------------------------------
PRO WRITE_MONTHLY_IGRA_PRCWATER, DATA, STATION, FILEOUT

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

  pw300 = data.pw300
  pw850 = data.pw850

  ;; Get dimension sizes
  ntim = N_ELEMENTS( pw300.time )
  nobs = N_ELEMENTS( pw850.obs_hour )

  ;; Define netCDF file
  cdfid = NCDF_CREATE( fileout, /clobber )
  tim_dimid = NCDF_DIMDEF( cdfid, 'time', ntim )
  obs_dimid = NCDF_DIMDEF( cdfid, 'obs_hour', nobs )

  tim_varid = NCDF_VARDEF( cdfid, 'time', [tim_dimid], /FLOAT )
  obs_varid = NCDF_VARDEF( cdfid, 'obs_hour', [obs_dimid], /FLOAT )

  pw300_varid = NCDF_VARDEF( cdfid, 'pw300', $
                          [ tim_dimid, obs_dimid ], /FLOAT )
  pw850_varid = NCDF_VARDEF( cdfid, 'pw850', $
                          [ tim_dimid, obs_dimid ], /FLOAT )

  ;; Write variable attributes
  NCDF_ATTPUT, cdfid, tim_varid, 'long_name', 'time'
  NCDF_ATTPUT, cdfid, tim_varid, 'units', 'days since 1-1-1900 00:00'
  NCDF_ATTPUT, cdfid, obs_varid, 'long_name', 'Observation Hour'
  NCDF_ATTPUT, cdfid, obs_varid, 'units', 'hour'

  NCDF_ATTPUT, cdfid, pw300_varid,  'long_name', 'precipitable water to 300 hPa'
  NCDF_ATTPUT, cdfid, pw300_varid,  'units', 'kg/m2'
  NCDF_ATTPUT, cdfid, pw300_varid,  'standard_name', 'tropospheric_precipitable_water'
  NCDF_ATTPUT, cdfid, pw300_varid, '_FillValue', -9999.99

  NCDF_ATTPUT, cdfid, pw850_varid,  'long_name', 'precipitable water to 850 hPa'
  NCDF_ATTPUT, cdfid, pw850_varid,  'units', 'kg/m2'
  NCDF_ATTPUT, cdfid, pw850_varid,  'standard_name', 'lower_troposphere_precipitable_water'
  NCDF_ATTPUT, cdfid, pw850_varid, '_FillValue', -9999.99
  
  ;; Write global attributes
  NCDF_ATTPUT, cdfid, 'description', 'Monthly means for IGRA precipitable water for station ' + station.station_number, /GLOBAL
  NCDF_ATTPUT, cdfid, 'station_name', station.station_name, /GLOBAL
  NCDF_ATTPUT, cdfid, 'station_number', station.station_number, /GLOBAL
  NCDF_ATTPUT, cdfid, 'station_latitude', station.latitude, /GLOBAL
  NCDF_ATTPUT, cdfid, 'station_longitude', station.longitude, /GLOBAL
  NCDF_ATTPUT, cdfid, 'station_elevation', station.elevation, /GLOBAL
  NCDF_ATTPUT, cdfid, 'created_by', 'Andrew P. Barrett <apbarret@nsidc.org', /GLOBAL
  NCDF_ATTPUT, cdfid, 'creation_date', systime(), /GLOBAL

  NCDF_CONTROL, cdfid, /ENDEF

  ;; Write variables
  NCDF_VARPUT, cdfid, tim_varid,  pw300.time - JULDAY( 1, 1, 1900, 0 )
  NCDF_VARPUT, cdfid, obs_varid,  pw300.obs_hour
  NCDF_VARPUT, cdfid, pw300_varid, pw300.pw
  NCDF_VARPUT, cdfid, pw850_varid, pw850.pw

  NCDF_CLOSE, cdfid

  RETURN

END

;;**********************************************************************
;; MAIN ROUTINE
;;**********************************************************************
PRO IGRA_DERIVED_TO_PRCWATER, STATION_NUMBER, RESULT

  prcStrt = SYSTIME( /SECONDS )

  ybeg = 1945
  yend = 2008

  ;; Get data
  profile = GET_IGRA_DERIVED( station_number, /VERBOSE )

  ;; Perform temperature check
;  PRINT, '%   Checking for temperatures below threshold'
;  ctStrt = SYSTIME( /SECONDS )
;  CHECK_TEMPERATURE, profile, COUNT=num_temperature, /VERBOSE
;  PRINT, '% CHECK_TEMPERATURE too ' + STRTRIM( ( SYSTIME( /SECONDS ) - ctStrt ) / 60., 2 ) + ' min'

  ;; Remove profiles with no vapour pressure
  PRINT, '%   Removing profiles with no vapour pressure'
  new_profile = REMOVE_PROFILES_WITH_NOVAP( PROFILE, /VERBOSE, COUNT=num_profile )

  ;; Remove profiles with either no surface, 850 or 700 hPa data
  PRINT, '%   Removing profiles with invalid surface, 850 or 700 hPa levels'
  new_profile = REMOVE_INVALID_LOWER_LEVELS( new_profile, /VERBOSE, COUNT=num_profile )

  ;; Truncate levels at highest mandatory level with t and vap data
  PRINT, '%   Truncating profiles at highest valid mandatory level'
  TRUNCATE_PROFILES, new_profile, /VERBOSE, COUNT=num_truncated

  ;; Calculate precipitable water
  PRINT, '%   Calculating precipitable water'
  pw300 = CALC_PRCWATER( new_profile, 300. )
  pw850 = CALC_PRCWATER( new_profile, 850. )

  ;; Compute monthly means
  pw300_mon = MONTHLY_MEAN_PRCWATER( pw300, new_profile, YBEG=1945, YEND=2008 )
  pw850_mon = MONTHLY_MEAN_PRCWATER( pw850, new_profile, YBEG=1945, YEND=2008 )
  result = {pw300: pw300_mon, $
            pw850: pw850_mon}

  
  !p.multi=[0,1,2]
  time = JULDAY( new_profile.month, new_profile.day, new_profile.year, new_profile.obs_hour )
  trange = JULDAY( [1,12],[1,31],[ybeg,yend] )
 
  dummy=LABEL_DATE( DATE_FORMAT=['%y'] )
  PLOT, time, pw300, MIN_VALUE=-9999., XTICKUNITS = ['Time'], $  
        XTICKFORMAT='LABEL_DATE'
  OPLOT, pw300_mon.time, pw300_mon.pw[*,0], color=240, MIN_VALUE=-9999.
  PLOT, time, pw850, MIN_VALUE=-9999., XTICKUNITS = ['Time'], $  
        XTICKFORMAT='LABEL_DATE'
  OPLOT, pw850_mon.time, pw850_mon.pw[*,0], color=240, MIN_VALUE=-9999.

;  OPLOT, time, pw850, MIN_VALUE=-9999.
;
  PRINT, '% IGRA_DERIVED_TO_PRCWATER: Time Elapsed: ' + $
         STRTRIM( ( SYSTIME( /SECONDS ) - prcStrt ) / 60., 2 ) + ' min'

  RETURN

END

PRO BATCH_IGRA_DERIVED_TO_PRCWATER

  station_list_file = 'derived-stations-arctic-subset.txt'

  READ_IGRA_STATION_LIST, station_list_file, station, NREC=nstation
  ;;nstation = 3
  
  FOR istat = 0, nstation-1 DO BEGIN

     PRINT, '%   Processing station ' + STRTRIM( station[istat].station_number, 2 )

     IGRA_DERIVED_TO_PRCWATER, station[istat].station_number, result
     
     fileout = STRTRIM( station[istat].station_number, 2 ) + '.prcwater.month.nc'
     WRITE_MONTHLY_IGRA_PRCWATER, result, station[istat], fileout

  ENDFOR

END
  
