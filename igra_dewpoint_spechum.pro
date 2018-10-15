;;----------------------------------------------------------------------
;; IGRA_DEWPOINT_SPECHUM - calculates specific humidity from dewpoint and
;;                         pressure following the methodfs used by Ross and
;;                         Elliot 1996, Tropospehric water vapour climatology
;;                         and trends over North America, J. Climate, 9.
;;
;; 2010-11-09 A.P.Barrett
;;----------------------------------------------------------------------

;; - calculates enhancement factor for vapour pressure calc
FUNCTION ENHANCEMENT_FACTOR, Pressure

  IF ( N_PARAMS() NE 1 ) THEN BEGIN
     PRINT, 'USAGE: RESULT = ENHANCEMENT_FACTOR( Pressure )'
     RETURN, -1
  ENDIF

  IF ( N_ELEMENTS( Pressure ) EQ 0 ) THEN BEGIN
     PRINT, '% ENHANCEMENT_FACTOR: Argument Pressure is not defined'
     RETURN, -1
  ENDIF

  factor = 1.0007 + ( 3.46 * Pressure * 1e-6 )

  RETURN, factor

END

;; Calculate vapour pressure
FUNCTION VAPOUR_PRESSURE, Temperature, Pressure

  b0 = 227.3
  b1 = 18.729
  b2 = 257.87
  b3 = 6.1121

  factor = ENHANCEMENT_FACTOR( Pressure )

  a0 = Temperature / b0
  a1 = b1 - a0
  a2 = a1 * Temperature
  a3 = Temperature + b2
  a4 = a2 / a3

  e = factor * b3 * EXP( a4 )

  RETURN, e

END

FUNCTION RELATIVE_HUMIDITY, E, ESAT

  RH = 100. * E / ESAT

  RETURN, RH

END

FUNCTION SPECIFIC_HUMIDITY, Vapress, Pressure

  b0 = 0.622
  b1 = 0.378

  a0 = b0 * Vapress
  a1 = b1 * Vapress
  a2 = Pressure - a1

  SpecHum = a0 / a2
  
  RETURN, SpecHum

END

;; Calculate dewpoint temperature from temperature and dewpoint depression
FUNCTION CALC_TDEW, Temperature, Dewpoint

  nrec = N_ELEMENTS( Temperature )

  tdew = MAKE_ARRAY( nrec, /FLOAT, VALUE = -9999.99 )

  isValid = WHERE( Temperature GT -9999. AND Dewpoint GT -9999., num_isValid )

  IF ( num_isValid GT 0 ) THEN BEGIN

     tdew[ isValid ] = Temperature[ isValid ] - Dewpoint[ isValid ]

  ENDIF

  RETURN, tdew

END


FUNCTION IGRA_DATA_TO_RH, DATA

  nrec = N_ELEMENTS( data.pressure )

  isValid = WHERE( data.pressure GT -9999. AND data.temperature GT -9999.99 AND $
                   data.dewpoint GT -9999, num_isValid )

  vapres = MAKE_ARRAY( nrec, /FLOAT, VALUE = -9999.99 )
  satVap = MAKE_ARRAY( nrec, /FLOAT, VALUE = -9999.99 )
  relHum = MAKE_ARRAY( nrec, /FLOAT, VALUE = -9999.99 )

  dewpoint = CALC_TDEW( data.temperature, data.dewpoint )

  vapres[ isValid ] = VAPOUR_PRESSURE( data.temperature[ isValid ], data.pressure[ isValid ] )
  satVap[ isValid ] = VAPOUR_PRESSURE( dewpoint[ isValid ], data.pressure[ isValid ] )

  relHum[ isValid ] = RELATIVE_HUMIDITY( satVap[ isValid ], vapres[ isValid ] )

  RETURN, relHum

END

FUNCTION IGRA_DATA_TO_SPECHUM, DATA

  nrec = N_ELEMENTS( data.pressure )

  isValid = WHERE( data.pressure GT -9999. AND data.temperature GT -9999.99 AND $
                   data.dewpoint GT -9999, num_isValid )

  vapres = MAKE_ARRAY( nrec, /FLOAT, VALUE = -9999.99 )
  satVap = MAKE_ARRAY( nrec, /FLOAT, VALUE = -9999.99 )
  specHum = MAKE_ARRAY( nrec, /FLOAT, VALUE = -9999.99 )

  dewpoint = CALC_TDEW( data.temperature, data.dewpoint )

  vapres[ isValid ] = VAPOUR_PRESSURE( data.temperature[ isValid ], data.pressure[ isValid ] )

  specHum[ isValid ] = SPECIFIC_HUMIDITY( vapres[ isValid ], data.pressure[ isValid ] )

  RETURN, specHum

END

;; Convert from kg/kg to g/kg
FUNCTION kg2g, data

 nrec = N_ELEMENTS( data )
 result = MAKE_ARRAY( nrec, /FLOAT, VALUE=-9999 )
 
 isValid = WHERE( data GT -9999., num_isValid )
 IF ( num_isValid GT 0 ) THEN BEGIN
    result[ isValid ] = data[ isValid ] * 1e3
 ENDIF

 RETURN, result

END

;; Returns column integrated water (precipitable water) in mm
FUNCTION PRECIPITABLE_WATER, specHum, Pressure, WLAYER=WLAYER

  g = 9.8 ;; gravity m/s2
  rho_water = 1e3 ;; density of water kg/m3

  nrec = N_ELEMENTS( specHum )

  ;; Check that specfic humidity and pressure have valid values
  numValid = TOTAL( specHum GT 0. )
  IF ( numValid NE nrec ) THEN BEGIN
     PRINT, '% PRECIPITABLE_WATER: Expects all specific humidity values to be valid'
     RETURN, -1
  ENDIF

  numValid = TOTAL( Pressure GT 0. )
  IF ( numValid NE nrec ) THEN BEGIN
     PRINT, '% PRECIPITABLE_WATER: Expects all pressure values to be valid'
     RETURN, -1
  ENDIF

  q0 = specHum[ 0:(nrec-2) ]
  q1 = specHum[ 1:(nrec-1) ]
  p0 = Pressure[ 0:(nrec-2) ]
  p1 = Pressure[ 1:(nrec-1) ]

  qAve = ( q0 + q1 ) / 2.
  pDel = p0 - p1
  
  a0 = qAve * pDel / g

  W = TOTAL( a0 ) ;; in kg / m2

  W = W / rho_water ;; in mm

  RETURN, W

END

FUNCTION CALC_PRECIP_WATER, specHum, Pressure, PLO=PLO, PHI=PHI

  IF ( N_PARAMS() NE 2 ) THEN BEGIN
     PRINT, 'USAGE: result = CALC_PRECIP_WATER( specHum, Pressure, PMIN=PMIN, PMAX=PMAX )'
     RETURN, -1
  ENDIF

  IF ( N_ELEMENTS( specHum ) EQ 0 ) THEN BEGIN
     PRINT, '% CALC_PRECIP_WATER: Argument specHum is not defined'
     RETURN, -1
  ENDIF

  IF ( N_ELEMENTS( Pressure ) EQ 0 ) THEN BEGIN
     PRINT, '% CALC_PRECIP_WATER: Argument Pressure is not defined'
     RETURN, -1
  ENDIF

  isValid = WHERE( specHum GT -9999. AND Pressure GT -9999., num_isValid )
  IF ( num_isValid LE 0 ) THEN BEGIN
     PRINT, '% CALC_PRECIP_WATER: No valid values pairs found'
     RETURN, -1
  ENDIF

  ;; Extract valid values
  q = specHum[ isValid ]
  p = Pressure[ isValid ]

  ;; Make sure that values are sorted in descending order
  indx = REVERSE( SORT( p ) )
  p = p[ indx ]
  q = q[ indx ]

  ;; Find minimum and maximum levels
  plo = N_ELEMENTS( plo ) EQ 0 ? p[ 0 ] : plo
  phi = N_ELEMENTS( phi ) EQ 0 ? p[ num_isValid - 1 ] : phi
  
  isLayer = WHERE( p LE plo AND p GE phi, numLayer )
  IF ( numLayer EQ 0 ) THEN BEGIN
     PRINT, '% CALC_PRECIP_WATER: no valid pressure levels found in layer'
     RETURN, -1
  ENDIF

  result = PRECIPITABLE_WATER( q[ isLayer ], p[ isLayer ] )

  RETURN, result
  
END

PRO TEST_CALCS, DATA

  nrec = 20  ;N_ELEMENTS( data.pressure )

  relHum = IGRA_DATA_TO_RH( data )

  specHum = IGRA_DATA_TO_SPECHUM( data )

  ;; Convert specific humidity to g/kg
  ;;specHum = kg2g( specHum )

  ;; Test precipitable water
  pw = CALC_PRECIP_WATER( specHum, data.pressure )

  fmt='(i3,4(1x,f8.2),1x,f12.6)'

  FOR irec = 0, ( nrec - 1 ) DO BEGIN

     PRINT, FORMAT=fmt, irec, data.pressure[ irec ], $
                        data.temperature[ irec ], $
                        data.dewpoint[ irec ], $
                        relHum[ irec ], $
                        specHum[ irec ]

  ENDFOR
  PRINT, FORMAT='("PRECIPITABLE WATER: ",f12.6)', pw

  RETURN

END
