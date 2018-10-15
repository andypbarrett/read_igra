
nprofile = N_ELEMENTS( profile )

minPressure = MAKE_ARRAY( nprofile, /FLOAT, VALUE=!VALUES.F_NAN )
id300       = MAKE_ARRAY( nprofile, /LONG, VALUE=-1 )

FOR ip = 0l, nprofile-1 DO BEGIN

   tmp = profile[ip].pressure

   idx = WHERE( tmp GT -9999., count )

   minPressure[ip] = MIN( tmp[idx], tmpId )

   id300[ip] = tmpId

ENDFOR

PRINT, MIN( minPressure, /NAN ), MAX( minPressure, /NAN ), FORMAT='("minPressure:  Min: ",f7.2,"  Max: ",f7.2)'

isvalid = WHERE( id300 GE 0, numValid )
IF ( numValid GT 0  ) THEN BEGIN
   PRINT, MAX( id300[isvalid] ), FORMAT='("Max ID of 300 hPa: ",i4)'
ENDIF ELSE BEGIN
   PRINT, 'No valid ids'
ENDELSE

END

