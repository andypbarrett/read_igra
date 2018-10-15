
;; Loops through Dai's rawinsonde files and finds max and min pressure
;; levels and numbers of reported levels

file = FINDFILE( '?????.txt*', COUNT=nfiles )
nfiles = 10

min_level = MAKE_ARRAY( nfiles, /FLOAT, VALUE=-9999.99 )
max_level = MAKE_ARRAY( nfiles, /FLOAT, VALUE=-9999.99 )
nlev      = MAKE_ARRAY( nfiles, /INTEGER, VALUE=0 )

FOR ifi = 0, ( nfiles-1 ) DO BEGIN

   IF ( STREGEX( file[ifi], '.gz', /BOOLEAN ) EQ 1 ) THEN BEGIN
      PRINT, 'Unzipping ' + file[ifi]
      SPAWN, 'gunzip ' + file[ifi]
      thisfile = STRMID( file[ifi], 0, 9 )
   ENDIF ELSE BEGIN
      thisfile = file[ifi]
   ENDELSE

   PRINT, 'Getting data from ' + thisfile + ': ' + $
          STRTRIM(ifi+1,2) + ' of ' + STRTRIM(nfiles,2)
   read_dai_radiosonde, thisfile, data, nrec=nrec

   prs = data.pressure
   idx = WHERE( prs GT -9998, count )
   IF ( count GT 0 ) THEN BEGIN
      min_level[ifi] = MAX( prs[ idx ] )
      max_level[ifi] = MIN( prs[ idx ] )
      nlev[ifi]      = MAX( data.nlevels )
   ENDIF

   PRINT, 'Zipping ' + thisfile
   SPAWN, 'gzip ' + thisfile

ENDFOR

fmt = '(2x,a12,1x,i2,2(1x,f8.2))'
FOR ifi = 0, (nfiles-1) DO BEGIN
   PRINT, FORMAT=fmt, file[ifi], nlev[ifi], min_level[ifi], max_level[ifi]
ENDFOR

END

