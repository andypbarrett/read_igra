;;----------------------------------------------------------------------
;; Loops through IGRA derived station data and plots surface geopotential
;; assuming first element in observed surface geopotential is the surface.
;;
;; 2010-12-02 A.P.Barrett
;;----------------------------------------------------------------------
FUNCTION GET_IGRA_DERIVED, STATION_ID, INDIR=INDIR, TYPE=TYPE, $
                           VERBOSE=VERBOSE

  indir = N_ELEMENTS( indir ) EQ 0 ? '.' : indir
  type = N_ELEMENTS( type ) EQ 0 ? 'dat' : type
 
  fili = STRTRIM( station_id, 2 ) + '.' + type

  IF ( indir NE '.' ) THEN BEGIN
     CD, indir, CURRENT=old_dir
  ENDIF

  IF ( KEYWORD_SET( verbose ) EQ 1 ) THEN BEGIN
     PRINT, '% GET_IGRA_DERIVED: Unzipping ' + fili + '.gz'
  ENDIF
  SPAWN, 'gunzip ' + fili + '.gz'

  IF ( KEYWORD_SET( verbose ) EQ 1 ) THEN BEGIN
     PRINT, '% GET_IGRA_DERIVED: Reading ' + fili
  ENDIF
  READ_IGRA_DERIVED, fili, result

  IF ( KEYWORD_SET( verbose ) EQ 1 ) THEN BEGIN
     PRINT, '% GET_IGRA_DERIVED: Zipping ' + fili
  ENDIF
  SPAWN, 'gzip ' + fili

  IF ( indir NE '.' ) THEN BEGIN
     CD, old_dir
  ENDIF

  RETURN, result

END

PRO BATCH_PLOT_IGRA_DERIVED_GPH

  start_time = SYSTIME(/SECONDS)

  station_list = 'derived-stations-arctic.txt'
  READ_IGRA_STATION_LIST, station_list, station, NREC=nstation 
;  nstation=20

  WINDOW, 0, XSIZE=1700, YSIZE=1000
  !P.MULTI=[0,2,5]
  imageid = 0
  FOR istat = 0, ( nstation - 1 ) DO BEGIN

     profile = GET_IGRA_DERIVED( station[ istat ].station_number, /VERBOSE )
     
     PLOT_IGRA_DERIVED_GPH, profile, station[ istat ]

     IF ( ( istat mod 10 ) EQ 9 ) THEN BEGIN
        imageid = imageid + 1
        fileout = STRING( FORMAT='("igra_derived_gph_",i02,".png")', imageid )
        SAVEIMAGE, fileout, /PNG 
     ENDIF

  ENDFOR

  ;; Print last image
  IF ( ( ( istat - 1 ) mod 10 ) EQ 9 ) THEN BEGIN
     imageid = imageid + 1
     fileout = STRING( FORMAT='("igra_derived_gph_",i02,".png")', imageid )
     SAVEIMAGE, fileout, /PNG 
  ENDIF

  PRINT, FORMAT='("Time Elapsed = ",f8.2," minutes")', $
         ( SYSTIME(/SECONDS)-start_time ) / 60.

END
