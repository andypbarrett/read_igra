;;----------------------------------------------------------------------
;; Writes IGRA data to an ascii file.
;;
;; 2010-11-23 A.P.Barrett
;;----------------------------------------------------------------------

PRO WRITE_HEADER, LUN, STRUCT

  fmt = '("#",a5,i4,i02,i02,i02,i04,i4)'

  PRINTF, LUN, FORMAT=fmt, struct.station_number, $
          struct.year, $
          struct.month, $
          struct.day, $
          struct.obs_hour, $
          struct.release_time, $
          struct.nlevels

  RETURN

END ;; end of WRITE_HEADER

PRO WRITE_LEVEL, LUN, ILEV, STRUCT, MISSING=MISSING

  missing = N_ELEMENTS( missing ) EQ 0 ? -9999. : missing

  fmt = '(i1,i1,i6,a1,i5,a1,i5,a1,i5,i5,i5)'

  PRINTF, LUN, FORMAT=fmt, $
          struct.major_level_type[ ilev ], $
          struct.minor_level_type[ ilev ], $
          ( struct.pressure[ ilev ] GT missing ) ? struct.pressure[ ilev ] * 100. : missing, $
          struct.pressure_flag[ ilev ], $
          ( struct.geopot_hgt[ ilev ] GT missing ) ? struct.geopot_hgt[ ilev ] : missing, $
          struct.geopot_hgt_flag[ ilev ], $
          ( struct.temperature[ ilev ] GT missing ) ? struct.temperature[ ilev ] * 10. : missing, $
          struct.temperature_flag[ ilev ], $
          ( struct.dewpoint[ ilev ] GT missing ) ? struct.dewpoint[ ilev ] * 10. : missing, $
          ( struct.wind_direction[ ilev ] GT missing ) ? struct.wind_direction[ ilev ] : missing, $
          ( struct.wind_speed[ ilev ] GT missing ) ? struct.wind_speed[ ilev ] * 10. : missing

  RETURN

END ;; end of WRITE_LEVEL

PRO WRITE_PROFILE, LUN, PROFILE0, MISSING=MISSING

  nlev = profile0.nlevels

  WRITE_HEADER, lun, profile0

  FOR ilev = 0, ( nlev - 1 ) DO BEGIN
     WRITE_LEVEL, lun, ilev, profile0, MISSING=missing
  ENDFOR

  RETURN

END ;; end of WRITE_PROFILE

PRO WRITE_IGRA_DATA, PROFILE, FILEOUT, STDOUT=STDOUT

  IF ( N_ELEMENTS( profile ) EQ 0 ) THEN BEGIN
     PRINT, '% WRITE_IGRA_DATA: Argument PROFILE is not defined'
     RETURN
  ENDIF

  IF ( KEYWORD_SET( STDOUT ) EQ 0 ) THEN BEGIN
     IF ( N_ELEMENTS( fileout ) EQ 0 ) THEN BEGIN
        PRINT, '% WRITE_IGRA_DATA: Argument FILEOUT is not defined'
        RETURN
     ENDIF
  ENDIF

  nprof = N_ELEMENTS( profile )

  ;; Open file
  IF ( KEYWORD_SET( STDOUT ) EQ 0 ) THEN BEGIN
     OPENW, lun, fileout, /GET_LUN
  ENDIF ELSE BEGIN
     lun = -1
  ENDELSE

  ;; Write data
  FOR iprof = 0, ( nprof - 1 ) DO BEGIN
     WRITE_PROFILE, lun, profile[ iprof ], MISSING=-9999.
  ENDFOR

  ;; Close file
  IF ( KEYWORD_SET( STDOUT ) EQ 0 ) THEN BEGIN
     CLOSE, lun
     FREE_LUN, lun
  ENDIF

END ;; end of WRITE_IGRA_DATA
