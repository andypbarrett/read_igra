;;----------------------------------------------------------------------
;; Finds the date of the first and last records in an IGRA station data set,
;; and the number of records.
;;
;; Dates are returned as Julian Dates
;;
;; 2010-08-23 A.P.Barrett
;;----------------------------------------------------------------------
FUNCTION DEFINE_DATA_REC

  data_record = {major_level_type: -1, $
                 minor_level_type: -1, $
                 pressure: -9999.99, $
                 pressure_flag: '', $
                 geopot_hgt: -9999.99, $
                 geopot_hgt_flag: '', $
                 temperature: -9999.99, $
                 temperature_flag: '', $
                 dewpoint: -9999.99, $
                 wind_direction: -9999.99, $
                 wind_speed: -9999.99}

  RETURN, data_record

END

FUNCTION DEFINE_HEADER_STR

  MAX_LEVEL = 20

  header_struct = {station_number: '', $
                   year: 0l, $
                   month: 0l, $
                   day: 0l, $
                   obs_hour: 0l, $
                   release_time: 0l, $
                   nlevels: 0l, $
                   level_data: REPLICATE( DEFINE_DATA_REC(), max_level )}

  RETURN, header_struct

END

FUNCTION PARSE_HEADER, LINE, STRUCT

  struct.station_number = STRMID( line, 1, 5 )
  struct.year           = LONG( STRMID( line, 6, 4 ) )
  struct.month          = LONG( STRMID( line, 10, 2 ) )
  struct.day            = LONG( STRMID( line, 12, 2 ) )
  struct.obs_hour       = LONG( STRMID( line, 14, 2 ) )
  struct.release_time   = LONG( STRMID( line, 16, 4 ) )
  struct.nlevels        = LONG( STRMID( line, 20, 4 ) )

  RETURN, struct

END

PRO GET_FIRST_AND_LAST_RECORD_DATE, FILE, FIRST_YEAR, FIRST_MONTH, $
                                    FIRST_DAY, FIRST_HOUR, $
                                    LAST_YEAR, LAST_MONTH, LAST_DAY, $
                                    LAST_HOUR, N_RECORD, num_rec1979, $
                                    max_level, VERBOSE=VERBOSE

  DEBUG = 0

  IF ( KEYWORD_SET( verbose ) EQ 0 ) THEN verbose = 0
  
  IF ( N_PARAMS() NE 12 ) THEN BEGIN
     PRINT, '% USAGE: GET_FIRST_AND_LAST_RECORD_DATE, FILE, FIRST_YEAR, ' + $
            'FIRST_MONTH, FIRST_DAY, FIRST_HOUR, ' + $
            'LAST_YEAR, LAST_MONTH, LAST_DAY, ' + $
            'LAST_HOUR, N_RECORD, NUM_REC1979, VERBOSE=VERBOSE'
     RETURN
  ENDIF

  IF ( N_ELEMENTS( FILE ) EQ 0 ) THEN BEGIN
     PRINT, '% READ_IGRA_DATA: Argument FILE is not defined'
     RETURN
  ENDIF

  result = -1

  header_struct = DEFINE_HEADER_STR()
 
  ;; Initialize date variables
  first_year  = 0
  first_month = 0
  first_day   = 0
  first_hour  = 0
  first_date  = 0
  last_year   = 0
  last_month  = 0
  last_day    = 0
  last_hour   = 0
  last_date   = 0
  
  ;; Open file
  OPENR, U, file, /GET_LUN

  irec = 0l
  iheader = 0l
  num_rec1979 = 0l
  max_level = 0l
  line = ''
  WHILE ( NOT EOF( U ) ) DO BEGIN

     ;; Read line of file
     READF, U, line

     ;; Check if Header Record
     IF ( STREGEX( line, '^#', /BOOLEAN ) EQ 1 ) THEN BEGIN

        IF ( verbose EQ 1 ) THEN BEGIN
           PRINT, 'Getting header information for record ' + $
                  STRTRIM( iheader, 2 ) + '...'
        ENDIF

        ;; Initialize counter for levels
        ilev = 0

        ;; Read Header information
        header_struct = PARSE_HEADER( line, header_struct )

        IF ( header_struct.nlevels GT max_level ) THEN BEGIN
           max_level = header_struct.nlevels
        ENDIF

        ;; Compute Julian date
        cur_date = JULDAY( header_struct.month, header_struct.day, $
                           header_struct.year, header_struct.obs_hour )

        ;; Check that current date is not before 'first_date'
        IF ( first_date GT 0 ) THEN BEGIN
           IF ( cur_date LT first_date ) THEN BEGIN
              PRINT, 'FIRST_LAST_REC_DATE: Current date is earlier than ' + $
                     'date of first record'
              PRINT, FORMAT='("     FIRST DATE: ",i4,2("-",i02),":",i2,"h")', $
                     first_year, first_month, first_day, first_hour
              PRINT, FORMAT='("   CURRENT DATE: ",i4,2("-",i02),":",i2,"h")', $
                     header_struct.year, header_struct.month, $
                     header_struct.day, header_struct.obs_hour
              first_year  = header_struct.year
              first_month = header_struct.month
              first_day   = header_struct.day
              first_hour  = header_struct.obs_hour
              first_date  = cur_date
           ENDIF
        ENDIF ELSE BEGIN
           first_year  = header_struct.year
           first_month = header_struct.month
           first_day   = header_struct.day
           first_hour  = header_struct.obs_hour
           first_date  = cur_date
        ENDELSE

        ;; Set last_date and check that it is not before current date
        IF ( last_date GT 0 ) THEN BEGIN
           IF ( cur_date GT last_date ) THEN BEGIN
              last_year  = header_struct.year
              last_month = header_struct.month
              last_day   = header_struct.day
              last_hour  = header_struct.obs_hour
              last_date  = cur_date
           ENDIF ELSE BEGIN
              PRINT, 'FIRST_LAST_REC_DATE: Current date is earlier than ' + $
                     'date of previous records'
              PRINT, FORMAT='("      LAST DATE: ",i4,2("-",i02),":",i2,"h")', $
                     last_year, last_month, last_day, last_hour
              PRINT, FORMAT='("   CURRENT DATE: ",i4,2("-",i02),":",i2,"h")', $
                     header_struct.year, header_struct.month, $
                     header_struct.day, header_struct.obs_hour
;;              PRINT, FORMAT='("Current Julian date: ",f10.2)',  cur_date
;;              PRINT, FORMAT='("Latest Julian date:  ",f10.2)',  last_date
;;              STOP
           ENDELSE
        ENDIF ELSE BEGIN
           last_date = cur_date
        ENDELSE

        IF ( DEBUG EQ 1 ) THEN BEGIN
           PRINT, FORMAT=hdr_fmt, $
                  iheader, header_struct.year, header_struct.month, $
                  header_struct.day, header_struct.obs_hour, $
                  header_struct.release_time, header_struct.nlevels
        ENDIF 

        iheader = iheader + 1

        IF ( header_struct.year GE 1979 AND $
             header_struct.year LE 2009 ) THEN BEGIN
           num_rec1979 = num_rec1979 + 1
        ENDIF

     ENDIF  

  ENDWHILE
  
  CLOSE, U
  FREE_LUN, U

  n_record = iheader

  IF ( VERBOSE EQ 1 ) THEN BEGIN
     PRINT, $
        FORMAT='("First observation:   ",i7,1x,i4,1x,i02,1x,i02,1x,i02,"h")', $
        0, first_year, first_month, first_day, first_hour
     PRINT, $
        FORMAT='("Last observation:    ",i7,1x,i4,1x,i02,1x,i02,1x,i02,"h")', $
        nheader, last_year, last_month, last_day, last_hour
     PRINT, FORMAT='("Number of Records:   ",i7)', nheader
  ENDIF

  RETURN

END
