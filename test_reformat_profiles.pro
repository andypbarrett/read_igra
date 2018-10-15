;; Tests the REFORMAT_PROFILES routine

fili = '01001.txt'
filo = '01001.sub.nc'

start_time = SYSTIME(/JULIAN)

;; Read radiosonde profiles for station
PRINT, '% Reading profiles from ' + fili
data = 0
READ_DAI_RADIOSONDE, fili, data

;; Just grab last 10 years of record
;idx = WHERE( data.year GE 2000, count )
;IF (count GT 0 ) THEN BEGIN
;   tmp = data[idx]
;   data = 0
;   data = tmp
;   tmp = 0
;ENDIF

;; Subset structure so that only fiest 7 levels are in file
;; - this reduces the amount of crunching
PRINT, '% Subsetting data...'
sub_data = 0
SUBSET_STRUCT, data, sub_data, 0, 6

;; Reformat data so that levels are in columns
PRINT, '% Reformatting data...'
ref_data = 0
ref_data = REFORMAT_PROFILES( sub_data )

;; Calculate monthly means
PRINT, '% Calculating monthly means...'
mon_data = 0
DAI_PROFILES_TO_MONTH, ref_data, mon_data
 
;; Write data to files
PRINT, '% Writing data to ' + filo
MONTH_PROFILES_TO_NETCDF, mon_data, filo

secinday = 86400.
time_elapsed = (SYSTIME(/JULIAN) - start_time) * secinday
PRINT, 'Time Elapsed: ' + STRTRIM( time_elapsed, 2 ) + ' seconds'

END


