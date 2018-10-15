;; A script to find the period of record of each Arctic station

FUNCTION FIND_DUPLICATE, por, y2d, NDUPLICATE=NDUPLICATE

  nduplicate = 0l

  ny2d = N_ELEMENTS( y2d )

  result = MAKE_ARRAY( ny2d, /LONG, VALUE=-1 )

  fmt = '("% FIND_DUPLICATE: Found more than one duplicate for ",i4,"-",i02,"-",i02,": ",i02,"h")'

  FOR iy = 0, ( ny2d - 1 ) DO BEGIN

     index = WHERE( por.year EQ y2d[iy].year AND $
                    por.month EQ y2d[iy].month AND $
                    por.day EQ y2d[iy].day AND $
                    por.obs_hour EQ y2d[iy].obs_hour, num_index )

     IF ( num_index GT 1 ) THEN BEGIN
        PRINT, FORMAT=fmt, y2d[iy].year, y2d[iy].month, y2d[iy].day, y2d[iy].obs_hour
     ENDIF

     IF ( num_index GT 0 ) THEN BEGIN
        result[ nduplicate : (nduplicate + num_index - 1) ] = index
        nduplicate = nduplicate + num_index
     ENDIF

  ENDFOR

  RETURN, result

END

PRO GET_IGRA_POR

;; - Arctic station file
station_list_file = 'igra_stations_arctic.txt'
;; location of data files
indir = 'data'

;; Get Arctic stations
READ_IGRA_STATION_LIST, station_list_file, station_list, NREC=nstation
;;nstation = 2

fmt = '(a5,1x,a20,2(2(1x,i4,"-",i02,"-",i02),1x,i6," --- "),2x,i6)'

OPENW, U, 'arctic_stations_por.txt', /GET_LUN

FOR istat = 0, ( nstation - 1 ) DO BEGIN

   PRINT, '% GET_IGRA_POR: Processing station ' + station_list[ istat ].station_number + $
          ' - ' + STRTRIM( istat+1, 2 ) + ' of ' + STRTRIM( nstation, 2 ) 

   profile_por = GET_IGRA_DATA( station_list[ istat ].station_number, INDIR=indir, $
                                /NOCLEANUP, /VERBOSE, STATUS=status_por )
   IF ( status_por EQ 0 ) THEN BEGIN
      npor = N_ELEMENTS( profile_por )
      ybeg_por = profile_por[ 0 ].year
      mbeg_por = profile_por[ 0 ].month
      dbeg_por = profile_por[ 0 ].day
      yend_por = profile_por[ npor - 1 ].year
      mend_por = profile_por[ npor - 1 ].month
      dend_por = profile_por[ npor - 1 ].day
   ENDIF ELSE BEGIN
      npor = 0
      ybeg_por = 0
      mbeg_por = 0
      dbeg_por = 0
      yend_por = 0
      mend_por = 0
      dend_por = 0
   ENDELSE

   profile_y2d = GET_IGRA_DATA( station_list[ istat ].station_number, SUFFIX='y2d', $
                                INDIR=indir, /NOCLEANUP, /VERBOSE, STATUS=status_y2d )
   IF ( status_y2d EQ 0 ) THEN BEGIN
      ny2d = N_ELEMENTS( profile_y2d )
      ybeg_y2d = profile_y2d[ 0 ].year
      mbeg_y2d = profile_y2d[ 0 ].month
      dbeg_y2d = profile_y2d[ 0 ].day
      yend_y2d = profile_y2d[ ny2d - 1 ].year
      mend_y2d = profile_y2d[ ny2d - 1 ].month
      dend_y2d = profile_por[ ny2d - 1 ].day
   ENDIF ELSE BEGIN
      ny2d = 0
      ybeg_y2d = 0
      mbeg_y2d = 0
      dbeg_y2d = 0
      yend_y2d = 0
      mend_y2d = 0
      dend_y2d = 0
   ENDELSE

   IF ( status_por EQ 0 AND status_y2d EQ 0 ) THEN BEGIN
      result = FIND_DUPLICATE( profile_por, profile_y2d, NDUPLICATE=nduplicate )
   ENDIF

   PRINTF, U, FORMAT=fmt, station_list[ istat].station_number, station_list[ istat ].station_name, $
           ybeg_por, mbeg_por, dbeg_por, yend_por, mend_por, dend_por, npor, $
           ybeg_y2d, mbeg_y2d, dbeg_y2d, yend_y2d, mend_y2d, dend_y2d, ny2d, $
           nduplicate
          

ENDFOR

CLOSE, U
FREE_LUN, U

END

   
   
