PRO WRITE_IGRA_STATION_LIST, FILO, DATA

  FMT = '(a2,2x,a5,2x,a35,1x,f6.2,1x,f7.2,1x,i4,1x,a1,a1,a1,2x,i4,1x,i4)'

  nrec = N_ELEMENTS( data )

  ;; Open file
  OPENW, U, filo, /GET_LUN

  ;; Write records
  FOR irec = 0, ( nrec - 1 ) DO PRINTF, U, FORMAT=FMT, data[ irec ]

  ;;Close file
  CLOSE, U
  FREE_LUN, U

  RETURN

END
     
