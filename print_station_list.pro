;;----------------------------------------------------------------------
;; prints data read by read_igra_station_list.
;;
;; 2010-08-19 A.P.Barrett
;;----------------------------------------------------------------------
PRO PRINT_STATION_LIST, LIST

  nstat = N_ELEMENTS( list )

  FOR ii = 0, ( nstat - 1 ) DO BEGIN
     PRINT, FORMAT='(a2,1x,a5,1x,a36,1x,f6.2,1x,f7.2,1x,f6.1,1x,'+$
            'a1,1x,a1,1x,a1,1x,i4,1x,i4)', list[ ii ]
  ENDFOR

END
