
PRO SET_MISSING, data

  idx = WHERE( data LE -9999. OR data GT 9.0e36, count )
  IF ( count GT 0 ) THEN data[idx] = !VALUES.F_NAN

  RETURN

END

FUNCTION DOCORR, x, y

  rhostr = " ----- "

  idx = WHERE( FINITE(x) EQ 1 AND FINITE(y) EQ 1, count )
  IF ( count GT 0 ) THEN BEGIN
     rho = CORRELATE( x[idx], y[idx] )
     tr  = rho * SQRT( count - 2 ) / SQRT( 1 - rho^2 )
     prob = T_PDF( tr, count-2 )
     print, prob, count
     STOP

     rhostr = STRING( rho, prob, FORMAT='(f9.4," (",f4.2,")")' )
;     CASE 1 OF
;        ( prob GT .90 AND prob LE 0.95): rhostr = STRING( rho, FORMAT='(f9.4,"* ")' )
;        ( prob GT .95 ): rhostr = STRING( rho, FORMAT='(f9.4,"**")' )
;        ELSE: rhostr = STRING( rho, FORMAT='(f9.4)' )
;     ENDCASE
  ENDIF


  RETURN, rhostr

END

;; Data files
fradi = '/raid/IGRA/Dai_homogenized_radiosonde_arctic/radiosonde.station.month.timeseries.QV.nc'
ferai = '/raid/ERA_Interim/monthly_mean/era_interim.station.month.timeseries.QV.nc'
fmeri = '/raid/MERRA/monthly/QV/merra.station.month.timeseries.QV.nc'
fjrai = '/raid/JRA25/1125_deg/monthly/anl_p/jra25.station.month.timeseries.QV.nc'
fcfsi = '/disks/arctic3_raid2/apbarret/CFSR/CFSR.station.month.timeseries.QV.nc'
fncpi = '/disks/arctic3_raid2/apbarret/NCEP_psd_HUM/NCEP.station.month.timeseries.QV.nc'
fe40i = '/raid/ERA40_two/e4moda.an.pl/ERA40.station.month.timeseries.QV.nc'

sonde = NCDF_READV( fradi, VARNAME='shum' )
sonde_std = NCDF_READV( fradi, VARNAME='zscore' )

erai = NCDF_READV( ferai, VARNAME='qv' ) * 1000.
erai_std = NCDF_READV( ferai, VARNAME='qv_std' )

merra = NCDF_READV( fmeri, VARNAME='qv' ) * 1000.
merra_std = NCDF_READV( fmeri, VARNAME='qv_std' )

jra25 = NCDF_READV( fjrai, VARNAME='qv' ) * 1000.
jra25_std = NCDF_READV( fjrai, VARNAME='qv_std' )

era40 = NCDF_READV( fe40i, VARNAME='qv' ) * 1000.
era40_std = NCDF_READV( fe40i, VARNAME='qv_std' )

cfsr = NCDF_READV( fcfsi, VARNAME='qv' ) * 1000.
cfsr_std = NCDF_READV( fcfsi, VARNAME='qv_std' )

ncep = NCDF_READV( fncpi, VARNAME='qv' )
ncep_std = NCDF_READV( fncpi, VARNAME='qv_std' )


SET_MISSING, sonde
SET_MISSING, sonde_std
SET_MISSING, erai
SET_MISSING, erai_std
SET_MISSING, merra
SET_MISSING, merra_std
SET_MISSING, era40
SET_MISSING, era40_std
SET_MISSING, cfsr
SET_MISSING, cfsr_std
SET_MISSING, ncep
SET_MISSING, ncep_std

!P.MULTI=[0,3,3]
ilev = 7
FOR istat=0, 8 DO BEGIN
   
   xmin = MIN( [ MIN( sonde_std[ istat, ilev, * ], /NAN ), MIN( erai_std[ istat, ilev, * ], /NAN ), $
                 MIN( merra_std[ istat, ilev, * ], /NAN ), MIN( jra25_std[ istat, ilev, * ], /NAN ), $
                 MIN( era40_std[ istat, ilev, * ], /NAN ), MIN( cfsr_std[ istat, ilev, * ], /NAN ), $
                 MIN( ncep_std[ istat, ilev, * ], /NAN ) ] )

   xmax = MAX( [ MAX( sonde_std[ istat, ilev, * ], /NAN ), MAX( erai_std[ istat, ilev, * ], /NAN ), $
                 MAX( merra_std[ istat, ilev, * ], /NAN ), MAX( jra25_std[ istat, ilev, * ], /NAN ), $
                 MAX( era40_std[ istat, ilev, * ], /NAN ), MAX( cfsr_std[ istat, ilev, * ], /NAN ), $
                 MAX( ncep_std[ istat, ilev, * ], /NAN ) ] )
   ymin = xmin
   ymax = xmax

   PLOT, sonde[ istat, ilev, * ], erai[ istat, ilev, * ], PSYM=3, $
         XRANGE=[xmin,xmax], XSTYLE=1, $
         YRANGE=[ymin,ymax], YSTYLE=1, $
         XTITLE='Radiosonde', YTITLE='Reanalysis', /NODATA
   PLOTS, !X.CRANGE, !Y.CRANGE

   OPLOT, sonde_std[ istat, ilev, * ], erai_std[ istat, ilev, * ], PSYM=3, COLOR=240
   OPLOT, sonde_std[ istat, ilev, * ], merra_std[ istat, ilev, * ], PSYM=3, COLOR=208
   OPLOT, sonde_std[ istat, ilev, * ], jra25_std[ istat, ilev, * ], PSYM=3, COLOR=160
   OPLOT, sonde_std[ istat, ilev, * ], era40_std[ istat, ilev, * ], PSYM=3, COLOR=80
   OPLOT, sonde_std[ istat, ilev, * ], cfsr_std[ istat, ilev, * ], PSYM=3, COLOR=16
   OPLOT, sonde_std[ istat, ilev, * ], ncep_std[ istat, ilev, * ], PSYM=3, COLOR=192

   corrcoef = MAKE_ARRAY( 6, /FLOAT )

   print, istat, DOCORR( sonde_std[ istat, ilev, * ], erai_std[ istat, ilev, * ] ), $
          DOCORR( sonde_std[ istat, ilev, * ], merra_std[ istat, ilev, * ] ), $
          DOCORR( sonde_std[ istat, ilev, * ], jra25_std[ istat, ilev, * ] ), $
          DOCORR( sonde_std[ istat, ilev, * ], era40_std[ istat, ilev, * ] ), $
          DOCORR( sonde_std[ istat, ilev, * ], cfsr_std[ istat, ilev, * ] ), $
          DOCORR( sonde_std[ istat, ilev, * ], ncep_std[ istat, ilev, * ] ), $
          FORMAT='(i2,6(1x,a16))'
          
ENDFOR

END

