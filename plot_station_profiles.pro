
igra = ncdf_readv( 'igra_station.shum.zonal_mean.1989to1989.nc', varname='average' )
ilevel = ncdf_readv( 'igra_station.shum.zonal_mean.1989to1989.nc', varname='level' )

dai = ncdf_readv( '../Dai_homogenized_radiosonde_arctic/dai_radiosonde.shum.clm.month.profiles.70Nto90N.1989to2001.nc', varname='average' )
dlevel = ncdf_readv( '../Dai_homogenized_radiosonde_arctic/dai_radiosonde.shum.clm.month.profiles.70Nto90N.1989to2001.nc', varname='level' )

!p.multi=[0,3,4]
FOR i=0, 11 DO BEGIN
   plot, igra[i,*], ilevel, yrange=[1013,299], ystyle=1, /ylog, $
         xrange=[0,5], xstyle=1
   oplot, dai[i,*], dlevel
ENDFOR

END
