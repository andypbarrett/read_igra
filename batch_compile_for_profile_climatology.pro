;; Compiles required procedures and loads color table for calculating
;; climatologies of profiles.
;;
;; 2010-11-22 A.P.Barrett

loadct, 39
.compile read_igra_data_new.pro
.compile calc_profile_climatology_qa.pro
.compile plot_igra_profile.pro
.compile process_profile_climatology.pro
.compile check_surface_temperature.pro
.compile check_surface_pressure.pro
.compile get_igra_por.pro
