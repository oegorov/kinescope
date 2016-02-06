FUNCTION KS_VOIGT, x, param, inst_vel=inst_vel
  vel=inst_vel;/2.
  param2=param[2]/2.35482
  b=(x-param[1])/(sqrt(2.)*param2)
  y=vel/(sqrt(2.)*param[2])
  res=voigt(y,b)/(sqrt(2.*!pi)*param2)
  norm=max(res)
  res=param[0]*res/norm
RETURN, res
END


FUNCTION KS_FIT_FUNC, x,param
  COMMON KS_FOR_MPFIT
  n_comps=(n_elements(param)-1)/3
  res=x*0.
  FOR jj=0,n_comps-1 DO BEGIN
    if current_method eq 1 then res+=gaussian(x,[param[jj*3],param[jj*3+1],param[jj*3+2]/2.35482]) else $
                               res+=ks_voigt(x,[param[jj*3],param[jj*3+1],param[jj*3+2]],inst_vel=inst_fwhm_vel)
  ENDFOR
  res+=param[n_elements(param)-1]
  
  RETURN,res
END


PRO KS_SORT_FIT_COMPS, maps, models, n_lines=n_lines
  
  intens=[maps.i1,maps.i2]
  velo=[maps.v1,maps.v2]
  if n_lines eq 3 then intens=[intens,maps.i3]
  if n_lines eq 3 then velo=[velo,maps.v3]
  
  s=sort(velo)
  
  maps_tmp=maps
  models_tmp=models
  IF n_lines eq 2 then begin
    m=max(intens,centcomp,/nan)
    
    if centcomp eq 1 then begin
      maps_tmp.i1=maps.i2
      maps_tmp.i1=maps.sigma2
      maps_tmp.i1=maps.v2
      maps_tmp.i1=maps.f2
      models_tmp.c1=models.c2
      maps_tmp.i2=maps.i1
      maps_tmp.i2=maps.sigma1
      maps_tmp.i2=maps.v1
      maps_tmp.i2=maps.f1
      models_tmp.c2=models.c1
    endif
    
    if s[0] eq centcomp then begin
      maps_tmp.is_set2=0
      maps_tmp.is_set3=1
      maps_tmp.i2=maps.i3
      maps_tmp.i3=maps.i2
      maps_tmp.v2=maps.v3
      maps_tmp.v3=maps.v2
      maps_tmp.sigma2=maps.sigma3
      maps_tmp.sigma3=maps.sigma2
      maps_tmp.f2=maps.f3
      maps_tmp.f3=maps.f2
      models_tmp.c2=models.c3
      models_tmp.c3=models.c2
    endif
  ENDIF
  
  IF n_lines eq 3 then begin
    if s[1] eq 0 then begin
      maps_tmp.i1=maps.i1
      maps_tmp.i1=maps.sigma1
      maps_tmp.i1=maps.v1
      maps_tmp.i1=maps.f1
      models_tmp.c1=models.c1
    endif
    if s[1] eq 1 then begin
      maps_tmp.i1=maps.i2
      maps_tmp.i1=maps.sigma2
      maps_tmp.i1=maps.v2
      maps_tmp.i1=maps.f2
      models_tmp.c1=models.c2
    endif
    if s[1] eq 2 then begin
      maps_tmp.i1=maps.i3
      maps_tmp.i1=maps.sigma3
      maps_tmp.i1=maps.v3
      maps_tmp.i1=maps.f3
      models_tmp.c1=models.c3
    endif
    if s[0] eq 0 then begin
      maps_tmp.i2=maps.i1
      maps_tmp.i2=maps.sigma1
      maps_tmp.i2=maps.v1
      maps_tmp.i2=maps.f1
      models_tmp.c2=models.c1
    endif
    if s[0] eq 1 then begin
      maps_tmp.i2=maps.i2
      maps_tmp.i2=maps.sigma2
      maps_tmp.i2=maps.v2
      maps_tmp.i2=maps.f2
      models_tmp.c2=models.c2
    endif
    if s[0] eq 2 then begin
      maps_tmp.i2=maps.i3
      maps_tmp.i2=maps.sigma3
      maps_tmp.i2=maps.v3
      maps_tmp.i2=maps.f3
      models_tmp.c2=models.c3
    endif
    if s[2] eq 0 then begin
      maps_tmp.i3=maps.i1
      maps_tmp.i3=maps.sigma1
      maps_tmp.i3=maps.v1
      maps_tmp.i3=maps.f1
      models_tmp.c3=models.c1
    endif
    if s[2] eq 1 then begin
      maps_tmp.i3=maps.i2
      maps_tmp.i3=maps.sigma2
      maps_tmp.i3=maps.v2
      maps_tmp.i3=maps.f2
      models_tmp.c3=models.c2
    endif
    if s[2] eq 2 then begin
      maps_tmp.i3=maps.i3
      maps_tmp.i3=maps.sigma3
      maps_tmp.i3=maps.v3
      maps_tmp.i3=maps.f3
      models_tmp.c3=models.c3
    endif
  ENDIF
  
  maps=maps_tmp
  models=models_tmp
  
;  
;  if n_lines eq 2 then begin
;      if asymmetry gt 0 then begin
;        maps.is_set3=1
;        tmp=maps.i2
;        maps.i2=maps.i3
;        maps.i3=tmp
;        
;        tmp=maps.v2
;        maps.v2=maps.v3
;        maps.v3=tmp
;        
;        tmp=maps.sigma2
;        maps.sigma2=maps.sigma3
;        maps.sigma3=tmp
;        
;        tmp=maps.f2
;        maps.f2=maps.f3
;        maps.f3=tmp
;        
;        tmp=models.c2
;        models.c2=models.c3
;        models.c3=tmp
;        
;      endif else maps.is_set2=1
;    endif
;    
;    
;    if n_lines eq 3 then begin
;      maps.is_set2=1
;      maps.is_set3=1
;      if maps.v2 gt maps.v3 then begin
;        maps.is_set3=1
;        tmp=maps.i2
;        maps.i2=maps.i3
;        maps.i3=tmp
;        
;        tmp=maps.v2
;        maps.v2=maps.v3
;        maps.v3=tmp
;        
;        tmp=maps.sigma2
;        maps.sigma2=maps.sigma3
;        maps.sigma3=tmp
;        
;        tmp=maps.f2
;        maps.f2=maps.f3
;        maps.f3=tmp
;        
;        tmp=models.c2
;        models.c2=models.c3
;        models.c3=tmp
;      endif
;    endif


END




PRO ks_fitting, xscale, prof, moments=moments, snr=snr, contin=contin, out_models=out_models, manual_params=manual_params, $
                      n_lines=n_lines, out_maps=out_maps, inst_vel=inst_vel, prof_type=prof_type, method=method
  ; Процедура, осуществляющая фиттинг текущего профиля prof
  ; Нужны входные данные (помимо шкалы длин волн xscale и профиля prof) для автоматического определения параметров фиттинга - moments = [mom1,mom2,mom3,mom4], snr=S/N,$
  ; contin = уровень континуума, inst_vel - инструментальный контур, prof_type - тип профиля (Фойгт - 0, гаусс - 1),
  ; MANUAL_PARAMS задает параметры фиттинга вручную =>  moments, snr и contin не нужны
  ; method - MPFIT или GENFIT
  ; CONTIN - ПЕРЕЗАПИСЫВАЕТСЯ!!! 
  COMMON KS_FOR_MPFIT
  
  nz=n_elements(xscale)
  
  out_maps={f1:!Values.F_NAN, i1:!Values.F_NAN, v1:!Values.F_NAN, sigma1:!Values.F_NAN, f2:!Values.F_NAN, i2:!Values.F_NAN,$
            v2:!Values.F_NAN, sigma2:!Values.F_NAN, f3:!Values.F_NAN, i3:!Values.F_NAN, v3:!Values.F_NAN, $
            sigma3:!Values.F_NAN, is_set1:0l,is_set2:0l,is_set3:0l,flux:!Values.F_NAN, contin:!Values.F_NAN, $
            resid:!Values.F_NAN}
            tmp=fltarr(nz)
            tmp[*]=!Values.F_NAN
  out_models={c1:tmp, c2:tmp, c3:tmp, resid: tmp}
  
  
  IF n_elements(MANUAL_PARAMS) EQ 0 THEN BEGIN
    ; ===== If we should get initial parameters automatically 
    
    moments_struct={mom1:moments[0],mom2:moments[1],mom3:moments[2],mom4:moments[3],snr:snr}
    ini_comps=KS_GET_INITIAL_COMPS(xscale, prof, moments_struct, contin)
    n_lines=total(ini_comps.comps)
    if N_lines eq 0 then return
    tmp=[0,0,0]
    params_struct={comps: ini_comps.comps,ampl: ini_comps.ampl,cent: ini_comps.cent, fwhm: ini_comps.fwhm,$
           setmax_ampl: ini_comps.setmax_ampl,setmax_cent: ini_comps.setmax_cent, setmax_fwhm: ini_comps.setmax_fwhm,$
           setmin_ampl: ini_comps.setmin_ampl,setmin_cent: ini_comps.setmin_cent, setmin_fwhm: ini_comps.setmin_fwhm,$
           fixampl: tmp,fixcent: tmp, fixfwhm: tmp,$
           max_ampl: ini_comps.max_ampl,max_cent: ini_comps.max_cent, max_fwhm: ini_comps.max_fwhm,$
           min_ampl: ini_comps.min_ampl,min_cent: ini_comps.min_cent, min_fwhm: ini_comps.min_fwhm,$
           cont: ini_comps.cont,setmax_cont: ini_comps.setmax_cont,setmin_cont: ini_comps.setmin_cont,$
           max_cont: ini_comps.max_cont,min_cont: ini_comps.min_cont,fixcont: 0}
    
      
  ENDIF ELSE BEGIN
    ; ===== If we set initial parameters manually
    
    N_lines=total(manual_params.comps)
    if N_lines eq 0 then return
    params_struct=manual_params
  ENDELSE
    param=fltarr(N_lines*3+1) ; 1 - cont
    parinfo=replicate({parinfo_str,fixed:0L,limits:fltarr(2),limited:lonarr(2)},N_lines*3+1)
    rec=where(params_struct.comps eq 1)
    for ind=0,N_lines-1 do begin
      param[ind*3]=double((params_struct.ampl)[rec[ind]])
      param[ind*3+1]=double((params_struct.cent)[rec[ind]])
      param[ind*3+2]=double((params_struct.fwhm)[rec[ind]])
      
      parinfo[ind*3].limited=[(params_struct.setmin_ampl)[rec[ind]],(params_struct.setmax_ampl)[rec[ind]]]
      parinfo[ind*3].limits=[(params_struct.min_ampl)[rec[ind]],(params_struct.max_ampl)[rec[ind]]]
      parinfo[ind*3].fixed=(params_struct.fixampl)[rec[ind]]
      parinfo[ind*3+1].limited=[(params_struct.setmin_cent)[rec[ind]],(params_struct.setmax_cent)[rec[ind]]]
      parinfo[ind*3+1].limits=[(params_struct.min_cent)[rec[ind]],(params_struct.max_cent)[rec[ind]]]
      parinfo[ind*3+1].fixed=(params_struct.fixcent)[rec[ind]]
      parinfo[ind*3+2].limited=[(params_struct.setmin_fwhm)[rec[ind]],(params_struct.setmax_fwhm)[rec[ind]]]
      parinfo[ind*3+2].limits=[(params_struct.min_fwhm)[rec[ind]],(params_struct.max_fwhm)[rec[ind]]]
      parinfo[ind*3+2].fixed=(params_struct.fixfwhm)[rec[ind]]
    endfor
    param[N_lines*3]=double(params_struct.cont)
    parinfo[N_lines*3].fixed=params_struct.fixcont
    parinfo[N_lines*3].limited=[params_struct.setmin_cont,params_struct.setmax_cont]
    parinfo[N_lines*3].limits=[params_struct.min_cont,params_struct.max_cont]
    contin=params_struct.cont
     
    
      
    err = sqrt(1000.D + abs(prof-contin))
  
    inst_fwhm_vel = inst_vel
    current_method = method
    res=mpfitfun('KS_FIT_FUNC',xscale,prof,err,param,yfit=yfit,parinfo=parinfo,/quiet,status=retstat)
    if prof_type eq 0 then begin
      out_models.c1=ks_voigt(xscale,[res[0],res[1],res[2]],inst_vel=inst_vel)
      if n_lines gt 1 then begin
        out_models.c2=ks_voigt(xscale,[res[3],res[4],res[5]],inst_vel=inst_vel)
        if n_lines eq 3 then out_models.c3=ks_voigt(xscale,[res[6],res[7],res[8]],inst_vel=inst_vel)
      endif
    endif else begin
      out_models.c1=gaussian(xscale,[res[0],res[1],res[2]/2.35482])
      if n_lines gt 1 then begin
        out_models.c2=gaussian(xscale,[res[3],res[4],res[5]/2.35482])
        if n_lines eq 3 then out_models.c3=gaussian(xscale,[res[6],res[7],res[8]/2.35482])
      endif
    endelse
    
    out_models.resid=prof-yfit
    
    out_maps.resid=total(abs(out_models.resid),/nan)
    out_maps.f1=total(out_models.c1,/nan)
    out_maps.f2=total(out_models.c2,/nan)
    out_maps.f3=total(out_models.c3,/nan)
    
    
    out_maps.i1=res[0]
    out_maps.v1=res[1]
    out_maps.sigma1=res[2]/2.35482
    out_maps.is_set1=1
    out_maps.is_set2=0
    out_maps.is_set3=0
    i=3
    if N_lines gt 1 then begin
      out_maps.i2=res[i]
      out_maps.v2=res[i+1]
      out_maps.sigma2=res[i+2]/2.35482
      i+=3
      out_maps.is_set2=1 
    endif
    if N_lines eq 3 then begin
      out_maps.i3=res[i]
      out_maps.v3=res[i+1]
      out_maps.sigma3=res[i+2]/2.35482
      i+=3
      out_maps.is_set3=1
    endif
    out_maps.contin=res[i]
    out_maps.flux=total(yfit)-res[i]*nz
    
    if n_lines gt 1 then KS_SORT_FIT_COMPS, out_maps, out_models, n_lines=n_lines
END