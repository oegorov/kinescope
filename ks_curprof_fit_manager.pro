PRO KS_CURPROFMAN_TRIGGER_SENS_SHOWRES, state
  COMMON KS_CURPROF_MANAGER
  for i=0,5 do WIDGET_CONTROL,ks_curprof_showres[i].obj,sens=state
END


PRO KS_CURPROFMAN_SHOW_PROF

  ; Отображает текущий профиль на панели с результатами
        COMMON KS_CURPROF_MANAGER
        COMMON KS_DATA
        COMMON KS_ANALYSIS
        
        xim=curprofman_pix.x
        yim=curprofman_pix.y
        
        nz=sxpar(header_cub,"NAXIS3")
        nx=sxpar(header_cub,"NAXIS1")
        ny=sxpar(header_cub,"NAXIS2")
        refpix=sxpar(header_cub,"CRPIX3")
        xdelt=sxpar(header_cub,"CDELT3")
        if refpix eq 0 then refpix=1
        xscale=(findgen(nz)-refpix+1)*xdelt+sxpar(header_cub,"CRVAL3")
        prof=fltarr(nz)
        prof[*]=cub[xim,yim,*]
        
        WIDGET_CONTROL, ks_curprof_disp.obj, GET_VALUE = index
        WSet, index
        Erase, color=cgcolor('white')
        
        if axes_info.ztype eq -1 then xtit = "X (unrecognized units)"
        if axes_info.ztype eq 0 or axes_info.ztype eq 1 then xtit = "Velocity, km s$\up-1$"
        if axes_info.ztype eq 1 then xscale = xscale/1e3
        
        if axes_info.ztype ge 2 and axes_info.ztype le 4 then xtit = "Wavelength, "+axes_info.zunit[axes_info.ztype+1]
        if axes_info.ztype ge 5 then xtit = "Frequency, "+axes_info.zunit[axes_info.ztype+1]
        
        cgplot,xscale,prof,$
           ytit = "Intensity, "+ axes_info.I, xtit = xtit,xr=ks_curprofman_inicomps.xr,yr=ks_curprofman_inicomps.yr,$
           xst=1,yst=1, background="white",psym=10,title="X: "+string(xim,format="(I0)")+"; Y: "+string(yim,format="(I0)")
           
           ks_curprofman_pos_on_disp=[!x.window,!y.window]
    
    if fit_results_maps.fitted[xim,yim] eq 1 then fit_done=1 else fit_done=0 
    
    if fit_done then begin
      snr=fit_results_maps.snr[xim,yim]
      nocont=0
      if finite(fit_results_maps.contin[xim,yim]) then this_contin=fit_results_maps.contin[xim,yim] else begin
        this_contin=0
        nocont=1
      endelse 
    endif else begin
      snr=moms_on_fly.snr[xim,yim]
      if finite(moms_on_fly.contin[xim,yim]) then this_contin=moms_on_fly.contin[xim,yim] else this_contin=0
    endelse
    
    WIDGET_CONTROL,ks_curprofman_monitors[2].obj,set_value=string(snr,format="(F0.1)")
    format="(F0.2)"
    IF fit_done then begin
       KS_CURPROFMAN_TRIGGER_SENS_SHOWRES,1
       if nocont eq 0 then begin
        WIDGET_CONTROL,ks_curprof_showres[1].obj,set_but=ks_curprof_showres[1].val
       endif else begin
        WIDGET_CONTROL,ks_curprof_showres[1].obj,set_but=0
        WIDGET_CONTROL,ks_curprof_showres[1].obj,sens=0
       endelse
       
         if fit_results_maps.is_set1[xim,yim] eq 1 then begin
           WIDGET_CONTROL,ks_curprof_showres[3].obj,set_but=ks_curprof_showres[3].val
           if ks_curprof_showres[3].val then cgoplot,xscale,fit_results_model.c1[xim,yim,*]+this_contin,color="green"
         endif else begin
           WIDGET_CONTROL,ks_curprof_showres[3].obj,set_but=0
           WIDGET_CONTROL,ks_curprof_showres[3].obj,sens=0
         endelse
         
         
         if fit_results_maps.is_set2[xim,yim] eq 1 then begin
           WIDGET_CONTROL,ks_curprof_showres[4].obj,set_but=ks_curprof_showres[4].val
           if ks_curprof_showres[4].val then cgoplot,xscale,fit_results_model.c2[xim,yim,*]+this_contin,color="blue"
         endif else begin
           WIDGET_CONTROL,ks_curprof_showres[4].obj,set_but=0
           WIDGET_CONTROL,ks_curprof_showres[4].obj,sens=0
         endelse
         
         if fit_results_maps.is_set3[xim,yim] eq 1 then begin
           WIDGET_CONTROL,ks_curprof_showres[5].obj,set_but=ks_curprof_showres[5].val
           if ks_curprof_showres[5].val then cgoplot,xscale,fit_results_model.c3[xim,yim,*]+this_contin,color="red"
         endif else begin
           WIDGET_CONTROL,ks_curprof_showres[5].obj,set_but=0
           WIDGET_CONTROL,ks_curprof_showres[5].obj,sens=0
         endelse
         
         
       if nocont ne 1 and ks_curprof_showres[1].val eq 1 then cgoplot,xscale,replicate(this_contin,nz),color="brown"
       
        WIDGET_CONTROL,ks_curprof_showres[2].obj,set_but=ks_curprof_showres[2].val
        WIDGET_CONTROL,ks_curprof_showres[0].obj,set_but=ks_curprof_showres[0].val
        
        if ks_curprof_showres[2].val eq 1 then cgoplot,xscale,(fit_results_model.resid[xim,yim,*]+this_contin),color="gold"
        if ks_curprof_showres[0].val eq 1 then begin
          tot=replicate(this_contin,nz)
          
          if ks_curprof_showres[3].val eq 1 and fit_results_maps.is_set1[xim,yim] then tot+=fit_results_model.c1[xim,yim,*]
          if ks_curprof_showres[4].val eq 1 and fit_results_maps.is_set2[xim,yim] then tot+=fit_results_model.c2[xim,yim,*]
          if ks_curprof_showres[5].val eq 1 and fit_results_maps.is_set3[xim,yim] then tot+=fit_results_model.c3[xim,yim,*]
          cgoplot,xscale,tot,color="magenta"
       endif
 ;=====>      print,this_contin,moms_on_fly.contin[xim,yim],fit_results_maps.contin[xim,yim],xim,yim
       ; Отображение результатов фиттинга (текстовых)
       
       flux=fit_results_maps.flux[xim,yim]
       resid=abs(fit_results_maps.resid[xim,yim])/flux*snr
       WIDGET_CONTROL,ks_curprofman_monitors[3].obj,set_value=string(flux,format=format)
       WIDGET_CONTROL,ks_curprofman_monitors[4].obj,set_value=string(this_contin,format=format)
       WIDGET_CONTROL,ks_curprofman_monitors[5].obj,set_value=string(resid,format=format)
       if fit_results_maps.is_set1[xim,yim] eq 1 then begin
         WIDGET_CONTROL,ks_curprofman_monitors[6].obj,set_value=string(fit_results_maps.v1[xim,yim],format=format)
         WIDGET_CONTROL,ks_curprofman_monitors[7].obj,set_value=string(fit_results_maps.sigma1[xim,yim],format=format)+$
         " ("+string(fit_results_maps.sigma1[xim,yim]*2.35482,format=format)+")"
         WIDGET_CONTROL,ks_curprofman_monitors[8].obj,set_value=string(fit_results_maps.i1[xim,yim],format=format)
         WIDGET_CONTROL,ks_curprofman_monitors[9].obj,set_value=string(fit_results_maps.f1[xim,yim],format=format)
       endif else begin
        for i=0,3 do WIDGET_CONTROL,ks_curprofman_monitors[6+i].obj,set_value="none"
       endelse
       if fit_results_maps.is_set2[xim,yim] eq 1 then begin
         WIDGET_CONTROL,ks_curprofman_monitors[10].obj,set_value=string(fit_results_maps.v2[xim,yim],format=format)
         WIDGET_CONTROL,ks_curprofman_monitors[11].obj,set_value=string(fit_results_maps.sigma2[xim,yim],format=format)+$
         " ("+string(fit_results_maps.sigma2[xim,yim]*2.35482,format=format)+")"
         WIDGET_CONTROL,ks_curprofman_monitors[12].obj,set_value=string(fit_results_maps.i2[xim,yim],format=format)
         WIDGET_CONTROL,ks_curprofman_monitors[13].obj,set_value=string(fit_results_maps.f2[xim,yim],format=format)
       endif else begin
        for i=0,3 do WIDGET_CONTROL,ks_curprofman_monitors[10+i].obj,set_value="none"
       endelse
       if fit_results_maps.is_set3[xim,yim] eq 1 then begin
         WIDGET_CONTROL,ks_curprofman_monitors[14].obj,set_value=string(fit_results_maps.v3[xim,yim],format=format)
         WIDGET_CONTROL,ks_curprofman_monitors[15].obj,set_value=string(fit_results_maps.sigma3[xim,yim],format=format)+$
         " ("+string(fit_results_maps.sigma3[xim,yim]*2.35482,format=format)+")"
         WIDGET_CONTROL,ks_curprofman_monitors[16].obj,set_value=string(fit_results_maps.i3[xim,yim],format=format)
         WIDGET_CONTROL,ks_curprofman_monitors[17].obj,set_value=string(fit_results_maps.f3[xim,yim],format=format)
       endif else begin
        for i=0,3 do WIDGET_CONTROL,ks_curprofman_monitors[14+i].obj,set_value="none"
       endelse
    ENDIF ELSE BEGIN
      KS_CURPROFMAN_TRIGGER_SENS_SHOWRES,0
      WIDGET_CONTROL,ks_curprofman_monitors[3].obj,set_value="none"
      for i=0,12 do WIDGET_CONTROL,ks_curprofman_monitors[5+i].obj,set_value="none"
      WIDGET_CONTROL,ks_curprofman_monitors[4].obj,set_value=string(this_contin,format=format)
    ENDELSE
        
        
        
 END       
        
        
        
        
        
        
;
;PRO KS_CURPROFMAN_SHOWCOMPS_ADJUST, index
;  COMMON KS_CURPROF_MANAGER
;  xim=curprofman_pix.x
;  yim=curprofman_pix.y
;  IF total(fit_results_maps.fitted[xim,yim],/nan) eq 0 then begin
;    *prof_storage[index].show_on=intarr(6)
;    return
;  ENDIF
;  (*prof_storage[index].show_on)[0]=1
;  (*prof_storage[index].show_on)[1]=0
;  (*prof_storage[index].show_on)[2]=1
;  IF (*prof_storage[index].fitted_isset)[0] eq 1 then (*prof_storage[index].show_on)[3]=1 else (*prof_storage[index].show_on)[3]=0
;  IF (*prof_storage[index].fitted_isset)[1] eq 1 then (*prof_storage[index].show_on)[4]=1 else (*prof_storage[index].show_on)[4]=0
;  IF (*prof_storage[index].fitted_isset)[2] eq 1 then (*prof_storage[index].show_on)[5]=1 else (*prof_storage[index].show_on)[5]=0 
;END




PRO KS_CURPROFMAN_FIT_RUN
  ; Running fitting of integral profiles
  COMMON KS_CURPROF_MANAGER
  COMMON KS_DATA
  COMMON KS_ANALYSIS
   
   xim=curprofman_pix.x
   yim=curprofman_pix.y
   nz=sxpar(header_cub,"NAXIS3")
   refpix=sxpar(header_cub,"CRPIX3")
   xdelt=sxpar(header_cub,"CDELT3")
   if refpix eq 0 then refpix=1
   xscale=(findgen(nz)-refpix+1)*xdelt+sxpar(header_cub,"CRVAL3")
   if axes_info.ztype eq 1 then xscale = xscale/1e3
   prof=fltarr(nz)
   prof[*]=cub[xim,yim,*]
   
   KS_CURPROFMAN_READ_COMPSVAL
   
   params={comps: ks_curprofman_inicomps.comps,ampl: ks_curprofman_inicomps.ampl,cent: ks_curprofman_inicomps.cent, fwhm: ks_curprofman_inicomps.fwhm,$
           setmax_ampl: ks_curprofman_inicomps.setmax_ampl,setmax_cent: ks_curprofman_inicomps.setmax_cent, setmax_fwhm: ks_curprofman_inicomps.setmax_fwhm,$
           setmin_ampl: ks_curprofman_inicomps.setmin_ampl,setmin_cent: ks_curprofman_inicomps.setmin_cent, setmin_fwhm: ks_curprofman_inicomps.setmin_fwhm,$
           fixampl: ks_curprofman_inicomps.fix_ampl,fixcent: ks_curprofman_inicomps.fix_cent, fixfwhm: ks_curprofman_inicomps.fix_fwhm,$
           max_ampl: ks_curprofman_inicomps.max_ampl,max_cent: ks_curprofman_inicomps.max_cent, max_fwhm: ks_curprofman_inicomps.max_fwhm,$
           min_ampl: ks_curprofman_inicomps.min_ampl,min_cent: ks_curprofman_inicomps.min_cent, min_fwhm: ks_curprofman_inicomps.min_fwhm,$
           cont: ks_curprofman_inicomps.cont,setmax_cont: ks_curprofman_inicomps.setmax_cont,setmin_cont: ks_curprofman_inicomps.setmin_cont,$
           max_cont: ks_curprofman_inicomps.max_cont,min_cont: ks_curprofman_inicomps.min_cont,fixcont: ks_curprofman_inicomps.fix_cont} 
   KS_FITTING, xscale, prof, contin=contin, out_models=out_models,manual_params=params, $
              out_maps=out_maps, inst_vel=inst_fwhm.vel, prof_type=fit_proftype, method=ks_curprofman_method
   
   
          fit_results_maps.i1[xim,yim]=out_maps.i1
          fit_results_maps.i2[xim,yim]=out_maps.i2
          fit_results_maps.i3[xim,yim]=out_maps.i3
          fit_results_maps.v1[xim,yim]=out_maps.v1
          fit_results_maps.v2[xim,yim]=out_maps.v2
          fit_results_maps.v3[xim,yim]=out_maps.v3
          fit_results_maps.f1[xim,yim]=out_maps.f1
          fit_results_maps.f2[xim,yim]=out_maps.f2
          fit_results_maps.f3[xim,yim]=out_maps.f3
          fit_results_maps.sigma1[xim,yim]=sqrt((out_maps.sigma1^2-extern_disp.vel^2) > 0)
          fit_results_maps.sigma2[xim,yim]=sqrt((out_maps.sigma2^2-extern_disp.vel^2) > 0)
          fit_results_maps.sigma3[xim,yim]=sqrt((out_maps.sigma3^2-extern_disp.vel^2) > 0)
          fit_results_maps.is_set1[xim,yim]=out_maps.is_set1
          fit_results_maps.is_set2[xim,yim]=out_maps.is_set2
          fit_results_maps.is_set3[xim,yim]=out_maps.is_set3
          fit_results_maps.fitted[xim,yim]=1
          fit_results_maps.contin[xim,yim]=out_maps.contin
          fit_results_maps.flux[xim,yim]=out_maps.flux
          fit_results_maps.resid[xim,yim]=out_maps.resid
          
          
          
          fit_results_model.c1[xim,yim,*]=out_models.c1
          fit_results_model.c2[xim,yim,*]=out_models.c2
          fit_results_model.c3[xim,yim,*]=out_models.c3
          fit_results_model.resid[xim,yim,*]=out_models.resid
          
          
          ;creation of total flux map
          tmp=0
          if fit_results_maps.is_set1[xim,yim] then tmp+=fit_results_maps.f1[xim,yim]
          if fit_results_maps.is_set2[xim,yim] then tmp+=fit_results_maps.f2[xim,yim]
          if fit_results_maps.is_set3[xim,yim] then tmp+=fit_results_maps.f3[xim,yim]
          if (fit_results_maps.is_set1[xim,yim]+fit_results_maps.is_set2[xim,yim]+fit_results_maps.is_set3[xim,yim]) eq 0 then $
              fit_results_maps.f_tot[xim,yim]=!Values.F_NAN else  fit_results_maps.f_tot[xim,yim]=tmp
          
          ;creation of shift map
          nomask=0
          if fit_results_maps.is_set1[xim,yim] eq 1 and fit_results_maps.is_set2[xim,yim] eq 1 and fit_results_maps.is_set3[xim,yim] eq 0 then begin
             fit_results_maps.v_shift[xim,yim]=fit_results_maps.v2[xim,yim]-fit_results_maps.v1[xim,yim]
             nomask=1
          endif
          if fit_results_maps.is_set1[xim,yim] eq 1 and fit_results_maps.is_set2[xim,yim] eq 0 and fit_results_maps.is_set3[xim,yim] eq 1 then begin
             fit_results_maps.v_shift[xim,yim]=fit_results_maps.v3[xim,yim]-fit_results_maps.v1[xim,yim]
             nomask=1
          endif
          if fit_results_maps.is_set1[xim,yim] eq 0 and fit_results_maps.is_set2[xim,yim] eq 1 and fit_results_maps.is_set3[xim,yim] eq 1 then begin
             arr=[fit_results_maps.v2[xim,yim]-fit_results_maps.v3[xim,yim],$
                  fit_results_maps.v3[xim,yim]-fit_results_maps.v2[xim,yim]]/2.
             m=max([fit_results_maps.f2[xim,yim],fit_results_maps.f3[xim,yim]],mpos)
             fit_results_maps.v_shift[xim,yim]=arr[mpos]
             nomask=1
          endif
          if fit_results_maps.is_set1[xim,yim] eq 1 and fit_results_maps.is_set2[xim,yim] eq 1 and fit_results_maps.is_set3[xim,yim] eq 1 then begin
             arr=[fit_results_maps.v2[xim,yim]-fit_results_maps.v1[xim,yim],$
                  fit_results_maps.v3[xim,yim]-fit_results_maps.v1[xim,yim]]
             m=max([fit_results_maps.f2[xim,yim],fit_results_maps.f3[xim,yim]],mpos)
             fit_results_maps.v_shift[xim,yim]=arr[mpos]
             nomask=1
          endif
           if nomask eq 0 then fit_results_maps.v_shift[xim,yim]=!Values.F_NAN
          KS_CURPROFMAN_SHOW_PROF
END


PRO KS_CURPROFMAN_READ_COMPSVAL
  ; Считываем значения параметров фиттинга компонент
  COMMON KS_CURPROF_MANAGER
  WIDGET_CONTROL, ks_curprofman_comp1_a, get_value=p0
  WIDGET_CONTROL, ks_curprofman_comp2_a, get_value=p1
  WIDGET_CONTROL, ks_curprofman_comp3_a, get_value=p2
  ks_curprofman_inicomps.ampl=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_curprofman_comp1_f, get_value=p0
  WIDGET_CONTROL, ks_curprofman_comp2_f, get_value=p1
  WIDGET_CONTROL, ks_curprofman_comp3_f, get_value=p2
  ks_curprofman_inicomps.fwhm=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_curprofman_comp1_c, get_value=p0
  WIDGET_CONTROL, ks_curprofman_comp2_c, get_value=p1
  WIDGET_CONTROL, ks_curprofman_comp3_c, get_value=p2
  ks_curprofman_inicomps.cent=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_curprofman_comp1_amin, get_value=p0
  WIDGET_CONTROL, ks_curprofman_comp2_amin, get_value=p1
  WIDGET_CONTROL, ks_curprofman_comp3_amin, get_value=p2
  ks_curprofman_inicomps.min_ampl=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_curprofman_comp1_fmin, get_value=p0
  WIDGET_CONTROL, ks_curprofman_comp2_fmin, get_value=p1
  WIDGET_CONTROL, ks_curprofman_comp3_fmin, get_value=p2
  ks_curprofman_inicomps.min_fwhm=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_curprofman_comp1_cmin, get_value=p0
  WIDGET_CONTROL, ks_curprofman_comp2_cmin, get_value=p1
  WIDGET_CONTROL, ks_curprofman_comp3_cmin, get_value=p2
  ks_curprofman_inicomps.min_cent=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_curprofman_comp1_amax, get_value=p0
  WIDGET_CONTROL, ks_curprofman_comp2_amax, get_value=p1
  WIDGET_CONTROL, ks_curprofman_comp3_amax, get_value=p2
  ks_curprofman_inicomps.max_ampl=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_curprofman_comp1_fmax, get_value=p0
  WIDGET_CONTROL, ks_curprofman_comp2_fmax, get_value=p1
  WIDGET_CONTROL, ks_curprofman_comp3_fmax, get_value=p2
  ks_curprofman_inicomps.max_fwhm=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_curprofman_comp1_cmax, get_value=p0
  WIDGET_CONTROL, ks_curprofman_comp2_cmax, get_value=p1
  WIDGET_CONTROL, ks_curprofman_comp3_cmax, get_value=p2
  ks_curprofman_inicomps.max_cent=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_curprofman_cont, get_value=p0
  WIDGET_CONTROL, ks_curprofman_contmin, get_value=p1
  WIDGET_CONTROL, ks_curprofman_contmax, get_value=p2
  ks_curprofman_inicomps.cont=p0
  ks_curprofman_inicomps.min_cont=p1
  ks_curprofman_inicomps.max_cont=p2
  
END

PRO KS_CURPROFMAN_SAVE_PS
        ; Сохраняет профиль в eps-файл
        COMMON KS_CURPROF_MANAGER
        COMMON KS_DATA
        win_title="Choose EPS-file to save line profile"

        file=DIALOG_PICKFILE(title=win_title,default_extension='ps',filter="ps",/write)
          if (file eq '') then return
          fdecomp,file,disk,psfile_dir,psfile_name,qual
          if (psfile_name eq '') then return
          cd,disk+psfile_dir
        
        ps_start,file,/quiet       
          if axes_info.ztype eq -1 then xtit = "X (unrecognized units)"
          if axes_info.ztype eq 0 or axes_info.ztype eq 1 then xtit = "Velocity, km s$\up-1$"
          if axes_info.ztype ge 2 and axes_info.ztype le 4 then xtit = "Wavelength, "+axes_info.zunit[axes_info.ztype+1]
          if axes_info.ztype ge 5 then xtit = "Frequency, "+axes_info.zunit[axes_info.ztype+1]
          
          
          xim=curprofman_pix.x
          yim=curprofman_pix.y
          nz=sxpar(header_cub,"NAXIS3")
          refpix=sxpar(header_cub,"CRPIX3")
          xdelt=sxpar(header_cub,"CDELT3")
          if refpix eq 0 then refpix=1
          xscale=(findgen(nz)-refpix+1)*xdelt+sxpar(header_cub,"CRVAL3")
          if axes_info.ztype eq 1 then xscale = xscale/1e3
          prof=fltarr(nz)
          prof[*]=cub[xim,yim,*]
          nocont=0
          if finite(fit_results_maps.contin[xim,yim]) then this_contin=fit_results_maps.contin[xim,yim] else begin
            this_contin=0
            nocont=1
          endelse
          cgplot,xscale,prof, $
              title="X: "+string(xim,format="(I0)")+"; Y: "+string(yim,format="(I0)"),$
               ytit = "Intensity, "+ axes_info.I, xtit = xtit,xr=ks_curprofman_inicomps.xr,yr=ks_curprofman_inicomps.yr,$
               xst=1,yst=1, psym=10,charsize=1.2
             
              if fit_results_maps.fitted[xim,yim] eq 1 then fit_done=1 else fit_done=0 
      
              IF fit_done then begin
                 if fit_results_maps.is_set1[xim,yim] eq 1 and ks_curprof_showres[3].val then cgoplot,xscale,fit_results_model.c1[xim,yim,*]+this_contin,color="green"
                 if fit_results_maps.is_set2[xim,yim] eq 1 and ks_curprof_showres[4].val then cgoplot,xscale,fit_results_model.c2[xim,yim,*]+this_contin,color="blue"
                 if fit_results_maps.is_set3[xim,yim] eq 1 and ks_curprof_showres[5].val then cgoplot,xscale,fit_results_model.c3[xim,yim,*]+this_contin,color="red"
                 if nocont ne 1 and ks_curprof_showres[1].val eq 1 then cgoplot,xscale,replicate(this_contin,nz),color="brown"
                 if ks_curprof_showres[2].val eq 1 then cgoplot,xscale,(fit_results_model.resid[xim,yim,*]+this_contin),color="gold"
                 if ks_curprof_showres[0].val eq 1 then begin
                    tot=replicate(this_contin,nz)
                    
                    if ks_curprof_showres[3].val eq 1 and fit_results_maps.is_set1[xim,yim] then tot+=fit_results_model.c1[xim,yim,*]
                    if ks_curprof_showres[4].val eq 1 and fit_results_maps.is_set2[xim,yim] then tot+=fit_results_model.c2[xim,yim,*]
                    if ks_curprof_showres[5].val eq 1 and fit_results_maps.is_set3[xim,yim] then tot+=fit_results_model.c3[xim,yim,*]
                    cgoplot,xscale,tot,color="magenta"
                 endif
              ENDIF
        ps_end
    
  END  


PRO KS_CURPROFMAN_UPD_COMPS
  COMMON KS_CURPROF_MANAGER
  COMMON KS_DATA
  COMMON KS_ANALYSIS
  
  
  ;Обновляем таблицу с начальными параметрами компонент фиттинга под текущий профиль
;  WIDGET_CONTROL,ks_table_curprofcomp,sens=(1-ks_curprofman_method)
;  WIDGET_CONTROL,ks_curprofman_setcomp1,sens=(1-ks_curprofman_method)
;  WIDGET_CONTROL,ks_curprofman_setcomp2,sens=(1-ks_curprofman_method)
;  WIDGET_CONTROL,ks_curprofman_setcomp3,sens=(1-ks_curprofman_method)
  
  xim=curprofman_pix.x
  yim=curprofman_pix.y
  nz=sxpar(header_cub,"NAXIS3")
  refpix=sxpar(header_cub,"CRPIX3")
  xdelt=sxpar(header_cub,"CDELT3")
  if refpix eq 0 then refpix=1
  xscale=(findgen(nz)-refpix+1)*xdelt+sxpar(header_cub,"CRVAL3")
  if axes_info.ztype eq 1 then xscale = xscale/1e3
  prof=fltarr(nz)
  prof[*]=cub[xim,yim,*]
  
  
  if moms_on_fly.done[xim,yim] eq 0 then begin
    moms=ks_moments(xscale, prof, contin=contin, snr=snr)
    moms_on_fly.mom0[xim,yim]=moms[0]
    moms_on_fly.mom1[xim,yim]=moms[1]
    moms_on_fly.mom2[xim,yim]=moms[2]
    moms_on_fly.mom3[xim,yim]=moms[3]
    moms_on_fly.mom4[xim,yim]=moms[4]
    moms_on_fly.snr[xim,yim]=snr
    moms_on_fly.contin[xim,yim]=contin
    moms_on_fly.done[xim,yim]=1
  endif
  
  
  if finite(moms_on_fly.contin[xim,yim]) then this_contin=moms_on_fly.contin[xim,yim] else this_contin=0
  moments_struct={mom1:moms_on_fly.mom1[xim,yim],mom2:moms_on_fly.mom2[xim,yim],mom3:moms_on_fly.mom3[xim,yim],mom4:moms_on_fly.mom4[xim,yim],snr:moms_on_fly.snr[xim,yim]}
  ini_comps=KS_GET_INITIAL_COMPS(xscale, prof, moments_struct, this_contin)
  
    fit_results_maps.mom0[xim,yim]=moms_on_fly.mom0[xim,yim]
    fit_results_maps.mom1[xim,yim]=moms_on_fly.mom1[xim,yim]
    fit_results_maps.mom2[xim,yim]=moms_on_fly.mom2[xim,yim]
    fit_results_maps.mom3[xim,yim]=moms_on_fly.mom3[xim,yim]
    fit_results_maps.mom4[xim,yim]=moms_on_fly.mom4[xim,yim]
    fit_results_maps.snr[xim,yim]=moms_on_fly.snr[xim,yim]
    if fit_results_maps.fitted[xim,yim] eq 1 then fit_results_maps.contin[xim,yim]=ini_comps.cont
          
    WIDGET_CONTROL,ks_curprofman_setcomp1,set_button=0 > (ini_comps.comps)[0] < 1
    WIDGET_CONTROL,ks_curprofman_setcomp2,set_button=0 > (ini_comps.comps)[1] < 1
    WIDGET_CONTROL,ks_curprofman_setcomp3,set_button=0 > (ini_comps.comps)[2] < 1
    
    WIDGET_CONTROL,ks_curprofman_comp1_a,set_value=string((ini_comps.ampl)[0],format="(G0.4)")
    WIDGET_CONTROL,ks_curprofman_comp2_a,set_value=string((ini_comps.ampl)[1],format="(G0.4)")
    WIDGET_CONTROL,ks_curprofman_comp3_a,set_value=string((ini_comps.ampl)[2],format="(G0.4)")
    WIDGET_CONTROL,ks_curprofman_comp1_f,set_value=string((ini_comps.fwhm)[0],format="(G0.4)")
    WIDGET_CONTROL,ks_curprofman_comp2_f,set_value=string((ini_comps.fwhm)[1],format="(G0.4)")
    WIDGET_CONTROL,ks_curprofman_comp3_f,set_value=string((ini_comps.fwhm)[2],format="(G0.4)")
    WIDGET_CONTROL,ks_curprofman_comp1_c,set_value=string((ini_comps.cent)[0],format="(G0.4)")
    WIDGET_CONTROL,ks_curprofman_comp2_c,set_value=string((ini_comps.cent)[1],format="(G0.4)")
    WIDGET_CONTROL,ks_curprofman_comp3_c,set_value=string((ini_comps.cent)[2],format="(G0.4)")
    
    WIDGET_CONTROL, ks_curprofman_comp1_amin, set_value=string((ini_comps.min_ampl)[0],format="(G0.4)")
    WIDGET_CONTROL, ks_curprofman_comp2_amin, set_value=string((ini_comps.min_ampl)[1],format="(G0.4)")
    WIDGET_CONTROL, ks_curprofman_comp3_amin, set_value=string((ini_comps.min_ampl)[2],format="(G0.4)")
    
    WIDGET_CONTROL, ks_curprofman_comp1_fmin, set_value=string((ini_comps.min_fwhm)[0],format="(G0.4)")
    WIDGET_CONTROL, ks_curprofman_comp2_fmin, set_value=string((ini_comps.min_fwhm)[1],format="(G0.4)")
    WIDGET_CONTROL, ks_curprofman_comp3_fmin, set_value=string((ini_comps.min_fwhm)[2],format="(G0.4)")
    
    WIDGET_CONTROL, ks_curprofman_comp1_cmin, set_value=string((ini_comps.min_cent)[0],format="(G0.4)")
    WIDGET_CONTROL, ks_curprofman_comp2_cmin, set_value=string((ini_comps.min_cent)[1],format="(G0.4)")
    WIDGET_CONTROL, ks_curprofman_comp3_cmin, set_value=string((ini_comps.min_cent)[2],format="(G0.4)")
    
    WIDGET_CONTROL, ks_curprofman_comp1_amax, set_value=string((ini_comps.max_ampl)[0],format="(G0.4)")
    WIDGET_CONTROL, ks_curprofman_comp2_amax, set_value=string((ini_comps.max_ampl)[1],format="(G0.4)")
    WIDGET_CONTROL, ks_curprofman_comp3_amax, set_value=string((ini_comps.max_ampl)[2],format="(G0.4)")
    
    WIDGET_CONTROL, ks_curprofman_comp1_fmax, set_value=string((ini_comps.max_fwhm)[0],format="(G0.4)")
    WIDGET_CONTROL, ks_curprofman_comp2_fmax, set_value=string((ini_comps.max_fwhm)[1],format="(G0.4)")
    WIDGET_CONTROL, ks_curprofman_comp3_fmax, set_value=string((ini_comps.max_fwhm)[2],format="(G0.4)")
    
    WIDGET_CONTROL, ks_curprofman_comp1_cmax, set_value=string((ini_comps.max_cent)[0],format="(G0.4)")
    WIDGET_CONTROL, ks_curprofman_comp2_cmax, set_value=string((ini_comps.max_cent)[1],format="(G0.4)")
    WIDGET_CONTROL, ks_curprofman_comp3_cmax, set_value=string((ini_comps.max_cent)[2],format="(G0.4)")
        
    ks_curprofman_inicomps.min_ampl=ini_comps.min_ampl
    ks_curprofman_inicomps.min_fwhm=ini_comps.min_fwhm
    ks_curprofman_inicomps.max_ampl=ini_comps.max_ampl
    ks_curprofman_inicomps.max_fwhm=ini_comps.max_fwhm
    ks_curprofman_inicomps.min_cent=ini_comps.min_cent
    ks_curprofman_inicomps.max_cent=ini_comps.max_cent
    ks_curprofman_inicomps.ampl=ini_comps.ampl
    ks_curprofman_inicomps.cent=ini_comps.cent
    ks_curprofman_inicomps.fwhm=ini_comps.fwhm
    ks_curprofman_inicomps.cont=this_contin
    ks_curprofman_inicomps.comps=ini_comps.comps
    ks_curprofman_inicomps.setmin_cont=ini_comps.setmin_cont
    ks_curprofman_inicomps.setmin_cent=ini_comps.setmin_cent
    ks_curprofman_inicomps.setmin_fwhm=ini_comps.setmin_fwhm
    ks_curprofman_inicomps.setmin_ampl=ini_comps.setmin_ampl
    ks_curprofman_inicomps.setmax_cont=ini_comps.setmax_cont
    ks_curprofman_inicomps.setmax_cent=ini_comps.setmax_cent
    ks_curprofman_inicomps.setmax_fwhm=ini_comps.setmax_fwhm
    ks_curprofman_inicomps.setmax_ampl=ini_comps.setmax_ampl
    ks_curprofman_inicomps.min_cont=ini_comps.min_cont
    ks_curprofman_inicomps.min_cent=ini_comps.min_cent
    ks_curprofman_inicomps.min_fwhm=ini_comps.min_fwhm
    ks_curprofman_inicomps.min_ampl=ini_comps.min_ampl
    ks_curprofman_inicomps.max_cont=ini_comps.max_cont
    ks_curprofman_inicomps.max_cent=ini_comps.max_cent
    ks_curprofman_inicomps.max_fwhm=ini_comps.max_fwhm
    ks_curprofman_inicomps.max_ampl=ini_comps.max_ampl
    
    WIDGET_CONTROL, ks_curprofman_comp1_csetmax, set_button=(ini_comps.setmax_cent)[0]
    WIDGET_CONTROL, ks_curprofman_comp1_csetmin, set_button=(ini_comps.setmin_cent)[0]
    WIDGET_CONTROL, ks_curprofman_comp2_csetmax, set_button=(ini_comps.setmax_cent)[1]
    WIDGET_CONTROL, ks_curprofman_comp2_csetmin, set_button=(ini_comps.setmin_cent)[1]
    WIDGET_CONTROL, ks_curprofman_comp3_csetmax, set_button=(ini_comps.setmax_cent)[2]
    WIDGET_CONTROL, ks_curprofman_comp3_csetmin, set_button=(ini_comps.setmin_cent)[2]
    WIDGET_CONTROL, ks_curprofman_comp1_fsetmax, set_button=(ini_comps.setmax_fwhm)[0]
    WIDGET_CONTROL, ks_curprofman_comp1_fsetmin, set_button=(ini_comps.setmin_fwhm)[0]
    WIDGET_CONTROL, ks_curprofman_comp2_fsetmax, set_button=(ini_comps.setmax_fwhm)[1]
    WIDGET_CONTROL, ks_curprofman_comp2_fsetmin, set_button=(ini_comps.setmin_fwhm)[1]
    WIDGET_CONTROL, ks_curprofman_comp3_fsetmax, set_button=(ini_comps.setmax_fwhm)[2]
    WIDGET_CONTROL, ks_curprofman_comp3_fsetmin, set_button=(ini_comps.setmin_fwhm)[2]
    WIDGET_CONTROL, ks_curprofman_comp1_asetmax, set_button=(ini_comps.setmax_ampl)[0]
    WIDGET_CONTROL, ks_curprofman_comp1_asetmin, set_button=(ini_comps.setmin_ampl)[0]
    WIDGET_CONTROL, ks_curprofman_comp2_asetmax, set_button=(ini_comps.setmax_ampl)[1]
    WIDGET_CONTROL, ks_curprofman_comp2_asetmin, set_button=(ini_comps.setmin_ampl)[1]
    WIDGET_CONTROL, ks_curprofman_comp3_asetmax, set_button=(ini_comps.setmax_ampl)[2]
    WIDGET_CONTROL, ks_curprofman_comp3_asetmin, set_button=(ini_comps.setmin_ampl)[2]
    WIDGET_CONTROL, ks_curprofman_contsetmin, set_button=(ini_comps.setmin_cont)
    WIDGET_CONTROL, ks_curprofman_contsetmax, set_button=(ini_comps.setmax_cont)
    
    WIDGET_CONTROL, ks_curprofman_contmin, set_val=string((ini_comps.min_cont),format="(G0.4)")
    WIDGET_CONTROL, ks_curprofman_contmax, set_val=string((ini_comps.max_cont),format="(G0.4)")
    WIDGET_CONTROL, ks_curprofman_cont, set_val=string((ini_comps.cont),format="(G0.4)")
    
    ks_curprofman_inicomps.xr=minmax(xscale)
    intens=abs(max(prof,/nan)-min(prof,/nan))
    ks_curprofman_inicomps.yr=[min(prof,/nan)-intens*0.05,max(prof,/nan)+intens*0.05]
    
END

PRO KS_CURPROFMAN_CURPOS_UPD
    COMMON KS_CURPROF_MANAGER
    COMMON KS_DISPLAY
    COMMON KS_DATA
    nx=sxpar(header_cub,"NAXIS1")
    ny=sxpar(header_cub,"NAXIS2")
    x=curprofman_pix.x
    y=curprofman_pix.y
    WIDGET_CONTROL, ks_curprofman_curpos[0],set_val=x
    WIDGET_CONTROL, ks_curprofman_curpos[1],set_val=y
    IF x eq 0 then WIDGET_CONTROL, ks_curprofman_navi_lt, sens=0 else WIDGET_CONTROL, ks_curprofman_navi_lt, sens=1
    IF y eq 0 then WIDGET_CONTROL, ks_curprofman_navi_dn, sens=0 else WIDGET_CONTROL, ks_curprofman_navi_dn, sens=1
    IF x eq (nx-1) then WIDGET_CONTROL, ks_curprofman_navi_rt, sens=0 else WIDGET_CONTROL, ks_curprofman_navi_rt, sens=1
    IF y eq (ny-1) then WIDGET_CONTROL, ks_curprofman_navi_up, sens=0 else WIDGET_CONTROL, ks_curprofman_navi_up, sens=1
END

PRO KS_CURPROFMAN_CHANGEPOS, x, y
  COMMON KS_DISPLAY
  COMMON KS_CURPROF_MANAGER
  COMMON KS_DATA
  COMMON KS_ANALYSIS
  
  nx=sxpar(header_cub,"NAXIS1")
  ny=sxpar(header_cub,"NAXIS2")
  if x ge 0 and y ge 0 and x lt nx and y lt ny then begin 
    curprofman_pix.x=x
    curprofman_pix.y=y
    x+=0.5
    y+=0.5
    xyad,header_cub,x,y,ra,dec
    mark_points=[{bpos,x:ra, y:dec}]
    sh_type=2
    mark_color="gold"
    ptr_free,curprof_selection.points
    curprof_selection={marker, name: " ", type: sh_type, color: mark_color, points: ptr_new(mark_points)}
    KS_SHOW_IMAGE,mode="cube"
    if curprof_monitor eq 0 then KS_SHOW_IMAGE,mode="result"
    KS_CURPROFMAN_UPD_COMPS
    KS_CURPROFMAN_SHOW_PROF
  endif
  KS_CURPROFMAN_CURPOS_UPD
END


PRO KS_CURPROF_FIT_MANAGER_EVENT, event
  COMMON KS_CURPROF_MANAGER
  COMMON KS_SIZES
  
  
  WIDGET_CONTROL,event.ID,get_uvalue=ev
  
  IF strpos(STRLOWCASE(ev),"show_fit_res_") ne -1 then begin
     mode=strmid(STRUPCASE(ev),13,1)
  CASE mode of
     '1': ks_curprof_showres[3].val=1-ks_curprof_showres[3].val
     '2': ks_curprof_showres[4].val=1-ks_curprof_showres[4].val
     '3': ks_curprof_showres[5].val=1-ks_curprof_showres[5].val
     'A': ks_curprof_showres[0].val=1-ks_curprof_showres[0].val
     'C': ks_curprof_showres[1].val=1-ks_curprof_showres[1].val
     'R': ks_curprof_showres[2].val=1-ks_curprof_showres[2].val
     ELSE:
    ENDCASE
    KS_CURPROFMAN_SHOW_PROF
   return 
  ENDIF
 
 
  IF strpos(STRUPCASE(ev),"PROF_COM_SET_") ne -1 then begin
   mode=strmid(STRUPCASE(ev),13,4)
   comp=(strmid(STRUPCASE(ev),17,1))
   if comp ne 'E' then comp=fix(comp) else comp=0
   CASE mode of
   
     'VALU': KS_CURPROFMAN_READ_COMPSVAL
     
     'COMP': ks_curprofman_inicomps.comps[comp]=1-(ks_curprofman_inicomps.comps)[comp]
     
     'CFIX': ks_curprofman_inicomps.fix_cent[comp]=1-(ks_curprofman_inicomps.fix_cent)[comp]
     
     'CSMI': ks_curprofman_inicomps.setmin_cent[comp]=1-(ks_curprofman_inicomps.setmin_cent)[comp]
     
     'CSMA': ks_curprofman_inicomps.setmax_cent[comp]=1-(ks_curprofman_inicomps.setmax_cent)[comp]
     
     'FFIX': ks_curprofman_inicomps.fix_fwhm[comp]=1-(ks_curprofman_inicomps.fix_fwhm)[comp]
     
     'FSMI': ks_curprofman_inicomps.setmin_fwhm[comp]=1-(ks_curprofman_inicomps.setmin_fwhm)[comp]
     
     'FSMA': ks_curprofman_inicomps.setmax_fwhm[comp]=1-(ks_curprofman_inicomps.setmax_fwhm)[comp]
     
     'AFIX': ks_curprofman_inicomps.fix_ampl[comp]=1-(ks_curprofman_inicomps.fix_ampl)[comp]
     
     'ASMI': ks_curprofman_inicomps.setmin_ampl[comp]=1-(ks_curprofman_inicomps.setmin_ampl)[comp]
     
     'ASMA': ks_curprofman_inicomps.setmax_ampl[comp]=1-(ks_curprofman_inicomps.setmax_ampl)[comp]
     
     'CTFX': ks_curprofman_inicomps.fix_cont=1-ks_curprofman_inicomps.fix_cont
     
     'CTMI': ks_curprofman_inicomps.setmin_cont=1-ks_curprofman_inicomps.setmin_cont
     
     'CTMA': ks_curprofman_inicomps.setmax_cont=1-ks_curprofman_inicomps.setmax_cont
      ELSE:
      
      ENDCASE
     return 
    ENDIF
  
  IF strpos(STRUPCASE(ev),"NAVI_") ne -1 then begin
   mode=strmid(STRUPCASE(ev),5,2)
   x=curprofman_pix.x
   y=curprofman_pix.y
   CASE mode of
     'CH': BEGIN
      WIDGET_CONTROL, ks_curprofman_curpos[0],get_val=x
      WIDGET_CONTROL, ks_curprofman_curpos[1],get_val=y
     END
     'UP': y+=1
     'DN': y-=1
     'LT': x-=1
     'RT': x+=1
     else: return
   ENDCASE
   KS_CURPROFMAN_CHANGEPOS,x,y
   return
  ENDIF
   
  Case ev OF
    'close': BEGIN
      WIDGET_CONTROL,ks_curprofman_b, map=0
    END
    
  'fit': KS_CURPROFMAN_FIT_RUN
  
  'save_ps': KS_CURPROFMAN_SAVE_PS
  
  'set_fit_method': BEGIN
    WIDGET_CONTROL, ks_curprofman_method_but,get_val=tmp
    ks_curprofman_method=tmp
    WIDGET_CONTROL, ks_curprofman_setcomp1,sens=(1-ks_curprofman_method)
    WIDGET_CONTROL, ks_curprofman_setcomp2,sens=(1-ks_curprofman_method)
    WIDGET_CONTROL, ks_curprofman_setcomp3,sens=(1-ks_curprofman_method)
    WIDGET_CONTROL, ks_table_curprofcomp,sens=(1-ks_curprofman_method)
  END
  
  
  
  'dispprof': BEGIN
    ; === События, генерируемые дисплеем профилей
       Xcur=event.x
       Ycur=event.y
       
       IF (event.TYPE LT 3) THEN BEGIN
          ; --- Мониторинг профиля
          
          start_pix=[ks_curprofman_pos_on_disp[0]*sz[3].x,ks_curprofman_pos_on_disp[2]*sz[3].y]
          fin_pix=[ks_curprofman_pos_on_disp[1]*sz[3].x,ks_curprofman_pos_on_disp[3]*sz[3].y]
          xs=(fin_pix[0]-start_pix[0])
          ys=(fin_pix[1]-start_pix[1])
          
          IF xcur lt start_pix[0] or xcur gt fin_pix[0] or ycur lt start_pix[1] or ycur gt fin_pix[1] then begin
            xcur_data_show="none"
            ycur_data_show="none"
          ENDIF ELSE BEGIN
            xcur_data=float(xcur-start_pix[0])/xs*((ks_curprofman_inicomps.xr)[1]-(ks_curprofman_inicomps.xr)[0])+(ks_curprofman_inicomps.xr)[0]
            ycur_data=float(ycur-start_pix[1])/ys*((ks_curprofman_inicomps.yr)[1]-(ks_curprofman_inicomps.yr)[0])+(ks_curprofman_inicomps.yr)[0]
            xcur_data_show=string(xcur_data,format="(F0.2)")
            ycur_data_show=string(ycur_data,format="(F0.2)")            
          ENDELSE
          WIDGET_CONTROL,ks_curprofman_monitors[0].obj,set_value=xcur_data_show
          WIDGET_CONTROL,ks_curprofman_monitors[1].obj,set_value=ycur_data_show
                
       ENDIF
  END
  
    'tabu':
    
    ELSE: print, "Not ready yet"
  ENDCASE
END


PRO KS_CURPROF_FIT_MANAGER
  COMMON KS_WIDGET_ELEMENTS
  COMMON KS_FLAGS_AND_PARAMS
  COMMON KS_SIZES
  COMMON KS_CURPROF_MANAGER
  
  
  ks_curprofman_b=WIDGET_BASE(TITLE="KINEScope: Curent Profile",/row,GROUP_LEADER=ks_mb, map=0)

  if (!VERSION.OS_FAMILY eq "Windows") then titfont="Sans Serif" else titfont="-misc-fixed-bold-r-normal--13-120-75-75-c-70-iso8859-1"
  
  curprofman_buttons=[{obj_par,'Fit','fit',0,0,1},$
                  {obj_par,'Close','close',0,0,1}]
  
  
  ks_curprofman_setup_b=WIDGET_BASE(ks_curprofman_b,/col)
  ks_curprofman_profandcomp_b=WIDGET_BASE(ks_curprofman_setup_b,/col)
  
  
  
  
  ; Настройки фиттинга
    
  
    ks_curprofman_comps_mb=WIDGET_BASE(ks_curprofman_profandcomp_b,/col,/frame)
    tmpb=widget_base(ks_curprofman_comps_mb,/row)
    lab_prof_indicator=WIDGET_LABEL(tmpb,val='Prof. components parameters:',xs=300, font=titfont)
    
    prof_storage=ks_new_profile(nprof=1)
    
    ks_curprofman_method_b=WIDGET_BASE(tmpb,/row)
    lab=WIDGET_LABEL(ks_curprofman_method_b,val="Fitting Method:",xs=90)
    ks_curprofman_method_but=CW_BGROUP(ks_curprofman_method_b,["MPFIT", "GENFIT"],/frame, uval='set_fit_method', /row, /EXCLUSIVE,SET_VALUE=ks_curprofman_method,/NO_RELEASE)
    
    
    ks_comp_b=WIDGET_BASE(ks_curprofman_comps_mb,/row,/frame)
    
    comp_select_base=WIDGET_BASE(ks_comp_b,/column,xpad=0,ypad=3)
    lab=WIDGET_LABEL(comp_select_base,val='Comp.')
    comp_vt_base=WIDGET_BASE(comp_select_base,/column,/nonexcl)
    ks_curprofman_setcomp1=WIDGET_BUTTON(comp_vt_base,value='1 (Cent)',uvalue="PROF_COM_SET_COMP0",ys=97,sens=(1-ks_curprofman_method))
    ks_curprofman_setcomp2=WIDGET_BUTTON(comp_vt_base,value='2 (Blue)',uvalue="PROF_COM_SET_COMP1",ys=87,sens=(1-ks_curprofman_method))
    ks_curprofman_setcomp3=WIDGET_BUTTON(comp_vt_base,value='3 (Red)',uvalue="PROF_COM_SET_COMP2",ys=57,sens=(1-ks_curprofman_method))
  
    
    ks_table_curprofcomp=WIDGET_TAB(ks_comp_b,uvalue='tabu',sens=(1-ks_curprofman_method) )
    max_decim=9
      ;###### Center
      ks_tmp_b=WIDGET_BASE(ks_table_curprofcomp,/column,tit="Shift from line center")
        
        ;### COMP1
        ks_comp1_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
              ks_curprofman_comp1_c=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(ks_curprofman_inicomps.cent)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_curprofman_comp1_cfix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_CFIX0")
        ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_curprofman_comp1_cmin=FSC_FIELD(ks_tmp1_b, title='Min.', decimal=max_decim,value=(ks_curprofman_inicomps.min_cent)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_comp1_csetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_CSMI0")
        ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_curprofman_comp1_cmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_curprofman_inicomps.max_cent)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_comp1_csetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_CSMA0")
        
        ;### COMP2
        ks_comp2_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
              ks_curprofman_comp2_c=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(ks_curprofman_inicomps.cent)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_curprofman_comp2_cfix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_CFIX1")
        ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
          ks_curprofman_comp2_cmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(ks_curprofman_inicomps.min_cent)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_comp2_csetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_CSMI1")
        ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
          ks_curprofman_comp2_cmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_curprofman_inicomps.max_cent)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_comp2_csetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_CSMA1")

        ;### COMP3
        ks_comp3_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
              ks_curprofman_comp3_c=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(ks_curprofman_inicomps.cent)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_curprofman_comp3_cfix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_CFIX2")
        ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
          ks_curprofman_comp3_cmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(ks_curprofman_inicomps.min_cent)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_comp3_csetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_CSMI2")
        ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
          ks_curprofman_comp3_cmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_curprofman_inicomps.max_cent)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_comp3_csetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_CSMA2")
          
      ;###### FWHM
      ks_tmp_b=WIDGET_BASE(ks_table_curprofcomp,/column,tit="FWHM")
        
        ;### COMP1
        ks_comp1_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
              ks_curprofman_comp1_f=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(ks_curprofman_inicomps.fwhm)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_curprofman_comp1_ffix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_FFIX0")
        ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_curprofman_comp1_fmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(ks_curprofman_inicomps.min_fwhm)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_comp1_fsetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_FSMI0")
        ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_curprofman_comp1_fmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_curprofman_inicomps.max_fwhm)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_comp1_fsetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_FSMA0")
        
        ;### COMP2
        ks_comp2_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
              ks_curprofman_comp2_f=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(ks_curprofman_inicomps.fwhm)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_curprofman_comp2_ffix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_FFIX1")
        ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
          ks_curprofman_comp2_fmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(ks_curprofman_inicomps.min_fwhm)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_comp2_fsetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_FSMI1")
        ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
          ks_curprofman_comp2_fmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_curprofman_inicomps.max_fwhm)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_comp2_fsetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_FSMA1")

        ;### COMP3
        ks_comp3_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
              ks_curprofman_comp3_f=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(ks_curprofman_inicomps.fwhm)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_curprofman_comp3_ffix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_FFIX2")
        ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
          ks_curprofman_comp3_fmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(ks_curprofman_inicomps.min_fwhm)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_comp3_fsetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_FSMI2")
        ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
          ks_curprofman_comp3_fmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_curprofman_inicomps.max_fwhm)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_comp3_fsetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_FSMA2")


      ;###### AMPLITUDE
      ks_tmp_b=WIDGET_BASE(ks_table_curprofcomp,/column,tit="Amplitude")
        
        ;### COMP1
        ks_comp1_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
              ks_curprofman_comp1_a=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(ks_curprofman_inicomps.ampl)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_curprofman_comp1_afix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_AFIX0")
        ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_curprofman_comp1_amin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(ks_curprofman_inicomps.min_ampl)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_comp1_asetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_ASMI0")
        ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_curprofman_comp1_amax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_curprofman_inicomps.max_ampl)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_comp1_asetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_ASMA0")
        
        ;### COMP2
        ks_comp2_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
              ks_curprofman_comp2_a=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(ks_curprofman_inicomps.ampl)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_curprofman_comp2_afix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_AFIX1")
        ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
          ks_curprofman_comp2_amin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(ks_curprofman_inicomps.min_ampl)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_comp2_asetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_ASMI1")
        ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
          ks_curprofman_comp2_amax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_curprofman_inicomps.max_ampl)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_comp2_asetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_ASMA1")

        ;### COMP3
        ks_comp3_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
              ks_curprofman_comp3_a=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(ks_curprofman_inicomps.ampl)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_curprofman_comp3_afix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_AFIX2")
        ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
          ks_curprofman_comp3_amin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(ks_curprofman_inicomps.min_ampl)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_comp3_asetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_ASMI2")
        ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
          ks_curprofman_comp3_amax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_curprofman_inicomps.max_ampl)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_comp3_asetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_ASMA2")

      ;###### CONTINUUM
      ks_tmp_b=WIDGET_BASE(ks_table_curprofcomp,/column,tit="Continuum")
        
        ks_comp1_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
              ks_curprofman_cont=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(ks_curprofman_inicomps.cont),xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_curprofman_contfix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_CTFX0")
        ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_curprofman_contmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(ks_curprofman_inicomps.min_cont),xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_contsetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_CTMI0")
        ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_curprofman_contmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_curprofman_inicomps.max_cont),xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_curprofman_contsetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_CTMA0")
  
  
  lab=WIDGET_LABEL(ks_curprofman_profandcomp_b,val="Note: Params. of axes and inst. contour will be taken from ANALYSIS menu!", font=titfont)
  
  
  tmp_b=WIDGET_BASE(ks_curprofman_setup_b,/col,ys=50)
  ks_curprofman_navi_b=WIDGET_BASE(ks_curprofman_setup_b,/row,/frame)
  ks_curprofman_navi_curpos_b=WIDGET_BASE(ks_curprofman_navi_b,/col)
  lab=WIDGET_LABEL(ks_curprofman_navi_curpos_b,val="Change current position:",font=titfont)
  
  ks_curprofman_curpos=lonarr(2)
  
  ks_curprofman_curpos[0]=FSC_FIELD(ks_curprofman_navi_curpos_b, /cr_only, title='X:', value=0,xsize=10,uvalue="NAVI_CH",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
  ks_curprofman_curpos[1]=FSC_FIELD(ks_curprofman_navi_curpos_b, /cr_only, title='Y:', value=0,xsize=10,uvalue="NAVI_CH",event_pro="KS_CURPROF_FIT_MANAGER_EVENT")
  
  ks_curprofman_shift_b=WIDGET_BASE(ks_curprofman_navi_b,/col,xpad=0)
  
  ks_curprofman_shift_top_b=WIDGET_BASE(ks_curprofman_shift_b,/row,xpad=0)
  tmp_b=WIDGET_BASE(ks_curprofman_shift_top_b,/col,xs=20)
  ks_curprofman_navi_up=WIDGET_BUTTON(ks_curprofman_shift_top_b,value='Up',uvalue="NAVI_UP", xs=40)
  ks_curprofman_shift_mid_b=WIDGET_BASE(ks_curprofman_shift_b,/row,xpad=0)
  ks_curprofman_navi_lt=WIDGET_BUTTON(ks_curprofman_shift_mid_b,value='Left',uvalue="NAVI_LT", xs=40)
  ks_curprofman_navi_rt=WIDGET_BUTTON(ks_curprofman_shift_mid_b,value='Right',uvalue="NAVI_RT", xs=40)
  ks_curprofman_shift_bot_b=WIDGET_BASE(ks_curprofman_shift_b,/row,xpad=0)
  tmp_b=WIDGET_BASE(ks_curprofman_shift_bot_b,/col,xs=20)
  ks_curprofman_navi_dn=WIDGET_BUTTON(ks_curprofman_shift_bot_b,value='Down',uvalue="NAVI_DN", xs=40)
  
  ks_curprofman_monitors=[{obj_par,'X: ','',0,0,1},$
                      {obj_par,'Y: ','',0,0,1},$
                      {obj_par,'S/N rat.: ','',0,0,1},$
                      {obj_par,'Tot. Flux: ','',0,0,1},$
                      {obj_par,'Contin.: ','',0,0,1},$
                      {obj_par,'Resid./Noise: ','',0,0,1},$
                      {obj_par,'Center: ','',0,0,1},$
                      {obj_par,'Disp. (FWHM): ','',0,0,1},$
                      {obj_par,'Intens.: ','',0,0,1},$
                      {obj_par,'Flux: ','',0,0,1},$
                      {obj_par,'Center: ','',0,0,1},$
                      {obj_par,'Disp. (FWHM): ','',0,0,1},$
                      {obj_par,'Intens.: ','',0,0,1},$
                      {obj_par,'Flux: ','',0,0,1},$
                      {obj_par,'Center: ','',0,0,1},$
                      {obj_par,'Disp. (FWHM): ','',0,0,1},$
                      {obj_par,'Intens.: ','',0,0,1},$
                      {obj_par,'Flux: ','',0,0,1}]
  
  
  
  
  ;Дисплей
  ks_curprof_disp={disp_par,0,'dispprof',0}
  
  ks_curprofman_out_and_but_b=WIDGET_BASE(ks_curprofman_b,/col)
  ks_curprofman_out_b=WIDGET_BASE(ks_curprofman_out_and_but_b,/row)
  
  ks_curprof_disp_base=WIDGET_BASE(ks_curprofman_out_b,/col)
  
  ks_curprof_disp.obj=WIDGET_DRAW(ks_curprof_disp_base,uvalue=ks_curprof_disp.uval, xsize=sz[3].x,ysize=sz[3].y,$
                                    /motion_event,/button_event,/frame)
  
  
  ks_curprof_disp_setup_and_mon_b=WIDGET_BASE(ks_curprof_disp_base,/row,/frame,ypad=0,yoffset=0)
  
  ks_curprof_disp_mon_b=WIDGET_BASE(ks_curprof_disp_setup_and_mon_b,/col,ypad=40,yoffset=0)
  output=lonarr(2)
  KS_Monitor_Cre,ks_curprof_disp_mon_b,ks_curprofman_monitors[0:1].name,output,/col,xs=[40,sz[4].y]
      ks_curprofman_monitors[0:1].obj=output
  
  
  ks_curprof_disp_setup_b=WIDGET_BASE(ks_curprof_disp_setup_and_mon_b,/col,ypad=0,yoffset=0)
  
  lab=WIDGET_LABEL(ks_curprof_disp_setup_b,val="Show on display:",font=titfont)
  
  ks_curprof_showres=[{obj_par,'Sum of selected','show_fit_res_a',0,1,0},$
                   {obj_par,'Continuum','show_fit_res_c',0,1,0},$
                   {obj_par,'Residual','show_fit_res_r',0,1,0},$
                   {obj_par,'1-st component','show_fit_res_1',0,1,0},$
                   {obj_par,'2-nd component','show_fit_res_2',0,1,0},$
                   {obj_par,'3-rd component','show_fit_res_3',0,1,0}]
  
  tmp=WIDGET_BASE(ks_curprof_disp_setup_b,/row)
  tmp0=WIDGET_BASE(tmp,/col,/frame)
  i0=0
  KS_Buttons_Cre,tmp0,ks_curprof_showres[i0:i0+2].name,ks_curprof_showres[i0:i0+2].uval,output,sens=ks_curprof_showres[i0:i0+2].sens,/nonexclusive,/frame
  ks_curprof_showres[i0:i0+2].obj=output 
  
  tmp0=WIDGET_BASE(tmp,/col,/frame)
  i0=3
  KS_Buttons_Cre,tmp0,ks_curprof_showres[i0:i0+2].name,ks_curprof_showres[i0:i0+2].uval,output,sens=ks_curprof_showres[i0:i0+2].sens,/nonexclusive,/frame
  ks_curprof_showres[i0:i0+2].obj=output 
  
  for i=0,5 do WIDGET_CONTROL,ks_curprof_showres[i].obj,set_button=ks_curprof_showres[i].val
  
  
  ; Результаты фиттинга
  ks_curprof_res_base=WIDGET_BASE(ks_curprofman_out_b,/row,xoffset=0,xpad=0)
  ks_curprof_res_fullbase=WIDGET_BASE(ks_curprof_res_base,/col,xoffset=0,xpad=0,/align_center);,xsize=sz[3].x)
  lab=WIDGET_LABEL(ks_curprof_res_fullbase,val="Fitting results:",font=titfont,/align_cent)
  
  
  
  ks_curprof_resbase=WIDGET_BASE(ks_curprof_res_fullbase,/col,xpad=0,xoffset=0)
  
  tit=["=== Total ===","=== Comp1 ===","=== Comp2 ===","=== Comp3 ==="]
  FOR bb=0,3 do begin
    tmp=WIDGET_BASE(ks_curprof_resbase,/col,/frame)
    lab=WIDGET_LABEL(tmp,val=tit[bb])
    i0=bb*4+2
    output=lonarr(1)
    for i=i0,i0+3 do begin
      KS_Monitor_Cre,tmp,ks_curprofman_monitors[i].name,output,/row,xs=[80,sz[4].y+10];,ys=[10,10]
      ks_curprofman_monitors[i].obj=output
    endfor
  ENDFOR
  
  
  
  lab=WIDGET_LABEL(ks_curprofman_out_and_but_b,font=titfont,val="Note: Disp. is corrected for inst., therm. and nat. broadening (see ANALYSIS menu).")
  
  
  ks_curprofman_butbase=WIDGET_BASE(ks_curprofman_out_and_but_b,/row, /frame)
  
  but=WIDGET_BUTTON(ks_curprofman_butbase,val="Save PS",uval="save_ps",xs=sz[2].x,ys=sz[2].y)
  tmp=WIDGET_BASE(ks_curprofman_butbase,xs=20)
  KS_Buttons_Cre,ks_curprofman_butbase, curprofman_buttons[0:1].name,curprofman_buttons[0:1].uval,output,sens=curprofman_buttons[0:1].sens,xs=sz[2].x,ys=sz[2].y
  curprofman_buttons[0:1].obj=output
  
    
    
  cgCENTERTLB,ks_curprofman_b
  WIDGET_CONTROL, ks_curprofman_b, /realize,group_leader=ks_mb
  Widget_Control, ks_curprof_disp.obj, Get_Value=wid
  WSet, wid
  Erase, color=cgcolor('white')
  XMANAGER,'KS_CURPROF_FIT_MANAGER', ks_curprofman_b,no_block=1



END