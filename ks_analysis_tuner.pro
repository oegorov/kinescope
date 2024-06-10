FUNCTION  KS_GET_NEWLIST_RESTYPES,res_loaded,num_res_subtypes,list_of_restypes
  new_list=['None']
  for i=0,n_elements(list_of_restypes)-1 do begin
    if i eq 0 then start=0 else start=total(num_res_subtypes[0:i-1])
    last = total(num_res_subtypes[0:i])-1
    if total(res_loaded[start:last]) gt 0 then new_list=[new_list, list_of_restypes[i]]
  endfor
  
  n=n_elements(new_list)
  if n gt 1 then new_list=new_list[1:n-1]
  RETURN, new_list
END

Function KS_NBINS, binmap, roi=roi
  if keyword_set(roi) then begin
  
  endif else binmap_for_analysis=binmap
  s=size(binmap_for_analysis)
  binmap_for_analysis=reform(binmap_for_analysis, s[1]*s[2])
  nbins=n_elements(uniq(binmap_for_analysis,sort(binmap_for_analysis)))

  RETURN, nbins
END

PRO KS_SHIFTED_COMP_MAPS, fit_results_maps, xim, yim
 ;Создаем карту распределения смещенной компоненты и карту разности скоростей:
 ; В случае если есть лишь смещенные компоненты, без центральной (хотя такого не предусмотрено)
 ; разница скоростей считается между ними и делится на 2, а смещенной считается слабейшая из них
 ; При 3 компонентах смещенной считается ярчайшая из двух, помимо центральной 
          
          
          ;
          nomask=0
          if fit_results_maps.is_set1[xim,yim] eq 1 and fit_results_maps.is_set2[xim,yim] eq 1 and fit_results_maps.is_set3[xim,yim] eq 0 then begin
             fit_results_maps.v_shift[xim,yim]=fit_results_maps.v2[xim,yim]-fit_results_maps.v1[xim,yim]
             fit_results_maps.v_shi_cmp[xim,yim]=fit_results_maps.v2[xim,yim]
             fit_results_maps.i_shi_cmp[xim,yim]=fit_results_maps.i2[xim,yim]
             fit_results_maps.f_shi_cmp[xim,yim]=fit_results_maps.f2[xim,yim]
             fit_results_maps.sigma_shi_cmp[xim,yim]=fit_results_maps.sigma2[xim,yim]
             nomask=1
          endif
          if fit_results_maps.is_set1[xim,yim] eq 1 and fit_results_maps.is_set2[xim,yim] eq 0 and fit_results_maps.is_set3[xim,yim] eq 1 then begin
             fit_results_maps.v_shift[xim,yim]=fit_results_maps.v3[xim,yim]-fit_results_maps.v1[xim,yim]
             fit_results_maps.v_shi_cmp[xim,yim]=fit_results_maps.v3[xim,yim]
             fit_results_maps.i_shi_cmp[xim,yim]=fit_results_maps.i3[xim,yim]
             fit_results_maps.f_shi_cmp[xim,yim]=fit_results_maps.f3[xim,yim]
             fit_results_maps.sigma_shi_cmp[xim,yim]=fit_results_maps.sigma3[xim,yim]
             nomask=1
          endif
          if fit_results_maps.is_set1[xim,yim] eq 0 and fit_results_maps.is_set2[xim,yim] eq 1 and fit_results_maps.is_set3[xim,yim] eq 1 then begin
             arr=[fit_results_maps.v2[xim,yim]-fit_results_maps.v3[xim,yim],$
                  fit_results_maps.v3[xim,yim]-fit_results_maps.v2[xim,yim]]/2.
             m=max([fit_results_maps.f2[xim,yim],fit_results_maps.f3[xim,yim]],mpos)
             fit_results_maps.v_shift[xim,yim]=arr[mpos]
             mpos=1-mpos
             arr=[fit_results_maps.v2[xim,yim],fit_results_maps.v3[xim,yim]]
             fit_results_maps.v_shi_cmp[xim,yim]=arr[mpos]
             arr=[fit_results_maps.i2[xim,yim],fit_results_maps.i3[xim,yim]]
             fit_results_maps.i_shi_cmp[xim,yim]=arr[mpos]
             arr=[fit_results_maps.f2[xim,yim],fit_results_maps.f3[xim,yim]]
             fit_results_maps.f_shi_cmp[xim,yim]=arr[mpos]
             arr=[fit_results_maps.sigma2[xim,yim],fit_results_maps.sigma3[xim,yim]]
             fit_results_maps.sigma_shi_cmp[xim,yim]=arr[mpos]
             nomask=1
          endif
          if fit_results_maps.is_set1[xim,yim] eq 1 and fit_results_maps.is_set2[xim,yim] eq 1 and fit_results_maps.is_set3[xim,yim] eq 1 then begin
             arr=[fit_results_maps.v2[xim,yim]-fit_results_maps.v1[xim,yim],$
                  fit_results_maps.v3[xim,yim]-fit_results_maps.v1[xim,yim]]
             m=max([fit_results_maps.f2[xim,yim],fit_results_maps.f3[xim,yim]],mpos)
             fit_results_maps.v_shift[xim,yim]=arr[mpos]
             arr=[fit_results_maps.v2[xim,yim],fit_results_maps.v3[xim,yim]]
             fit_results_maps.v_shi_cmp[xim,yim]=arr[mpos]
             arr=[fit_results_maps.i2[xim,yim],fit_results_maps.i3[xim,yim]]
             fit_results_maps.i_shi_cmp[xim,yim]=arr[mpos]
             arr=[fit_results_maps.f2[xim,yim],fit_results_maps.f3[xim,yim]]
             fit_results_maps.f_shi_cmp[xim,yim]=arr[mpos]
             arr=[fit_results_maps.sigma2[xim,yim],fit_results_maps.sigma3[xim,yim]]
             fit_results_maps.sigma_shi_cmp[xim,yim]=arr[mpos]                  
             nomask=1
          endif
           if nomask eq 0 then begin
             fit_results_maps.v_shift[xim,yim]=!Values.D_NAN
             fit_results_maps.v_shi_cmp[xim,yim]=!Values.D_NAN
             fit_results_maps.i_shi_cmp[xim,yim]=!Values.D_NAN
             fit_results_maps.f_shi_cmp[xim,yim]=!Values.D_NAN
             fit_results_maps.sigma_shi_cmp[xim,yim]=!Values.D_NAN
           endif
           
           
           ; 
           
END


PRO KS_ADJUST_RESLIST
  ; Обновляем список возможных результатов в выпадающих меню, а также устанавливаем текущий
  COMMON KS_FLAGS_AND_PARAMS
  COMMON KS_ANALYSIS
  COMMON KS_DISPLAY
  COMMON KS_WIDGET_ELEMENTS
  
; === ADJUST TYPES LIST
          new_list=KS_GET_NEWLIST_RESTYPES(res_loaded,num_res_subtypes,list_of_restypes)
          if show_type[1].val eq -1 then show_type[1].val=0 else begin
            WIDGET_CONTROL,show_type[1].obj,get_val=old_list
            cur_set=old_list[show_type[1].val]
            rec=where(new_list eq cur_set,nr)
            if nr eq 1 then show_type[1].val = rec else show_type[1].val=0
          endelse  
          WIDGET_CONTROL,show_type[1].obj,set_val=new_list
          WIDGET_CONTROL,show_type[1].obj,set_combobox_select=show_type[1].val
          
          ; === ADJUST SUBTYPES LIST
          new_list=KS_GET_NEWLIST_RESSUBTYPES(res_loaded,num_res_subtypes,new_list[show_type[1].val])
          
          if show_type[2].val eq -1 then show_type[2].val=0 else begin
            WIDGET_CONTROL,show_type[2].obj,get_val=old_list
            cur_set=old_list[show_type[2].val]
            rec=where(new_list eq cur_set,nr)
            if nr eq 1 then show_type[2].val = rec else show_type[2].val=0
          endelse
          WIDGET_CONTROL,show_type[2].obj,set_val=new_list
          WIDGET_CONTROL,show_type[2].obj,set_combobox_select=show_type[2].val
          
          KS_CHOOSE_CUR_RES
          KS_SHOW_IMAGE,mode="result"
          
END


PRO KS_ANALYSIS_READ_LIMITS
  COMMON KS_ANAL_MANAGER_WIDGET
  COMMON KS_ANALYSIS
  
  WIDGET_CONTROL, ks_anlim_comp1_amin, get_value=p0
  WIDGET_CONTROL, ks_anlim_comp2_amin, get_value=p1
  WIDGET_CONTROL, ks_anlim_comp3_amin, get_value=p2
  ks_analysis_limiter.min_ampl=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_anlim_comp1_fmin, get_value=p0
  WIDGET_CONTROL, ks_anlim_comp2_fmin, get_value=p1
  WIDGET_CONTROL, ks_anlim_comp3_fmin, get_value=p2
  ks_analysis_limiter.min_fwhm=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_anlim_comp1_cmin, get_value=p0
  WIDGET_CONTROL, ks_anlim_comp2_cmin, get_value=p1
  WIDGET_CONTROL, ks_anlim_comp3_cmin, get_value=p2
  ks_analysis_limiter.min_cent=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_anlim_comp1_amax, get_value=p0
  WIDGET_CONTROL, ks_anlim_comp2_amax, get_value=p1
  WIDGET_CONTROL, ks_anlim_comp3_amax, get_value=p2
  ks_analysis_limiter.max_ampl=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_anlim_comp1_fmax, get_value=p0
  WIDGET_CONTROL, ks_anlim_comp2_fmax, get_value=p1
  WIDGET_CONTROL, ks_anlim_comp3_fmax, get_value=p2
  ks_analysis_limiter.max_fwhm=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_anlim_comp1_cmax, get_value=p0
  WIDGET_CONTROL, ks_anlim_comp2_cmax, get_value=p1
  WIDGET_CONTROL, ks_anlim_comp3_cmax, get_value=p2
  ks_analysis_limiter.max_cent=[p0,p1,p2]
END


 



pro KS_ANALYSIS_TUNER_EVENT, event
  COMMON KS_FLAGS_AND_PARAMS
  COMMON KS_ANAL_MANAGER_WIDGET
  COMMON KS_DATA
  COMMON KS_ANALYSIS
  COMMON KS_SIZES
  COMMON KS_WIDGET_ELEMENTS
  COMMON KS_DISPLAY
  
  WIDGET_CONTROL,event.ID,get_uvalue=ev
  
  
    IF strpos(STRUPCASE(ev),"PROF_COM_SET_") ne -1 then begin
     mode=strmid(STRUPCASE(ev),13,4)
     comp=(strmid(STRUPCASE(ev),17,1))
     if comp ne 'E' then comp=fix(comp) else comp=0
   CASE mode of
   
   'VALU': KS_ANALYSIS_READ_LIMITS
   
   'CSMI': ks_analysis_limiter.setmin_cent[comp]=1-(ks_analysis_limiter.setmin_cent)[comp]
   
   'CSMA': ks_analysis_limiter.setmax_cent[comp]=1-(ks_analysis_limiter.setmax_cent)[comp]
   
   'FSMI': ks_analysis_limiter.setmin_fwhm[comp]=1-(ks_analysis_limiter.setmin_fwhm)[comp]
   
   'FSMA': ks_analysis_limiter.setmax_fwhm[comp]=1-(ks_analysis_limiter.setmax_fwhm)[comp]
      
   'ASMI': ks_analysis_limiter.setmin_ampl[comp]=1-(ks_analysis_limiter.setmin_ampl)[comp]
   
   'ASMA': ks_analysis_limiter.setmax_ampl[comp]=1-(ks_analysis_limiter.setmax_ampl)[comp]
   
    ELSE:
    
    ENDCASE
   return 
  ENDIF
  
  
  
  Case ev OF
  
  'set_fitreg_all': BEGIN
    cur_fit_selection.type=-1
    an_infomessage[1]='whole field'
    n_fitpoints=sxpar(header_cub,"NAXIS1")*sxpar(header_cub,"NAXIS2")
    fit_pixels=lindgen(n_fitpoints)
    if binmap_loaded then begin
      n_fitbins=KS_NBINS(binmap)
    endif
    WIDGET_CONTROL,ks_an_info_label[0],set_value=an_infomessage[0]+an_infomessage[1]
    if not binmap_loaded then WIDGET_CONTROL,ks_an_info_label[1],set_value='N points for fit: '+string(n_fitpoints,format="(I0)") else $
        WIDGET_CONTROL,ks_an_info_label[1],set_value='N bins for fit: '+string(n_fitbins,format="(I0)")
    KS_SHOW_IMAGE,mode="cube"
  END
  
  'set_bordcolor': BEGIN
    cur_fit_selection.color=cgpickcolorname(cur_fit_selection.color,group_leader=ks_anal_b)
    KS_SHOW_IMAGE,mode="cube"
    an_infomessage[1]=cur_fit_selection.color+" shape"
    WIDGET_CONTROL,ks_an_info_label[0],set_value=an_infomessage[0]+an_infomessage[1]
  END
  
  'set_overwrite': fit_overwrite=1-fit_overwrite
  
  'cor_ax_info': BEGIN
    c=2.99792458e5
    WIDGET_CONTROL,wid_ax_info[0],get_val=tmp
    axes_info.ztype = tmp-1 
    WIDGET_CONTROL,wid_ax_info[1],get_val=tmp
    if VALID_NUM(tmp) then axes_info.zref = float(tmp) else WIDGET_CONTROL,wid_ax_info[1],set_val=axes_info.zref
    WIDGET_CONTROL,wid_ax_info[2],get_val=tmp
    axes_info.I = string(tmp)
    if round(axes_info.zref) gt 0 then begin
      if (~finite(inst_fwhm.wav) or inst_fwhm.wav eq 0) and (finite(inst_fwhm.vel) and inst_fwhm.vel gt 0) then begin
        inst_fwhm.wav = inst_fwhm.vel*axes_info.zref/c
      endif else begin
        if (finite(inst_fwhm.wav) and inst_fwhm.wav gt 0) then begin
          inst_fwhm.vel = inst_fwhm.wav/axes_info.zref*c
        endif
      endelse
      if inst_fwhm.type eq 0 then WIDGET_CONTROL, wid_inst_fwhm[0],set_val=inst_fwhm.vel $
      else WIDGET_CONTROL, wid_inst_fwhm[0],set_val=inst_fwhm.wav 
    endif
  END
  
  'cor_inst_fwhm': BEGIN
    c=2.99792458e5
    WIDGET_CONTROL,wid_inst_fwhm[0],get_val=tmp
    if valid_num(tmp) then begin
      if inst_fwhm.type eq 0 then begin
        if inst_fwhm.vel ne tmp then begin
          inst_fwhm.vel = float(tmp)
          inst_fwhm.wav = float(tmp)/2.99792458e5*axes_info.zref
        endif
      endif else begin
        if inst_fwhm.wav ne tmp then begin
          inst_fwhm.wav = float(tmp)
          inst_fwhm.vel = float(tmp)/axes_info.zref*c
        endif
      endelse
    endif else begin
        if inst_fwhm.type eq 0 then WIDGET_CONTROL,wid_inst_fwhm[0],set_val=inst_fwhm.vel $
        else WIDGET_CONTROL,wid_inst_fwhm[0],set_val=inst_fwhm.wav
    endelse
    
     
    WIDGET_CONTROL,wid_inst_fwhm[1],get_val=tmp1
    if inst_fwhm.type ne tmp1 then begin
      inst_fwhm.type = tmp1
      if inst_fwhm.type eq 0 then WIDGET_CONTROL,wid_inst_fwhm[0],set_val=inst_fwhm.vel else $
                                  WIDGET_CONTROL,wid_inst_fwhm[0],set_val=inst_fwhm.wav
    endif
     
  END
  
  
  'cor_extern_disp': BEGIN
    c=2.99792458e5
    WIDGET_CONTROL,wid_extern_disp[0],get_val=tmp
    if valid_num(tmp) then begin
      if extern_disp.type eq 0 then begin
        if extern_disp.vel ne tmp then begin
          extern_disp.vel = float(tmp)
          extern_disp.wav = float(tmp)/2.99792458e5*axes_info.zref
        endif
      endif else begin
        if extern_disp.wav ne tmp then begin
          extern_disp.wav = float(tmp)
          extern_disp.vel = float(tmp)/axes_info.zref*c
        endif
      endelse
    endif else begin
        if extern_disp.type eq 0 then WIDGET_CONTROL,wid_extern_disp[0],set_val=extern_disp.vel $
        else WIDGET_CONTROL,wid_extern_disp[0],set_val=extern_disp.wav
    endelse
    
     
    WIDGET_CONTROL,wid_extern_disp[1],get_val=tmp1
    if extern_disp.type ne tmp1 then begin
      extern_disp.type = tmp1
      if extern_disp.type eq 0 then WIDGET_CONTROL,wid_extern_disp[0],set_val=extern_disp.vel else $
                                  WIDGET_CONTROL,wid_extern_disp[0],set_val=extern_disp.wav
    endif
     
  END
  
  
  
  'calc_moms': BEGIN
        nx=sxpar(header_cub,"NAXIS1")
        ny=sxpar(header_cub,"NAXIS2")
        nz=sxpar(header_cub,"NAXIS3")
        refpix=sxpar(header_cub,"CRPIX3")
        xdelt=sxpar(header_cub,"CDELT3")
        if xdelt eq 0 then xdelt=sxpar(header_cub,"CD3_3")
        if refpix eq 0 then refpix=1
        xscale=(findgen(nz)-refpix+1)*xdelt+sxpar(header_cub,"CRVAL3")
        
        xscale=KS_X_CONVERSION(xscale, typein=axes_info.ztype, typeout=0, cent_lam=axes_info.zref)
        xind=get_num(nx,ny,/x)
        yind=get_num(nx,ny,/y)
        FOR i=0L,n_fitpoints-1 DO BEGIN
          KS_STATE_MONITOR, 1, done=(i+1.)/n_fitpoints
          xim=xind[fit_pixels[i]]
          yim=yind[fit_pixels[i]]
          if fit_overwrite eq 0 and (fit_results_maps.fitted[xim,yim] eq 1 or fit_results_maps.fitted[xim,yim] eq 2) then continue
          this_done=0
          IF axes_info.ztype eq 0 or axes_info.ztype eq 1 then begin
            if moms_on_fly.done[xim,yim] eq 1 then begin
              this_done=1
              fit_results_maps.mom0[xim,yim]=moms_on_fly.mom0[xim,yim]
              fit_results_maps.mom1[xim,yim]=moms_on_fly.mom1[xim,yim]
              fit_results_maps.mom2[xim,yim]=moms_on_fly.mom2[xim,yim]
              fit_results_maps.mom3[xim,yim]=moms_on_fly.mom3[xim,yim]
              fit_results_maps.mom4[xim,yim]=moms_on_fly.mom4[xim,yim]
              fit_results_maps.snr[xim,yim]=moms_on_fly.snr[xim,yim]
              fit_results_maps.contin[xim,yim]=moms_on_fly.contin[xim,yim]
              fit_results_maps.fitted[xim,yim]=2
            endif
          ENDIF
          IF this_done then continue
          moms=ks_moments(xscale, reform(cub[xim,yim,*]), contin=contin, snr=snr)
          fit_results_maps.mom0[xim,yim]=moms[0]
          fit_results_maps.mom1[xim,yim]=moms[1]
          fit_results_maps.mom2[xim,yim]=moms[2]
          fit_results_maps.mom3[xim,yim]=moms[3]
          fit_results_maps.mom4[xim,yim]=moms[4]
          fit_results_maps.snr[xim,yim]=snr
          fit_results_maps.contin[xim,yim]=contin
          fit_results_maps.fitted[xim,yim]=2
        ENDFOR
        KS_STATE_MONITOR,0
        rec=where(fit_results_maps.fitted ge 1,nr)
        if nr ge min_maps_points then begin
          res_loaded[total(num_res_subtypes[0:3]):total(num_res_subtypes[0:6])-1]=1
          KS_TRIGGER_SENS_RES_PANEL, 1
          KS_ADJUST_RESLIST
          
          ;file="~/Science/HoII/IFP/Results/Kinescope_test/test_moments.ps"
         ; KS_SAVE_TO_PS_ALL_MOMENTS, file, sn_break=5
        endif    
  END
  
  'fitting': BEGIN
        nx=sxpar(header_cub,"NAXIS1")
        ny=sxpar(header_cub,"NAXIS2")
        nz=sxpar(header_cub,"NAXIS3")
        refpix=sxpar(header_cub,"CRPIX3")
        xdelt=sxpar(header_cub,"CDELT3")
        if xdelt eq 0 then xdelt=sxpar(header_cub,"CD3_3")
        if refpix eq 0 then refpix=1
        xscale=(findgen(nz)-refpix+1)*xdelt+sxpar(header_cub,"CRVAL3")
        xscale=KS_X_CONVERSION(xscale, typein=axes_info.ztype, typeout=0, cent_lam=axes_info.zref)
        xind=get_num(nx,ny,/x)
        yind=get_num(nx,ny,/y)
        
        FOR i=0L,n_fitpoints-1 DO BEGIN
          KS_STATE_MONITOR,2, done=(i+1.)/n_fitpoints
          xim=xind[fit_pixels[i]]
          yim=yind[fit_pixels[i]]
          ; Есле не перезаписывать, то пропускаем готовое
          if fit_overwrite eq 0 and (fit_results_maps.fitted[xim,yim] eq 1) then continue 
          
          prof=double(reform(cub[xim,yim,*]))
          ;Сначала считаем моменты
          
          if fit_results_maps.fitted[xim,yim] eq 0 then begin
            IF (axes_info.ztype eq 0 or axes_info.ztype eq 1) and moms_on_fly.done[xim,yim] eq 1 then begin
              fit_results_maps.mom0[xim,yim]=moms_on_fly.mom0[xim,yim]
              fit_results_maps.mom1[xim,yim]=moms_on_fly.mom1[xim,yim]
              fit_results_maps.mom2[xim,yim]=moms_on_fly.mom2[xim,yim]
              fit_results_maps.mom3[xim,yim]=moms_on_fly.mom3[xim,yim]
              fit_results_maps.mom4[xim,yim]=moms_on_fly.mom4[xim,yim]
              fit_results_maps.snr[xim,yim]=moms_on_fly.snr[xim,yim]
              fit_results_maps.contin[xim,yim]=moms_on_fly.contin[xim,yim]
              fit_results_maps.fitted[xim,yim]=2
            ENDIF ELSE BEGIN
              moms=ks_moments(xscale, prof, contin=contin, snr=snr)
              fit_results_maps.mom0[xim,yim]=moms[0]
              fit_results_maps.mom1[xim,yim]=moms[1]
              fit_results_maps.mom2[xim,yim]=moms[2]
              fit_results_maps.mom3[xim,yim]=moms[3]
              fit_results_maps.mom4[xim,yim]=moms[4]
              fit_results_maps.snr[xim,yim]=snr
              fit_results_maps.contin[xim,yim]=contin
              fit_results_maps.fitted[xim,yim]=2
            ENDELSE
          endif
        
          ; А теперь - фиттинг
          
          contin=fit_results_maps.contin[xim,yim]
          
          KS_FITTING, xscale, prof, moments=[fit_results_maps.mom1[xim,yim],fit_results_maps.mom2[xim,yim],$
                      fit_results_maps.mom3[xim,yim],fit_results_maps.mom4[xim,yim]], snr=fit_results_maps.snr[xim,yim],$
                      contin=contin, out_models=out_models,limiter=ks_analysis_limiter,sort_mode=ks_analysis_sortmode,$
                      n_lines=n_lines, out_maps=out_maps, inst_vel=inst_fwhm.vel, prof_type=fit_proftype, method=fit_method
          
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
          
          KS_SHIFTED_COMP_MAPS, fit_results_maps, xim, yim
          
          
          
        ENDFOR
        
        rec=where(fit_results_maps.fitted ge 1,nr)
        if nr ge min_maps_points then begin
          ; Загружаем моменты
          res_loaded[total(num_res_subtypes[0:3]):total(num_res_subtypes[0:6])-1]=1
          rec=where(fit_results_maps.fitted eq 1,nr)
          if nr ge min_maps_points then begin
            ; Загружаем интегральные карты
            res_loaded[0]=1
            res_loaded[total(num_res_subtypes[0:5]):total(num_res_subtypes[0:7])-1]=1
              rec=where(fit_results_maps.is_set1 eq 1,nr)
              if nr ge min_maps_points then begin
              ; Загружаем карты центральной компоненты
                res_loaded[1]=1
                for ii=0,2 do res_loaded[total(num_res_subtypes[0:ii])]=1
              endif
              rec=where(fit_results_maps.is_set2 eq 1,nr)
              if nr ge min_maps_points then begin
              ; Загружаем карты синей компоненты
                res_loaded[2]=1
                for ii=0,2 do res_loaded[total(num_res_subtypes[0:ii])+1]=1
              endif
              rec=where(fit_results_maps.is_set3 eq 1,nr)
              if nr ge min_maps_points then begin
              ; Загружаем карты красной компоненты
                res_loaded[3]=1
                for ii=0,2 do res_loaded[total(num_res_subtypes[0:ii])+2]=1
              endif
              rec=where((fit_results_maps.is_set2 eq 1 or fit_results_maps.is_set3 eq 1) and fit_results_maps.is_set1 eq 1,nr)
              ; Загружаем карту отклонений от центра
              if nr ge min_maps_points then begin
                res_loaded[4]=1
                for ii=0,2 do res_loaded[total(num_res_subtypes[0:ii])+3]=1
                res_loaded[total(num_res_subtypes[0:1])+4]=1
              endif
          endif
          KS_TRIGGER_SENS_RES_PANEL, 1
          KS_ADJUST_RESLIST
        
          
        
        endif
        KS_STATE_MONITOR,0   
  END
  
  'set_fit_method': BEGIN
      WIDGET_CONTROL, fit_method_but,get_val=tmp
      fit_method=tmp
   END
   
   'set_central_criteria': BEGIN
    WIDGET_CONTROL, fit_setcent_but,get_val=tmp
    ks_analysis_sortmode=tmp
   END
   
  'set_fit_proftype': BEGIN
      WIDGET_CONTROL, fit_proftype_but,get_val=tmp
      fit_proftype=tmp
   END
  
  
  'set_ncomps_limits': BEGIN
      WIDGET_CONTROL, fit_complimits_but,get_val=tmp
      ks_analysis_limiter.ncomps_max=tmp+1
  END
  
  'use_limits': BEGIN
    ks_analysis_limiter.lim=1-ks_analysis_limiter.lim
    WIDGET_CONTROL,fit_complimits_but,sens=ks_analysis_limiter.lim
    WIDGET_CONTROL,ks_table_lims_profcomp,sens=ks_analysis_limiter.lim
  END
  'SET_RESID_THRES': BEGIN
      WIDGET_CONTROL, ks_simplify_thresh,get_val=tmp
      ks_analysis_limiter.resid_thresh=tmp
   END
  
  'set_simplify': BEGIN
    ks_analysis_limiter.simplify=1-ks_analysis_limiter.simplify
    WIDGET_CONTROL,ks_simplify_thresh,sens=ks_analysis_limiter.simplify
  END
  
  'close': BEGIN
    WIDGET_CONTROL,ks_anal_b, map=0
  END
  
  ELSE:
  ENDCASE

END


pro KS_ANALYSIS_TUNER
  COMMON KS_FLAGS_AND_PARAMS
  COMMON KS_ANAL_MANAGER_WIDGET
  COMMON KS_DATA
  COMMON KS_ANALYSIS
  COMMON KS_SIZES
  ; Управление режимами анализа
  ks_anal_b=WIDGET_BASE(TITLE="KINEScope: Analysis Tuner",/col,GROUP_LEADER=ks_mb, map=0)

  if (!VERSION.OS_FAMILY eq "Windows") then titfont="Sans Serif" else titfont="-misc-fixed-bold-r-normal--13-120-75-75-c-70-iso8859-1"
  
  anal_buttons=[{obj_par,'Set Whole Field','set_fitreg_all',0,0,1},$
                {obj_par,'Set Border Color','set_bordcolor',0,0,1},$
                {obj_par,'Get Moms. and S/N','calc_moms',0,0,0},$
                {obj_par,'Fitting!','fitting',0,0,0},$
                {obj_par,'Close','close',0,0,1}]

  
  
  ; === Info about selected for fitting region
  
  ks_an_infobase=WIDGET_BASE(ks_anal_b,/col,/frame)
  an_infomessage=["Ready for profiles fitting inside ",'whole field']
  ks_an_info_label=lonarr(2)
  ks_an_info_label[0]=WIDGET_LABEL(ks_an_infobase,val= an_infomessage[0]+an_infomessage[1],font=titfont, xs=400)
  tmp=WIDGET_BASE(ks_an_infobase,/row)
  ks_an_info_label[1]=WIDGET_LABEL(tmp,val= 'N points for fit: '+'0',xs=200)

  KS_Buttons_Cre,tmp,anal_buttons[0:1].name,anal_buttons[0:1].uval,output,sens=anal_buttons[0:1].sens,xs=sz[2].x,ys=sz[2].y
  anal_buttons[0:1].obj=output
  

  ; === Checking data cube parsing quality
  ks_an_checkbase=WIDGET_BASE(ks_anal_b,/col,/frame)
  lab=WIDGET_LABEL(ks_an_checkbase,val="Please, check the rightness of the data cube parameters:",font=titfont)
  
  wid_ax_info=lonarr(3)
  wid_inst_fwhm=lonarr(2)
  wid_extern_disp=lonarr(2)
  
  tmp=WIDGET_BASE(ks_an_checkbase,/row, /align_center)
  lab=WIDGET_LABEL(tmp,val="Z-axis units: ")
  wid_ax_info[0]=CW_BGROUP(tmp,axes_info.zunit,xpad=0, uval='cor_ax_info', /row, /EXCLUSIVE,SET_VALUE=0,/NO_RELEASE)
  
  tmp=WIDGET_BASE(ks_an_checkbase,/row, /align_center)
  wid_ax_info[1]=FSC_FIELD(tmp,Title='Ref. Lambda (for Wave <=> Vel conversion):', $
    value=axes_info.zref,uvalue='cor_ax_info',Event_Pro='KS_ANALYSIS_TUNER_EVENT',/CR_Only,LabelSize=260,xs=8)
  wid_ax_info[2]=FSC_FIELD(tmp,Title='Intensity units:', $
    value=axes_info.I,uvalue='cor_ax_info',Event_Pro='KS_ANALYSIS_TUNER_EVENT',/CR_Only,LabelSize=100,xs=10)
  
  tmp=WIDGET_BASE(ks_an_checkbase,/row, /align_center)
  wid_inst_fwhm[0]=FSC_FIELD(tmp,Title='FWHM of Instrumental Contour:', $
    value=0.,uvalue='cor_inst_fwhm',Event_Pro='KS_ANALYSIS_TUNER_EVENT',/CR_Only,LabelSize=250,xs=8)
  wid_inst_fwhm[1]=CW_BGROUP(tmp,["km/s", "A"],xpad=0, uval='cor_inst_fwhm', /row, /EXCLUSIVE,SET_VALUE=0,/NO_RELEASE)
  
  tmp=WIDGET_BASE(ks_an_checkbase,/row, /align_center)
  wid_extern_disp[0]=FSC_FIELD(tmp,Title='Dispersion to subtract (thermal, natural):', $
    value=0.,uvalue='cor_extern_disp',Event_Pro='KS_ANALYSIS_TUNER_EVENT',/CR_Only,LabelSize=250,xs=8)
  wid_extern_disp[1]=CW_BGROUP(tmp,["km/s", "A"],xpad=0, uval='cor_extern_disp', /row, /EXCLUSIVE,SET_VALUE=0,/NO_RELEASE)
  
  
  ; === Fitting parameters
  
  ks_an_fitparbase=WIDGET_BASE(ks_anal_b,/col,/frame)
  lab=WIDGET_LABEL(ks_an_fitparbase,val="Fitting parameters:",font=titfont)
  
  
  ks_an_fitparbase_f=WIDGET_BASE(ks_an_fitparbase,/row)
  ks_an_fitparbase_l=WIDGET_BASE(ks_an_fitparbase_f,/col,/frame)


  
  ; === Here we put the limits on number of components and on each of them
  
  
    ks_an_limits_b=WIDGET_BASE(ks_an_fitparbase_l,/row)
    
    lab=WIDGET_LABEL(ks_an_limits_b,val="Max. num. of components:",xs=180)
    fit_complimits_but=CW_BGROUP(ks_an_limits_b,['1', '2','3'],/frame, uval='set_ncomps_limits', /row,set_val=ks_analysis_limiter.ncomps_max-1, /EXCLUSIVE,/NO_RELEASE)
    
      
    ks_comp_b=WIDGET_BASE(ks_an_fitparbase_l,/row,/frame)
;    
    comp_names_base=WIDGET_BASE(ks_comp_b,/column,xpad=0,ypad=3)
    lab=WIDGET_LABEL(comp_names_base,val='Comp.')
    lab=WIDGET_LABEL(comp_names_base,ys=97,val='1 (Cent)')
    lab=WIDGET_LABEL(comp_names_base,ys=87,val='2 (Blue)')
    lab=WIDGET_LABEL(comp_names_base,ys=87,val='3 (Red)')
  
    ks_table_lims_profcomp=WIDGET_TAB(ks_comp_b,uvalue='tabu',sens=1-ks_analysis_limiter.lim)
   
   
  
    
    max_decim=9
;    
      ;###### Center
      ks_tmp_b=WIDGET_BASE(ks_table_lims_profcomp,/column,tit="Shift from the line center")
;        
        ;### COMP1
        ks_comp1_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
            ks_anlim_comp1_cmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(ks_analysis_limiter.min_cent)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_ANALYSIS_TUNER_event")
            ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
            ks_anlim_comp1_csetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_CSMI0")
          ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
            ks_anlim_comp1_cmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_analysis_limiter.max_cent)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_ANALYSIS_TUNER_event")
            ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
            ks_anlim_comp1_csetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_CSMA0")
        
        ;### COMP2
        ks_comp2_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
        ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
          ks_anlim_comp2_cmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(ks_analysis_limiter.min_cent)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_ANALYSIS_TUNER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_anlim_comp2_csetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_CSMI1")
        ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
          ks_anlim_comp2_cmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_analysis_limiter.max_cent)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_ANALYSIS_TUNER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_anlim_comp2_csetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_CSMA1")
;
        ;### COMP3
        ks_comp3_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
            ks_anlim_comp3_cmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(ks_analysis_limiter.min_cent)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_ANALYSIS_TUNER_event")
            ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
            ks_anlim_comp3_csetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_CSMI2")
          ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
            ks_anlim_comp3_cmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_analysis_limiter.max_cent)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_ANALYSIS_TUNER_event")
            ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
            ks_anlim_comp3_csetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_CSMA2")
;          
      ;###### FWHM
      ks_tmp_b=WIDGET_BASE(ks_table_lims_profcomp,/column,tit="FWHM")
        
        ;### COMP1
        ks_comp1_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
         ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_anlim_comp1_fmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(ks_analysis_limiter.min_fwhm)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_ANALYSIS_TUNER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_anlim_comp1_fsetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_FSMI0")
        ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_anlim_comp1_fmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_analysis_limiter.max_fwhm)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_ANALYSIS_TUNER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_anlim_comp1_fsetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_FSMA0")
        
        ;### COMP2
        ks_comp2_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
         ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
          ks_anlim_comp2_fmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(ks_analysis_limiter.min_fwhm)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_ANALYSIS_TUNER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_anlim_comp2_fsetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_FSMI1")
        ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
          ks_anlim_comp2_fmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_analysis_limiter.max_fwhm)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_ANALYSIS_TUNER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_anlim_comp2_fsetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_FSMA1")

        ;### COMP3
        ks_comp3_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
         ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
          ks_anlim_comp3_fmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(ks_analysis_limiter.min_fwhm)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_ANALYSIS_TUNER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_anlim_comp3_fsetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_FSMI2")
        ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
          ks_anlim_comp3_fmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_analysis_limiter.max_fwhm)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_ANALYSIS_TUNER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_anlim_comp3_fsetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_FSMA2")


      ;###### AMPLITUDE
      ks_tmp_b=WIDGET_BASE(ks_table_lims_profcomp,/column,tit="Amplitude")
        
        ;### COMP1
        ks_comp1_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
         ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_anlim_comp1_amin=FSC_FIELD(ks_tmp1_b, title='Min.', decimal=max_decim, value=(ks_analysis_limiter.min_ampl)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_ANALYSIS_TUNER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_anlim_comp1_asetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_ASMI0")
        ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_anlim_comp1_amax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_analysis_limiter.max_ampl)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_ANALYSIS_TUNER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_anlim_comp1_asetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_ASMA0")
        
        ;### COMP2
        ks_comp2_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
        ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
          ks_anlim_comp2_amin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(ks_analysis_limiter.min_ampl)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_ANALYSIS_TUNER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_anlim_comp2_asetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_ASMI1")
        ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
          ks_anlim_comp2_amax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_analysis_limiter.max_ampl)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_ANALYSIS_TUNER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_anlim_comp2_asetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_ASMA1")

        ;### COMP3
        ks_comp3_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
         ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
          ks_anlim_comp3_amin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(ks_analysis_limiter.min_ampl)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_ANALYSIS_TUNER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_anlim_comp3_asetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_ASMI2")
        ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
          ks_anlim_comp3_amax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(ks_analysis_limiter.max_ampl)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_ANALYSIS_TUNER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_anlim_comp3_asetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_ASMA2")

  

      ks_an_fitparbase_r=WIDGET_BASE(ks_an_fitparbase_f,/col,/frame)
      
      
      ; Limiter
      ks_an_limits_b=WIDGET_BASE(ks_an_fitparbase_r,/row)
      fit_uselimits_but=CW_BGROUP(ks_an_limits_b,["No limits", "Set limits"],/frame, set_val=ks_analysis_limiter.lim, uval='use_limits', /row, /EXCLUSIVE,/NO_RELEASE)
    
    
      WIDGET_CONTROL,fit_complimits_but,sens=ks_analysis_limiter.lim
    WIDGET_CONTROL,ks_table_lims_profcomp,sens=ks_analysis_limiter.lim
    
    
      
      ; Simplify
      ks_tmp1_b=WIDGET_BASE(ks_an_fitparbase_r,/frame,/col)
      ks_tmp_b=WIDGET_BASE(ks_tmp1_b,/nonex,/row)
      fit_simplify_but=WIDGET_BUTTON(ks_tmp_b,val="Try lower num. of comps first", uvalue="set_simplify")
      WIDGET_CONTROL,fit_simplify_but,set_button=ks_analysis_limiter.simplify
      
      ks_tmp_b=WIDGET_BASE(ks_tmp1_b,/row)
      ks_simplify_thresh=FSC_FIELD(ks_tmp_b, title="Residual's thresh. (sigma)",decimal=max_decim, value=ks_analysis_limiter.resid_thresh,xsize=5,uvalue="SET_RESID_THRES",event_pro="KS_ANALYSIS_TUNER_event")
      widget_control,ks_simplify_thresh,sens=ks_analysis_limiter.simplify
      
  
      
      
      ks_tmp_b=WIDGET_BASE(ks_an_fitparbase_r,/row)
      lab=WIDGET_LABEL(ks_tmp_b,val="Prof. type:",xs=80)
      fit_proftype_but=CW_BGROUP(ks_tmp_b,["Voigt", "Gauss"],/frame, uval='set_fit_proftype', /row, /EXCLUSIVE,SET_VALUE=fit_proftype,/NO_RELEASE)
      
      ks_tmp_b=WIDGET_BASE(ks_an_fitparbase_r,/row)
      lab=WIDGET_LABEL(ks_tmp_b,val="Fit. method:",xs=80)
      fit_method_but=CW_BGROUP(ks_tmp_b,["MPFIT", "GENFIT"],/frame, uval='set_fit_method', /row, /EXCLUSIVE,SET_VALUE=fit_method,/NO_RELEASE)



    ; How to detect main comp?
      ks_tmp1_b=WIDGET_BASE(ks_an_fitparbase,/frame,/row)
      lab=WIDGET_LABEL(ks_tmp1_b,val="Use")
      fit_setcent_but=CW_BGROUP(ks_tmp1_b,["Brightest", "Closest to Mom1", "Narrowest"], set_val=ks_analysis_sortmode, uval='set_central_criteria', /row, /EXCLUSIVE,/NO_RELEASE)
      lab=WIDGET_LABEL(ks_tmp1_b,val="component as the CENTRAL (for 2 comps. fit)")
  
      
      

  
    ks_an_butbase=WIDGET_BASE(ks_anal_b,/row, /align_center)
      
      ks_tmp_b=WIDGET_BASE(ks_an_butbase,/nonex,/row)
      fit_overwrite_but=WIDGET_BUTTON(ks_tmp_b,val="Overwrite", uvalue="set_overwrite")
      WIDGET_CONTROL,fit_overwrite_but,set_button=fit_overwrite
      
      KS_Buttons_Cre,ks_an_butbase,anal_buttons[2:4].name,anal_buttons[2:4].uval,output,sens=anal_buttons[2:4].sens,xs=sz[2].x,ys=sz[2].y
      anal_buttons[2:4].obj=output
      
  
  
 
  
  
  
  cgCENTERTLB,ks_anal_b
  WIDGET_CONTROL, ks_anal_b, /realize,group_leader=ks_mb
  XMANAGER,'KS_ANALYSIS_TUNER', ks_anal_b,no_block=1


END