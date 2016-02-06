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


pro KS_ANALYSIS_TUNER_EVENT, event
  COMMON KS_FLAGS_AND_PARAMS
  COMMON KS_ANAL_MANAGER_WIDGET
  COMMON KS_DATA
  COMMON KS_ANALYSIS
  COMMON KS_SIZES
  COMMON KS_WIDGET_ELEMENTS
  COMMON KS_DISPLAY
  
  WIDGET_CONTROL,event.ID,get_uvalue=ev
  
  Case ev OF
  
  'set_fitreg_all': BEGIN
    cur_fit_selection.type=-1
    an_infomessage[1]='whole field'
    n_fitpoints=sxpar(header_cub,"NAXIS1")*sxpar(header_cub,"NAXIS2")
    fit_pixels=lindgen(n_fitpoints)
    WIDGET_CONTROL,ks_an_info_label[0],set_value=an_infomessage[0]+an_infomessage[1]
    WIDGET_CONTROL,ks_an_info_label[1],set_value='N points for fit: '+string(n_fitpoints,format="(I0)")
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
          
          prof=reform(cub[xim,yim,*])
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
                      contin=contin, out_models=out_models,$
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

;=====>
          print,contin,out_maps.contin,xim,yim
          
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
              ; Загружаем карту отклонения от центра
              if nr ge min_maps_points then res_loaded[total(num_res_subtypes[0:1])+3]=1
          endif
          KS_STATE_MONITOR,0
          KS_TRIGGER_SENS_RES_PANEL, 1
          KS_ADJUST_RESLIST
        
          
        
        endif   
  END
  
  'set_fit_method': BEGIN
      WIDGET_CONTROL, fit_method_but,get_val=tmp
      fit_method=tmp
   END
  
  'set_fit_proftype': BEGIN
      WIDGET_CONTROL, fit_proftype_but,get_val=tmp
      fit_proftype=tmp
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
  
  
  ks_fitoverview_b=WIDGET_BASE(ks_an_fitparbase,/row,/frame)
      ks_tmp_b=WIDGET_BASE(ks_fitoverview_b,/nonex,/row)
      fit_overwrite_but=WIDGET_BUTTON(ks_tmp_b,val="Overwrite", uvalue="set_overwrite")
      WIDGET_CONTROL,fit_overwrite_but,set_button=fit_overwrite

      lab=WIDGET_LABEL(ks_fitoverview_b,val="Profile type:",xs=90)
      fit_proftype_but=CW_BGROUP(ks_fitoverview_b,["Voigt", "Gauss"],/frame, uval='set_fit_proftype', /row, /EXCLUSIVE,SET_VALUE=fit_proftype,/NO_RELEASE)
      
      lab=WIDGET_LABEL(ks_fitoverview_b,val="Fitting Method:",xs=90)
      fit_method_but=CW_BGROUP(ks_fitoverview_b,["MPFIT", "GENFIT"],/frame, uval='set_fit_method', /row, /EXCLUSIVE,SET_VALUE=fit_method,/NO_RELEASE)



  fit_field_setup={fit_setup_struct, ncomps:0L, cent:fltarr(3), $
    ampl:fltarr(3), fwhm:fltarr(3), fixcent:intarr(3),fixfwhm:intarr(3),fixampl:intarr(3),$
    setmin_cent:intarr(3),setmin_fwhm:intarr(3),setmin_ampl:intarr(3),setmax_cent:intarr(3),$
    setmax_fwhm:intarr(3),setmax_ampl:intarr(3),min_cent:fltarr(3),min_fwhm:fltarr(3),$
    min_ampl:fltarr(3),max_cent:fltarr(3),max_fwhm:fltarr(3),max_ampl:fltarr(3),$
    do_voigt:0L,genfit:0L, inst_fwhm:0E, snr_clip:0L, extern_disp: 0E}

  
  ks_an_butbase=WIDGET_BASE(ks_anal_b,/row, /align_center)
      
      KS_Buttons_Cre,ks_an_butbase,anal_buttons[2:4].name,anal_buttons[2:4].uval,output,sens=anal_buttons[2:4].sens,xs=sz[2].x,ys=sz[2].y
      anal_buttons[2:4].obj=output
      
  
  cgCENTERTLB,ks_anal_b
  WIDGET_CONTROL, ks_anal_b, /realize,group_leader=ks_mb
  XMANAGER,'KS_ANALYSIS_TUNER', ks_anal_b,no_block=1


END