PRO KS_SAVE_RES_PS, indexes, file, sn_break=sn_break, contin=contin
  ; This routine saves main results obtained during the fitting into .ps-file
    COMMON KS_DATA
    COMMON KS_ANALYSIS
    COMMON KS_FLAGS_AND_PARAMS
    COMMON KS_RES_MANAGER_WIDGET
  
    tnames_maps=TAG_NAMES(fit_results_maps)
    simp_ind_max=n_elements(res_loaded)-1
    n_indexes=n_elements(indexes)
    
    nz=sxpar(header_cub,"NAXIS3")
    nx=sxpar(header_cub,"NAXIS1")
    ny=sxpar(header_cub,"NAXIS2")
    xind=get_num(nx,ny,/x)
    yind=get_num(nx,ny,/y)
    
    if not keyword_set(sn_break) then sn_break = 1.
    snr_arr=fit_results_maps.snr
    mask=where(snr_arr lt sn_break, nmask)
    first=1
    
    rec=where(finite(snr_arr) and snr_arr ge sn_break, nreg)
    if nreg le min_maps_points then begin
      res=DIALOG_MESSAGE("Not enough good points for saving maps")
      return
    endif
    
    x0=min(xind[rec])
    x1=max(xind[rec])
    y0=min(yind[rec])
    y1=max(yind[rec])
    
    
    if n_elements(contin) eq 1 then begin
      do_cont=1
      tindex=where(strcmp(tnames_maps,map_tags[contin]) eq 1,nn)
      data_cont=fit_results_maps.(tindex)
      if nmask gt 0 then data_cont[mask]=!Values.F_NAN
      nomis=where(finite(data_cont[x0:x1,y0:y1]),n_nomis)
      if n_nomis gt min_maps_points*3 then tmp=sigrange((data_cont[x0:x1,y0:y1])[nomis],frac=0.97,range=cont_range, Missing='NAN') else $
        cont_range=minmax(data_cont[x0:x1,y0:y1],/NAN)
    endif else do_cont=0
    
    FOR i=0, n_indexes - 1 do begin
      KS_STATE_MONITOR, 4, done=(i+1.)/n_indexes
      if indexes[i] gt simp_ind_max then continue
      tindex=where(strcmp(tnames_maps,map_tags[indexes[i]]) eq 1,nn)
      if nn ne 1 then begin
        print, "ERROR: Can't find map tag name "+map_tags[indexes[i]]+" in res.maps structure. Check it!!!"
        continue
      endif
                
      data_to_save=fit_results_maps.(tindex)
      cur_title=res_modes[indexes[i]].menu_name
        
      if nmask gt 0 then data_to_save[mask]=!Values.F_NAN
      nomis=where(finite(data_to_save[x0:x1,y0:y1]),n_nomis)
      if n_nomis gt min_maps_points*3 then tmp=sigrange((data_to_save[x0:x1,y0:y1])[nomis],frac=0.97,range=brt_range, Missing='NAN') else $
        brt_range=minmax(data_to_save[x0:x1,y0:y1],/NAN)
      if first then begin
        ps_start,file
        first=0
      endif else erase
      cgimage,data_to_save[x0:x1,y0:y1],minval=brt_range[0],maxval=brt_range[1],/keep_asp,position=[0.1,0.05,0.9,0.85],ctind=33,/axes,title=cur_title,/norm, missing_color="white",MISSING_VAL=!Values.F_NAN
      if do_cont then cgcontour,bytscl(data_cont[x0:x1,y0:y1],min=cont_range[0],max=cont_range[1]),nlev=7,color="white",/over,/onimage,lab=0
      cgcolorbar,pos=[0.1,0.92,0.9,0.97],range=[brt_range[0],brt_range[1]],ctind=33,/norm
            
    ENDFOR
      
    if first eq 0 then ps_end  
    KS_STATE_MONITOR, 0
END



Function KS_READ_PAR,FILE
; READ ASCII-file with KINESCOPE stack file
; ( in fits-header format)
  s=''
  par=''
  openr,u,file,/get_lun
   while not(eof(u)) do begin
    readf,u,s
    if strcompress(s,/remove) ne '' then par=[par,s]
   endwhile
  close,u
  free_lun,u
  if n_elements(par) gt 1 then par=par[1:*]
  return,par
END

function KS_sxpar, par, name
  ; Считываем параметр из лога
  curpar=par
  for i=0,n_elements(curpar)-1 do begin
    v1 = STRSPLIT(curpar[i], "( /)", /regex, /EXTRACT)
    curpar[i]=v1[0]
  endfor
  cur_ind= WHERE(STRMATCH(curpar, "*"+name+"*", /FOLD_CASE) EQ 1, nr)
  if  nr eq 0 then return,''
  cur=curpar[cur_ind[0]]
  v1 = STRSPLIT(cur, '=', /EXTRACT)
  v2= STRSPLIT(v1[1], "'", /EXTRACT)
  res=v2[1]
  return,res
end

PRO KS_SAVE_RES_FITS, files, indexes
  COMMON KS_FLAGS_AND_PARAMS
  COMMON KS_RES_MANAGER_WIDGET
  COMMON KS_DATA
  COMMON KS_ANALYSIS
  
  tnames_maps=TAG_NAMES(fit_results_maps)
  tnames_mods=TAG_NAMES(fit_results_model)
  simp_ind_max=n_elements(res_loaded)-1
  n_indexes=n_elements(indexes)
  FOR i=0, n_indexes - 1 do begin
      KS_STATE_MONITOR, 3, done=(i+1.)/n_indexes
      file=files[i]
      inf=FILE_INFO(file)
      IF (file EQ '')  THEN continue
      fdecomp,file,disk,file_dir,file_name,qual
      file_dir=disk+file_dir
      file_name+='.'+qual
      IF (file_name eq '.') then continue
      
      if indexes[i] le simp_ind_max then begin
        tindex=where(strcmp(tnames_maps,map_tags[indexes[i]]) eq 1,nn)
        if nn ne 1 then begin
          print, "ERROR: Can't find map tag name "+map_tags[indexes[i]]+" in res.maps structure. Check it!!!"
          continue
        endif
                
        data_to_save=fit_results_maps.(tindex)
        header_to_save=header_cub_2d
        sxaddhist,"KINESCOPE: "+res_modes[indexes[i]].menu_name+" map created", header_to_save
        if strpos(res_modes[indexes[i]].menu_name, "Comp. Dispersion") ne -1 then $
          sxaddhist,"KINESCOPE: Thermal and natural broadening with sigma="+string(extern_disp.vel,format="(F0.2)")+" km/s subtracted; Obtained disp. < 0 masked", header_to_save
      endif else begin
        tindex=where(strcmp(tnames_mods,mod_tags[indexes[i]-simp_ind_max-1]) eq 1,nn)
        if nn ne 1 then begin
          print, "ERROR: Can't find model tag name "+mod_tags[indexes[i]-simp_ind_max-1]+" in res.model structure. Check it!!!"
          continue
        endif
        data_to_save=fit_results_model.(tindex)
        header_to_save=header_cub
        sxaddhist,"KINESCOPE: "+res_modes[indexes[i]].menu_name+" created", header_to_save
      endelse
      writefits,file, data_to_save, header_to_save
      WIDGET_CONTROL, res_modes[indexes[i]].stobj,set_val="OK"
    ENDFOR
    KS_STATE_MONITOR, 0
END


PRO KS_LOAD_RES_FITS, files, indexes, success=success
  COMMON KS_FLAGS_AND_PARAMS
  COMMON KS_RES_MANAGER_WIDGET
  COMMON KS_DATA
  COMMON KS_ANALYSIS
  
  wrong=[-1]
  success=0
  nx=sxpar(header_cub,"NAXIS1")
  ny=sxpar(header_cub,"NAXIS2")
  nz=sxpar(header_cub,"NAXIS3")
  tnames_maps=TAG_NAMES(fit_results_maps)
  tnames_mods=TAG_NAMES(fit_results_model)
  simp_ind_max=n_elements(res_loaded)-1
  FOR i=0,n_elements(files)-1 do begin 
    KS_STATE_MONITOR, 5, done=(i+1.)/n_elements(files)
    file=files[i]
    inf=FILE_INFO(file)
    IF NOT (inf.exists) OR (file EQ '')  THEN continue
      
    fdecomp,file,disk,file_dir,file_name,qual
    file_name+='.'+qual
    IF (file_name eq '.') then continue
      
    data_loaded=readfits(file, header_loaded)
      
    if sxpar(header_loaded,"NAXIS1") ne nx or sxpar(header_loaded,"NAXIS2") ne ny then begin
      wrong=[wrong,i]
      continue
    endif
      
    ;Если загружаем карту
    if indexes[i] le simp_ind_max then begin
        tindex=where(strcmp(tnames_maps,map_tags[indexes[i]]) eq 1,nn)
        if nn ne 1 then begin
          print, "ERROR: Can't find map tag name "+map_tags[indexes[i]]+" in res.maps structure. Check it!!!"
          continue
        endif
        fit_results_maps.(tindex)=data_loaded
        res_loaded[indexes[i]] = 1
        rec = where(~finite(data_loaded),nr)
        if nr gt 0 then begin
          r=where(['mom0','mom1','mom2','mom3','mom4','snr','contin'] eq map_tags[indexes[i]], nnr)
          if nnr eq 1 then bad_mask_moms[rec]=1
        endif
        success+=1
    endif else begin
    ; Если загружаем модель
       if sxpar(header_loaded,"NAXIS3") ne nz then begin
          wrong=[wrong,i]
          continue
       endif
       tindex=where(strcmp(tnames_mods,mod_tags[indexes[i]-simp_ind_max-1]) eq 1,nn)
       if nn ne 1 then begin
          print, "ERROR: Can't find model tag name "+mod_tags[indexes[i]-simp_ind_max-1]+" in res.model structure. Check it!!!"
          continue
       endif
       fit_results_model.(tindex)=data_loaded
       rec=where(finite(total(data_loaded,3)),nr)
       if tindex eq 0 then begin
         fit_results_maps.is_set1[*]=0
         if nr gt 0 then fit_results_maps.is_set1[rec]=1
       endif
       if tindex eq 1 then begin
         fit_results_maps.is_set2[*]=0
         if nr gt 0 then fit_results_maps.is_set2[rec]=1
       endif
       if tindex eq 2 then begin
          fit_results_maps.is_set3[*]=0
         if nr gt 0 then fit_results_maps.is_set3[rec]=1
       endif
      endelse
      WIDGET_CONTROL, res_modes[indexes[i]].stobj,set_val="OK"
   ENDFOR

   rec=where(~finite(fit_results_maps.mom0) or ~finite(fit_results_maps.mom1) or ~finite(fit_results_maps.mom2) or ~finite(fit_results_maps.mom3) or $
              ~finite(fit_results_maps.mom4) or ~finite(fit_results_maps.snr) or ~finite(fit_results_maps.contin), nr)
   if nr ne 0 then fit_results_maps.fitted[rec] = 0
   rec=where((finite(fit_results_maps.mom0) and finite(fit_results_maps.mom1) and finite(fit_results_maps.mom2) and finite(fit_results_maps.mom3) and $
             finite(fit_results_maps.mom4) and finite(fit_results_maps.snr) and finite(fit_results_maps.contin)) and $
             (~finite(fit_results_maps.f_tot) or ~finite(fit_results_maps.f1) or ~finite(fit_results_maps.i1) or ~finite(fit_results_maps.v1) or $
             ~finite(fit_results_maps.sigma1) or ~finite(fit_results_maps.resid)), nr)
   if nr gt 0 then fit_results_maps.fitted[rec] = 2
   rec=where((finite(fit_results_maps.mom0) and finite(fit_results_maps.mom1) and finite(fit_results_maps.mom2) and finite(fit_results_maps.mom3) and $
             finite(fit_results_maps.mom4) and finite(fit_results_maps.snr) and finite(fit_results_maps.contin)) and $
             (finite(fit_results_maps.f_tot) and finite(fit_results_maps.f1) and finite(fit_results_maps.i1) and finite(fit_results_maps.v1) and $
             finite(fit_results_maps.sigma1) and finite(fit_results_maps.resid)) and (fit_results_maps.is_set1 eq 1 or fit_results_maps.is_set2 eq 1 $
             or fit_results_maps.is_set3 eq 1), nr)
   if nr gt 0 then fit_results_maps.fitted[rec] = 1
   
   
   if n_elements(wrong) gt 1 then begin
   
    r=dialog_message("Incorrect dimmension for set: "+res_modes[indexes[wrong[1:n_elements(wrong)-1]]].menu_name)
   endif
   
   KS_STATE_MONITOR, 0
END


pro KS_RES_MANAGER_EVENT, event
  COMMON KS_FLAGS_AND_PARAMS
  COMMON KS_RES_MANAGER_WIDGET
  COMMON KS_DATA
  COMMON KS_ANALYSIS
  COMMON KS_SIZES
  
  WIDGET_CONTROL,event.ID,get_uvalue=ev
  
  sub_event=strmid(ev,0,4)
  IF sub_event eq 'save' or sub_event eq 'load' then begin
    indexes=where(res_modes[*].val eq 1, n_indexes)
    if n_indexes eq 0 then begin
      if (ev ne 'load_stack' and ev ne 'save_stack') then return
      n_indexes=n_elements(res_modes[*])
      indexes=indgen(n_indexes)
    endif
    stack_keywords=["FLUX_TOT","FLUX_C  ","FLUX_B  ","FLUX_R  ","FLUX_SHI","INTENS_C","INTENS_B","INTENS_R","INT_SHI ","VELO_C  ","VELO_B  ","VELO_R  ","VELO_SHI","VDIF_MAP",$
    "DISP_C  ","DISP_B  ","DISP_R  ","DISP_SHI","MOMENT_0","MOMENT_1","MOMENT_2","MOMENT_3","MOMENT_4","SN_RATIO","CONTIN  ","RESID_MP","MOD_CENT","MOD_BLUE","MOD_RED ","MOD_RES "]
    stack_fsuff=["fl_tot","fl_c","fl_b","fl_r","fl_shi","int_c","int_b","int_r","int_shi","vel_c","vel_b","vel_r","vel_shi","vel_diffmap","disp_c","disp_b","disp_r","disp_shi",$
                      "mom0","mom1","mom2","mom3","mom4","snr","cont","resid","model_c","model_b","model_r","model_resid"]
  endif
  
  Case ev OF
  
  'load_stack': BEGIN
     filter=['*.txt']
     files_to_load=['']
     stackfile=DIALOG_PICKFILE(title='Load Stack',filter=filter,get_path=w_dir)
     cd,w_dir
     inf=FILE_INFO(stackfile)
     IF NOT (inf.exists) OR (stackfile EQ '')  THEN return
     fdecomp,stackfile,disk,file_dir,file_name,qual
     file_dir=disk+file_dir
     file_name+='.'+qual
     IF (file_name eq '.') then return
     par=ks_read_par(stackfile)
  
     dir=ks_sxpar(par,"DATA_DIR")
     name=ks_sxpar(par,"NAME")
     FOR i=0, n_indexes - 1 do begin
      file=ks_sxpar(par,stack_keywords[indexes[i]])
      if file then file=slash(dir)+file
      files_to_load=[files_to_load,file]
     ENDFOR
     files_to_load=files_to_load[1:n_indexes]
     KS_LOAD_RES_FITS, files_to_load, indexes, success=success
     if success gt 0 then begin
      KS_TRIGGER_SENS_RES_PANEL, 1
      KS_ADJUST_RESLIST
     endif
    
  END
  
  'save_stack': BEGIN
     filter=['*.txt']
     files_to_save=['']
     stackfile=DIALOG_PICKFILE(title='Save Stack',filter=filter,get_path=w_dir,default_ext="txt")
     cd,w_dir
     inf=FILE_INFO(stackfile)
     IF (stackfile EQ '')  THEN return
     fdecomp,stackfile,disk,file_dir,file_name,qual
     
     file_dir=slash(disk+file_dir)
     IF (file_name+'.'+qual eq '.') then return
     stack_dir=slash(file_dir+file_name)
     simp_ind_max=n_elements(res_loaded)-1
     first=1
     FOR i=0, n_indexes - 1 do begin
        ok=0
        file=''
        if indexes[i] le simp_ind_max then begin
          if res_loaded[indexes[i]] eq 1 then ok=1
        endif else begin
           if indexes[i] eq simp_ind_max+1 and total(fit_results_maps.is_set1) gt min_maps_points then ok=1
           if indexes[i] eq simp_ind_max+2 and total(fit_results_maps.is_set2) gt min_maps_points then ok=1
           if indexes[i] eq simp_ind_max+3 and total(fit_results_maps.is_set3) gt min_maps_points then ok=1
           if indexes[i] eq simp_ind_max+4 and total((fit_results_maps.is_set1+fit_results_maps.is_set2+fit_results_maps.is_set3) < 1) gt min_maps_points then ok=1
        endelse
        if ok then begin
          if first then begin  
            ff=file_info(stack_dir)
            if not ff.exists then file_mkdir,stack_dir
     
            openw,u,stackfile,/get_lun
            printf,u,"SIMPLE  =                            T / KINESCOPE PARAMETERS"
            printf,u,"DATA_DIR= '"+stack_dir+"'  / DIR containing results"  
            printf,u,"NAME    = '"+file_name+"'  / OUTPUT FILENAME"
            first=0
          endif
            file=file_name+'_'+stack_fsuff[indexes[i]]+'.fits'
            printf,u,stack_keywords[indexes[i]]+"= '"+file+"'  / PATH TO THIS RESULT FILE"
            file=stack_dir+file
        endif
        files_to_save=[files_to_save,file]
      ENDFOR
      if first eq 0 then begin
        printf,u,"END"
        close,u
        free_lun,u
      endif
      files_to_save=files_to_save[1:n_indexes]
      if first eq 0 then KS_SAVE_RES_FITS, files_to_save, indexes else r=DIALOG_MESSAGE("Nothing to save!")
  END
  
  
  'save_selected':BEGIN
    filter=['*.f*ts','*.F*TS']
    files_to_save=['']
    simp_ind_max=n_elements(res_loaded)-1
    FOR i=0, n_indexes - 1 do begin
      ok=0
      if indexes[i] le simp_ind_max then begin
        file=''
        if res_loaded[indexes[i]] eq 1 then ok=1
      endif else begin
         if indexes[i] eq simp_ind_max+1 and total(fit_results_maps.is_set1) gt min_maps_points then ok=1
         if indexes[i] eq simp_ind_max+2 and total(fit_results_maps.is_set2) gt min_maps_points then ok=1
         if indexes[i] eq simp_ind_max+3 and total(fit_results_maps.is_set3) gt min_maps_points then ok=1
         if indexes[i] eq simp_ind_max+4 and total((fit_results_maps.is_set1+fit_results_maps.is_set2+fit_results_maps.is_set3) < 1) gt min_maps_points then ok=1
      endelse
      if ok then begin
        file=DIALOG_PICKFILE(title='Save '+res_modes[indexes[i]].menu_name,filter=filter,get_path=w_dir,default_ext="fits")
        cd,w_dir
      endif
      files_to_save=[files_to_save,file]
    ENDFOR
    files_to_save=files_to_save[1:n_indexes]
    KS_SAVE_RES_FITS, files_to_save, indexes
    
  END
  
  'save_ps':BEGIN
    filter=['*.ps']
    file=DIALOG_PICKFILE(title='Save selected files to .ps',filter=filter,get_path=w_dir,default_ext="ps")
    cd,w_dir
    inf=FILE_INFO(file)
    IF (file EQ '')  THEN return
    fdecomp,file,disk,file_dir,file_name,qual
    file_dir=disk+file_dir
    file_name+='.'+qual
    IF (file_name eq '.') then return
    
    KS_SAVE_RES_PS, indexes, file, sn_break=5, contin=0
    
  END
  
  
  'load_selected': BEGIN
    filter=['*.f*ts','*.F*TS']
    files_to_load=['']
    FOR i=0, n_indexes - 1 do begin
      file=DIALOG_PICKFILE(title='Load '+res_modes[indexes[i]].menu_name,filter=filter,get_path=w_dir)
      cd,w_dir
      files_to_load=[files_to_load,file]
    ENDFOR
    files_to_load=files_to_load[1:n_indexes]
    KS_LOAD_RES_FITS, files_to_load, indexes, success=success
    if success gt 0 then begin
      KS_TRIGGER_SENS_RES_PANEL, 1
      KS_ADJUST_RESLIST
    endif  
  END
  
  'close': BEGIN
    WIDGET_CONTROL,ks_resman_b, map=0
  END
  
  'mode_select': BEGIN
    id=event.id
    rec=where(res_modes[*].obj eq id, nr)
    if nr eq 1 then res_modes[rec].val=1-res_modes[rec].val
  END
  
  ELSE: BEGIN
  
    sub_event=strmid(ev,0,6)
    IF sub_event eq "select" then begin
      sub_mode=strmid(ev,7,3)
      sub_type=fix(strmid(ev,11,1))
      possible_start=intarr(7)
      possible_nums=[5,4,5,4,5,3,4]
      for i=1,6 do possible_start[i]=total(possible_nums[0:i-1])
      if sub_type eq 0 then indexes=indgen(total(possible_nums)) else indexes=indgen(possible_nums[sub_type-1])+possible_start[sub_type-1]
      IF sub_mode eq 'all' then val=1
      IF sub_mode eq 'non' then val=0
      FOR i=0,n_elements(indexes)-1 do  begin
      WIDGET_CONTROL,res_modes[indexes[i]].obj,set_button=val
      res_modes[indexes[i]].val=val
      endfor
    ENDIF
  
  ENDELSE
  
  ENDCASE

END



PRO KS_RES_MANAGER 
  COMMON KS_WIDGET_ELEMENTS
  COMMON KS_FLAGS_AND_PARAMS
  COMMON KS_RES_MANAGER_WIDGET
  COMMON KS_SIZES
  
  ks_resman_b=WIDGET_BASE(TITLE="KINEScope: Results Manager",/row,GROUP_LEADER=ks_mb, map=0)

  if (!VERSION.OS_FAMILY eq "Windows") then titfont="Sans Serif" else titfont="-misc-fixed-bold-r-normal--13-120-75-75-c-70-iso8859-1"
  
  resman_buttons=[{obj_par,'Load Stack','load_stack',0,0,1},$
                  {obj_par,'Save Stack','save_stack',0,0,1},$
                  {obj_par,'Load Selected','load_selected',0,0,1},$
                  {obj_par,'Save Selected','save_selected',0,0,1},$
                  {obj_par,'Save to PS','save_ps',0,0,1},$
                  {obj_par,'Close','close',0,0,1},$
                  {obj_par,'Select All','select_all_0',0,0,1},$
                  {obj_par,'Select None','select_non_0',0,0,1}]
  
  
  
  ks_resman_butbase=WIDGET_BASE(ks_resman_b,/col, /frame)
  KS_Buttons_Cre,ks_resman_butbase,resman_buttons[0:5].name,resman_buttons[0:5].uval,output,sens=resman_buttons[0:5].sens,xs=sz[2].x,ys=sz[2].y,break_arr=[1,3,4]
  resman_buttons[0:5].obj=output

  ks_selection_base_wraper=WIDGET_BASE(ks_resman_b,/col, xpad=0,xoffset=0)
  ks_selection_base=WIDGET_BASE(ks_selection_base_wraper,/row, xpad=0,xoffset=0)
  
  tmp={obj_wstat_par,name:'',uval:'',obj:0L,stobj:0L,val:0E,sens:1L, menu_name:''}
  
  res_modes_buts=[{obj_par,'Sel. All','select_all_1',0,0,1},$
                  {obj_par,'Sel. None','select_non_1',0,0,1},$
                  {obj_par,'Sel. All','select_all_2',0,0,1},$
                  {obj_par,'Sel. None','select_non_2',0,0,1},$
                  {obj_par,'Sel. All','select_all_3',0,0,1},$
                  {obj_par,'Sel. None','select_non_3',0,0,1},$
                  {obj_par,'Sel. All','select_all_4',0,0,1},$
                  {obj_par,'Sel. None','select_non_4',0,0,1},$
                  {obj_par,'Sel. All','select_all_5',0,0,1},$
                  {obj_par,'Sel. None','select_non_5',0,0,1},$
                  {obj_par,'Sel. All','select_all_6',0,0,1},$
                  {obj_par,'Sel. None','select_non_6',0,0,1},$
                  {obj_par,'Sel. All','select_all_7',0,0,1},$
                  {obj_par,'Sel. None','select_non_7',0,0,1}]
  
  res_modes=[{obj_wstat_par,'Total','mode_select',0,0,0,1, "Total Flux"},$ ;FLUX
             {obj_wstat_par,'Central comp.','mode_select',0,0,0,1, "Cent. Comp. Flux"},$ ;FLUX
             {obj_wstat_par,'Blue comp.','mode_select',0,0,0,1, "Blue Comp. Flux"},$ ;FLUX
             {obj_wstat_par,'Red comp.','mode_select',0,0,0,1, "Red Comp. Flux"},$ ;FLUX
             {obj_wstat_par,'Shifted comp.','mode_select',0,0,0,1, "Shifted Comp. Flux"},$ ;FLUX
             {obj_wstat_par,'Central comp.','mode_select',0,0,0,1, "Cent. Comp. Intens."},$ ;Intens
             {obj_wstat_par,'Blue comp.','mode_select',0,0,0,1,"Blue Comp. Intens."},$ ;Intens
             {obj_wstat_par,'Red comp.','mode_select',0,0,0,1,"Red Comp. Intens."},$ ;Intens
             {obj_wstat_par,'Shifted comp.','mode_select',0,0,0,1,"Shifted Comp. Intens."},$ ;Intens
             {obj_wstat_par,'Central comp.','mode_select',0,0,0,1,"Cent. Comp. Velocity"},$ ;Velocity
             {obj_wstat_par,'Blue comp.','mode_select',0,0,0,1,"Blue Comp. Velocity"},$ ;Velocity
             {obj_wstat_par,'Red comp.','mode_select',0,0,0,1,"Red Comp. Velocity"},$ ;Velocity
             {obj_wstat_par,'Shifted comp.','mode_select',0,0,0,1,"Shifted Comp. Velocity"},$ ;Velocity
             {obj_wstat_par,'Difference map','mode_select',0,0,0,1,"Map of vel. differences"},$ ;Velocity
             {obj_wstat_par,'Central comp.','mode_select',0,0,0,1,"Cent. Comp. Dispersion"},$ ;Dispersion
             {obj_wstat_par,'Blue comp.','mode_select',0,0,0,1,"Blue Comp. Dispersion"},$ ;Dispersion
             {obj_wstat_par,'Red comp.','mode_select',0,0,0,1,"Red Comp. Dispersion"},$ ;Dispersion
             {obj_wstat_par,'Shifted comp.','mode_select',0,0,0,1,"Shifted Comp. Dispersion"},$ ;Dispersion
             {obj_wstat_par,'MOM0: Flux','mode_select',0,0,0,1,'MOM0: Flux'},$ ;Moments
             {obj_wstat_par,'MOM1: Velocity','mode_select',0,0,0,1,'MOM1: Velocity'},$ ;Moments
             {obj_wstat_par,'MOM2: Dispers.','mode_select',0,0,0,1,'MOM2: Dispersion'},$ ;Moments
             {obj_wstat_par,'MOM3: Assym.','mode_select',0,0,0,1,'MOM3: Assymmetry'},$ ;Moments
             {obj_wstat_par,'MOM4: Kurt.','mode_select',0,0,0,1,'MOM4: Kurt.'},$ ;Moments
             {obj_wstat_par,'S/N ratio','mode_select',0,0,0,1,'S/N ratio'},$ ;Other
             {obj_wstat_par,'Continuum','mode_select',0,0,0,1,'Continuum'},$ ;Other
             {obj_wstat_par,'Residuals','mode_select',0,0,0,1,'Residuals'},$ ;Other
             {obj_wstat_par,'Central comp.','mode_select',0,0,0,1,'Model of Central Comp.'},$ ;Models
             {obj_wstat_par,'Blue comp.','mode_select',0,0,0,1,'Model of Blue Comp.'},$ ;Models
             {obj_wstat_par,'Red comp.','mode_select',0,0,0,1,'Model of Red Comp.'},$ ;Models
             {obj_wstat_par,'Residuals','mode_select',0,0,0,1,'Rediduals of Model'}] ;Models
  
    tmp_base=WIDGET_BASE(ks_selection_base,/col,/frame)
    lab=WIDGET_LABEL(tmp_base,val="Flux",font=titfont)
    tmp2_base=WIDGET_BASE(tmp_base,/row,/align_center,xoffset=0,xpad=0)
    bst=0
    KS_Buttons_Cre,tmp2_base,res_modes_buts[bst:bst+1].name,res_modes_buts[bst:bst+1].uval,output,sens=res_modes_buts[bst:bst+1].sens,xs=sz[2].x/2,ys=sz[2].y
    res_modes_buts[bst:bst+1].obj=output
    start=0
    num=5
    output=lonarr(n_elements(res_modes[start:start+num-1]))
    KS_Buttons_Cre,tmp_base,res_modes[start:start+num-1].name,res_modes[start:start+num-1].uval,output,sens=res_modes[start:start+num-1].sens,/nonexclusive,$
                   /cre_stat,status=status
    res_modes[start:start+num-1].obj=output
    res_modes[start:start+num-1].stobj=status
    
    
    tmp_base=WIDGET_BASE(ks_selection_base,/col,/frame)
    lab=WIDGET_LABEL(tmp_base,val="Intensity",font=titfont)
    tmp2_base=WIDGET_BASE(tmp_base,/row,/align_center,xoffset=0,xpad=0)
    bst+=2
    KS_Buttons_Cre,tmp2_base,res_modes_buts[bst:bst+1].name,res_modes_buts[bst:bst+1].uval,output,sens=res_modes_buts[bst:bst+1].sens,xs=sz[2].x/2,ys=sz[2].y
    res_modes_buts[bst:bst+1].obj=output
    start+=num
    num=4
    output=lonarr(n_elements(res_modes[start:start+num-1]))
    KS_Buttons_Cre,tmp_base,res_modes[start:start+num-1].name,res_modes[start:start+num-1].uval,output,sens=res_modes[start:start+num-1].sens,/nonexclusive,$
                   /cre_stat,status=status
    res_modes[start:start+num-1].obj=output
    res_modes[start:start+num-1].stobj=status
    
    
    tmp_base=WIDGET_BASE(ks_selection_base,/col,/frame)
    lab=WIDGET_LABEL(tmp_base,val="Velocity",font=titfont)
    tmp2_base=WIDGET_BASE(tmp_base,/row,/align_center,xoffset=0,xpad=0)
    bst+=2
    KS_Buttons_Cre,tmp2_base,res_modes_buts[bst:bst+1].name,res_modes_buts[bst:bst+1].uval,output,sens=res_modes_buts[bst:bst+1].sens,xs=sz[2].x/2,ys=sz[2].y
    res_modes_buts[bst:bst+1].obj=output
    start+=num
    num=5
    output=lonarr(n_elements(res_modes[start:start+num-1]))
    KS_Buttons_Cre,tmp_base,res_modes[start:start+num-1].name,res_modes[start:start+num-1].uval,output,sens=res_modes[start:start+num-1].sens,/nonexclusive,$
                   /cre_stat,status=status
    res_modes[start:start+num-1].obj=output
    res_modes[start:start+num-1].stobj=status
    
    tmp_base=WIDGET_BASE(ks_selection_base,/col,/frame)
    lab=WIDGET_LABEL(tmp_base,val="Dispersion",font=titfont)
    tmp2_base=WIDGET_BASE(tmp_base,/row,/align_center,xoffset=0,xpad=0)
    bst+=2
    KS_Buttons_Cre,tmp2_base,res_modes_buts[bst:bst+1].name,res_modes_buts[bst:bst+1].uval,output,sens=res_modes_buts[bst:bst+1].sens,xs=sz[2].x/2,ys=sz[2].y
    res_modes_buts[bst:bst+1].obj=output
    start+=num
    num=4
    output=lonarr(n_elements(res_modes[start:start+num-1]))
    KS_Buttons_Cre,tmp_base,res_modes[start:start+num-1].name,res_modes[start:start+num-1].uval,output,sens=res_modes[start:start+num-1].sens,/nonexclusive,$
                   /cre_stat,status=status
    res_modes[start:start+num-1].obj=output
    res_modes[start:start+num-1].stobj=status
  
    tmp_base=WIDGET_BASE(ks_selection_base,/col,/frame)
    lab=WIDGET_LABEL(tmp_base,val="Moments",font=titfont)
    tmp2_base=WIDGET_BASE(tmp_base,/row,/align_center,xoffset=0,xpad=0)
    bst+=2
    KS_Buttons_Cre,tmp2_base,res_modes_buts[bst:bst+1].name,res_modes_buts[bst:bst+1].uval,output,sens=res_modes_buts[bst:bst+1].sens,xs=sz[2].x/2,ys=sz[2].y
    res_modes_buts[bst:bst+1].obj=output
    start+=num
    num=5
    output=lonarr(n_elements(res_modes[start:start+num-1]))
    KS_Buttons_Cre,tmp_base,res_modes[start:start+num-1].name,res_modes[start:start+num-1].uval,output,sens=res_modes[start:start+num-1].sens,/nonexclusive,$
                   /cre_stat,status=status
    res_modes[start:start+num-1].obj=output
    res_modes[start:start+num-1].stobj=status
    
    
    
    tmp_base=WIDGET_BASE(ks_selection_base,/col,/frame)
    lab=WIDGET_LABEL(tmp_base,val="Other",font=titfont)
    tmp2_base=WIDGET_BASE(tmp_base,/row,/align_center,xoffset=0,xpad=0)
    bst+=2
    KS_Buttons_Cre,tmp2_base,res_modes_buts[bst:bst+1].name,res_modes_buts[bst:bst+1].uval,output,sens=res_modes_buts[bst:bst+1].sens,xs=sz[2].x/2,ys=sz[2].y
    res_modes_buts[bst:bst+1].obj=output
    start+=num
    num=3
    output=lonarr(n_elements(res_modes[start:start+num-1]))
    KS_Buttons_Cre,tmp_base,res_modes[start:start+num-1].name,res_modes[start:start+num-1].uval,output,sens=res_modes[start:start+num-1].sens,/nonexclusive,$
                   /cre_stat,status=status
    res_modes[start:start+num-1].obj=output
    res_modes[start:start+num-1].stobj=status
    
    tmp_base=WIDGET_BASE(ks_selection_base,/col,/frame)
    lab=WIDGET_LABEL(tmp_base,val="Models",font=titfont)
    tmp2_base=WIDGET_BASE(tmp_base,/row,/align_center,xoffset=0,xpad=0)
    bst+=2
    KS_Buttons_Cre,tmp2_base,res_modes_buts[bst:bst+1].name,res_modes_buts[bst:bst+1].uval,output,sens=res_modes_buts[bst:bst+1].sens,xs=sz[2].x/2,ys=sz[2].y
    res_modes_buts[bst:bst+1].obj=output
    start+=num
    num=4
    output=lonarr(n_elements(res_modes[start:start+num-1]))
    KS_Buttons_Cre,tmp_base,res_modes[start:start+num-1].name,res_modes[start:start+num-1].uval,output,sens=res_modes[start:start+num-1].sens,/nonexclusive,$
                   /cre_stat,status=status
    res_modes[start:start+num-1].obj=output
    res_modes[start:start+num-1].stobj=status


    ;########### 
    ks_tmp_base=WIDGET_BASE(ks_selection_base_wraper,/row,/align_center)
    lab=WIDGET_LABEL(ks_tmp_base,val="OK",font=titfont)
    lab=WIDGET_LABEL(ks_tmp_base,val=": Data exists and changes are saved; ")
    lab=WIDGET_LABEL(ks_tmp_base,val="NS",font=titfont)
    lab=WIDGET_LABEL(ks_tmp_base,val=": Data exists, but changes are not saved")
    ks_selbuts_base=WIDGET_BASE(ks_selection_base_wraper,/row,/align_center)
    KS_Buttons_Cre,ks_selbuts_base,resman_buttons[6:7].name,resman_buttons[6:7].uval,output,sens=resman_buttons[6:7].sens,xs=sz[2].x,ys=sz[2].y
    resman_buttons[6:7].obj=output
    
    
  cgCENTERTLB,ks_resman_b
  WIDGET_CONTROL, ks_resman_b, /realize,group_leader=ks_mb
  XMANAGER,'KS_RES_MANAGER', ks_resman_b,no_block=1

END