PRO KS_DEF
    COMMON KS_SIZES, sz
    COMMON KS_WIDGET_ELEMENTS, buttons, ks_mb, ks_file_info, ks_vm_res_b, ks_vm_cub_b, ks_st_cub_b, ks_st_res_b,$
                               field_brtmax, field_brtmin, field_brtfrac, field_channels,field_channels_obj, $
                               field_brtmax_obj, field_brtmin_obj, field_brtfrac_obj, but_auto_brt, field_xrange_obj, field_xrange, chan_string_cur, $
                               monitors, ks_sml_b, ks_smr_b, ks_shape_b, field_zoom_cub_obj,field_zoom_cub,state_monitor,$
                               field_zoom_res_obj,field_zoom_res, but_curprof_monitor, list_of_restypes,field_snr_mask,field_snr_mask_obj
                                
    COMMON KS_DISPLAY, ks_cube_disp, ks_res_disp, pos_on_disp,  cur_channel, cur_im, h_cur_im, cur_res, h_cur_res, $
                       cur_res_index,cur_res_name, xscr_cur, yscr_cur, mode_view, show_type, start_draw_bord, bord_draw,clone_wID, $
                       selection_mode, selection_shape, zoom_border, zoom_factor, markers, color_tab, curprof_monitor, snr_mask,$
                       clone_parallel_res_ID,clone_parallel_cub_ID, curprof_selection, prof_xrange
    COMMON KS_DATA, file_cub, header_cub, cub, cub_2d, header_cub_2d, image, header_image, filenames, axes_info, $
                    extern_disp, inst_fwhm, binmap, header_binmap
    COMMON KS_FLAGS_AND_PARAMS, cub_loaded, im_loaded, res_loaded, num_res_subtypes, binmap_loaded
    COMMON KS_BRIGHTNESS, brtmin, brtmax, brtfrac, auto_br
    COMMON KS_ANALYSIS, cur_fit_selection, moms_on_fly, n_fitpoints, fit_results_maps, ks_analysis_limiter, ks_analysis_sortmode, $
                        fit_results_model, fit_pixels,fit_overwrite, fit_proftype, fit_method, min_maps_points, map_tags, mod_tags
    COMMON KS_RES_MANAGER_WIDGET, ks_resman_b, resman_buttons, res_modes
    COMMON KS_ANAL_MANAGER_WIDGET, ks_anal_b, anal_buttons, an_infomessage, ks_an_info_label,wid_ax_info, wid_inst_fwhm,$
                                   fit_proftype_but, fit_method_but, wid_extern_disp,fit_setcent_but,$
                                    ks_simplify_thresh,fit_simplify_but,fit_uselimits_but,fit_complimits_but,ks_table_lims_profcomp,$
                          ks_anlim_comp3_asetmax,ks_anlim_comp3_asetmin,ks_anlim_comp3_fsetmax,ks_anlim_comp3_fsetmin,ks_anlim_comp3_csetmax,ks_anlim_comp3_csetmin,$
                          ks_anlim_comp2_asetmax,ks_anlim_comp2_asetmin,ks_anlim_comp2_fsetmax,ks_anlim_comp2_fsetmin,ks_anlim_comp2_csetmax,ks_anlim_comp2_csetmin,$
                          ks_anlim_comp1_asetmax,ks_anlim_comp1_asetmin,ks_anlim_comp1_fsetmax,ks_anlim_comp1_fsetmin,ks_anlim_comp1_csetmax,ks_anlim_comp1_csetmin,$
                          ks_anlim_comp3_amax,ks_anlim_comp3_amin,ks_anlim_comp3_fmax,ks_anlim_comp3_fmin,ks_anlim_comp3_cmax,ks_anlim_comp3_cmin,$
                          ks_anlim_comp2_amax,ks_anlim_comp2_amin,ks_anlim_comp2_fmax,ks_anlim_comp2_fmin,ks_anlim_comp2_cmax,ks_anlim_comp2_cmin,$
                          ks_anlim_comp1_amax,ks_anlim_comp1_amin,ks_anlim_comp1_fmax,ks_anlim_comp1_fmin,ks_anlim_comp1_cmax,ks_anlim_comp1_cmin
    COMMON KS_PROF_MANAGER, ks_profman_b, profman_buttons, ks_table_prof, ks_table_profcomp,ks_prof_disp, prof_default, prof_storage, n_prof_table, prof_selected,$
                          profile_selection, n_prof_options, ks_profman_method_but, ks_profman_method,ks_profman_setcomp1,ks_profman_setcomp2,ks_profman_setcomp3,$ 
                          ks_profman_comp3_asetmax,ks_profman_comp3_asetmin,ks_profman_comp3_amin,ks_profman_comp3_amax,ks_profman_comp3_afix,ks_profman_comp3_a,$
                          ks_profman_comp2_asetmax,ks_profman_comp2_asetmin,ks_profman_comp2_amin,ks_profman_comp2_amax,ks_profman_comp2_afix,ks_profman_comp2_a,$
                          ks_profman_comp1_asetmax,ks_profman_comp1_asetmin,ks_profman_comp1_amin,ks_profman_comp1_amax,ks_profman_comp1_afix,ks_profman_comp1_a,$
                          ks_profman_comp3_fsetmax,ks_profman_comp3_fsetmin,ks_profman_comp3_fmin,ks_profman_comp3_fmax,ks_profman_comp3_ffix,ks_profman_comp3_f,$
                          ks_profman_comp2_fsetmax,ks_profman_comp2_fsetmin,ks_profman_comp2_fmin,ks_profman_comp2_fmax,ks_profman_comp2_ffix,ks_profman_comp2_f,$
                          ks_profman_comp1_fsetmax,ks_profman_comp1_fsetmin,ks_profman_comp1_fmin,ks_profman_comp1_fmax,ks_profman_comp1_ffix,ks_profman_comp1_f,$
                          ks_profman_comp3_csetmax,ks_profman_comp3_csetmin,ks_profman_comp3_cmin,ks_profman_comp3_cmax,ks_profman_comp3_cfix,ks_profman_comp3_c,$
                          ks_profman_comp2_csetmax,ks_profman_comp2_csetmin,ks_profman_comp2_cmin,ks_profman_comp2_cmax,ks_profman_comp2_cfix,ks_profman_comp2_c,$
                          ks_profman_comp1_csetmax,ks_profman_comp1_csetmin,ks_profman_comp1_cmin,ks_profman_comp1_cmax,ks_profman_comp1_cfix,ks_profman_comp1_c,$
                          proftab_edit, ks_profman_contsetmin, ks_profman_contsetmax, ks_profman_contmin, ks_profman_contmax, ks_profman_cont, ks_profman_contfix,$
                          ks_prof_showres,ks_profman_monitors,ks_profman_pos_on_disp, ks_profman_xrange, ks_profman_field_xrange_obj,ks_profman_field_xrange
                          
    COMMON KS_CURPROF_MANAGER, ks_curprofman_b, curprofman_buttons, ks_table_curprofcomp,ks_curprof_disp, curprofman_pix, ks_curprofman_inicomps, $
                          n_curprof_options, ks_curprofman_method_but, ks_curprofman_method,ks_curprofman_setcomp1,ks_curprofman_setcomp2,ks_curprofman_setcomp3,$ 
                          ks_curprofman_comp3_asetmax,ks_curprofman_comp3_asetmin,ks_curprofman_comp3_amin,ks_curprofman_comp3_amax,ks_curprofman_comp3_afix,ks_curprofman_comp3_a,$
                          ks_curprofman_comp2_asetmax,ks_curprofman_comp2_asetmin,ks_curprofman_comp2_amin,ks_curprofman_comp2_amax,ks_curprofman_comp2_afix,ks_curprofman_comp2_a,$
                          ks_curprofman_comp1_asetmax,ks_curprofman_comp1_asetmin,ks_curprofman_comp1_amin,ks_curprofman_comp1_amax,ks_curprofman_comp1_afix,ks_curprofman_comp1_a,$
                          ks_curprofman_comp3_fsetmax,ks_curprofman_comp3_fsetmin,ks_curprofman_comp3_fmin,ks_curprofman_comp3_fmax,ks_curprofman_comp3_ffix,ks_curprofman_comp3_f,$
                          ks_curprofman_comp2_fsetmax,ks_curprofman_comp2_fsetmin,ks_curprofman_comp2_fmin,ks_curprofman_comp2_fmax,ks_curprofman_comp2_ffix,ks_curprofman_comp2_f,$
                          ks_curprofman_comp1_fsetmax,ks_curprofman_comp1_fsetmin,ks_curprofman_comp1_fmin,ks_curprofman_comp1_fmax,ks_curprofman_comp1_ffix,ks_curprofman_comp1_f,$
                          ks_curprofman_comp3_csetmax,ks_curprofman_comp3_csetmin,ks_curprofman_comp3_cmin,ks_curprofman_comp3_cmax,ks_curprofman_comp3_cfix,ks_curprofman_comp3_c,$
                          ks_curprofman_comp2_csetmax,ks_curprofman_comp2_csetmin,ks_curprofman_comp2_cmin,ks_curprofman_comp2_cmax,ks_curprofman_comp2_cfix,ks_curprofman_comp2_c,$
                          ks_curprofman_comp1_csetmax,ks_curprofman_comp1_csetmin,ks_curprofman_comp1_cmin,ks_curprofman_comp1_cmax,ks_curprofman_comp1_cfix,ks_curprofman_comp1_c,$
                          curproftab_edit, ks_curprofman_contsetmin, ks_curprofman_contsetmax, ks_curprofman_contmin, ks_curprofman_contmax, ks_curprofman_cont, ks_curprofman_contfix,$
                          ks_curprof_showres,ks_curprofman_monitors,ks_curprofman_pos_on_disp,ks_curprofman_curpos, ks_curprofman_navi_lt,ks_curprofman_navi_rt,ks_curprofman_navi_up,$
                          ks_curprofman_navi_dn                      
    
    
    COMMON KS_PV_DIAG_MANAGER, ks_pvman_b,pvman_buttons, ks_table_pv
    
                          
    COMMON KS_FOR_MPFIT, current_method, inst_fwhm_vel
    
    tmp={sizes,x:0L,y:0L}
    tmp={obj_par,name:'',uval:'',obj:0L,val:0E,sens:1L}
    tmp={disp_par,obj:0L,uval:'',is_set:0L}
    tmp={pos,x0:0E,y0:0E,x1:0E,y1:0E}
    tmp={bpos,x:0E,y:0E}
    tmp={marker, name:'',type:0L, color:'', points: ptr_new()}
    sz=[{sizes,0,0},$  ;0 - Все окно
        {sizes,512,512},$   ;1 - Дисплей
        {sizes,125,30},$   ;2 - Размер кнопок
        {sizes,400,400},$   ;3 - Дисплей для профилей
        {sizes,25,70},$   ;4 - Размер каждого поля мониторов (два варианта)
        {sizes,23,23}]    ; 5 - резерв для скролл-бара
        
   ; Флаги и начальные значения
   cub_loaded=0
   im_loaded=0 
   binmap_loaded=0
   num_res_subtypes = [5,4,5,4,5,1,1,1]; количество подтипов в каждом типе отображаемого результата (напр., Поток => 1,2,3 комп.) 
   list_of_restypes = ['Flux','Intensity','Velocity','Dispersion','Moments','S/N ratio','Continuum','Residuals']
   
   ;Tag names for results
    map_tags=strupcase(['f_tot','f1','f2','f3','f_shi_cmp','i1','i2','i3','i_shi_cmp','v1','v2','v3','v_shi_cmp','v_shift','sigma1','sigma2','sigma3','sigma_shi_cmp','mom0','mom1','mom2','mom3','mom4','snr','contin','resid'])
   mod_tags=strupcase(['c1','c2','c3','resid'])
   

   res_loaded=intarr(total(num_res_subtypes))
   min_maps_points = 10 ; минимальное число профитированных точек, чтобы отображать карты
    
   filenames=["","",""]
   cur_channel=-1 ; -1  - интеграл, остальное - поканальное
   chan_string_cur="All"
   start_draw_bord=0
   zoom_border=[replicate({pos,0.,0.,0.,0.},2)] ; 0 - cub, 1 - res
   zoom_factor=[replicate({x:1.,y:1.},2)]
   bord_draw=[{bpos,-1,-1}]
   xscr_cur=[-1,-1]
   yscr_cur=[-1,-1]
   
   color_tab = replicate({colors,$
              R: BytArr(!D.Table_Size), $ ; The current R color vector.
              G: BytArr(!D.Table_Size), $ ; The current G color vector.
              B: BytArr(!D.Table_Size), $ ; The current B color vector.
              NAME: "", $                 ; The name of the current color table.
              INDEX: 0, $                 ; The index number of the current color table.
              TYPE: "", $                 ; The type of color table (e.g, BREWER or IDL).
              BREWER: 0, $                ; Set to 1 if using BREWER color tables, else to 0.
              REVERSED: 0B },2)
   
   loadct,0,rgb_table=ctab,/sil
   color_tab[0].R=ctab[*,0]
   color_tab[0].G=ctab[*,1]
   color_tab[0].B=ctab[*,2]
   
   loadct,33,rgb_table=ctab,/sil
   color_tab[1].R=ctab[*,0]
   color_tab[1].G=ctab[*,1]
   color_tab[1].B=ctab[*,2]
   color_tab[1].index=33
   
   pos_on_disp={cub: [0.,0.,1.,1.], res: [0.,0.,1.,1.]}
   
   markers=[{marker, name:'-1',type:-1L, color:'green', points: ptr_new()}] ; маркеры для отображения на изображениях. 0-й - пропускать!
   cur_fit_selection={marker, name:'',type:-1L, color:'blue', points: ptr_new()} ; здесь будут храниться границы области для фиттинга
   n_fitpoints=0
   profile_selection=[{marker, name:'-1',type:-1L, color:'red', points: ptr_new()}] ; здесь интегральные профили
   curprof_selection={marker, name:'-1',type:-1L, color:'gold', points: ptr_new()} ; здесь текущий профиль
   ;--- яркость для куба somo, поканальных, изображения,  результатов.  
   n_restype=total(num_res_subtypes)
   brtmin=fltarr(n_restype+3)
   brtmax=fltarr(n_restype+3)
   brtfrac=fltarr(n_restype+3)+97.
   
   ;--- Диапазон по X для профиля
   prof_xrange=fltarr(2)
   ks_profman_xrange=fltarr(2)
   snr_mask=10. ; Минимальное значение сигнал/шум. Все что ниже - маскируется 
    
   auto_br=fltarr(2)+1 
   curprof_monitor=0
   
    ntypes=9
    axes_info={I:'',ztype:0l, zunit:strarr(ntypes), zref:0E}
    ;ztype = -1 (unrecognized); 0 (vel km/s); 1 (vel cm/s); 2 (wav A); 3 (wav cm); 4 (wav nm); 5 (nu Hz); 6 (nu MHz); 7 (nu GHz) 
    axes_info.zunit=["Units", "km/s","m/s","A", "cm", "nm", "Hz", "MHz", "GHz"]
    axes_info.ztype=-1
    axes_info.I='counts'
    
   fit_overwrite=1 ; Можно ли перезаписывать результаты фиттинга
   fit_method = 0 ; 0  - MPFIT, 1 - GENFIT
   fit_proftype = 0 ; 0  - Voigt, 1 - Gauss
   ks_analysis_sortmode = 0 ; set cent.comp (in case of 2) by : 0 - intensity; 1 - offset from Mom1; 2 - fwhm
   clone_parallel_cub_ID=-1
   clone_parallel_res_ID=-1
   
   
   ;Integrated Profiles
   n_prof_table=0
   prof_selected=-1
   ks_profman_method=0
   n_prof_options=7
   proftab_edit={state:0L,prof:0L}
   
    prof_default={ks_prof_struct,name:'',color:'', y:ptr_new(), x:ptr_new(),xr:ptr_new(),yr:ptr_new(), comps:ptr_new(), cent:ptr_new(), snr:0D, $
        amp_norm:0L, ampl:ptr_new(), fwhm:ptr_new(), fixcent:ptr_new(),fixfwhm:ptr_new(),fixampl:ptr_new(),cont:0D,fixcont:0L,setmin_cont:0L,$
        setmax_cont:0L,min_cont:0E,max_cont:0E,link_ampl:ptr_new(),link_cent:ptr_new(),link_fwhm:ptr_new(),mom0:0D,mom1:0D,mom2:0D,mom3:0D,mom4:0D,$
        setmin_cent:ptr_new(),setmin_fwhm:ptr_new(),setmin_ampl:ptr_new(),setmax_cent:ptr_new(),setmax_fwhm:ptr_new(),setmax_ampl:ptr_new(),$
        min_cent:ptr_new(),min_fwhm:ptr_new(),min_ampl:ptr_new(),max_cent:ptr_new(),max_fwhm:ptr_new(),max_ampl:ptr_new(),fitted_comps:ptr_new(),$
        fitted_ampl:ptr_new(),fitted_cent:ptr_new(),fitted_fwhm:ptr_new(),fitted_flux:ptr_new(),fitted_cont:0D,fitted_isset:ptr_new(), fitted_resid:ptr_new(), show_on: ptr_new()}
      
    prof_default.x=ptr_new([0])
    prof_default.y=ptr_new([0])
    prof_default.xr=ptr_new([0.,0.])
    prof_default.yr=ptr_new([0.,0.])
    prof_default.cent=ptr_new(double([0.,30.,-30.]))
    prof_default.fwhm=ptr_new(double([25.,25.,25.]))
    prof_default.ampl=ptr_new(double([1.,0.4,0.4]))
    prof_default.cont=0.D
    prof_default.amp_norm=1 
    prof_default.comps=ptr_new([1,0,0])
     
    prof_default.min_cent=ptr_new(double([-1000.,-1000.,-1000.]))
    prof_default.min_fwhm=ptr_new(double([0.,0.,0.]))
    prof_default.min_ampl=ptr_new(double([0.,0.,0.]))
    prof_default.min_cont=0.D
      
    prof_default.max_cent=ptr_new(double([1000.,1000.,1000.]))
    prof_default.max_fwhm=ptr_new(double([120.,120.,120.]))
    prof_default.max_ampl=ptr_new(double([1.,1.,1.]))
    prof_default.max_cont=0.D
      
    prof_default.setmin_cent=ptr_new([0,0,0])
    prof_default.setmin_fwhm=ptr_new([0,0,0])
    prof_default.setmin_ampl=ptr_new([1,1,1])
    prof_default.setmin_cont=0
      
    prof_default.setmax_cent=ptr_new([0,0,0])
    prof_default.setmax_fwhm=ptr_new([0,0,0])
    prof_default.setmax_ampl=ptr_new([0,0,0])
    prof_default.setmax_cont=0
      
    prof_default.fixcent=ptr_new([0,0,0])
    prof_default.fixfwhm=ptr_new([0,0,0])
    prof_default.fixampl=ptr_new([0,0,0])
    prof_default.fixcont=0
 
    prof_default.link_cent=ptr_new([0.,0.,0.])
    prof_default.link_fwhm=ptr_new([0.,0.,0.])
    prof_default.link_ampl=ptr_new([0.,0.,0.])
    
    prof_default.fitted_cent=ptr_new(double([0.,0.,0.]))
    prof_default.fitted_fwhm=ptr_new(double([0.,0.,0.]))
    prof_default.fitted_ampl=ptr_new(double([0.,0.,0.]))
    prof_default.fitted_flux=ptr_new(double([0.,0.,0.]))
    prof_default.fitted_isset=ptr_new([0,0,0])
    prof_default.fitted_resid=ptr_new([0])
    prof_default.fitted_comps=ptr_new(dblarr(3,1))
    prof_default.fitted_cont=0.D
    prof_default.show_on=ptr_new([1,0,1,1,1,1])
    
   
   
   
   
   
   ;Current Profiles
   ks_curprofman_method=0
   ks_curprofman_inicomps={prof_ini_setup, ampl: dblarr(3), fwhm: dblarr(3), cent: dblarr(3), comps: intarr(3), setmin_ampl: intarr(3), $ 
                            setmax_ampl: intarr(3), setmin_cent: intarr(3), setmax_cent:intarr(3), setmin_fwhm:intarr(3), setmax_fwhm:intarr(3),$
                            fix_ampl:intarr(3), fix_cent: intarr(3), fix_fwhm: intarr(3), min_cent: dblarr(3), max_cent: dblarr(3), $
                            min_fwhm:dblarr(3),max_fwhm:dblarr(3), min_ampl:dblarr(3),max_ampl:dblarr(3),cont:0.D,setmin_cont:0L,setmax_cont:0L,fix_cont:0L,$
                            min_cont:0.D,max_cont:0.D, xr:fltarr(2), yr: fltarr(2)}
    
   ks_curprofman_inicomps.max_ampl=Double([1.D,1.D,1.D])
   ks_curprofman_inicomps.min_ampl=Double([0.05D,0.05D,0.05D])
   ks_curprofman_inicomps.max_cent=Double([1000.,1000.,1000.])
   ks_curprofman_inicomps.min_cent=Double([-1000.,-1000.,-1000.])
   ks_curprofman_inicomps.max_fwhm=Double([120.,120.,120.])
   ks_curprofman_inicomps.min_fwhm=Double([10.,10.,10.])
   ks_curprofman_inicomps.comps=[1,1,1]
   
   
   
   ; Пределы для фиттинга
   
   ks_analysis_limiter={lim:0L,ncomps_max:0L, setmin_cent:intarr(3),setmin_fwhm:intarr(3),setmin_ampl:intarr(3),setmax_cent:intarr(3),$
    setmax_fwhm:intarr(3),setmax_ampl:intarr(3),min_cent:dblarr(3),min_fwhm:dblarr(3),$
    min_ampl:dblarr(3),max_cent:dblarr(3),max_fwhm:dblarr(3),max_ampl:dblarr(3), resid_thresh:0E, simplify: 0L}
   
   ks_analysis_limiter.max_cent=Double([1000.,1000.,1000.])
   ks_analysis_limiter.min_cent=Double([-1000.,-1000.,-1000.])
   ks_analysis_limiter.max_fwhm=Double([120.,120.,120.])
   ks_analysis_limiter.min_fwhm=Double([0.,0.,0.])
   ks_analysis_limiter.max_ampl=Double([1.E,1.E,1.E])
   ks_analysis_limiter.min_ampl=Double([0.01E,0.01E,0.01E])
   ks_analysis_limiter.ncomps_max=3
   ks_analysis_limiter.resid_thresh=3.
   
END




; ###### Всякие функции-сохранялки/загружалки
  PRO KS_SAVE_PS, type=type
        ; Сохраняет изображение с дисплея в eps-файл
        COMMON KS_DISPLAY
        COMMON KS_FLAGS_AND_PARAMS
        COMMON KS_BRIGHTNESS
        COMMON KS_DATA
        
        if not keyword_set(type) then type = 0
        
        fdecomp,filenames[0],disk,cubdir,cubfile,qual
        cubdir=disk+cubdir
        
        if type eq 0 then begin
          win_title="Choose file to save image from left pannel" 
          ;file="~/Science/HoII/IFP/Results/Kinescope_test/test_save.eps"
          current_image=cur_im
          current_head=h_cur_im
          if show_type[0].val eq 0 then title="Sum of data cub channels"
          if show_type[0].val eq 2 then begin
            if cur_channel[0] eq -1 then title="Sum of data cub channels" else begin
              title="Sum of channels: "
              nchan=n_elements(cur_channel)
              if nchan eq 1 then title+=string(cur_channel[0],format="(I0)") else begin
                chan=cur_channel[sort(cur_channel)]
                for i = 0,n_elements(chan)-2 do title+=string(chan[i],format="(I0)")+", "
                title+=string(chan[n_elements(chan)-1],format="(I0)")
              endelse
            endelse
          endif
          if show_type[0].val eq 1 then begin
            fdecomp,filenames[1],disk,imadir,title,qual
            title="Image: "+title+"."+qual
          endif
        endif else begin
          if total(res_loaded) eq 0 or curprof_monitor eq 1 then return
          win_title="Choose file to save image from right pannel"
          ;file="~/Science/HoII/IFP/Results/Kinescope_test/test_save1.eps"
          current_image=cur_res
          current_head=h_cur_res
          title=cur_res_name+" (S/N > "+string(snr_mask,format="(I0)")+")"
        endelse
    
        
    
        pos=[0.11,0.06,0.99,0.88]
        
        nx=sxpar(current_head,"NAXIS1")
        ny=sxpar(current_head,"NAXIS2")

        ;вычисляем индекс массива с яркостями и тд для конкретной картинки
        if type eq 0 then brtindex=show_type[0].val else brtindex=cur_res_index+3
        
        pallete=bytarr(3,!D.TABLE_SIZE)
        pallete[0,*]=color_tab[type].R
        pallete[1,*]=color_tab[type].G
        pallete[2,*]=color_tab[type].B
        
        minval=brtmin[brtindex]
        maxval=brtmax[brtindex]
        
        if zoom_factor[type].x eq 1 and zoom_factor[type].y eq 1 then begin 
           xrange=[0,nx]
           yrange=[0,ny]
           imshow=current_image
           hshow=current_head
        endif else begin
           adxy,current_head,zoom_border[type].x0,zoom_border[type].y0,x0,y0
           adxy,current_head,zoom_border[type].x1,zoom_border[type].y1,x1,y1

           xind=get_num(nx,ny,/x)
           yind=get_num(nx,ny,/y)
           x0=round(x0)
           x1=round(x1)
           y0=round(y0)
           y1=round(y1)
           rec=where(xind ge x0 and xind le x1 and yind ge y0 and yind le y1, nrec)
           if nrec ge 4 then begin
              hextract,current_image,current_head,imshow,hshow,min(xind[rec]),max(xind[rec]),min(yind[rec]),max(yind[rec])
              xrange=[min(xind[rec]),max(xind[rec])+1]
              yrange=[min(yind[rec]),max(yind[rec])+1]
           endif else return
        endelse
        
        
          file=DIALOG_PICKFILE(title=win_title,default_extension='eps',filter="*ps",/write)
          if (file eq '') then return
          fdecomp,file,disk,psfile_dir,psfile_name,qual
          if (psfile_name eq '') then return
          cd,disk+psfile_dir
        cgps_open,file,/encaps,/quiet       
        cgimage,imshow,minval=minval,maxval=maxval,$
                    /keep_asp,oposition=opos,pos=pos,$
                    xrange=xrange,yrange=yrange,palette=pallete,MISSING_VAL="NAN",MISSING_IND=0,MISSING_COLOR="white";,/axes,axkey={xst:5,yst:5}
        KS_SHOW_MARKERS, current_head
        imcontour,imshow,hshow,xminor=2,yminor=2,/normal,/nodata,charsize=1.,/noerase,type=1,$
                    xtit='RA (2000)',ytit='DEC (2000)',subtit=" ",pos=opos
        IF (minval ne maxval) then $
          cgcolorbar,pos=[opos[0],opos[3]+0.01,opos[2],opos[3]+0.05],range=[minval,maxval],/norm,/top,$
          palette=pallete,title=title,charsize=1.
        cgps_close
    
  END  



  PRO KS_FREE_ALL_POINTERS
     ;Перед выходом - удаляет все указатели, очищая память
    COMMON KS_PROF_MANAGER
    COMMON KS_ANALYSIS
    COMMON KS_DISPLAY
    
    n=n_elements(markers)
    for i=0,n-1 do ptr_free,markers[i].points
    n=n_elements(cur_fit_selection)
    for i=0,n-1 do ptr_free,cur_fit_selection[i].points
    n=n_elements(profile_selection)
    for i=0,n-1 do ptr_free,profile_selection[i].points
    
    nprf=n_elements(prof_storage)
    for i=0,nprf-1 do ptr_free,prof_storage[i].xr,prof_storage[i].yr,prof_storage[i].x,prof_storage[i].y,prof_storage[i].comps,$
               prof_storage[i].x, prof_storage[i].cent, prof_storage[i].fwhm, prof_storage[i].ampl, $
               prof_storage[i].max_cent,prof_storage[i].max_fwhm,prof_storage[i].max_ampl,$
               prof_storage[i].min_cent, prof_storage[i].min_fwhm, prof_storage[i].min_ampl,$
               prof_storage[i].setmax_cent,prof_storage[i].setmax_fwhm,prof_storage[i].setmax_ampl,$
               prof_storage[i].setmin_cent, prof_storage[i].setmin_fwhm, prof_storage[i].setmin_ampl,$
               prof_storage[i].fixcent, prof_storage[i].fixfwhm, prof_storage[i].fixampl,prof_storage[i].show_on,$
               prof_storage[i].link_cent, prof_storage[i].link_fwhm, prof_storage[i].link_ampl,prof_storage[i].fitted_isset,prof_storage[i].fitted_comps,$
               prof_storage[i].fitted_cent, prof_storage[i].fitted_fwhm, prof_storage[i].fitted_ampl,prof_storage[i].fitted_flux,prof_storage[i].fitted_resid
      
     ptr_free,prof_default.xr,prof_default.yr,prof_default.x,prof_default.y,prof_default.comps,$
               prof_default.x, prof_default.cent, prof_default.fwhm, prof_default.ampl, $
               prof_default.max_cent,prof_default.max_fwhm,prof_default.max_ampl,$
               prof_default.min_cent, prof_default.min_fwhm, prof_default.min_ampl,$
               prof_default.setmax_cent,prof_default.setmax_fwhm,prof_default.setmax_ampl,$
               prof_default.setmin_cent, prof_default.setmin_fwhm, prof_default.setmin_ampl,$
               prof_default.fixcent, prof_default.fixfwhm, prof_default.fixampl,prof_default.show_on,$
               prof_default.link_cent, prof_default.link_fwhm, prof_default.link_ampl,prof_default.fitted_isset,prof_default.fitted_comps,$
               prof_default.fitted_cent, prof_default.fitted_fwhm, prof_default.fitted_ampl,prof_default.fitted_flux,prof_default.fitted_resid
  END





; ####### Функции, необходимые для фиттинга и прочего анализа

PRO KS_GET_CUB_UNITS
  ; Извлекаем из шапки куба информацию об единицах интенсивности и вдоль осей, а также о центральной длине волны 
  COMMON KS_DATA
  COMMON KS_ANAL_MANAGER_WIDGET
  
  axes_info.ztype=-1
  unit=sxpar(header_cub,"BUNIT")
  if not keyword_set(unit) then axes_info.I = "counts" else axes_info.I = unit 
  zunit=sxpar(header_cub,"CTYPE3")
  xdelt=sxpar(header_cub,"CDELT3")
  if xdelt eq 0 then xdelt=sxpar(header_cub,"CD3_3")
  if strpos(zunit,"WAV") ne -1 then begin
    if abs(xdelt) lt 1 and abs(xdelt) gt 0.1  then axes_info.ztype=2 else axes_info.ztype=3
  endif    
    
  if strpos(zunit,"FREQ") ne -1 then axes_info.ztype=5
  if strpos(zunit,"ELO") ne -1 or strpos(zunit,"VRAD") ne -1 or strpos(zunit,"VOPT") ne -1 then begin
    axes_info.ztype=0
    if abs(xdelt) gt 100 then axes_info.ztype=1
  endif
  
  axes_info.zref=0.
  sperange=sxpar(header_cub,"SPERANGE")
  if strpos(sperange,"SII") ne -1 then axes_info.zref=6716.4
  if strpos(sperange,"H-alpha") ne -1 or strpos(sperange,"Halpha") ne -1 then axes_info.zref=6562.78
  
  WIDGET_CONTROL, wid_ax_info[0], set_val=axes_info.ztype+1
  WIDGET_CONTROL, wid_ax_info[1], set_val=axes_info.zref
  WIDGET_CONTROL, wid_ax_info[2], set_val=axes_info.I
  
  inst_fwhm={vel:0e, wav:0e, type:0l}
  extern_disp={vel:0e, wav:0e, type:0l}
  if strpos(sxpar(header_cub,"DISPERSE"),"IFP751") ne -1 then begin
     inst_fwhm.wav=0.48
     if (strpos(sperange,"H-alpha") ne -1 or strpos(sperange,"Halpha") ne -1) then inst_fwhm.vel=22.
     if strpos(sperange,"SII") ne -1 then inst_fwhm.vel=21.4
   endif
  
  WIDGET_CONTROL,wid_inst_fwhm[1],get_val=tmp
  inst_fwhm.type=tmp
  if inst_fwhm.type eq 0 then WIDGET_CONTROL, wid_inst_fwhm[0], set_val=inst_fwhm.vel else $
                           WIDGET_CONTROL, wid_inst_fwhm[0], set_val=inst_fwhm.wav
                           
                           
   if (strpos(sperange,"H-alpha") ne -1 or strpos(sperange,"Halpha") ne -1) then begin
      extern_disp.vel=sqrt(9.1^2+3^2)
      extern_disp.wav=sqrt(9.1^2+3^2)/2.99792458e5*6562.78
   endif
   
   WIDGET_CONTROL,wid_extern_disp[1],get_val=tmp
   extern_disp.type=tmp
   if extern_disp.type eq 0 then WIDGET_CONTROL, wid_extern_disp[0], set_val=extern_disp.vel else $
                           WIDGET_CONTROL, wid_extern_disp[0], set_val=extern_disp.wav
   
END



FUNCTION KS_GET_NLINES, moments_struct
  ;Computes the number of lines to be used in fitting
  N_lines=fix(moments_struct.mom1)*0
  rec=where(moments_struct.snr gt 1,nr)
  if nr gt 0 then begin
  
  ;OLD
;  if nr gt 0 then begin
;    n_lines[rec]=1
;    rec=where((abs(moments_struct.mom3) gt 0.1) or (moments_struct.mom4 lt -0.7), nr)
;    if nr gt 0 then n_lines[rec]=2
;    rec=where( (abs(moments_struct.mom3) gt 0.5) and (moments_struct.mom4 gt -0.5), nr)
;    if nr gt 0 then N_lines[rec]=3
;  endif
  ;#####
  
  ;NEW
  
    got=0
       
       rec=where( (moments_struct.mom4 gt 0.2) and got eq 0,nr)
       if nr gt 0 then      n_lines[rec]=1
       rec=where( ((abs(moments_struct.mom3) gt 0.3) or moments_struct.mom4 le -0.8 or abs(moments_struct.mom3*moments_struct.mom4) gt 0.15) and n_lines eq 0, nr)
       if nr gt 0 then  n_lines[rec]=2
  
    rr=where(n_lines eq 0,nrr)
    if nrr gt 0 then begin
      y=(moments_struct.mom4[rr]+2.)/0.05
      x=(moments_struct.mom3[rr]+1.)/0.05
      rec=where( y ge 43 and (x-20.3)^2+(y-56.5)^2 le 17.5^2,nr)
      if nr gt 0 then n_lines[rr[rec]]=1 
      rec=where(y lt (-2.05*x+53.8) or y lt 2.25*x-35.53 or y le 16,nr)
      if nr gt 0 then n_lines[rr[rec]]=2
      rec=where(y gt 31 and (x-20.44)^2+(y-28.31)^2 gt 18.7^2 and (x-20.3)^2+(y-56.5)^2 gt 17.5^2,nr)
      if nr gt 0 then n_lines[rr[rec]]=2
    endif 
    
     rec=where(n_lines eq 0,nr)
     if nr gt 0 then begin
       n_lines[rec]=1
       rec1=where((abs((moments_struct.mom3)[rec]) gt 0.2) or ((moments_struct.mom4)[rec] lt -0.75),nr) 
       if nr gt 0 then n_lines[rec[rec1]]=2
       rec1=where((abs((moments_struct.mom3)[rec]) gt 0.5) and ((moments_struct.mom4)[rec] gt -0.5),nr)
       if nr gt 0 then n_lines[rec[rec1]]=2
       rec1=where(abs((moments_struct.mom3)[rec]*(moments_struct.mom4)[rec]) gt 0.2,nr)
       if nr gt 0 then n_lines[rec[rec1]]=2
       rec1=where((moments_struct.mom4)[rec] gt 0.2,nr)
       if nr gt 0 then n_lines[rec[rec1]]=1        
        
       rec1= where((x[rec]-20.33)^2+(y[rec]-19.45)^2 le 7.48^2,nr) 
       if nr gt 0 and got eq 0 then begin
          
          dir_moms="/Users/mors/Science/IDLWorkspace/KINESCOPE/moments/"
          m3_m4_1=readfits(dir_moms+"m3_m4_1comp.fits",h11,/sil)
          m3_m4_2=readfits(dir_moms+"m3_m4_2comp.fits",h12,/sil)
          m3_m4_3=readfits(dir_moms+"m3_m4_3comp.fits",h13,/sil)
          m3_ax=findgen(sxpar(h11,"NAXIS1"))*sxpar(h11,"CDELT1")+sxpar(h11,"CRVAL1")
          m4_ax=findgen(sxpar(h11,"NAXIS2"))*sxpar(h11,"CDELT2")+sxpar(h11,"CRVAL2")
        
          for i=0,nr-1 do begin
            m=min(abs(m3_ax-(moments_struct.mom3)[rec[rec1[i]]]),mpos1)
            m=min(abs(m4_ax-(moments_struct.mom3)[rec[rec1[i]]]),mpos2)
            c1_prob=(m3_m4_1[mpos1,mpos2])
            c2_prob=(m3_m4_2[mpos1,mpos2])
            c3_prob=(m3_m4_3[mpos1,mpos2])
            m=max([c1_prob,c2_prob,c3_prob],mpos)
            s_prob=sort([c1_prob,c2_prob,c3_prob])
            n_lines[rec[rec1[i]]]=mpos+1
          endfor
       endif
    endif 
  
  ENDIF
  
  return,N_lines
END


function KS_moments,x_in,y_in,contin=contin, snr=snr,iter_done=iter_done
  ;Computes central moments of the vector
  ;ALSO OUTPUT: contin (val), snr (val)
  
  contin=0.
  snr=0.
  moms=dblarr(5)
  
  ; вычищаем все, что имеет значения Nan или 0
  nz = n_elements(x_in)
  REC=WHERE(FINITE(X_in) AND FINITE(Y_in) and y_in ne 0, NREC)
  if nrec lt nz*0.4 then return,moms
  X=X_in[REC]
  Y=Y_in[REC]
  nz = n_elements(x)
  ; Нормируем спектр, предполагая уровень континуума - минимум. Отбрасываем все, что ниже 5%
  ; Второй итерацией уже используем посчитанный уровень континуума и шума в точках за пределами 3sigma от центра линии
  
  contin=min(y,/nan)
  intens=max(y,/nan)-contin
  noise=stddev(y,/nan)
  iter_done=0
  FOR iter=0,1 do begin
    yn=(y-contin)/intens
    if iter eq 1 then use=where(y-contin gt 3*intens/snr, nuse) else begin
      use=where(yn gt 0.25, nuse)
      if nuse lt 5 then begin
        use=where(yn gt 0.15, nuse)
        if nuse lt 5 then begin
          use=where(yn gt 0.1, nuse)
          if nuse lt 5 then begin
            use=where(yn gt 0.05, nuse)
            if nuse lt 3 then begin
              use=where(yn ne 0.0, nuse)
              if nuse lt 3 then break
            endif
          endif
         endif
       endif
    endelse
    if nuse eq 0 then break
    yn=yn[use]
    xuse=x[use]
    
    m1=total(xuse*yn)/total(yn)
    m2=sqrt(total((xuse-m1)^2*yn)/total(yn))
    m3=total((xuse-m1)^3*yn)/total(yn)/m2^3
    m4=(total((xuse-m1)^4.*yn)/total(yn))/m2^4-3.  
    
    moms=[0,m1,m2,m3,m4]
    sigm=moms[2]
    moment1=moms[1]
    iter_done+=1
    if iter eq 0 then begin 
      min_num_pix=5
      rec=where((x lt moment1-4*sigm or x gt moment1+4*sigm and finite(y)), nrec)  ;and x ge x_in[2] and x le x_in[nz-3]
      if nrec lt min_num_pix then begin
        rec=where((x lt moment1-3*sigm or x gt moment1+3*sigm and finite(y)), nrec)
        if nrec lt min_num_pix then begin
          rec=where((x lt moment1-2*sigm or x gt moment1+2*sigm and finite(y)), nrec)
          if nrec lt min_num_pix then rec=[indgen(nz/4),nz-1-indgen(nz/4)]
        endif
      endif
      robomean,y[rec],5.,0.5,  contin, avdev, noise, vr, sk, kt
      ysort=y[sort(abs(x-moment1))]
      intens=(max(ysort[0:5],/nan))
      snr=(intens-contin)/noise
      if snr lt 0 then snr=0
    endif
  ENDFOR
  moms[0]=total(y)-contin*nz
  return, moms
end


FUNCTION KS_GET_INITIAL_COMPS, xscale, prof, moments_struct, contin, limiter=limiter, ncomps_forced=ncomps_forced
  COMMON KS_DATA
    
    if n_elements(limiter) eq 0 then do_lim=0 else begin
      if limiter.lim eq 0 then do_lim=0 else do_lim=1
    endelse
    
    
    
    n_lines=ks_get_nlines(moments_struct)
    if do_lim then n_lines=(n_lines < limiter.ncomps_max)
    if keyword_set(ncomps_forced) then n_lines=ncomps_forced

    ysort=prof[sort(abs(xscale-moments_struct.mom1))]
    intens=(max(ysort[0:5]))-contin
    ampl=[1.,0.2,0.2]*intens
    fw=replicate(moments_struct.mom2,3)/sqrt(n_lines)*2.35482
    cent=[moments_struct.mom1,moments_struct.mom1+2.5*moments_struct.mom3/abs(moments_struct.mom3)*(abs(moments_struct.mom3)*sqrt(abs(moments_struct.mom2^2-inst_fwhm.vel^2))^3)^0.33,$
    moments_struct.mom1-2.5*moments_struct.mom3/abs(moments_struct.mom3)*(abs(moments_struct.mom3)*sqrt(abs(moments_struct.mom2^2-inst_fwhm.vel^2))^3)^0.33]
    rec=where(~finite(intens),nr)
    if nr gt 0 then intens[rec]=0
    rec=where(~finite(fw),nr)
    if nr gt 0 then fw[rec]=0
    rec=where(~finite(cent),nr)
    if nr gt 0 then cent[rec]=0
    
    s=sort(cent[1:2]-cent[0])
    comps=[1,0,0]
    if n_lines eq 3 then comps=[1,1,1]
    if n_lines eq 2 then begin
      rec=where(s eq 0)
      comps[rec+1]=1
    endif
    
    
    
    ;##### Define borders
    smncnt=1
    smxcnt=1
    mncnt=min(prof,/nan)
    mxcnt=max(prof,/nan)
    smna=[1,1,1]
    mna=[replicate(0.,3)]
    smxa=[1,1,1]
    mxa=[replicate((mxcnt-mncnt)*1.05,3)]
    
    smnc=[1,1,1]
    smxc=[1,1,1]
    nx=n_elements(xscale)
    mnc=[replicate(xscale[fix(nx/10)],3)]
    mxc=[replicate(xscale[fix(nx*9/10)],3)]
    
    smnf=[1,1,1]
    smxf=[1,1,1]
    mnf=[replicate(23.5482/2.,3)]
    mxf=[replicate(moments_struct.mom2*2.35482*2,3)]
    
    
    if do_lim then begin
      for i=0,2 do begin
        if limiter.setmin_ampl[i] then mna[i]=(mna[i] > limiter.min_ampl[i]*(mxcnt-mncnt))
        if limiter.setmax_ampl[i] then mxa[i]=(mxa[i] < limiter.max_ampl[i]*(mxcnt-mncnt))
        if limiter.setmin_fwhm[i] then mnf[i]=(mnf[i] > limiter.min_fwhm[i])
        if limiter.setmax_fwhm[i] then mxf[i]=(mxf[i] < limiter.max_fwhm[i])
        if limiter.setmin_cent[i] then mnc[i]=(mnc[i] > limiter.min_cent[i])
        if limiter.setmax_cent[i] then mxc[i]=(mxc[i] < limiter.max_cent[i])
        
        ;Проверяем, чтоб все значения теперь были в заданных границах. Если не так - берем на 3% отличающееся от крайнего
        if smnc[i] eq 1 and cent[i] lt mnc[i] then cent[i]=mnc[i]+0.03*abs(mnc[i])
        if smna[i] eq 1 and ampl[i] lt mna[i] then ampl[i]=mna[i]+0.03*abs(mna[i])
        if smnf[i] eq 1 and fw[i] lt mnf[i] then fw[i]=mnf[i]+0.03*abs(mnf[i])
        if smxc[i] eq 1 and cent[i] gt mxc[i] then cent[i]=mxc[i]-0.03*abs(mxc[i])
        if smxa[i] eq 1 and ampl[i] gt mxa[i] then ampl[i]=mxa[i]-0.03*abs(mxa[i])
        if smxf[i] eq 1 and fw[i] gt mxf[i] then fw[i]=mxf[i]-0.03*abs(mxf[i])
        
      endfor
    ENDIF
    
    
    cent=double(cent)
    ampl=double(ampl)
    fw=double(fw)
    contin=double(contin)    
    mnc=double(mnc)
    mxc=double(mxc)
    mnf=double(mnf)
    mxf=double(mxf)
    mna=double(mna)
    mxa=double(mxa)
    mncnt=double(mncnt)
    mxcnt=double(mxcnt)
    contin=double(contin)
    
    prof_ini={prf_ini_str, ampl: [ampl[0],(ampl[1:2])[s]], cent: [cent[0],(cent[1:2])[s]], comps: comps, fwhm: [fw[0],(fw[1:2])[s]], $
              min_ampl: [mna[0],(mna[1:2])[s]], max_ampl: [mxa[0],(mxa[1:2])[s]],min_fwhm: [mnf[0],(mnf[1:2])[s]], max_fwhm: [mxf[0],(mxf[1:2])[s]],$
              min_cent: [mnc[0],(mnc[1:2])[s]], max_cent: [mxc[0],(mxc[1:2])[s]], setmin_ampl: [smna[0],(smna[1:2])[s]], setmax_ampl: [smxa[0],(smxa[1:2])[s]], $
              setmin_fwhm: [smnf[0],(smnf[1:2])[s]], setmax_fwhm: [smxf[0],(smxf[1:2])[s]],setmin_cent: [smnc[0],(smnc[1:2])[s]], setmax_cent: [smxc[0],(smxc[1:2])[s]],$
              setmin_cont: smncnt, setmax_cont: smxcnt, min_cont: mncnt, max_cont: mxcnt, cont: contin}
    return, prof_ini
END

;#################

; ########### Вспомогательные процедуры
          
    function ks_float2str, num, decim
      if num eq 0 then return,string(0.,format="(G0."+string(decim,format="(I0)")+")")
      lg=alog10(abs(num))
      if lg lt 0 then add=0 else add=floor(lg)+1
      return ,string(num,format="(G0."+string(decim+add,format="(I0)")+")")
    end
          
          
              PRO KS_STATE_MONITOR, state, done=done
                COMMON KS_WIDGET_ELEMENTS
                ; Мониторим прогресс. 
                ; state = 0 => Ready
                ;       = 1 => Moments calculation 
                ;       = 2 => Line fitting
                ;       = 3 => Fits saving
                ;       = 4 => PS saving
                ;       = 5 => Fits loading
                
                ; done = part of done work
                
                
                IF state eq 0 then curval="    Ready!               "
                IF state eq 1 then curval="Moments calculation"
                IF state eq 2 then curval="  Line fitting "
                IF state eq 3 then curval="  Fits saving "
                IF state eq 4 then curval="  PS saving   "
                IF state eq 5 then curval="  Fits loading "
                if state gt 0 then curval=curval+" ("+string(done*100.,format="(F0.1)")+"%)"
                WIDGET_CONTROL, state_monitor, set_value=curval
              
              END
              


                PRO KS_INI_RES_STRUCT, nx,ny,nz
                  ; Инициализация новых структур с результатами (размерность как и у куба - nx, ny)
                  COMMON KS_ANALYSIS
                  COMMON KS_DISPLAY
                  tmp=dblarr(nx,ny)
                  tmp[*]=!Values.F_NAN
                  tmp1=intarr(nx,ny)
                  tmp1[*]=!Values.F_NAN
                  tmp2=dblarr(nx,ny,nz)
                  tmp2[*]=!Values.F_NAN
                  fit_results_maps = {f_tot:tmp, f1:tmp, i1:tmp, v1:tmp, sigma1:tmp, f2:tmp, i2:tmp, v2:tmp, sigma2:tmp,$
                                      f3:tmp, i3:tmp, v3:tmp, sigma3:tmp, v_shift:tmp, is_set1:tmp1,is_set2:tmp1,is_set3:tmp1,$
                                      flux:tmp, mom0:tmp, mom1:tmp, mom2:tmp, mom3:tmp, mom4:tmp, snr:tmp, contin:tmp, resid:tmp,$
                                      fitted: tmp1, f_shi_cmp: tmp, i_shi_cmp: tmp, v_shi_cmp: tmp, sigma_shi_cmp: tmp}
                  
                  fit_results_model = {c1:tmp2, c2:tmp2, c3:tmp2, resid: tmp2}                    
                  WIDGET_CONTROL,show_type[1].obj,set_val=["None"]
                  show_type[1].val=-1
                  WIDGET_CONTROL,show_type[2].obj,set_val=["None"]
                  show_type[2].val=-1
                 
                END
                
                
                PRO KS_TRIGGER_SENS_RES_PANEL, new_state
                  ; Изменяет чувствиетльность всех интерактивных полей управления просмотром результатов 
                  ;          (в случае загрузки нового куба или получения/загрузки каких-то результатов)
                  COMMON KS_WIDGET_ELEMENTS
                  
                  
                  WIDGET_CONTROL, ks_vm_res_b,sensitive=new_state
                  WIDGET_CONTROL, ks_st_res_b,sensitive=new_state
                  field_brtfrac_obj[1]->SetProperty, NonSensitive=1-new_state
                  field_brtmin_obj[1]->SetProperty, NonSensitive=1-new_state
                  field_brtmax_obj[1]->SetProperty, NonSensitive=1-new_state
                  WIDGET_CONTROL,but_auto_brt[1], sens=new_state
                  field_zoom_res_obj->SetProperty, NonSensitive=1-new_state
                  field_snr_mask_obj->SetProperty, NonSensitive=1-new_state
                  buttons[9:10].sens=new_state
                  buttons[12].sens=new_state
                  WIDGET_CONTROL,buttons[9].obj,sensitive=new_state
                  WIDGET_CONTROL,buttons[10].obj,sensitive=new_state
                  WIDGET_CONTROL,buttons[12].obj,sensitive=new_state
                  
                END
                
                

                PRO KS_Buttons_Cre,base,name,uval,but_obj,xs=xs,ys=ys,break_arr=break_arr,frame=frame,$
                          sens=sens,nonexclusive=nonexclusive,EXCLUSIVE=EXCLUSIVE,No_release=No_release, cre_stat=cre_stat ,status=status
                          ; #### Реализация кнопок
                  if (!VERSION.OS_FAMILY eq "Windows") then titfont="Sans Serif" else titfont="-misc-fixed-bold-r-normal--13-120-75-75-c-70-iso8859-1"
                  nbut=n_elements(name)
                  tmp=base
                  if not (keyword_set(xs)) then xs=0
                  if not (keyword_set(ys)) then ys=0
                  if not (keyword_set(frame)) then frame=0
                  if not (keyword_set(No_release)) then No_release=0
                  
                  if keyword_set(cre_stat) then status=lonarr(nbut)
                  
                  
                  nbr=N_elements(break_arr)
                  j=0
                  dobrk=-10
                  IF (nbr gt 0) then begin
                    break_arr=break_arr[sort(break_arr)]
                    dobrk=break_arr[j]
                  ENDIF
                  
                  but_obj=lonarr(nbut)
                  For i=0,nbut-1 do begin
                    if (nbr gt 0 and dobrk eq i-1) then begin
                      j++
                      if (nbr gt j) then dobrk=break_arr[j] else dobrk=-10
                      i--
                      tmp_base=WIDGET_BASE(base,ys=10,xs=30,xpad=0,ypad=0,xoffset=0,yoffset=0,frame=frame)
                
                    endif else begin 
                      
;                      if (keyword_set(nonexclusive) and not (keyword_set(exclusive))) then $
;                          tmp=WIDGET_BASE(base,/nonexclusive,xpad=0,ypad=0,xoffset=0,yoffset=0,frame=frame) else nonexclusive=0
;                      if (keyword_set(exclusive) and not (keyword_set(nonexclusive))) then $
;                          tmp=WIDGET_BASE(base,/exclusive,xpad=0,ypad=0,xoffset=0,yoffset=0,frame=frame) else exclusive=0
;                  
                      thisbutbase=WIDGET_BASE(base,/row,xpad=0,ypad=0,xoffset=0,yoffset=0) 

                      if (xs+ys gt 0) then tmp=WIDGET_BASE(thisbutbase,xs=xs,ys=ys,nonexclusive=nonexclusive,exclusive=exclusive,xpad=0,ypad=0,xoffset=0,yoffset=0) else $
                          tmp=WIDGET_BASE(thisbutbase,nonexclusive=nonexclusive,exclusive=exclusive,xpad=0,ypad=0,xoffset=0,yoffset=0)
                  
                      but_obj[i]=WIDGET_BUTTON(tmp,value=name[i],uvalue=uval[i],xsize=xs,ysize=ys,sensitive=sens[i],frame=frame,NO_RELEASE=No_release)
                      if keyword_set(cre_stat) then status[i]=WIDGET_LABEL(thisbutbase,val="     ",font=titfont)
                    endelse
                  ENDFOR
                END
                
                
                PRO KS_Monitor_Cre,base,name,mon_obj,row=row,column=column,xs=xs,ys=ys,center=center,strcol=strcol,def_value=def_value
                  ; #### Процедурка для реализации мониторов текущих параметров
                  if not (keyword_set(def_value)) then def_value='none'
                  if not (keyword_set(center)) then center=0 
                  nmon=n_elements(name)
                  if (keyword_set(row)) then tmp0=WIDGET_BASE(base,/row,align_center=center,yoffset=0,ypad=0) else begin
                    if (keyword_set(column)) then tmp0=WIDGET_BASE(base,/column,align_center=center,yoffset=0,ypad=0) else begin
                      tmp0=base
                    endelse
                  endelse
                    if not (keyword_set(strcol)) then strcol=0 else strcol=1
                  
                  nxs=n_elements(xs)
                  nys=n_elements(ys)
                  
                  if (nxs ne 2) then xs=[20,70]
                  if (nys ne 2) then begin
                    if (!VERSION.OS_FAMILY eq "Windows") then ys=[25,25] else ys=[15,15]
                  endif
                  For i=0,nmon-1 do begin
                    tmp=WIDGET_BASE(tmp0,row=1-strcol,col=strcol)
                      tmp1=WIDGET_LABEL(tmp,value=name[i],xs=xs[0],ys=ys[1],/align_left)
                      mon_obj[i]=WIDGET_LABEL(tmp,value=def_value,xs=xs[1],ys=ys[1],/align_left)
                  ENDFOR
                END
                
                
                PRO KS_GETCHANNELS 
                   COMMON KS_WIDGET_ELEMENTS
                   COMMON KS_DISPLAY
                   COMMON KS_DATA
                    
                    WIDGET_CONTROL,field_channels, get_value=chan_string
                    if strupcase(chan_string) eq "ALL" or chan_string eq '-1' then begin
                      cur_channel=-1
                      WIDGET_CONTROL,field_channels, set_value="All" 
                    endif else begin
                      check_str=stregex(chan_string,"[^[:digit:],-]") ; Ищем не цифры, тире или запятые в запросе
                      if check_str ne -1 then begin
                        mes=dialog_message("I can't show these channels. Please, use syntax: 'Ch1-Ch2,Ch3,Ch4-Ch5, ...' for choosing several channels or 'All' or '-1' for all of them!")
                        WIDGET_CONTROL,field_channels, set_value=chan_string_cur
                        return
                      endif
                      chan_string_arr=strsplit(chan_string,",",/extract,count=n_str)
                      check_str1=stregex(chan_string_arr,"^[[:digit:]]") ; проверяем, везде ли первым идет цифра
                      check_str2=stregex(chan_string_arr,"[[:digit:]]$") ; проверяем, везде ли последним идет цифра
                      rec=where(check_str1 eq -1 or check_str2 eq -1,nr)
                      if nr ne 0 then begin
                        mes=dialog_message("I can't show these channels. Please, use syntax: 'Ch1-Ch2,Ch3,Ch4-Ch5, ...' for choosing several channels or 'All' or '-1' for all of them!")
                        WIDGET_CONTROL,field_channels, set_value=chan_string_cur
                        return
                      endif
                      
                      chan_construction=[-1]
                      nz=sxpar(header_cub,"NAXIS3")
                      for i=0,n_str-1 do begin
                        this_chan_str=strsplit(chan_string_arr[i],"-",/extract,count=n_substr)
                        if n_substr gt 2 then begin
                          mes=dialog_message("I can't show these channels. Please, use syntax: 'Ch1-Ch2,Ch3,Ch4-Ch5, ...' for choosing several channels or 'All' or '-1' for all of them!")
                          WIDGET_CONTROL,field_channels, set_value=chan_string_cur
                          return
                        endif
                        this_chan=fix(this_chan_str)
                        if n_substr eq 1 then begin
                          if this_chan[0] ge 0 and this_chan[0] lt nz then chan_construction=[chan_construction,this_chan[0]]
                        endif
                        if n_substr eq 2 then begin
                          if this_chan[0] gt this_chan[1] or this_chan[0] ge nz or this_chan[1] le 0 then continue
                          start=this_chan[0]
                          finish= this_chan[1] < (nz-1)
                          chan_construction=[chan_construction,indgen(finish-start+1)+start]
                        endif
                      endfor
                      ntot_chan=n_elements(chan_construction)
                      if ntot_chan eq 1 then begin
                        mes=dialog_message("Incorrect range of channels!")
                        WIDGET_CONTROL,field_channels, set_value=chan_string_cur
                        return
                      endif
                      chan_string_cur=chan_string
                      cur_channel=chan_construction[1:ntot_chan-1]
                    endelse
                END
                
                PRO KS_ZOOM_UPD,type=type
                    ; Подстраивает зум-фактор под текущий зум_бордер (при переключении режима отображения, например)
                    ; Если зум-фактор = 1, то отображается весь файл, независимо от зум-бордер
                    ; Если зум-бордер за пределами изображения, то зум-фактор устанавливается равным 1 
                    COMMON KS_DISPLAY
                    COMMON KS_WIDGET_ELEMENTS
                    if not keyword_set(type) then type = 0 else type=1 
                    
                    if type eq 0 then begin
                      h=h_cur_im 
                      cur_image=cur_im
                    endif else begin
                      h=h_cur_res
                      cur_image=cur_res
                    endelse
                    nx=sxpar(h,"NAXIS1")
                    ny=sxpar(h,"NAXIS2")
                    if zoom_factor[type].x eq 1 and zoom_factor[type].y eq 1 then begin
                      xyad,h, 0,0, ra0,dec0
                      xyad,h, nx-1,ny-1, ra1,dec1
                      zoom_border[type].x0=ra0
                      zoom_border[type].y0=dec0
                      zoom_border[type].x1=ra1
                      zoom_border[type].y1=dec1
                    endif else begin

                      adxy,h,zoom_border[type].x0,zoom_border[type].y0,x0,y0
                      adxy,h,zoom_border[type].x1,zoom_border[type].y1,x1,y1
                      
;                      x0=round(x0)
;                      x1=round(x1)
;                      y0=round(y0)
;                      y1=round(y1)
;                      
                      add_x0=0
                      add_x1=0
                      add_y0=0
                      add_y1=0
                      if x1 le 0 or y1 le 0 or x0 ge nx-1 or y0 ge ny-1 then begin 
                        zoom_factor[type].x=1
                        zoom_factor[type].y=1
                        xyad,h, 0,0, ra0,dec0
                        xyad,h, nx-1,ny-1, ra1,dec1
                        zoom_border[type].x0=ra0
                        zoom_border[type].y0=dec0
                        zoom_border[type].x1=ra1
                        zoom_border[type].y1=dec1
                      endif else begin
                        if x0 lt 0 then add_x0=ceil(-x0)
                        if x1 ge nx then add_x1=ceil(x1-nx+1)
                        if y0 lt 0 then add_y0=ceil(-y0)
                        if y1 ge ny then add_y1=ceil(y1-ny+1)
                        if add_x0+add_x1+add_y1+add_y0 ne 0 then begin
                          new_im=fltarr(nx+add_x0+add_x1,ny+add_y0+add_y1)
                          new_im[*]=!VALUES.F_NAN
                          new_im[add_x0:add_x0+nx-1,add_y0:add_y0+ny-1]=cur_image[*,*]
                          nx=nx+add_x0+add_x1
                          ny=ny+add_y0+add_y1
                          sxaddpar,h,"NAXIS1",nx
                          sxaddpar,h,"NAXIS2",ny
                          sxaddpar,h,"CRPIX1",(sxpar(h,"CRPIX1") > 1)+add_x0
                          sxaddpar,h,"CRPIX2",(sxpar(h,"CRPIX2") > 1)+add_y0
                          if type eq 0 then begin
                            cur_im=new_im
                            h_cur_im=h
                          endif else begin
                            cur_res=new_im
                            h_cur_res=h
                          endelse
                        endif
                        zoom_factor[type].x = double(nx)/(x1-x0+1)
                        zoom_factor[type].y = double(ny)/(y1-y0+1)                        
                      endelse
                      if type eq 0 then WIDGET_CONTROL, field_zoom_cub, set_val=zoom_factor[type].x else WIDGET_CONTROL, field_zoom_res, set_val=zoom_factor[type].x
                    endelse
                END
                
                
                 PRO KS_ZOOM_CHANGE, zoom_factor,zoom_border,h,type, cur_zoom
                    ; Меняет зум (границы) в соответствии с новым значением
                    zoom_factor[type].x=cur_zoom
                    nx=sxpar(h,"NAXIS1")
                    ny=sxpar(h,"NAXIS2")
                    if zoom_factor[type].x lt 1 or zoom_factor[type].y lt 1 then begin
                      zoom_factor[type].x=1. & zoom_factor[type].y=1.
                      xyad,h, 0,0, ra0,dec0
                      xyad,h, nx-1,ny-1, ra1,dec1
                    endif else begin
                      adxy,h,(zoom_border[type].x0+zoom_border[type].x1)/2.,(zoom_border[type].y0+zoom_border[type].y1)/2.,xc,yc
                      xc=round(xc)
                      yc=round(yc)
                      dpix=round((double(nx)/zoom_factor[type].x-1)/2.)
                      if xc-dpix lt 0 or yc-dpix lt 0 then dpix1=min([yc,xc]) else dpix1=dpix
                      if xc+dpix gt nx-1 or yc+dpix gt ny-1 then dpix2=min([nx-1-xc,ny-1-yc]) else dpix2=dpix
                      dpix=min([dpix1,dpix2])
                      zoom_factor[type].x = double(nx)/(2*dpix+1)
                      zoom_factor[type].y = double(ny)/(2*dpix+1)
                      xyad,h,xc-dpix,yc-dpix,ra0,dec0
                      xyad,h,xc+dpix,yc+dpix,ra1,dec1
                    endelse
                    zoom_border[type].x0=ra0
                    zoom_border[type].y0=dec0
                    zoom_border[type].x1=ra1
                    zoom_border[type].y1=dec1
                END
                
                
                PRO KS_SHOW_PARALLEL_POSITION, xcur_im, ycur_im, type=type
                  ;Процедура, отображающая на соседнем дисплее крест, соответствующий положению курсора
                  COMMON KS_DISPLAY
                  COMMON KS_SIZES
                  COMMON KS_FLAGS_AND_PARAMS
                 
                  IF not keyword_set(type) then cur_type=1 else cur_type=1-type
                  IF cur_type eq 1 then begin
                    WIDGET_CONTROL, ks_cube_disp.obj, GET_VALUE = tmp
                    WSET, tmp
                    Device, Copy=[0, 0, sz[1].x, sz[1].y, 0, 0, clone_parallel_cub_ID]
                    if total(res_loaded) eq 0 or curprof_monitor eq 1 then return
                    if ~finite(xcur_im) or ~finite(ycur_im) then begin
                      WIDGET_CONTROL, ks_res_disp.obj, GET_VALUE = tmp
                      WSET, tmp
                      Device, Copy=[0, 0, sz[1].x, sz[1].y, 0, 0, clone_parallel_res_ID]
                      return
                    endif
                    current_disp=ks_res_disp
                    cur_clone=clone_parallel_res_ID
                    cur_pos_on_disp=pos_on_disp.res
                    current_header=h_cur_res
                    ref_header=h_cur_im
                    KS_DISP_TO_IM_CONV, 0, 0, x0, y0, mode="result"
                    KS_DISP_TO_IM_CONV, sz[1].x, sz[1].y, x1, y1, mode="result"
                  endif else begin
                    WIDGET_CONTROL, ks_res_disp.obj, GET_VALUE = tmp
                    WSET, tmp
                    Device, Copy=[0, 0, sz[1].x, sz[1].y, 0, 0, clone_parallel_res_ID]
                    if ~finite(xcur_im) or ~finite(ycur_im) then begin
                      WIDGET_CONTROL, ks_cube_disp.obj, GET_VALUE = tmp
                      WSET, tmp
                      Device, Copy=[0, 0, sz[1].x, sz[1].y, 0, 0, clone_parallel_cub_ID]
                      return
                    endif
                    current_disp=ks_cube_disp
                    cur_clone=clone_parallel_cub_ID 
                    cur_pos_on_disp=pos_on_disp.cub
                    current_header=h_cur_im
                    ref_header=h_cur_res
                    KS_DISP_TO_IM_CONV, 0, 0, x0, y0, mode="cube"
                    KS_DISP_TO_IM_CONV, sz[1].x, sz[1].y, x1, y1, mode="cube"
                    
                  endelse
                  xyxy,ref_header,current_header,xcur_im,ycur_im
                  WIDGET_CONTROL, current_disp.obj, GET_VALUE = index
                  WSET, index
                  Device, Copy=[0, 0, sz[1].x, sz[1].y, 0, 0, cur_clone]
                  if xcur_im ge x0+(x1-x0)*cur_pos_on_disp[0] and xcur_im le x0+(x1-x0)*cur_pos_on_disp[2] and $
                     ycur_im ge y0+(y1-y0)*cur_pos_on_disp[1] and ycur_im le y0+(y1-y0)*cur_pos_on_disp[3] then cgplot,xcur_im,ycur_im,psym=1,col="TG4",/data,xst=5,yst=5,pos=[0,0,1,1],symsize=3,xr=[x0,x1],yr=[y0,y1],/noer
 
                END
                
                
                FUNCTION KS_GET_SELECTION_REGION, header, fit_selection
                  ;===== Выбираем пиксели, попадающие в выбранную ранее область
                  nx=sxpar(header,"NAXIS1")
                  ny=sxpar(header,"NAXIS2")
                  if fit_selection.type ne -1 then begin
                    xind=get_num(nx,ny,/x)
                    yind=get_num(nx,ny,/y)
                    fit_reg_x=(*fit_selection.points).x
                    fit_reg_y=(*fit_selection.points).y
                    for i=0,n_elements(fit_reg_x)-1 do begin
                      adxy,header,fit_reg_x[i],fit_reg_y[i],xpix,ypix
                      fit_reg_x[i]=xpix
                      fit_reg_y[i]=ypix
                    endfor
                    if fit_selection.type eq 0 then begin  
                       ;For rectangle or free shape
                       ;Из-за того, что мы пиксель считаем не от его центра, а от начала, 
                       ;а программа polyfillv криво считает 0-е пиксели (видимо, сдвигая все на 1) - прибавляем 0.5
                       if total(abs(fit_reg_x-fit_reg_x[0])) gt 0 $
                          and total(abs(fit_reg_y-fit_reg_y[0])) gt 0  then $
                          entire_reg= polyfillv(fit_reg_x+0.5,fit_reg_y+0.5,nx,ny) $
                       else entire_reg=-1
                    endif
                    if fit_selection.type eq 1 then begin
                       ; For circle shape
                       ;Из-за того, что мы пиксель считаем не от его центра, а от начала, отнимаем 0.5
                            dist_circle, rad_dist, [nx,ny], ((fit_reg_x[0]-0.5) < (nx-1)), ((fit_reg_y[0]-0.5) < (ny-1) )
                            entire_reg = where( abs(rad_dist) le sqrt((fit_reg_x[0]-fit_reg_x[1])^2+(fit_reg_y[0]-fit_reg_y[1])^2))
                    endif
                    if fit_selection.type eq 2 then begin
                       ; For point
                       entire_reg = where(xind eq floor(fit_reg_x[0]) and yind eq floor(fit_reg_y[0]))
                    endif 
                  endif else entire_reg = lindgen(nx*ny)
                  return,entire_reg
                END
                
                FUNCTION KS_SAVE_SHAPE, bord_draw_onIm_x,bord_draw_onIm_y,selection_shape, h, mark_name,mark_color
                  ; Сохраняет текущую кривую в нужную структуру
                  npoints=n_elements(bord_draw_onIm_x)
                  mark_points = replicate({bpos,x:0E, y:0E},npoints)
                  mark_points[*].x = bord_draw_onIm_x
                  mark_points[*].y = bord_draw_onIm_y
                  
                  ; marker type: 0 - free (and rectangle);  1 - circle; 2 - point  
                  sh_type=round(selection_shape.val)
                  if selection_shape.val eq 2 then sh_type = 0
                  if selection_shape.val eq 1 then begin
                    ; circle apperture
                    if round(mark_points[1].x) eq 0 then begin
                      mark_points=[{bpos,x:mark_points[0].x, y:mark_points[0].y}]
                      sh_type=2
                      npoints=1
                    endif else begin
                      mark_points[1].y = mark_points[0].y
                      mark_points[1].x = mark_points[0].x - mark_points[1].x
                    endelse
                  endif else begin
                    if total(abs(round(mark_points[*].x)-round(mark_points[0].x))+abs(round(mark_points[*].y)-round(mark_points[0].y))) eq 0 then begin
                      mark_points=[{bpos,x:mark_points[0].x, y:mark_points[0].y}]
                      sh_type=2
                      npoints=1
                    endif
                  endelse
                  
                  for i=0,npoints-1 do begin
                    xyad,h,mark_points[i].x,mark_points[i].y,ra,dec
                    mark_points[i].x=ra
                    mark_points[i].y=dec
                  endfor
                  insert={marker, name: mark_name, type: sh_type, color: mark_color, points: ptr_new(mark_points)}
                  return, insert
                END
                
                
                
                FUNCTION KS_IMCUBE, cube, channel=channel, head_in=head_in, head_out=head_out
                  ; Создает изображение (интегральное; в одном; в нескольких каналах) 
                  IF n_elements(channel) eq 0 then channel=-1
                  s=size(cube)
                  rec=where(channel ge 0 and channel lt s[3],nr)
                  if nr gt 0 then begin
                    if nr eq 1 then img=reform(cube[*,*,channel]) else img=reform(total(cube[*,*,channel],3))
                  endif else begin
                    nr=1
                    if s[0] gt 2 then begin
                      img=total(cube,3) 
                      nr=s[3]
                    endif else img=reform(cube)
                    
                  endelse
                  if keyword_set(head_in) then begin
                    head_out=head_in
                    sxaddpar,head_out,"NAXIS",2
                    if sxpar(head_out,"WCSAXES") then sxaddpar,head_out,"WCSAXES",2
                    sxdelpar,head_out,"NAXIS3"
                    sxdelpar,head_out,"NAXIS4"
                    sxdelpar,head_out,"CRVAL3"
                    sxdelpar,head_out,"CRVAL4"
                    sxdelpar,head_out,"CDELT3"
                    sxdelpar,head_out,"CD3_3"
                    sxdelpar,head_out,"CD3_2"
                    sxdelpar,head_out,"CD3_1"
                    sxdelpar,head_out,"CD4_3"
                    sxdelpar,head_out,"CD4_2"
                    sxdelpar,head_out,"CD4_1"
                    sxdelpar,head_out,"CD4_4"
                    sxdelpar,head_out,"CDELT4"
                    sxdelpar,head_out,"CTYPE3"
                    sxdelpar,head_out,"CTYPE4"
                    sxdelpar,head_out,"CRPIX3"
                    sxdelpar,head_out,"CRPIX4"
                    sxaddhist,"KS_IMCUBE: Image created from data cube as the sum of "+string(nr,format="(I0)")+" channels",head_out
                  endif
                  RETURN, img
                END

            
            FUNCTION KS_X_CONVERSION, x, typein=typein, typeout=typeout, cent_lam=cent_lam
              ; Конвертирует шкалу длин волн или скоростей или частот в другой тип или единицы
              if not keyword_set(typeout) then typeout=0
              x_out=x
              c=2.99792458e5
              if typeout eq 0 then begin
                if typein eq 1 then x_out=x/1e3
                if typein eq 2 or typein eq 3 or typein eq 4 then x_out=(x-cent_lam)/cent_lam*c
               ; ДОБАВИТЬ КОНВЕРТАЦИЮ ИЗ ЧАСТОТ!!!
                
              endif
              
              
              return, x_out
            END 



FUNCTION KS_NEW_PROFILE, old=old, nprof=nprof, insert=insert
  ; Добавляет к струкуре OLD с интегральными профилями новые дефолтные в колличестве NPROF штук или тот, что в переменной INSERT.
  ; Если OLD не задано - то обнуляет все, создавая NPROF профилей с дефолтными значениями
  COMMON KS_DATA
  COMMON KS_PROF_MANAGER
  
  prof_insert=prof_default
  if not keyword_set(nprof) then nprof=1
  if nprof gt 1 then prof_insert=replicate(prof_default,nprof)
  
  FOR i=0,nprof-1 DO BEGIN
     prof_insert[i].x=ptr_new(*prof_default.x)
     prof_insert[i].y=ptr_new(*prof_default.y)
     prof_insert[i].xr=ptr_new(*prof_default.xr)
     prof_insert[i].yr=ptr_new(*prof_default.yr)
     prof_insert[i].comps=ptr_new(*prof_default.comps)
     prof_insert[i].cent=ptr_new(*prof_default.cent)
     prof_insert[i].fwhm=ptr_new(*prof_default.fwhm)
     prof_insert[i].ampl=ptr_new(*prof_default.ampl)
     prof_insert[i].setmin_cent=ptr_new(*prof_default.setmin_cent)
     prof_insert[i].setmin_fwhm=ptr_new(*prof_default.setmin_fwhm)
     prof_insert[i].setmin_ampl=ptr_new(*prof_default.setmin_ampl)
     prof_insert[i].setmax_cent=ptr_new(*prof_default.setmax_cent)
     prof_insert[i].setmax_fwhm=ptr_new(*prof_default.setmax_fwhm)
     prof_insert[i].setmax_ampl=ptr_new(*prof_default.setmax_ampl)
     prof_insert[i].min_cent=ptr_new(*prof_default.min_cent)
     prof_insert[i].min_fwhm=ptr_new(*prof_default.min_fwhm)
     prof_insert[i].min_ampl=ptr_new(*prof_default.min_ampl)
     prof_insert[i].max_cent=ptr_new(*prof_default.max_cent)
     prof_insert[i].max_fwhm=ptr_new(*prof_default.max_fwhm)
     prof_insert[i].max_ampl=ptr_new(*prof_default.max_ampl)
     prof_insert[i].fixcent=ptr_new(*prof_default.fixcent)
     prof_insert[i].fixfwhm=ptr_new(*prof_default.fixfwhm)
     prof_insert[i].fixampl=ptr_new(*prof_default.fixampl)
     prof_insert[i].fitted_cent=ptr_new(*prof_default.fitted_cent)
     prof_insert[i].fitted_fwhm=ptr_new(*prof_default.fitted_fwhm)
     prof_insert[i].fitted_flux=ptr_new(*prof_default.fitted_flux)
     prof_insert[i].fitted_ampl=ptr_new(*prof_default.fitted_ampl)
     prof_insert[i].link_cent=ptr_new(*prof_default.link_cent)
     prof_insert[i].link_fwhm=ptr_new(*prof_default.link_fwhm)
     prof_insert[i].link_ampl=ptr_new(*prof_default.link_ampl)
     prof_insert[i].fitted_isset=ptr_new(*prof_default.fitted_isset)
     prof_insert[i].fitted_resid=ptr_new(*prof_default.fitted_resid)
     prof_insert[i].fitted_comps=ptr_new(*prof_default.fitted_comps)
     prof_insert[i].show_on=ptr_new(*prof_default.show_on)
  ENDFOR
  
  
  IF n_elements(insert) ne 0 then begin
  
    n=n_elements(insert)
    if n gt nprof then n=nprof 
    nx=sxpar(header_cub,"NAXIS1")
    ny=sxpar(header_cub,"NAXIS2")
    nz=sxpar(header_cub,"NAXIS3")
    refpix=sxpar(header_cub,"CRPIX3")
    xdelt=sxpar(header_cub,"CDELT3")
    if xdelt eq 0 then xdelt=sxpar(header_cub,"CD3_3")
    if refpix eq 0 then refpix=1
    xscale=(findgen(nz)-refpix+1)*xdelt+sxpar(header_cub,"CRVAL3")
    if axes_info.ztype eq 1 then xscale = xscale/1e3
    prof=dblarr(nz)
    for i=0,n-1 do begin
      prof_insert[i].color=insert[i].color
      prof_insert[i].name=insert[i].name
      pixels = KS_GET_SELECTION_REGION(header_cub, insert[i])
      for j=0,nz-1 do begin
        cubslice=reform(cub[*,*,j])
        prof[j]=double(total(cubslice[pixels]))
      endfor
      
      moments=ks_moments(xscale, prof, contin=contin, snr=snr)
      moments_struct={mom1:moments[1],mom2:moments[2],mom3:moments[3],mom4:moments[4],snr:snr}
      
      
      ini_comps=KS_GET_INITIAL_COMPS(xscale, prof, moments_struct, contin)
      
      
      *prof_insert[i].y=prof
      *prof_insert[i].x=xscale
      *prof_insert[i].xr=[xscale[0],xscale[nz-1]]
      intens=abs(max(prof,/nan)-contin)
      (*prof_insert[i].yr)[0]=min(prof,/nan)-0.05*intens
      (*prof_insert[i].yr)[1]=max(prof,/nan)+0.05*intens
      prof_insert[i].amp_norm=0
       prof_insert[i].cont=contin
       prof_insert[i].snr=snr
      *prof_insert[i].fitted_resid=prof
      *prof_insert[i].fitted_comps=dblarr(3,nz)
      
       prof_insert[i].mom0=moments[0]
       prof_insert[i].mom1=moments[1]
       prof_insert[i].mom2=moments[2]
       prof_insert[i].mom3=moments[3]
       prof_insert[i].mom4=moments[4]
      
      
      *prof_insert[i].cent=ini_comps.cent
      *prof_insert[i].ampl=ini_comps.ampl
      *prof_insert[i].fwhm=ini_comps.fwhm
      *prof_insert[i].comps=ini_comps.comps
      
      *prof_insert[i].setmin_cent=ini_comps.setmin_cent
      *prof_insert[i].setmin_ampl=ini_comps.setmin_ampl
      *prof_insert[i].setmin_fwhm=ini_comps.setmin_fwhm
      prof_insert[i].setmin_cont=ini_comps.setmin_cont
      *prof_insert[i].setmax_cent=ini_comps.setmax_cent
      *prof_insert[i].setmax_ampl=ini_comps.setmax_ampl
      *prof_insert[i].setmax_fwhm=ini_comps.setmax_fwhm
      prof_insert[i].setmax_cont=ini_comps.setmax_cont
      *prof_insert[i].min_cent=ini_comps.min_cent
      *prof_insert[i].min_ampl=ini_comps.min_ampl
      *prof_insert[i].min_fwhm=ini_comps.min_fwhm
      prof_insert[i].min_cont=ini_comps.min_cont
      *prof_insert[i].max_cent=ini_comps.max_cent
      *prof_insert[i].max_ampl=ini_comps.max_ampl
      *prof_insert[i].max_fwhm=ini_comps.max_fwhm
      prof_insert[i].max_cont=ini_comps.max_cont
      
;      *prof_insert[i].cent=[cent[0],(cent[1:2])[s]]
;      *prof_insert[i].ampl=[ampl[0],(ampl[1:2])[s]]
;      *prof_insert[i].fwhm=[fw[0],(fw[1:2])[s]]
;      *prof_insert[i].comps=comps
    endfor
  endif
    
  
  if n_elements(old) gt 0 then prof_out=[old,prof_insert] else prof_out=prof_insert
  RETURN, prof_out
end 



                
;############## Процедуры, нужные для отображения 
      FUNCTION KS_CHECK_COLOR, val, default=default
        ;Check whether input value correspond to existant color name
        if not keyword_set(default) then default="white"
        out = default
        possible_color=cgcolor(/names)
        rec=where(possible_color eq STRUPCASE(val),nr)
        if nr eq 1 then out=val
        return, out 
      END  
      
      
      PRO KS_CHOOSE_CUR_RES
        ; Выбирает нужные данные для дальнейшего отображения на панели результатов
        COMMON KS_DATA
        COMMON KS_ANALYSIS
        COMMON KS_DISPLAY
        COMMON KS_WIDGET_ELEMENTS
        COMMON KS_FLAGS_AND_PARAMS
        
        WIDGET_CONTROL,show_type[1].obj,get_val=types_list
        cur_type=types_list[show_type[1].val]
        WIDGET_CONTROL,show_type[2].obj,get_val=subtypes_list
        cur_subtype=subtypes_list[show_type[2].val]
        
        nx=sxpar(header_cub_2d,"NAXIS1")
        ny=sxpar(header_cub_2d,"NAXIS2")
        
        
        CASE cur_type OF
          "Moments": begin
            possible=["MOM0: Flux","MOM1: Vel","MOM2: Disp","MOM3: Assym","MOM4: Kurt"]
            index=where(possible eq cur_subtype)
            CASE index OF
              0: cur_res=fit_results_maps.mom0
              1: cur_res=fit_results_maps.mom1
              2: cur_res=fit_results_maps.mom2
              3: cur_res=fit_results_maps.mom3
              4: cur_res=fit_results_maps.mom4
            ENDCASE
            cur_res_name=cur_subtype
            cur_res_index=total(num_res_subtypes[0:4])+index
          end
          "Flux": begin
            possible=["Total","Central comp.","Blue comp.", "Red comp.","Shift.comp."]
            index=where(possible eq cur_subtype)
            CASE index OF
              0: cur_res=fit_results_maps.f_tot
              1: cur_res=fit_results_maps.f1
              2: cur_res=fit_results_maps.f2
              3: cur_res=fit_results_maps.f3
              4: cur_res=fit_results_maps.f_shi_cmp
            ENDCASE
            cur_res_name=cur_subtype+"  "+cur_type
            cur_res_index=index
          end
          "Intensity": begin
            possible=["Central comp.","Blue comp.", "Red comp.","Shift.comp."]
            index=where(possible eq cur_subtype)
            CASE index OF
              0: cur_res=fit_results_maps.i1
              1:  cur_res=fit_results_maps.i2
              2:  cur_res=fit_results_maps.i3
              3: cur_res=fit_results_maps.i_shi_cmp
            ENDCASE
            cur_res_name=cur_subtype+"  "+cur_type
            cur_res_index=total(num_res_subtypes[0])+index
          end
          "Velocity": begin
            possible=["Central comp.","Blue comp.", "Red comp.","Shift.comp.","Diff. map"]
            index=where(possible eq cur_subtype)
            CASE index OF
              0:  cur_res=fit_results_maps.v1
              1:  cur_res=fit_results_maps.v2
              2:  cur_res=fit_results_maps.v3
              3:  cur_res=fit_results_maps.v_shift
              4: cur_res=fit_results_maps.v_shi_cmp

            ENDCASE
            cur_res_name=cur_subtype+"  "+cur_type
            cur_res_index=total(num_res_subtypes[0:1])+index
          end
          "Dispersion": begin
            possible=["Central comp.","Blue comp.", "Red comp.","Shift.comp."]
            index=where(possible eq cur_subtype)
            CASE index OF
              0: cur_res=fit_results_maps.sigma1
              1:  cur_res=fit_results_maps.sigma2
              2:  cur_res=fit_results_maps.sigma3
              3: cur_res=fit_results_maps.sigma_shi_cmp
            ENDCASE
            cur_res_name=cur_subtype+"  "+cur_type
            cur_res_index=total(num_res_subtypes[0:2])+index
          end
          "S/N ratio": begin
            cur_res=fit_results_maps.snr
            cur_res_name=cur_type
            cur_res_index=total(num_res_subtypes[0:3])
          end
          "Continuum": begin
            cur_res=fit_results_maps.contin
            cur_res_name=cur_type
            cur_res_index=total(num_res_subtypes[0:5])
          end
          "Residuals": begin
            cur_res=fit_results_maps.resid/fit_results_maps.flux
            cur_res_name=cur_type
            cur_res_index=total(num_res_subtypes[0:6])
          end
        endcase
        h_cur_res=header_cub_2d
        rec=where(fit_results_maps.snr le snr_mask,nr)
        if nr gt 0 then cur_res[rec]=!Values.F_NAN
        KS_BRTFIELDS_ADJUST, type=1
      END
      
      
      
      FUNCTION  KS_GET_NEWLIST_RESSUBTYPES,res_loaded,num_res_subtypes,type, set_num=set_num
        new_list=['None']
        
        CASE type of
        
          "Moments": begin
            set_num=4
            possible=["MOM0: Flux","MOM1: Vel", "MOM2: Disp", "MOM3: Assym", "MOM4: Kurt"]
          end
          "Flux": begin
            set_num=0
            possible=["Total", "Central comp.", "Blue comp.", "Red comp.","Shift.comp."]
          end
          "Intensity": begin
            set_num=1
            possible=["Central comp.", "Blue comp.", "Red comp.","Shift.comp."]
          end
          "Velocity": begin
            set_num=2
            possible=["Central comp.", "Blue comp.", "Red comp.","Shift.comp.", "Diff. map"]
          end
          "Dispersion": begin
            set_num=3
            possible=["Central comp.", "Blue comp.", "Red comp.","Shift.comp."]
          end
          "S/N ratio": begin
            set_num=5
            possible=["Total"]
          end
          "Continuum": begin
            set_num=6
            possible=["Total"]
          end
          "Residuals": begin
            set_num=7
            possible=["Total"]
          end
          ELSE: set_num=-1
        endcase
        
        if set_num ne -1 then begin
          if set_num eq 0 then start=0 else start=total(num_res_subtypes[0:set_num-1])
          last = total(num_res_subtypes[0:set_num])-1
          rec = where(res_loaded[start:last] eq 1, nr)
          if nr gt 0 then  new_list=possible[rec]
        endif
        RETURN, new_list
      END



      PRO KS_SHOW_PROFILE, xim,yim, ref_header=ref_header
      ; Отображает текущий профиль на панели с результатами
      ; ref_header - если координаты не куба, а какого-то другого изображения (чья шапка)
        COMMON KS_DISPLAY
        COMMON KS_DATA
        COMMON KS_ANALYSIS
        COMMON KS_WIDGET_ELEMENTS
        
        nz=sxpar(header_cub,"NAXIS3")
        nx=sxpar(header_cub,"NAXIS1")
        ny=sxpar(header_cub,"NAXIS2")
        refpix=sxpar(header_cub,"CRPIX3")
        xdelt=sxpar(header_cub,"CDELT3")
        if xdelt eq 0 then xdelt=sxpar(header_cub,"CD3_3")
        if refpix eq 0 then refpix=1
        xscale=(findgen(nz)-refpix+1)*xdelt+sxpar(header_cub,"CRVAL3")
        prof=fltarr(nz)
        if keyword_set(ref_header) then begin
          xyxy,ref_header,header_cub,xim,yim
          xim=round(xim)
          yim=round(yim)
        endif
        inreg=1
        if xim ge 0 and xim le (nx-1) and yim ge 0 and yim le (ny-1) then prof[*]=cub[xim,yim,*] else inreg=0
        
        WIDGET_CONTROL, ks_res_disp.obj, GET_VALUE = index
        WSet, index
        
        ;Проверка на то, что точка не забита NAN
        rr=where(finite(prof),nr)
        if nr lt 2 then return 
        
        if axes_info.ztype eq -1 then xtit = "X (unrecognized units)"
        if axes_info.ztype eq 0 or axes_info.ztype eq 1 then xtit = "Velocity, km s$\up-1$"
        if axes_info.ztype eq 1 then xscale = xscale/1e3
        
        if axes_info.ztype ge 2 and axes_info.ztype le 4 then xtit = "Wavelength, "+axes_info.zunit[axes_info.ztype+1]
        if axes_info.ztype ge 5 then xtit = "Frequency, "+axes_info.zunit[axes_info.ztype+1]
        
          
        
        ; Проводим анализ моментов для отмечания на профиле центра, континуума, SNR и FWHM (???)
        if inreg then begin
          if moms_on_fly.done[xim,yim] eq 1 then begin
            moms=[moms_on_fly.mom0[xim,yim], moms_on_fly.mom1[xim,yim],moms_on_fly.mom2[xim,yim]]
            snr=moms_on_fly.snr[xim,yim]
            contin=moms_on_fly.contin[xim,yim]
          endif else begin
            moms=ks_moments(xscale, prof, contin=contin, snr=snr)
            moms_on_fly.mom0[xim,yim]=moms[0]
            moms_on_fly.mom1[xim,yim]=moms[1]
            moms_on_fly.mom2[xim,yim]=moms[2]
            moms_on_fly.mom3[xim,yim]=moms[3]
            moms_on_fly.mom4[xim,yim]=moms[4]
            moms_on_fly.snr[xim,yim]=snr
            moms_on_fly.contin[xim,yim]=contin
            moms_on_fly.done[xim,yim]=1
          endelse
          m1=max(prof,/NAN)
          m0=min(prof,/NAN) 
        endif else begin
          moms=[0,0,0]
          snr=0
          contin=0
          m1=0
          m0=0 
        endelse
        maxpos=moms[1]
        show_xscale=prof_xrange
        WIDGET_CONTROL,field_xrange[0],get_value=tmp
        show_xscale[0]=tmp
        WIDGET_CONTROL,field_xrange[1],get_value=tmp
        show_xscale[1]=tmp
        if xscale[0] gt show_xscale[0] then show_xscale[0]=xscale[0]
        if xscale[nz-1] lt show_xscale[1] then show_xscale[1]=xscale[nz-1]
        cgplot,xscale,prof, title="X = "+string(xim,format="(I0)")+"; Y = "+string(yim,format="(I0)")+"; S/N = "+string(snr,format="(I0)"),$
              ytit = "Intensity, "+ axes_info.I, xtit = xtit ,xst=1,yst=1, background="white",psym=10,xrange=[show_xscale[0],show_xscale[1]]
        cgoplot,[maxpos,maxpos],[m0,m1],linest=2,color="BLK6"      ;[xscale[maxpos],xscale[maxpos]]
        cgoplot,[maxpos+4*moms[2],maxpos+4*moms[2]],[m0,m1],linest=1,color="BLK6";,xrange=[show_xscale[0],show_xscale[1]]
        cgoplot,[maxpos-4*moms[2],maxpos-4*moms[2]],[m0,m1],linest=1,color="BLK6";,xrange=[show_xscale[0],show_xscale[1]]
        cgoplot,[xscale[0],xscale[nz-1]],[contin,contin],linest=1,color="BLK6";,xrange=[show_xscale[0],show_xscale[1]]
        
        ; Если фиттинг в данной точке проведен - загружаем модели
        IF inreg then begin
          ;if fit_results_maps.fitted[xim,yim] eq 1 then begin
            if finite(fit_results_maps.contin[xim,yim]) then this_contin=fit_results_maps.contin[xim,yim] else this_contin=0
            if fit_results_maps.is_set1[xim,yim] eq 1 then cgoplot,xscale,(fit_results_model.c1[xim,yim,*]+this_contin),color="green"
            if fit_results_maps.is_set2[xim,yim] eq 1 then cgoplot,xscale,(fit_results_model.c2[xim,yim,*]+this_contin),color="blue"
            if fit_results_maps.is_set3[xim,yim] eq 1 then cgoplot,xscale,(fit_results_model.c3[xim,yim,*]+this_contin),color="red"
            if fit_results_maps.is_set1[xim,yim] eq 1 or fit_results_maps.is_set2[xim,yim] eq 1 $
              or fit_results_maps.is_set3[xim,yim] eq 1 then begin
                cgoplot,xscale,(fit_results_model.resid[xim,yim,*]+this_contin),color="yellow"
                
                tot=xscale*0+this_contin
                if fit_results_maps.is_set1[xim,yim] eq 1 then tot+=fit_results_model.c1[xim,yim,*]
                if fit_results_maps.is_set2[xim,yim] eq 1 then tot+=fit_results_model.c2[xim,yim,*]
                if fit_results_maps.is_set3[xim,yim] eq 1 then tot+=fit_results_model.c3[xim,yim,*]
                
                 cgoplot,xscale,tot,color="magenta"
                
                
                endif
              
              
;          endif
        ENDIF
       
        
        
      END
      
      PRO KS_DRAW_BORDER, xcur,ycur, mode=mode, press=press, release=release, further_mode=further_mode
        ; Все, что касается рисования рамок. 
        ; mode= 'cube' or 'result'
        ; bord_pos_0 - position of start point of border[x,y]
        COMMON KS_SIZES
        COMMON KS_DISPLAY
        
        if not keyword_set(mode) then mode="cube"
        if not (mode eq "cube" or mode eq "result") then return
        if mode eq "cube" then dev = ks_cube_disp else dev = ks_res_disp
        
        WIDGET_CONTROL, dev.obj, GET_VALUE = index
        WSet, index
        
        ; Продолжение рисования рамки
        IF (start_draw_bord eq 1 or start_draw_bord eq 2) THEN BEGIN
          Device, Copy=[0, 0, sz[1].x, sz[1].y, 0, 0, clone_wID]
          IF (Xcur ge sz[1].x-1 or Xcur le 0 or Ycur ge sz[1].y-1 or Ycur le 0) or (release EQ 1) or (release EQ 4) then BEGIN
            ;====== Окончание рисования рамки
            xdrawcur=0>Xcur<(sz[1].x-1)
            ydrawcur=0>Ycur<(sz[1].y-1)
            further_mode=start_draw_bord
            start_draw_bord=0
            WDelete, clone_wID
            
            if selection_shape.val eq 0 then begin
              ;If rectangle shape
              bord_draw=[bord_draw,replicate({bpos,-1,-1},4)]
              bord_draw[0].x = Min([bord_draw[0].x, xdrawcur],Max=max_x)
              bord_draw[4].x=bord_draw[0].x
              bord_draw[0].y = Min([bord_draw[0].y, ydrawcur],Max=max_y)
              bord_draw[4].y=bord_draw[0].y
              bord_draw[1].x=max_x
              bord_draw[1].y=bord_draw[0].y
              bord_draw[2].x=max_x
              bord_draw[2].y=max_y
              bord_draw[3].x=bord_draw[0].x
              bord_draw[3].y=max_y
            endif
            if selection_shape.val eq 1 then begin
              ;If circle shape
              bord_draw=[bord_draw,{bpos,-1,-1}]
              bord_draw[1].x=sqrt((xdrawcur-bord_draw[0].x)^2+(ydrawcur-bord_draw[0].y)^2)
              bord_draw[1].y=bord_draw[1].x
            endif
            if selection_shape.val eq 2 then begin
              ;If free shape
              bord_draw=[bord_draw,{bpos,-1,-1},{bpos,-1,-1}]
              npnt=n_elements(bord_draw)
              bord_draw[npnt-2].x=xdrawcur
              bord_draw[npnt-2].y=ydrawcur
              bord_draw[npnt-1].x=bord_draw[0].x
              bord_draw[npnt-1].y=bord_draw[0].y
            endif
          ENDIF ELSE BEGIN
            
            sx = bord_draw[0].x
            sy = bord_draw[0].y
            if selection_shape.val eq 0 then cgPlotS, [sx, sx, xcur, xcur, sx], [sy, ycur, ycur, sy, sy], /Device, Color="TG4"
            if selection_shape.val eq 1 then tvcircle, sqrt((xcur-sx)^2+(ycur-sy)^2),(sx),(sy), "TG4", /Device
            if selection_shape.val eq 2 then begin
              bord_draw=[bord_draw,{bpos,-1,-1}]
              npnt=n_elements(bord_draw)
              bord_draw[npnt-1].x=xcur
              bord_draw[npnt-1].y=ycur
              cgPlots,[bord_draw.x,sx],[bord_draw.y,sy], /Device, Color="TG4"
            endif
          ENDELSE
        ENDIF
        
        
        
        IF (press GT 0) THEN BEGIN
         ;Начало рисования рамки
         IF press eq 1 or press eq 4 then begin
         ; Left or right button, Рисуем рамку
           IF (start_draw_bord eq 0) THEN BEGIN
            if press eq 1 then start_draw_bord=1 else start_draw_bord=2  
            Window, /Free, /Pixmap, XSize=sz[1].x, YSize=sz[1].y
            clone_wID = !D.Window
            Device, Copy=[0, 0, sz[1].x, sz[1].y, 0, 0, index]
            bord_draw=[{bpos,-1,-1}]
            bord_draw[0].x=xcur
            bord_draw[0].y=ycur
           ENDIF
         ENDIF
        ENDIF
      END
      
     
      
      
      PRO KS_DISP_TO_IM_CONV, xdisp, ydisp, xim, yim, mode=mode
          ; По заданным координатам на дисплее выдает координаты в пикселях (с долями) на изображении
          ; mode = "cube" | "result"
          COMMON KS_DISPLAY
          COMMON KS_SIZES
          
          if not keyword_set(mode) then mode = "cube"
          if not (mode eq "cube" or mode eq "result") then return
          if mode eq "cube" then begin
            h=h_cur_im
            type=0 
            cur_pos_on_disp=pos_on_disp.cub
          endif else begin
            h=h_cur_res
            type=1
            cur_pos_on_disp=pos_on_disp.res
          endelse 
           
          nx=sxpar(h,"NAXIS1")
          ny=sxpar(h,"NAXIS2")
          disp2im={x:double(nx)/round((sz[1].x)*(cur_pos_on_disp[2]-cur_pos_on_disp[0])), y:double(ny)/round((sz[1].y)*(cur_pos_on_disp[3]-cur_pos_on_disp[1]))} 
          ;########################################
           IF (mode_view[type].val eq 0) then begin
              adxy,h,zoom_border[type].x0,zoom_border[type].y0,x0,y0
              xim=((2*(xdisp-cur_pos_on_disp[0]*sz[1].x))/2.*disp2im.x/zoom_factor[type].x)+(x0); < (nx-1)
              yim=((2*(ydisp-cur_pos_on_disp[1]*sz[1].y))/2.*disp2im.y/zoom_factor[type].y)+(y0); < (ny-1)
           ENDIF ELSE BEGIN
              xim=xdisp+xscr_cur[type]
              yim=ydisp+yscr_cur[type]
           ENDELSE    
      END
      
      
      PRO KS_MONITOR_IMAGE, xcur,ycur, mode=mode
          ; Определяет координаты X,Y, Ra, dec и интенсивность изображения => выводит на соответствующие мониторы
          ; mode = "cube" | "result"
          COMMON KS_DISPLAY
          COMMON KS_WIDGET_ELEMENTS
          COMMON KS_SIZES
          COMMON KS_FLAGS_AND_PARAMS
          
          if not keyword_set(mode) then mode = "cube"
          if not (mode eq "cube" or mode eq "result") then return
          
          if mode eq "cube" then begin
            monitored_im=cur_im
            h=h_cur_im
            type=0 
            cur_pos_on_disp=pos_on_disp.cub
          endif else begin
            monitored_im=cur_res
            h=h_cur_res
            type=1
            cur_pos_on_disp=pos_on_disp.res
          endelse 
           
          nx=sxpar(h,"NAXIS1")
          ny=sxpar(h,"NAXIS2")
          ;########################################
          ;Мониторинг изображения   
           if (Xcur ge cur_pos_on_disp[0]*sz[1].x and Xcur le (cur_pos_on_disp[2]*sz[1].x-1) $   
              and Ycur ge cur_pos_on_disp[1]*sz[1].y and Ycur le (cur_pos_on_disp[3]*sz[1].y-1)) then begin
               KS_DISP_TO_IM_CONV, xcur,ycur, xcur_im_float, ycur_im_float, mode=mode 
               xcur_im= 0 > floor(xcur_im_float) < (nx-1)
               ycur_im= 0 > floor(ycur_im_float) < (ny-1)
               if monitored_im[xcur_im,ycur_im] lt 1e5 then Icur=string(monitored_im[xcur_im,ycur_im],format='(F0.3)') else $
                  Icur=string(monitored_im[xcur_im,ycur_im],format='(E0.5)')
               xyad,h,xcur_im,ycur_im,ra,dec
               radec_str=strsplit(adstring(ra,dec,1),/extract)
               racur=strjoin(radec_str[0:2]," ")
               deccur=strjoin(radec_str[3:5]," ")
               xcur_im_show=string(xcur_im,format='(I4)')
               ycur_im_show=string(ycur_im,format='(I4)')
               

               ;Отображаем текущий профиль
               IF mode eq "cube" and cub_loaded and curprof_monitor eq 1 then begin
                if show_type[0].val eq 1 then ref_head=h_cur_im else ref_head=0 
                KS_SHOW_PROFILE,xcur_im,ycur_im,ref_header=ref_head
               ENDIF   
               
           endif else begin
            xcur_im_float=!Values.F_NAN
            ycur_im_float=!Values.F_NAN
            xcur_im_show='None'
            ycur_im_show='None'
            racur='None'
            deccur='None'
            Icur='None'
           endelse
         
           KS_SHOW_PARALLEL_POSITION, xcur_im_float, ycur_im_float, type=type
         
           WIDGET_CONTROL,monitors[0].obj,set_value=xcur_im_show
           WIDGET_CONTROL,monitors[1].obj,set_value=ycur_im_show
           WIDGET_CONTROL,monitors[2].obj,set_value=racur
           WIDGET_CONTROL,monitors[3].obj,set_value=deccur
           WIDGET_CONTROL,monitors[4].obj,set_value=Icur
      
           
          ;########################################
      END

      PRO KS_SHOW_MARKERS, h
      ; Показываем маркеры и другие области
        COMMON KS_DISPLAY
        COMMON KS_ANALYSIS
        COMMON KS_PROF_MANAGER
        
        all_marked_regions=[markers,cur_fit_selection,profile_selection,curprof_selection] 
        rec=where(all_marked_regions.type ne -1, nrec)
        if nrec eq 0 then return
        ;if nmarks le 1 then return
        for i=0,(nrec-1) do begin
          ra=(*all_marked_regions[rec[i]].points).x
          dec=(*all_marked_regions[rec[i]].points).y
          npnt=n_elements(ra)
          x=ra*0
          y=ra*0
          for j=0,npnt-1 do begin
            adxy,h,ra[j],dec[j],xcur,ycur
            x[j]=xcur
            y[j]=ycur
          endfor
          IF all_marked_regions[rec[i]].type eq 0 then begin
            ; free line
            cgoplot,x,y,col=all_marked_regions[rec[i]].color
            cgtext,mean(x),mean(y),all_marked_regions[rec[i]].name,col=all_marked_regions[rec[i]].color,/data
          endif
          IF all_marked_regions[rec[i]].type eq 1 then begin
            ; circle
            tvcircle, sqrt((x[0]-x[1])^2+(y[0]-y[1])^2),x[0],y[0], all_marked_regions[rec[i]].color, /data
            cgtext,x[0],y[0],all_marked_regions[rec[i]].name,col=all_marked_regions[rec[i]].color,/data
          endif
          IF all_marked_regions[rec[i]].type eq 2 then begin
            ; point
            cgoplot,x[0],y[0],col=all_marked_regions[rec[i]].color,psym=1
            cgtext,x[0]+2,y[0]+2,all_marked_regions[rec[i]].name,col=all_marked_regions[rec[i]].color,/data
          ENDIF
        ENDFOR
      END



      PRO KS_BRTFIELDS_ADJUST, type=type, range=range
      ; Вывод в соответствующие поля корректной информации об яркости
      ; Расчет диапазона яркости в случае автоматического ее определения
      ; type=0,1 - for [cube,results] display
      ; Если насильно надо задать min и max (при выключенной галочке auto) - то указать как range = [min,max]
        COMMON KS_BRIGHTNESS
        COMMON KS_WIDGET_ELEMENTS
        COMMON KS_DISPLAY
        COMMON KS_FLAGS_AND_PARAMS
        
        IF NOT KEYWORD_SET(type) then type=0
        ;вычисляем индекс массива с яркостями и тд для конкретной картинки
        if type eq 0 then brtindex=show_type[0].val else brtindex=cur_res_index+3
                    
        
        
       ; Если мин=макс, то насильно заставляем считать в авто режиме
       
       IF auto_br[type] eq 0 then begin
        force_auto=0
        if n_elements(range) ne 2 then begin
          if brtmin[brtindex] eq brtmax[brtindex] then force_auto=1
        endif else begin
          if float(range[0]) eq float(range[1]) then force_auto=1
        endelse
        if force_auto then begin
          auto_br[type]=1
          WIDGET_CONTROL,but_auto_brt[type],set_button=1
        endif
       endif
        
        
        
        
        IF auto_br[type] eq 0 then begin
          if n_elements(range) eq 2 then begin
             brtmin[brtindex]=float(range[0])
             brtmax[brtindex]=float(range[1])
          endif
        ENDIF ELSE BEGIN
          WIDGET_CONTROL,field_brtfrac[type],set_val=brtfrac[brtindex]
          if type eq 0 then begin
            rec=where(finite(cur_im),nrec)
            if nrec gt 5 then show=sigrange(cur_im[rec],frac=brtfrac[brtindex]/100.,range=range, missing=0) else range=[brtmin[brtindex],brtmax[brtindex]] 
          endif else begin
            rec=where(finite(cur_res),nrec)
            if nrec gt 5 then begin
              if (cur_res_index lt total(num_res_subtypes[0:1]) or cur_res_index ge total(num_res_subtypes[0:2])) and $ 
                  cur_res_index ne total(num_res_subtypes[0:4])+1 and cur_res_index ne total(num_res_subtypes[0:4])+3 and $
                  cur_res_index ne total(num_res_subtypes[0:4])+4 then $
                            show=sigrange(cur_res[rec],frac=brtfrac[brtindex]/100.,range=range, missing=0) else $  ; Учли возможность скорости, ассиметрии и курт иметь 0-е значение
                            show=sigrange(cur_res[rec],frac=brtfrac[brtindex]/100.,range=range)
            endif else range=[brtmin[brtindex],brtmax[brtindex]]
          endelse          
          brtmin[brtindex]=float(range[0])
          brtmax[brtindex]=float(range[1])
        ENDELSE
        
        WIDGET_CONTROL, field_brtmin[type], set_val=brtmin[brtindex]
        WIDGET_CONTROL, field_brtmax[type], set_val=brtmax[brtindex]
      END



      PRO KS_SHOW_IMAGE, mode=mode, change_mode=change_mode, testcolor=testcolor
        ; mode="cube", "result"
        ; Показываем изображение.
        COMMON KS_DISPLAY        
        COMMON KS_SIZES
        COMMON KS_BRIGHTNESS
        COMMON KS_FLAGS_AND_PARAMS
        

        if mode eq "cube" then begin
          type=0
          current_disp=ks_cube_disp 
          current_image=cur_im
          current_head=h_cur_im
          if (clone_parallel_cub_ID) ge 0 then begin
              WDelete, clone_parallel_cub_ID
              clone_parallel_cub_ID=-1
          endif
        endif else begin
          if total(res_loaded) eq 0 then return
          type=1
          current_disp=ks_res_disp
          current_image=cur_res
          current_head=h_cur_res
          if (clone_parallel_res_ID) ge 0 then begin
             WDelete, clone_parallel_res_ID
             clone_parallel_res_ID=-1
          endif
        endelse
        
        WIDGET_CONTROL, current_disp.obj, GET_VALUE = index
        WSET, index          
        
        nx=sxpar(current_head,"NAXIS1")
        ny=sxpar(current_head,"NAXIS2")

        ;вычисляем индекс массива с яркостями и тд для конкретной картинки
        if type eq 0 then brtindex=show_type[0].val else brtindex=cur_res_index+3
        
        
        pallete=bytarr(3,!D.TABLE_SIZE)
        pallete[0,*]=color_tab[type].R
        pallete[1,*]=color_tab[type].G
        pallete[2,*]=color_tab[type].B
        
        minval=brtmin[brtindex]
        maxval=brtmax[brtindex]
        tvpar=0
        keep_asp=1
        
        IF (mode_view[type].val eq 0) THEN begin
            
            if zoom_factor[type].x eq 1 and zoom_factor[type].y eq 1 then begin 
              xrange=[0,nx]
              yrange=[0,ny]
              imshow=current_image
            endif else begin
              adxy,current_head,zoom_border[type].x0,zoom_border[type].y0,x0,y0
              adxy,current_head,zoom_border[type].x1,zoom_border[type].y1,x1,y1
              xind=get_num(nx,ny,/x)
              yind=get_num(nx,ny,/y)
              x0=round(x0)
              x1=round(x1)
              y0=round(y0)
              y1=round(y1)
              rec=where(xind ge x0 and xind le x1 and yind ge y0 and yind le y1, nrec)
              if nrec ge 4 then begin
                imshow=current_image[min(xind[rec]):max(xind[rec]),min(yind[rec]):max(yind[rec])]
                xrange=[min(xind[rec]),max(xind[rec])+1]
                yrange=[min(yind[rec]),max(yind[rec])+1]
              endif else begin
                imshow=fltarr(x1-x0+1,y1-y0+1)
                xrange=[x0,x1+1]
                yrange=[x0,x1+1]
              endelse  
            endelse
            if type eq 0 then pos=[0.,0.,1.,1.] else pos=[0.0,0.0,1.,0.87]
        ENDIF ELSE BEGIN
              IF (keyword_set(change_mode)) then begin
                WIDGET_CONTROL,current_disp.obj,draw_xsize=nx,draw_ysize=ny
                  xscr_cur[type]=(nx-sz[1].x) > 0 
                  yscr_cur[type]=(ny-sz[1].y) > 0         
                  WIDGET_CONTROL,current_disp.obj,set_draw_view=[xscr_cur[type],yscr_cur[type]]          
              ENDIF
              imshow=current_image
              tvpar=1
              xrange=[xscr_cur[type],xscr_cur[type]+sz[1].x]
              yrange=[yscr_cur[type],yscr_cur[type]+sz[1].y]
              keep_asp=0
              pos=[0.,0.,1.,1.]
        ENDELSE
        
        
        
        
        
        if not keyword_set(testcolor) then $
              cgimage,imshow,-xscr_cur[type],-yscr_cur[type],minval=minval,maxval=maxval,$
                    keep_asp=keep_asp,oposition=opos,tv=tvpar,pos=pos,$
                    xrange=xrange,yrange=yrange,palette=pallete,MISSING_VAL="NAN",MISSING_IND=0 $
        else cgimage,imshow,-xscr_cur[type],-yscr_cur[type],minval=minval,maxval=maxval,MISSING_VAL="NAN",MISSING_IND=0,$
                    keep_asp=keep_asp,oposition=opos,tv=tvpar,xrange=xrange,yrange=yrange,pos=pos;, _extra=extra
        IF (mode_view[type].val eq 0) THEN cur_pos_on_disp=opos else cur_pos_on_disp=[0.,0.,1.,1.]
        IF (type eq 1 and mode_view[type].val eq 0 and minval ne maxval) then begin
        if not keyword_set(testcolor) then cgcolorbar,pos=[cur_pos_on_disp[0],cur_pos_on_disp[3]+0.01,cur_pos_on_disp[2],cur_pos_on_disp[3]+0.05],range=[minval,maxval],/norm,/top,palette=pallete,title=cur_res_name $
            else cgcolorbar,pos=[cur_pos_on_disp[0],cur_pos_on_disp[3]+0.01,cur_pos_on_disp[2],cur_pos_on_disp[3]+0.05],range=[minval,maxval],/norm,title=cur_res_name,/top
        
        endif
        if type eq 0 then pos_on_disp.cub=cur_pos_on_disp else pos_on_disp.res=cur_pos_on_disp
        KS_SHOW_MARKERS, current_head

        
        Window, /Free, /Pixmap, XSize=sz[1].x, YSize=sz[1].y
        IF type eq 0 then clone_parallel_cub_ID = !D.Window else clone_parallel_res_ID = !D.Window
        Device, Copy=[0, 0, sz[1].x, sz[1].y, 0, 0, index]
        
      END


      PRO KS_PROCESS_SELECTION, mode=mode, cur_sel_mode=cur_sel_mode
      ; Действия с выделенной областью
      ; mode = "cube" | "result"
      ; cur_sel_mode = 0 => brt adjust; 1 => zoom; 2 => marker; 3 => cross-section; 4 => ...
          COMMON KS_DISPLAY        
          COMMON KS_SIZES
          COMMON KS_BRIGHTNESS
          COMMON KS_WIDGET_ELEMENTS
          COMMON KS_ANALYSIS
          COMMON KS_ANAL_MANAGER_WIDGET
          COMMON KS_PROF_MANAGER
          COMMON KS_DATA
          COMMON KS_FLAGS_AND_PARAMS
          COMMON KS_CURPROF_MANAGER
          
          if mode eq "cube" then begin
            type=0
            h=h_cur_im
            current_image=cur_im
            cur_pos_on_disp=pos_on_disp.cub
          endif else begin
            type=1
            h=h_cur_res
            current_image=cur_res
            cur_pos_on_disp=pos_on_disp.res
          endelse
          
          nx=sxpar(h,"NAXIS1")
          ny=sxpar(h,"NAXIS2")
          KS_DISP_TO_IM_CONV, bord_draw.x, bord_draw.y, bord_draw_onIm_x, bord_draw_onIm_y, mode=mode
          xind=get_num(nx,ny,/x)
          yind=get_num(nx,ny,/y)
          if selection_shape.val eq 0 or selection_shape.val eq 2 then begin  
            ;For rectangle or free shape
            if n_elements(bord_draw_onIm_x) ge 3 and $
            total(abs(bord_draw_onIm_x-bord_draw_onIm_x[0])) gt 0 $
            and total(abs(bord_draw_onIm_y-bord_draw_onIm_y[0])) gt 0  then $
            ;Из-за того, что мы пиксель считаем не от его центра, а от начала, 
            ;а программа polyfillv криво считает 0-е пиксели (видимо, сдвигая все на 1) - прибавляем 0.5
                entire_reg= polyfillv(bord_draw_onIm_x+0.5,bord_draw_onIm_y+0.5,nx,ny)$
                else entire_reg=-1
          endif
          if selection_shape.val eq 1 then begin
            ; For circle shape
            ;Из-за того, что мы пиксель считаем не от его центра, а от начала, вычитаем 0.5
            dist_circle, rad_dist, [nx,ny], ((bord_draw_onIm_x[0]-0.5) < (nx-1)), ((bord_draw_onIm_y[0]-0.5) < (ny-1) )
            if mode_view[type].val eq 0 then begin
              bord_draw_onIm_x[1]=bord_draw[1].x*double(nx)/round((sz[1].x)*(cur_pos_on_disp[2]-cur_pos_on_disp[0]))/zoom_factor[type].x
            endif else bord_draw_onIm_x[1]-=xscr_cur[type]
            entire_reg = where( abs(rad_dist) le bord_draw_onIm_x[1])
          endif 
          CASE cur_sel_mode OF
            0: BEGIN ;Подстройка яркости
              if n_elements(entire_reg) gt 10 then begin
                rec=where(finite(current_image[entire_reg]),nrec)
                if nrec gt 10 then begin
                  show=sigrange(current_image[entire_reg[rec]],frac=0.96,range=range, missing=0)
                  IF auto_br[type] eq 1 then begin
                    auto_br[type]=0
                    WIDGET_CONTROL,but_auto_brt[type],set_button=0
                  ENDIF
                  KS_BRTFIELDS_ADJUST,type=type, range=range
                  KS_SHOW_IMAGE, mode=mode
                endif
              endif
            END
            1: BEGIN ; Зум
              if n_elements(entire_reg) gt 10 then begin 
                xpos_c=round((min(xind[entire_reg])+max(xind[entire_reg]))/2.)
                ypos_c=round((min(yind[entire_reg])+max(yind[entire_reg]))/2.)
                dx=(max(xind[entire_reg])-min(xind[entire_reg]))/2.
                dy=(max(yind[entire_reg])-min(yind[entire_reg]))/2.
                ss=round(max([dx,dy]))
                if xpos_c-ss lt 0 or ypos_c-ss lt 0 then ss1=min([xpos_c,ypos_c]) else ss1=ss
                if (xpos_c+ss) gt (nx-1) or (ypos_c+ss) gt (ny-1) then ss2=min([nx-1-xpos_c,ny-1-ypos_c]) else ss2=ss
                ss=min([ss1,ss2])
                
                x0=(xpos_c-ss)
                x1=(xpos_c+ss)
                y0=(ypos_c-ss)
                y1=(ypos_c+ss)
                
                xyad,h, x0,y0, ra0,dec0
                xyad,h, x1,y1, ra1,dec1
                zoom_factor[type].x=double(nx)/float(x1-x0+1)
                zoom_factor[type].y=double(ny)/float(y1-y0+1)
                if zoom_factor[type].x lt 1 or zoom_factor[type].y lt 1 then begin
                  zoom_factor[type].x=1. & zoom_factor[type].y=1.
                  xyad,h, 0,0, ra0,dec0
                  xyad,h, nx-1,ny-1, ra1,dec1
                endif
                zoom_border[type].x0=ra0
                zoom_border[type].y0=dec0
                zoom_border[type].x1=ra1
                zoom_border[type].y1=dec1
                
                chmode=0
                IF (mode_view[type].val eq 1) then BEGIN
                  chmode=1
                  mode_view[type].val=0
                  if type eq 0 then WIDGET_CONTROL,ks_cube_disp.obj,draw_xsize=sz[1].x,draw_ysize=sz[1].y
                  if type eq 1 then WIDGET_CONTROL,ks_res_disp.obj,draw_xsize=sz[1].x,draw_ysize=sz[1].y
                   xscr_cur[type]=-1
                   yscr_cur[type]=-1
                   WIDGET_CONTROL,mode_view[type].obj,set_val=0
                ENDIF
                if type eq 0 then WIDGET_CONTROL,field_zoom_cub,set_val=zoom_factor[type].x else  WIDGET_CONTROL,field_zoom_res,set_val=zoom_factor[type].x
                KS_SHOW_IMAGE, mode=mode, change_mode=chmode
              endif
            END
            2: BEGIN ; Поставить маркер
              mark_name = string(n_elements(markers),format="(I0)")
              mark_color = "green"
              insert=KS_SAVE_SHAPE(bord_draw_onIm_x,bord_draw_onIm_y,selection_shape, h, mark_name, mark_color)
              markers=[markers, insert]
              KS_SHOW_IMAGE,mode="cube"
              if curprof_monitor eq 0 then KS_SHOW_IMAGE,mode="result"
            END
            3: BEGIN ; Cross-section
              
            END
            4: BEGIN ; Get integral profile
              mark_name = string(n_elements(profile_selection),format="(I0)")
              mark_color = "red"
              insert=KS_SAVE_SHAPE(bord_draw_onIm_x,bord_draw_onIm_y,selection_shape, h, mark_name, mark_color)
              if cub_loaded then begin
                IF insert.type ne 2 then begin
                  profile_selection=[profile_selection, insert]
                  prof_storage=KS_NEW_PROFILE(old=prof_storage, insert=insert)
                  KS_PROFMAN_UPD_TABLE
                  WIDGET_CONTROL,ks_profman_b,map=1
                ENDIF ELSE BEGIN
                  
                  ; Режим фиттинга текущего профиля
                  adxy,header_cub,(*insert.points).x,(*insert.points).y,curx,cury
                  curx=floor(curx)
                  cury=floor(cury)
                  IF curx ge 0 and curx lt sxpar(header_cub,"NAXIS1") and cury ge 0 and cury lt sxpar(header_cub,"NAXIS2") then begin 
                    curprofman_pix={x:curx,y:cury}
                    curprof_selection=insert
                    
                    curprof_selection.color="gold"
                    curprof_selection.name=""
                    KS_CURPROFMAN_UPD_COMPS
                    KS_CURPROFMAN_SHOW_PROF
                    KS_CURPROFMAN_CURPOS_UPD
                    WIDGET_CONTROL,ks_curprofman_b,map=1
                  ENDIF
                ENDELSE 
              endif
              KS_SHOW_IMAGE,mode="cube"
              if curprof_monitor eq 0 then KS_SHOW_IMAGE,mode="result"
            END
           
            5: BEGIN ; Select fit region
              mark_name = cur_fit_selection.name
              mark_color = cur_fit_selection.color
              insert=KS_SAVE_SHAPE(bord_draw_onIm_x,bord_draw_onIm_y,selection_shape, h, mark_name, mark_color)
              cur_fit_selection = insert
              an_infomessage[1]=mark_color+" shape"
              WIDGET_CONTROL,ks_an_info_label[0],set_value=an_infomessage[0]+an_infomessage[1]
              
              if cub_loaded then fit_pixels = KS_GET_SELECTION_REGION(header_cub, cur_fit_selection) else fit_pixels=-1
              if fit_pixels[0] eq -1 then n_fitpoints = 0 else n_fitpoints = n_elements(fit_pixels) 
              WIDGET_CONTROL,ks_an_info_label[1],set_value='N points for fit: '+string(n_fitpoints,format="(I0)")
              if n_fitpoints eq 0 then anal_buttons[2:3].sens=0 else anal_buttons[2:3].sens=1
              WIDGET_CONTROL,anal_buttons[2].obj,sens=anal_buttons[2].sens
              WIDGET_CONTROL,anal_buttons[3].obj,sens=anal_buttons[3].sens
              KS_SHOW_IMAGE,mode="cube"
              if curprof_monitor eq 0 then KS_SHOW_IMAGE,mode="result"
            END
            
            6: BEGIN ;Mask or something else
            
            END
            
            7: BEGIN ;PV diagram
            
            END
            
            ELSE:
         ENDCASE
      END



      PRO KS_LOAD_FILE, mode=mode
        ; Процедура для загрузки куба, изображения или лога с результатами
        ; mode="cube", "image", "binmap", "result"
        COMMON KS_DATA
        COMMON KS_DISPLAY
        COMMON KS_WIDGET_ELEMENTS
        COMMON KS_BRIGHTNESS
        COMMON KS_FLAGS_AND_PARAMS
        COMMON KS_ANALYSIS
        COMMON KS_ANAL_MANAGER_WIDGET
        type=(where(["cube","image","binmap"] eq mode, nr))[0]
        if nr ne 1 then begin
          mes=dialog_message("Error! Incorrect usage of KS_LOAD_FILE (incorrect mode)")
          return
        endif  
        tit=["Data Cube", "Image", "BinMap"]
        filter=['*.f*ts','*.F*TS']
        file=DIALOG_PICKFILE(title='Load '+tit[type],filter=filter,get_path=w_dir)
        inf=FILE_INFO(file)
        IF NOT (inf.exists) OR (file EQ '')  THEN return ELSE BEGIN
          cd,w_dir
          fdecomp,file,disk,file_dir,file_name,qual
          file_dir=disk+file_dir
          file_name+='.'+qual
          IF (file_name eq '.') then return
          h_tmp=headfits(file)
          
          ; проверка на то, что загружаем куб или изображение в первый раз (далее нужно для зума)
          if total([cub_loaded,im_loaded, binmap_loaded]) eq 0 then check_param=1 else check_param=0
          
          IF TYPE eq 0 then begin
          ; Проверяем, куб ли это???
            if sxpar(h_tmp,"NAXIS") lt 3 or sxpar(h_tmp,"NAXIS") gt 4 or (sxpar(h_tmp,"NAXIS") eq 4 and sxpar(h_tmp,"NAXIS4") gt 1) or (sxpar(h_tmp,"NAXIS") eq 3 and sxpar(h_tmp,"NAXIS3") eq 1) then begin
              mes=dialog_message("File should be 3D data cube!")
              return
            endif
            
            cub=readfits(file,/sil)
            header_cub=h_tmp
            cub_loaded=1
            cub_2d=KS_IMCUBE(cub, chan=cur_channel, head_in=header_cub, head_out=header_cub_2d)
            
            cur_im=cub_2d
            h_cur_im=header_cub_2d
            
          ENDIF
          
          IF TYPE eq 1 or TYPE eq 2 then begin
          ; Проверяем, 2D-картинка ли это??? Если вдруг куб - то конвертим в картинку
            if sxpar(h_tmp,"NAXIS") lt 2 then begin
              mes=dialog_message("File should be at least 2D-size!")
              return
            endif
            
            if sxpar(h_tmp,"NAXIS") gt 3 and sxpar(h_tmp,"NAXIS3") gt 1 and sxpar(h_tmp,"NAXIS4") gt 1 then begin
              mes=dialog_message("File should be 2D or 3D size!")
              return
            endif
            
            if type eq 1 then begin
              image=readfits(file,/sil)
              header_image=h_tmp
              im_loaded=1
              if sxpar(h_tmp,"NAXIS") gt 3 then begin
                if sxpar(h_tmp,"NAXIS4") gt 1 then image=reform(total(image,4)) else image=reform(image) 
              endif
              if sxpar(h_tmp,"NAXIS") gt 2 then image=KS_IMCUBE(image, head_in=header_image, head_out=header_image)
              cur_im=image
              h_cur_im=header_image
            endif else begin
              binmap=readfits(file,/sil)
              header_binmap=h_tmp
              binmap_loaded=1
              if sxpar(h_tmp,"NAXIS") gt 3 then begin
                if sxpar(h_tmp,"NAXIS4") gt 1 then binmap=reform(total(binmap,4)) else binmap=reform(binmap) 
              endif
              if sxpar(h_tmp,"NAXIS") gt 2 then binmap=KS_IMCUBE(binmap, head_in=header_binmap, head_out=header_binmap)
              cur_binmap=binmap
              h_cur_binmap=header_binmap
            endelse
          ENDIF
          
          filenames[type]=file
          WIDGET_CONTROL,ks_file_info[type],set_value=tit[type]+" Loaded: "+file
          
          WIDGET_CONTROL, ks_sml_b,sensitive=1
          WIDGET_CONTROL, ks_smr_b,sensitive=1
          WIDGET_CONTROL, ks_shape_b,sensitive=1
          
          IF TYPE eq 0 then begin
            ; If cub loaded
            buttons[1].sens=1
            WIDGET_CONTROL,buttons[1].obj,sensitive=1
            buttons[4:5].sens=1
            WIDGET_CONTROL,buttons[4].obj,sensitive=1
            WIDGET_CONTROL,buttons[5].obj,sensitive=1
            
            buttons[13:15].sens=1
            WIDGET_CONTROL,buttons[13].obj,sensitive=1
            WIDGET_CONTROL,buttons[14].obj,sensitive=1
            WIDGET_CONTROL,buttons[15].obj,sensitive=1
            
            field_channels_obj->SetProperty, NonSensitive=0
            show_type[0].val=0
            WIDGET_CONTROL, show_type[0].obj,set_val=0
            
            WIDGET_CONTROL,but_curprof_monitor,sens=1
            if total(res_loaded) eq 0 then begin
              WIDGET_CONTROL, but_curprof_monitor,set_button=1
              curprof_monitor=1
            endif
            
            nx=sxpar(h_cur_im,"NAXIS1")
            ny=sxpar(h_cur_im,"NAXIS2")
            
            nz=sxpar(header_cub,"NAXIS3")
            refpix=sxpar(header_cub,"CRPIX3")
            xdelt=sxpar(header_cub,"CDELT3")
            if xdelt eq 0 then xdelt=sxpar(header_cub,"CD3_3") 
            if refpix eq 0 then refpix=1
            prof_xrange=([0,nz-1]-refpix+1)*xdelt+sxpar(header_cub,"CRVAL3")
            WIDGET_CONTROL, field_xrange[0],set_value=prof_xrange[0]
            WIDGET_CONTROL, field_xrange[1],set_value=prof_xrange[1]
            moms_on_fly={snr: dblarr(nx,ny), mom0:dblarr(nx,ny), mom1: dblarr(nx,ny), $
                        mom2: dblarr(nx,ny), mom3: dblarr(nx,ny), mom4: dblarr(nx,ny), contin: dblarr(nx,ny), done: intarr(nx,ny)}
            
            anal_buttons[2:3].sens=1
            if cur_fit_selection.type eq -1 then begin
              n_fitpoints=nx*ny
              fit_pixels=lindgen(nx*ny)
            endif else begin
              fit_pixels = KS_GET_SELECTION_REGION(header_cub, cur_fit_selection)
              if fit_pixels[0] eq -1 then begin
                n_fitpoints = 0 
                anal_buttons[2:3].sens=0
              endif else n_fitpoints = n_elements(fit_pixels) 
            endelse
            WIDGET_CONTROL,anal_buttons[2].obj,sens=anal_buttons[2].sens
            WIDGET_CONTROL,anal_buttons[3].obj,sens=anal_buttons[3].sens
            WIDGET_CONTROL,ks_an_info_label[1],set_value='N points for fit: '+string(n_fitpoints,format="(I0)")
            KS_GET_CUB_UNITS
            
            KS_INI_RES_STRUCT,nx,ny,sxpar(header_cub,"NAXIS3")
            res_loaded=intarr(total(num_res_subtypes))
            WIDGET_CONTROL,ks_res_disp.obj,get_val=tmp
            wset,tmp
            erase
            if (clone_parallel_res_ID) ge 0 then begin
             WDelete, clone_parallel_res_ID
             clone_parallel_res_ID=-1
            endif
            xyad,header_cub_2d,0.,0.,ra0,dec0
            xyad,header_cub_2d,sxpar(header_cub_2d,"NAXIS1")-1.,sxpar(header_cub_2d,"NAXIS2")-1.,ra1,dec1
            zoom_border[1]={pos,ra0,dec0,ra1,dec1}
            zoom_factor[1].x=1
            zoom_factor[1].y=1
            WIDGET_CONTROL,field_zoom_res,set_val=zoom_factor[1].x
                  
            KS_TRIGGER_SENS_RES_PANEL, 0
          ENDIF
          
          IF TYPE eq 1 then begin
            buttons[3].sens=1
            WIDGET_CONTROL,buttons[3].obj,sensitive=1
            show_type[0].val=1
            WIDGET_CONTROL, show_type[0].obj,set_val=1
          ENDIF
          
          ;IF TYPE eq 0 OR TYPE EQ 1 THEN BEGIN
            WIDGET_CONTROL, ks_vm_cub_b,sensitive=1
            WIDGET_CONTROL, ks_st_cub_b,sensitive=1
            field_brtfrac_obj[0]->SetProperty, NonSensitive=0
            field_brtmin_obj[0]->SetProperty, NonSensitive=0
            field_brtmax_obj[0]->SetProperty, NonSensitive=0
            field_zoom_cub_obj->SetProperty, NonSensitive=0
            buttons[7].sens=1
            buttons[11].sens=1
            WIDGET_CONTROL,buttons[7].obj,sensitive=1
            WIDGET_CONTROL,buttons[11].obj,sensitive=1
            
            WIDGET_CONTROL,but_auto_brt[0], sens=1
            mode_for_show="cube"
            type_brt=0
            WIDGET_CONTROL,field_brtfrac[0],set_value=brtfrac[show_type[0].val]
;          ENDIF
          
;          IF TYPE eq 2 then begin
;            buttons[1].sens=1
;            WIDGET_CONTROL,buttons[1].obj,sensitive=1
;            buttons[4].sens=1
;            WIDGET_CONTROL,buttons[4].obj,sensitive=1
;            buttons[12].sens=1 
;            WIDGET_CONTROL,buttons[12].obj,sensitive=1
;            WIDGET_CONTROL, ks_vm_res_b,sensitive=1
;            WIDGET_CONTROL, ks_st_res_b,sensitive=1
;            field_brtfrac_obj[1]->SetProperty, NonSensitive=0
;            field_brtmin_obj[1]->SetProperty, NonSensitive=0
;            field_brtmax_obj[1]->SetProperty, NonSensitive=0
;            WIDGET_CONTROL,but_auto_brt[1], sens=1
;            field_zoom_res_obj->SetProperty, NonSensitive=0
;            field_snr_mask_obj->SetProperty, NonSensitive=0
;            buttons[8].sens=1
;            WIDGET_CONTROL,buttons[8].obj,sensitive=1
;            
;            mode_for_show="result"
;            type_brt=1
;            show_type[1].val=1
;            WIDGET_CONTROL,field_brtfrac[1],set_value=brtfrac[show_type[1].val+3]
;          ENDIF
          
          if check_param eq 1 or (zoom_factor[0].x eq 1 and (type eq 0 or type eq 1)) then begin
            ; Если это первый загруженный куб или изображение, то устанавливаем начальные границы области зума во все изображение
            xyad,h_cur_im,0.,0.,ra0,dec0
            xyad,h_cur_im,sxpar(h_cur_im,"NAXIS1")-1.,sxpar(h_cur_im,"NAXIS2")-1.,ra1,dec1
            zoom_border[0]={pos,ra0,dec0,ra1,dec1}
            zoom_factor[0].x=1
            zoom_factor[0].y=1
          endif else begin
            ;if type eq 0 or type eq 1 then begin
              ;KS_ZOOM_CHANGE, zoom_factor,zoom_border,h_cur_im,0, zoom_factor[0].x
              KS_ZOOM_UPD,type=0
              ;WIDGET_CONTROL,field_zoom_cub,set_val=zoom_factor[0].x
            ;endif
          endelse
          
          ;вычисляем индекс массива с яркостями и тд для конкретной картинки
;          if type_brt eq 0 then 
          brtindex=show_type[0].val; else brtindex=cur_res_index+3
          
;          if type_brt eq 0 then begin
            rec=where(finite(cur_im))
            if auto_br[type_brt] then show=sigrange(cur_im[rec],frac=brtfrac[brtindex]/100.,range=range, missing=0) else range=minmax(cur_im[rec],/nan) 
;          endif else begin
;            rec=where(finite(cur_res))
;            if auto_br[type_brt] then show=sigrange(cur_res[rec],frac=brtfrac[brtindex]/100.,range=range, missing=0) else range=minmax(cur_res[rec],/nan)
;          endelse
            brtmin[brtindex]=float(range[0])
            brtmax[brtindex]=float(range[1])
          
            WIDGET_CONTROL, field_brtmin[type_brt], set_val=brtmin[brtindex]
            WIDGET_CONTROL, field_brtmax[type_brt], set_val=brtmax[brtindex]
          
          KS_SHOW_IMAGE, mode=mode_for_show, /change_mode
        ENDELSE  
      END
        



;############################################## ОБРАБОТЧИК СОБЫТИЙ
PRO KS_GO_EVENT, event
  
  COMMON KS_DATA
  COMMON KS_DISPLAY
  COMMON KS_WIDGET_ELEMENTS
  COMMON KS_FLAGS_AND_PARAMS
  COMMON KS_BRIGHTNESS
  COMMON KS_SIZES
  COMMON KS_ANAL_MANAGER_WIDGET
  COMMON KS_RES_MANAGER_WIDGET
  COMMON KS_PROF_MANAGER
  
  WIDGET_CONTROL,event.ID,get_uvalue=ev
  
  IF strlen(ev) ge 4 then sub_event=strmid(ev,0,4) else sub_event=""
  IF sub_event eq "BRT_" THEN BEGIN
    ev_type=strmid(ev,4,4)
    ev_mode=strmid(ev,8,5)
    if ev_type eq "CUB_" THEN type=0 else type=1
    choose_mode=["cube","result"]
    ;вычисляем индекс массива с яркостями и тд для конкретной картинки
    if type eq 0 then brtindex=show_type[0].val else brtindex=cur_res_index+3
    CASE ev_mode OF
      
      "MIMAX": BEGIN
        IF auto_br[type] eq 1 then begin
          auto_br[type]=0
          WIDGET_CONTROL,but_auto_brt[type],set_button=0
        ENDIF
        WIDGET_CONTROL,field_brtmin[type],get_val=tmp
        brtmin[brtindex]=tmp
        WIDGET_CONTROL,field_brtmax[type],get_val=tmp
        brtmax[brtindex]=tmp
        KS_SHOW_IMAGE, mode=choose_mode[type]
      END
      
      "AUSCL": BEGIN
        auto_br[type]=1-auto_br[type]
        IF auto_br[type] eq 0 then begin
          WIDGET_CONTROL,field_brtmax[type],get_val=tmp
          brtmax[brtindex]=tmp
          WIDGET_CONTROL,field_brtmin[type],get_val=tmp
          brtmin[brtindex]=tmp
        endif else begin
          WIDGET_CONTROL,field_brtfrac[type],get_val=tmp
          brtfrac[brtindex]=tmp
          KS_BRTFIELDS_ADJUST, type=type
        endelse
        KS_SHOW_IMAGE, mode=choose_mode[type]
      END
      
      "FRACT": BEGIN
          IF auto_br[type] eq 0 then begin
              auto_br[type]=1
              WIDGET_CONTROL,but_auto_brt[type],set_button=1
          ENDIF
          WIDGET_CONTROL,field_brtfrac[type],get_val=tmp
          brtfrac[brtindex]=tmp
          IF brtfrac[brtindex] le 0 or brtfrac[brtindex] ge 100 then begin
                brtfrac[brtindex]=99.
                WIDGET_CONTROL,field_brtfrac[type],set_val=brtfrac[brtindex]
          ENDIF
          KS_BRTFIELDS_ADJUST, type=type
          KS_SHOW_IMAGE, mode=choose_mode[type]
      END
      
      ELSE:
      
   ENDCASE
  
  ENDIF ELSE BEGIN
   
      CASE ev OF
          
          'load_cube': KS_LOAD_FILE, mode="cube"
          'load_image': KS_LOAD_FILE, mode="image"
          
          'show_header_c':XDISPSTR,header_cub,TITLE='Data Cube Header',group_leader=ks_mb
          'show_header_i':XDISPSTR,header_image,TITLE='Image Header',group_leader=ks_mb
          
          'load_binmap': KS_LOAD_FILE, mode="binmap"
          
          'res_man': WIDGET_CONTROL,ks_resman_b,map=1
          'analysis': WIDGET_CONTROL,ks_anal_b,map=1
          'prof_man': WIDGET_CONTROL,ks_profman_b,map=1
          
          'prof_display_xrange': BEGIN
              WIDGET_CONTROL,field_xrange[0],get_val=tmp
              prof_xrange[0]=tmp
              WIDGET_CONTROL,field_xrange[1],get_val=tmp
              prof_xrange[1]=tmp
;              KS_SHOW_IMAGE, mode=choose_mode[type]
            END
          
          'sel_mode_left': BEGIN
              WIDGET_CONTROL,selection_mode[0].obj,get_value=tmp
              IF (selection_mode[0].val eq tmp) then RETURN
              selection_mode[0].val=tmp
          END
          'sel_mode_right': BEGIN
              WIDGET_CONTROL,selection_mode[1].obj,get_value=tmp
              IF (selection_mode[1].val eq tmp) then RETURN
              selection_mode[1].val=tmp
          END
          'sel_shape': BEGIN
              WIDGET_CONTROL,selection_shape.obj,get_value=tmp
              IF (selection_shape.val eq tmp) then RETURN
              selection_shape.val=tmp
          END
          
          
          
          'view_mode_cub': BEGIN
              WIDGET_CONTROL,mode_view[0].obj,get_value=tmp
              IF (mode_view[0].val eq tmp) then RETURN
              mode_view[0].val=tmp
              if (im_loaded or cub_loaded) then BEGIN
                IF tmp eq 0 THEN BEGIN
                   WIDGET_CONTROL,ks_cube_disp.obj,draw_xsize=sz[1].x,draw_ysize=sz[1].y
                   xscr_cur[0]=-1
                   yscr_cur[0]=-1
                ENDIF
                KS_SHOW_IMAGE,mode="cube",/change_mode
             ENDIF
           END
          
          'view_mode_res': BEGIN
              WIDGET_CONTROL,mode_view[1].obj,get_value=tmp
              IF (mode_view[1].val eq tmp) then RETURN
              mode_view[1].val=tmp
              if total(res_loaded) ne 0 then BEGIN
                IF tmp eq 0 THEN BEGIN
                   WIDGET_CONTROL,ks_res_disp.obj,draw_xsize=sz[1].x,draw_ysize=sz[1].y
                   xscr_cur[1]=-1
                   yscr_cur[1]=-1
                ENDIF
                KS_SHOW_IMAGE,mode="result",/change_mode
             ENDIF
           END
          
          
          'show_type_cub': BEGIN
              WIDGET_CONTROL,show_type[0].obj,get_value=tmp
                  IF (show_type[0].val eq tmp) then RETURN
                  IF not cub_loaded and (tmp eq 0 or tmp eq 2) then begin
                    WIDGET_CONTROL,show_type[0].obj,set_value=1
                    show_type[0].val=1
                    RETURN
                  ENDIF
                  IF not im_loaded and (tmp eq 1) then begin
                    WIDGET_CONTROL,show_type[0].obj,set_value=show_type[0].val
                    RETURN
                  ENDIF
                  show_type[0].val=tmp
                  
                  if show_type[0].val eq 0 then begin
                    cur_im=cub_2d
                    h_cur_im=header_cub_2d
                  endif 
                  
                  if show_type[0].val eq 1 then begin
                    cur_im=image
                    h_cur_im=header_image
                  endif
                  
                  if show_type[0].val eq 2 then begin
                    KS_GETCHANNELS 
                    cur_im=KS_IMCUBE(cub, channel=cur_channel, head_in=header_cub, head_out=h_cur_im)
                  endif
                    KS_ZOOM_UPD,type=0
                    ;KS_ZOOM_CHANGE, zoom_factor,zoom_border,h_cur_im,0, zoom_factor[0].x
                    ;WIDGET_CONTROL,field_zoom_cub,set_val=zoom_factor[0].x
                    KS_BRTFIELDS_ADJUST,type=0
                    KS_SHOW_IMAGE,mode="cube";, /change_mode
          END
          
          'show_channels': BEGIN
              IF cub_loaded eq 0 then return
              WIDGET_CONTROL,show_type[0].obj,set_value=2
              show_type[0].val=2
              KS_GETCHANNELS
              cur_im=KS_IMCUBE(cub, channel=cur_channel, head_in=header_cub, head_out=h_cur_im)
          
              KS_BRTFIELDS_ADJUST,type=0
              KS_SHOW_IMAGE,mode="cube", /change_mode
          END
          
          
          'dispcub': BEGIN
              ; === События, генерируемые дисплеем куба или изображения
              IF im_loaded or cub_loaded THEN BEGIN
                      Xcur=event.x
                      Ycur=event.y
                    ;--- Скроллинг изображения
                      IF (event.TYPE EQ 3) THEN BEGIN
                            xscr_cur[0]=Xcur
                            yscr_cur[0]=Ycur
                            KS_SHOW_IMAGE,mode="cube"
                      ENDIF
                      
                       IF (event.TYPE LT 3) THEN BEGIN
                          ; --- Мониторинг изображения
                          KS_MONITOR_IMAGE, xcur, ycur, mode="cube"
                          ;Рисуем "рамки"
                          further_mode=0
                          KS_DRAW_BORDER, xcur,ycur, mode="cube", further_mode=further_mode, press=event.press, release=event.release
                          IF further_mode gt 0 then begin
                             ; _____ Здесь некие действия с выбранной областью
                             cur_sel_mode=selection_mode[further_mode-1].val
                             if further_mode eq 2 then cur_sel_mode+=4
                             KS_PROCESS_SELECTION, mode="cube", cur_sel_mode=cur_sel_mode
                          ENDIF
                      ENDIF
              ENDIF
          END
          
          
          'dispres': BEGIN
              ; === События, генерируемые дисплеем куба или изображения
              IF total(res_loaded) gt 1 and curprof_monitor eq 0 THEN BEGIN
                      Xcur=event.x
                      Ycur=event.y
                    ;--- Скроллинг изображения
                      IF (event.TYPE EQ 3) THEN BEGIN
                            xscr_cur[1]=Xcur
                            yscr_cur[1]=Ycur
                            KS_SHOW_IMAGE,mode="result"
                      ENDIF
                      
                       IF (event.TYPE LT 3) THEN BEGIN
                          ; --- Мониторинг изображения
                          KS_MONITOR_IMAGE, xcur, ycur, mode="result"
                          ;Рисуем "рамки"
                          further_mode=0
                          KS_DRAW_BORDER, xcur,ycur, mode="result", further_mode=further_mode, press=event.press, release=event.release
                          IF further_mode gt 0 then begin
                             ; _____ Здесь некие действия с выбранной областью
                             cur_sel_mode=selection_mode[further_mode-1].val
                             if further_mode eq 2 then cur_sel_mode+=4
                             KS_PROCESS_SELECTION, mode="result", cur_sel_mode=cur_sel_mode
                          ENDIF
                      ENDIF
              ENDIF
          END
          
          'snr_mask': BEGIN
            WIDGET_CONTROL,field_snr_mask,get_val=tmp
            if valid_num(tmp) then begin
                if tmp lt 0 then WIDGET_CONTROL,field_snr_mask,set_val=snr_mask else begin
                snr_mask=tmp
                KS_CHOOSE_CUR_RES
                KS_BRTFIELDS_ADJUST,type=1
                KS_SHOW_IMAGE,mode="result"
                endelse
            endif else WIDGET_CONTROL,field_snr_mask,set_val=snr_mask
          END
          
          
          'zoom_cub': BEGIN
            type=0
            h=h_cur_im
            WIDGET_CONTROL,field_zoom_cub,get_val=tmp
            if valid_num(tmp) then begin
                KS_ZOOM_CHANGE,zoom_factor,zoom_border, h, type, tmp
            endif
            WIDGET_CONTROL,field_zoom_cub,set_val=zoom_factor[type].x
            KS_SHOW_IMAGE,mode="cube"
          END
          
          'zoom_res': BEGIN
            type=1
            h=h_cur_res
            WIDGET_CONTROL,field_zoom_res,get_val=tmp
            if valid_num(tmp) then begin
                KS_ZOOM_CHANGE,zoom_factor,zoom_border, h, type, tmp
            endif
            WIDGET_CONTROL,field_zoom_res,set_val=zoom_factor[type].x
            KS_SHOW_IMAGE,mode="result"
          END
          
          'zoom_reset_cub': BEGIN
              nx=sxpar(h_cur_im,"NAXIS1")
              ny=sxpar(h_cur_im,"NAXIS2")
              xyad,h_cur_im,0,0,ra0,dec0
              xyad,h_cur_im,nx-1,ny-1,ra1,dec1
              zoom_border[0].x0=ra0
              zoom_border[0].y0=dec0
              zoom_border[0].x1=ra1
              zoom_border[0].y1=dec1
              zoom_factor[0].x=1
              zoom_factor[0].y=1
              WIDGET_CONTROL,field_zoom_cub,set_val=zoom_factor[0].x
              KS_SHOW_IMAGE,mode="cube"
           END
           
           'zoom_reset_res': BEGIN
              nx=sxpar(h_cur_res,"NAXIS1")
              ny=sxpar(h_cur_res,"NAXIS2")
              xyad,h_cur_res,0,0,ra0,dec0
              xyad,h_cur_res,nx-1,ny-1,ra1,dec1
              zoom_border[1].x0=ra0
              zoom_border[1].y0=dec0
              zoom_border[1].x1=ra1
              zoom_border[1].y1=dec1
              zoom_factor[1].x=1
              zoom_factor[1].y=1
              WIDGET_CONTROL,field_zoom_res,set_val=zoom_factor[1].x
              KS_SHOW_IMAGE,mode="result"
           END
           
           
           'coltab_cub': BEGIN
            if cub_loaded  or im_loaded then XCOLORS,NotifyPro='KS_SHOW_IMAGE', mode="cube",/drag,Group_Leader=ks_mb,/block,$
            ColorInfo=colorInfo,/testcolor, brewer=color_tab[0].brewer,reverse=color_tab[0].reversed,$
            title="Load Color Table for Left Pannel",index=color_tab[0].index $ 
            else XCOLORS,Group_Leader=ks_mb,/block,ColorInfo=colorInfo, brewer=color_tab[0].brewer,$
            reverse=color_tab[0].reversed, title="Load Color Table for Left Pannel",index=color_tab[0].index
            
            ctmp={colors,$
              R: BytArr(!D.Table_Size), $ ; The current R color vector.
              G: BytArr(!D.Table_Size), $ ; The current G color vector.
              B: BytArr(!D.Table_Size), $ ; The current B color vector.
              NAME: "", $                 ; The name of the current color table.
              INDEX: 0, $                 ; The index number of the current color table.
              TYPE: "", $                 ; The type of color table (e.g, BREWER or IDL).
              BREWER: 0, $                ; Set to 1 if using BREWER color tables, else to 0.
              REVERSED: 0B}
             copy_struct,colorInfo,ctmp
             color_tab[0]=ctmp
             if cub_loaded or im_loaded then KS_SHOW_IMAGE,mode="cube"
            end
           'coltab_res': BEGIN
              if total(res_loaded) gt 0 then XCOLORS,NotifyPro='KS_SHOW_IMAGE', mode="result",/drag,Group_Leader=ks_mb,/block, $
              ColorInfo=colorInfo,/testcolor,title="Load Color Table for Right Pannel",$
              brewer=color_tab[1].brewer,reverse=color_tab[1].reversed,index=color_tab[1].index $
              else XCOLORS,Group_Leader=ks_mb,/block, ColorInfo=colorInfo,title="Load Color Table for Right Pannel",$
              brewer=color_tab[1].brewer,reverse=color_tab[1].reversed,index=color_tab[1].index
              ctmp={colors,$
                R: BytArr(!D.Table_Size), $ ; The current R color vector.
                G: BytArr(!D.Table_Size), $ ; The current G color vector.
                B: BytArr(!D.Table_Size), $ ; The current B color vector.
                NAME: "", $                 ; The name of the current color table.
                INDEX: 0, $                 ; The index number of the current color table.
                TYPE: "", $                 ; The type of color table (e.g, BREWER or IDL).
                BREWER: 0, $                ; Set to 1 if using BREWER color tables, else to 0.
                REVERSED: 0B}
               copy_struct,colorInfo,ctmp
               color_tab[1]=ctmp   
               if total(res_loaded) gt 0 then KS_SHOW_IMAGE,mode="result"
            END
           
           'RESDISP_AS_PROF_MONITOR': BEGIN
                curprof_monitor=1-curprof_monitor
                IF curprof_monitor eq 0 and show_type[1].val ne -1 and show_type[2].val ne -1 then begin
                  KS_BRTFIELDS_ADJUST,type=1
                  KS_SHOW_IMAGE,mode="result"
                ENDIF
              END 
           
           
           
           'show_type_p1_res': BEGIN
              type = event.str
              index=event.index
              if index eq show_type[1].val then return
              show_type[1].val=index
              new_list=KS_GET_NEWLIST_RESSUBTYPES(res_loaded,num_res_subtypes,type)
              WIDGET_CONTROL, show_type[2].obj,set_value=new_list
              WIDGET_CONTROL, show_type[2].obj,SET_COMBOBOX_SELECT=0
              show_type[2].val=0
              KS_CHOOSE_CUR_RES
              KS_SHOW_IMAGE,mode="result"
          END
           
          'show_type_p2_res': BEGIN
           type = event.index
           if show_type[2].val eq type then return
           show_type[2].val = type
           KS_CHOOSE_CUR_RES
           KS_SHOW_IMAGE,mode="result"
           
          END
           
          'saveps_cub': KS_SAVE_PS, type=0
          'saveps_res': KS_SAVE_PS, type=1
          
           
           
           
          'quit': BEGIN
             is_set=WIDGET_INFO(ks_anal_b,/VALID_ID)
             if (is_set eq 1) then WIDGET_CONTROL, ks_anal_b, /DESTROY
             is_set=WIDGET_INFO(ks_resman_b,/VALID_ID)
             if (is_set eq 1) then WIDGET_CONTROL, ks_resman_b, /DESTROY
             is_set=WIDGET_INFO(ks_profman_b,/VALID_ID)
             if (is_set eq 1) then WIDGET_CONTROL, ks_profman_b, /DESTROY
             KS_FREE_ALL_POINTERS
             WIDGET_CONTROL,/DESTROY,event.top 
          END
         Else:  
       ENDCASE
 
  ENDELSE
END




PRO KS_GO
  
  COMMON KS_SIZES
  COMMON KS_DISPLAY
  COMMON KS_WIDGET_ELEMENTS
  COMMON KS_FLAGS_AND_PARAMS
  COMMON KS_BRIGHTNESS
  
  ks_mb=WIDGET_BASE(TITLE="KINEmatics SCOPE (2015)",/row)

  if (!VERSION.OS_FAMILY eq "Windows") then titfont="Sans Serif" else titfont="-misc-fixed-bold-r-normal--13-120-75-75-c-70-iso8859-1"
  
  ;####### База для кнопок
  ks_butbase=WIDGET_BASE(ks_mb,/col)
  
      
      ;==== Кнопки & мониторы & Настройки селектора
      
      buttons=[{obj_par,'Load Cube','load_cube',0,0,1},$ ;#0
               {obj_par,'Cube Header','show_header_c',0,0,0},$ ;#1
               {obj_par,'Load Image','load_image',0,0,1},$ ;#2
               {obj_par,'Image Header','show_header_i',0,0,0},$ ;#3
               {obj_par,'Analysis','analysis',0,0,0},$ ;#4
               {obj_par,'Results Manager','res_man',0,0,0},$ ;#5
               {obj_par,'Quit','quit',0,0,1},$ ;#6
               ;Buttons for zoom
               {obj_par,'Reset to 1','zoom_reset_cub',0,0,0},$ ;#7
               {obj_par,'Set Color','coltab_cub',0,0,1},$  ;#8
               {obj_par,'Reset to 1','zoom_reset_res',0,0,0},$ ;#9
               {obj_par,'Set Color','coltab_res',0,0,0},$ ;#10
               {obj_par,'Save PS','saveps_cub',0,0,0},$ ;#11
               {obj_par,'Save PS','saveps_res',0,0,0},$ ;#12
               
               ;Prof and markers managers
               {obj_par,'Profiles manager','prof_man',0,0,0},$ ;#13
               {obj_par,'Markers manager','mark_man',0,0,0},$ ;#14
               
               {obj_par,'Load Voronoi Bin Map','load_binmap',0,0,0}] ;#15
               
      
      
      ks_butbase_cols=WIDGET_BASE(ks_butbase,/row, xpad=0,xoffset=0)
      ks_butbase_l=WIDGET_BASE(ks_butbase_cols,/col, xpad=0,xoffset=0)
      ;output=lonarr(2)
      KS_Buttons_Cre,ks_butbase_l,buttons[0:1].name,buttons[0:1].uval,output,sens=buttons[0:1].sens,xs=sz[2].x,ys=sz[2].y;,break_arr=[1,3,5]
      buttons[0:1].obj=output
      
      ks_butbase_r=WIDGET_BASE(ks_butbase_cols,/col, xpad=0,xoffset=0)
      ;output=lonarr(2)
      KS_Buttons_Cre,ks_butbase_r,buttons[2:3].name,buttons[2:3].uval,output,sens=buttons[2:3].sens,xs=sz[2].x,ys=sz[2].y;,break_arr=[1,3,5]
      
      buttons[2:3].obj=output
      ;output=lonarr(3)
      ind=[15,4,5,13,14,6]
      KS_Buttons_Cre,ks_butbase,buttons[ind].name,buttons[ind].uval,output,sens=buttons[ind].sens,xs=sz[2].x*2+3,ys=sz[2].y,break_arr=[0,2,4]
      buttons[ind].obj=output
      
      ks_break=WIDGET_BASE(ks_butbase,/column, ys=1)
      
      
      lab=WIDGET_LABEL(ks_butbase,val="Selection Shape:",font=titfont)
      ks_shape_b=WIDGET_BASE(ks_butbase,/row,/frame, sens=0, xpad=0)
      selection_shape={obj_par,'','sel_shape',0,0L,1}
      selection_shape.obj=CW_BGROUP(ks_shape_b,['Rectangle','Circle','Free'],xpad=0, uval=selection_shape.uval, /row, /EXCLUSIVE,SET_VALUE=selection_shape.val,/NO_RELEASE)
      
      
      lab=WIDGET_LABEL(ks_butbase,val="Selection Mode:",font=titfont)
      ks_selmode_b=WIDGET_BASE(ks_butbase,/row)
      selection_mode=[{obj_par,'','sel_mode_left',0,0L,1},$
                      {obj_par,'','sel_mode_right',0,0L,1}]
                      
      ks_selmode_l_b=WIDGET_BASE(ks_selmode_b,/column,/frame)
      lab=WIDGET_LABEL(ks_selmode_l_b,val="Left button:",font=titfont)
              ks_sml_b=WIDGET_BASE(ks_selmode_l_b,/col,sens=0,xpad=0)
              selection_mode[0].obj=CW_BGROUP(ks_sml_b,['Brt. Adjust','Zoom','Set Marker','Cross-section'],xpad=0, uval=selection_mode[0].uval, /col, /EXCLUSIVE,SET_VALUE=selection_mode[0].val,/NO_RELEASE)

      ks_selmode_r_b=WIDGET_BASE(ks_selmode_b,/column,/frame)
      lab=WIDGET_LABEL(ks_selmode_r_b,val="Right button:",font=titfont)
              ks_smr_b=WIDGET_BASE(ks_selmode_r_b,/col,sens=0,xpad=0)
              selection_mode[1].obj=CW_BGROUP(ks_smr_b,['Get int. profile','Select for fit', 'Mask', 'PV'],xpad=0, uval=selection_mode[1].uval, /col, /EXCLUSIVE,SET_VALUE=selection_mode[1].val,/NO_RELEASE)
      
      
      
      
      
      ;ks_break=WIDGET_BASE(ks_butbase,/column, ys=20)  ; 225
      ks_monitors_b=WIDGET_BASE(ks_butbase,/column,/frame,xpad=0,xoffset=0,ypad=0,yoffset=0)
      
      monitors=[{obj_par,'X (pix): ','',0,0,1},$
                      {obj_par,'Y (pix): ','',0,0,1},$
                      {obj_par,'RA (2000): ','',0,0,1},$
                      {obj_par,'DEC (2000): ','',0,0,1},$
                      {obj_par,'Val: ','',0,0,1}]
      output=lonarr(1)
      
      ks_mon_cols_b=WIDGET_BASE(ks_monitors_b,/row,xpad=0,xoffset=0,ypad=0,yoffset=0)
      ks_mon_cols_l_b=WIDGET_BASE(ks_mon_cols_b,/col,xpad=0,xoffset=0,ypad=0,yoffset=0)
               for i=0,1 do begin
                KS_Monitor_Cre,ks_mon_cols_l_b,monitors[i].name,output,/row,/center,xs=[48,sz[4].x]
                monitors[i].obj=output
               endfor
      ks_mon_cols_r_b=WIDGET_BASE(ks_mon_cols_b,/col,xpad=0,xoffset=0,ypad=0,yoffset=0)
               for i=2,3 do begin
                KS_Monitor_Cre,ks_mon_cols_r_b,monitors[i].name,output,/row,/center,xs=[65,sz[4].y]
                monitors[i].obj=output
               endfor
               
                KS_Monitor_Cre,ks_monitors_b,monitors[4].name,output,/row,/center,xs=[30,sz[4].y]
                monitors[4].obj=output
               
      
      
      ks_status_base=WIDGET_BASE(ks_butbase,/row,/frame)
      lab=WIDGET_LABEL(ks_status_base,val="State:  ", font=titfont)
      state_monitor=WIDGET_LABEL(ks_status_base,val="    Ready!               ", font=titfont)
      
      
      
      
      
      ; ========
      
      ks_database=WIDGET_BASE(ks_mb,/col)
      ks_file_info=lonarr(2)
      
      ks_file_info[0]=WIDGET_LABEL(ks_database, val="Data cube not loaded",font=titfont, xs=2*sz[5].x+2*sz[1].x);+5)
      ks_file_info[1]=WIDGET_LABEL(ks_database, val="Image not loaded",font=titfont, xs=2*sz[5].x+2*sz[1].x);+5)
      
      ;###### База для дисплеев
      ks_displays_base=WIDGET_BASE(ks_database,/row,xpad=0,ypad=0,xoffset=0,yoffset=0)
      ks_cube_disp={disp_par,0,'dispcub',0}
      
       ks_cube_disp_only_base=WIDGET_BASE(ks_displays_base,/col,xpad=0,ypad=0,xoffset=0,yoffset=0,scr_xsize=sz[1].x+sz[5].x,scr_ysize=sz[1].y+sz[5].y,/frame)
       ks_cube_disp.obj=WIDGET_DRAW(ks_cube_disp_only_base,uvalue=ks_cube_disp.uval, xsize=sz[1].x,ysize=sz[1].y,x_scroll_size=sz[1].x,$
                                              y_scroll_size=sz[1].y,/motion_event,/button_event,/app_scroll);,xoffset=0,yoffset=0)
      
       ks_res_disp={disp_par,0,'dispres',0}
       ks_base_res_disp_only=WIDGET_BASE(ks_displays_base,xsize=sz[1].x+sz[5].x,ysize=sz[1].y+sz[5].y,/frame)
       ks_res_disp.obj=WIDGET_DRAW(ks_base_res_disp_only,uvalue=ks_res_disp.uval, xsize=sz[1].x,ysize=sz[1].y,x_scroll_size=sz[1].x,$
                                              y_scroll_size=sz[1].y,/motion_event,/button_event,/app_scroll)
      
      
      
      xs_imblock=(sz[1].x+sz[5].x-2)/2
      
      
       ; === Панели настройки зума и всяких других штук
       ks_zoomcol_b=widget_base(ks_database,/row,xpad=0,ypad=0)
       ks_zccub_b=widget_base(ks_zoomcol_b,/row,scr_xsize=xs_imblock*2+3,xpad=0,ypad=0,xoffset=0,yoffset=0,/frame)
       field_zoom_cub=FSC_FIELD(ks_zccub_b,Title='Zoom factor:', value=zoom_factor[0].x,uvalue='zoom_cub',Event_Pro='KS_GO_Event',$
                                /CR_Only,xs=8,nonsens=1, object=field_zoom_cub_obj)
       output=lonarr(2)
       KS_Buttons_Cre,ks_zccub_b,buttons[7:8].name,buttons[7:8].uval,output,sens=buttons[7:8].sens,xs=70,ys=sz[2].y
       buttons[7:8].obj=output
       
       ; -- использовать дисплей для результатов для мониторинга профилей (если куб загружен)
       ks_tmp_b=WIDGET_BASE(ks_zccub_b,/row,/nonex)
       but_curprof_monitor=WIDGET_BUTTON(ks_tmp_b,val="Cur. Prof.", uvalue="RESDISP_AS_PROF_MONITOR")
       WIDGET_CONTROL,but_curprof_monitor,set_button=curprof_monitor
       WIDGET_CONTROL,but_curprof_monitor,sens=0
       
       ;save ps
       KS_Buttons_Cre,ks_zccub_b,buttons[11].name,buttons[11].uval,output,sens=buttons[11].sens,xs=70,ys=sz[2].y
       buttons[11].obj=output
       
       
       ks_zcres_b=widget_base(ks_zoomcol_b,/row,scr_xsize=xs_imblock*2+3,xpad=0,ypad=0,xoffset=0,yoffset=0,/frame)
       field_zoom_res=FSC_FIELD(ks_zcres_b,Title='Zoom factor:', value=zoom_factor[1].x,uvalue='zoom_res',Event_Pro='KS_GO_Event',$
                                /CR_Only,xs=8,nonsens=1, object=field_zoom_res_obj)
       output=lonarr(2)
       KS_Buttons_Cre,ks_zcres_b,buttons[9:10].name,buttons[9:10].uval,output,sens=buttons[9:10].sens,xs=70,ys=sz[2].y
       buttons[9:10].obj=output
       field_snr_mask=FSC_FIELD(ks_zcres_b,Title='Min S/N:', value=snr_mask,uvalue='snr_mask',Event_Pro='KS_GO_Event',$
                                /CR_Only,xs=5,nonsens=1, object=field_snr_mask_obj)
       
       ;save ps
       KS_Buttons_Cre,ks_zcres_b,buttons[12].name,buttons[12].uval,output,sens=buttons[12].sens,xs=70,ys=sz[2].y
       buttons[12].obj=output
       
       ;=== Панели настройки параметров отображения куба (изображения)
       ks_impar_b=widget_base(ks_database,/row,xpad=0,ypad=0)
       
      mode_view=[{obj_par,'','view_mode_cub',0,0,1},$
                 {obj_par,'','view_mode_res',0,0,1}]
      show_type=[{obj_par,'','show_type_cub',0,0,1},$
                 {obj_par,'','show_type_p1_res',0,-1,1},$
                 {obj_par,'','show_type_p2_res',0,-1,1}]
      

              ; ========= Настройки панели с кубом и изображением
       ks_mode_and_type_left_b=widget_base(ks_impar_b,/col,scr_xsize=xs_imblock,xpad=0,ypad=0,xoffset=0,yoffset=0)
       
         ;===Подгонять изображение под размер экрана, или показывать целиком?
            ks_cubdispmode_b=widget_base(ks_mode_and_type_left_b,/col,/frame)
              lab=WIDGET_LABEL(ks_cubdispmode_b,val="Display Mode:",font=titfont)
              ks_vm_cub_b=WIDGET_BASE(ks_cubdispmode_b,/row,sens=0)
              mode_view[0].obj=CW_BGROUP(ks_vm_cub_b,['Fit to Display','Real Size'], uval=mode_view[0].uval, /row, /EXCLUSIVE,SET_VALUE=mode_view[0].val,/NO_RELEASE)
         
         ;=== Что показывать? Куб интегральный, поканальные или изображение?  
            ks_cubshtype_b=widget_base(ks_mode_and_type_left_b,/col,/frame)
              lab=WIDGET_LABEL(ks_cubshtype_b,val="Show on Display:",font=titfont)
              ks_st_cub_b=WIDGET_BASE(ks_cubshtype_b,/row,sens=0,xpad=0)
              show_type[0].obj=CW_BGROUP(ks_st_cub_b,['Somo','Image','Chan.'],xpad=0, uval=show_type[0].uval, /row, /EXCLUSIVE,SET_VALUE=show_type[0].val,/NO_RELEASE)
              field_channels=FSC_FIELD(ks_st_cub_b,Title='', value=chan_string_cur,uvalue='show_channels',Event_Pro='KS_GO_Event',/CR_Only,xs=3,nonsens=1-cub_loaded, object=field_channels_obj)
      
        
       
       ks_brt_left_b=widget_base(ks_impar_b,/col,scr_xsize=xs_imblock,xpad=0,ypad=0,xoffset=0,yoffset=0,/frame)
        
        ; === Параметры настройки яркости
      
            but_auto_brt=lonarr(2)
            field_brtmin=lonarr(2)
            field_brtmax=lonarr(2)
            field_brtfrac=lonarr(2)
            field_brtmin_obj=objarr(2)
            field_brtmax_obj=objarr(2)
            field_brtfrac_obj=objarr(2)
            
            lab=WIDGET_LABEL(ks_brt_left_b,val="Brightness:",font=titfont)
                ks_tmp_b=WIDGET_BASE(ks_brt_left_b,/row,/frame)
                  ks_tmp1_b=WIDGET_BASE(ks_tmp_b,/nonex,/row)
                    but_auto_brt[0]=WIDGET_BUTTON(ks_tmp1_b,val="Auto", uvalue="BRT_CUB_AUSCL")
                    WIDGET_CONTROL,but_auto_brt[0],set_button=auto_br[0]
                    WIDGET_CONTROL,but_auto_brt[0],sens=0
                  field_brtfrac[0]=FSC_FIELD(ks_tmp_b,Title='Fraction (%):', value=brtfrac[0],uvalue='BRT_CUB_FRACT',Event_Pro='KS_GO_Event',/CR_Only,LabelSize=80,xs=6,nonsens=1, object=tmp_obj)
                  field_brtfrac_obj[0]=tmp_obj
                ks_tmp2_b=WIDGET_BASE(ks_brt_left_b,/row)
                  field_brtmin[0]=FSC_FIELD(ks_tmp2_b,Title='Min:', value=brtmin[0],uvalue='BRT_CUB_MIMAX',Event_Pro='KS_GO_Event',/CR_Only,LabelSize=25,xs=8,nonsens=1, object=tmp_obj)
                  field_brtmin_obj[0]=tmp_obj
                  field_brtmax[0]=FSC_FIELD(ks_tmp2_b,Title='Max:', value=brtmax[0],uvalue='BRT_CUB_MIMAX',Event_Pro='KS_GO_Event',/CR_Only,LabelSize=25,xs=8,nonsens=1, object=tmp_obj)
                  field_brtmax_obj[0]=tmp_obj
       
       
       
       
       ; ========= Настройки панели с результатами
       ks_mode_and_type_right_b=widget_base(ks_impar_b,/col,scr_xsize=xs_imblock,xpad=0,ypad=0,xoffset=0,yoffset=0)
      
          ;===Подгонять изображение под размер экрана, или показывать целиком?
          tmp_b=widget_base(ks_mode_and_type_right_b,/col,/frame)
            lab=WIDGET_LABEL(tmp_b,val="Display Mode:",font=titfont)
            ks_vm_res_b=WIDGET_BASE(tmp_b,/row,sens=0)
            mode_view[1].obj=CW_BGROUP(ks_vm_res_b,['Fit to Display','Real Size'], uval=mode_view[1].uval, /row, /EXCLUSIVE,SET_VALUE=mode_view[1].val,/NO_RELEASE, xs=sz[1].x/2-10)
         
         
         ;=== Что показывать? Интенсивности, скорости, fwhm (и для какой компоненты?)  
          tmp_b=widget_base(ks_mode_and_type_right_b,/col,/frame)
             lab=WIDGET_LABEL(tmp_b,val="Show on Display:",font=titfont)
             ks_st_res_b=WIDGET_BASE(tmp_b,/row,sens=0,xoffset=0,xpad=0)
             show_type[1].obj=WIDGET_COMBOBOX(ks_st_res_b,val=['None'], uval=show_type[1].uval,xoffset=0)
             lab=WIDGET_LABEL(ks_st_res_b,val=":",font=titfont,xs=4)
             show_type[2].obj=WIDGET_COMBOBOX(ks_st_res_b,val=['None'], uval=show_type[2].uval,xoffset=0)
       
       ks_brt_right_b=widget_base(ks_impar_b,/col,scr_xsize=xs_imblock,xpad=0,ypad=0,xoffset=0,yoffset=0,/frame)
      
          ; === Параметры настройки яркости
          
          lab=WIDGET_LABEL(ks_brt_right_b,val="Brightness:",font=titfont)
              ks_tmp_b=WIDGET_BASE(ks_brt_right_b,/row,/frame)
                ks_tmp1_b=WIDGET_BASE(ks_tmp_b,/nonex,/row)
                  but_auto_brt[1]=WIDGET_BUTTON(ks_tmp1_b,val="Auto", uvalue="BRT_RES_AUSCL")
                  WIDGET_CONTROL,but_auto_brt[1],set_button=auto_br[1]
                  WIDGET_CONTROL,but_auto_brt[1],sens=0
                field_brtfrac[1]=FSC_FIELD(ks_tmp_b,Title='Fraction (%):', value=brtfrac[4],uvalue='BRT_RES_FRACT',Event_Pro='KS_GO_Event',/CR_Only,LabelSize=90,xs=6,nonsens=1, object=tmp_obj)
                field_brtfrac_obj[1]=tmp_obj
              ks_tmp_b=WIDGET_BASE(ks_brt_right_b,/row)
                field_brtmin[1]=FSC_FIELD(ks_tmp_b,Title='Min:', value=brtmin[4],uvalue='BRT_RES_MIMAX',Event_Pro='KS_GO_Event',/CR_Only,LabelSize=25,xs=8,nonsens=1, object=tmp_obj)
                field_brtmin_obj[1]=tmp_obj
                field_brtmax[1]=FSC_FIELD(ks_tmp_b,Title='Max:', value=brtmax[4],uvalue='BRT_RES_MIMAX',Event_Pro='KS_GO_Event',/CR_Only,LabelSize=25,xs=8,nonsens=1, object=tmp_obj)
                field_brtmax_obj[1]=tmp_obj
  
                ; === Диапазон длин волн/скоростей (для профиля)
                field_xrange_obj=objarr(2)
                field_xrange=lonarr(2)
                lab=WIDGET_LABEL(ks_brt_right_b,val="Prof. X-range:",font=titfont)
                ks_tmp_b=WIDGET_BASE(ks_brt_right_b,/row,/frame)
                field_xrange[0]=FSC_FIELD(ks_tmp_b,Title='Min:', value=prof_xrange[0],uvalue='prof_display_xrange',Event_Pro='KS_GO_Event',/CR_Only,LabelSize=25,xs=8,nonsens=0, object=tmp_obj)
                field_xrange_obj[0]=tmp_obj
                field_xrange[1]=FSC_FIELD(ks_tmp_b,Title='Max:', value=prof_xrange[1],uvalue='prof_display_xrange',Event_Pro='KS_GO_Event',/CR_Only,LabelSize=25,xs=8,nonsens=0, object=tmp_obj)
                field_xrange_obj[1]=tmp_obj

  
  
  
  KS_INI_RES_STRUCT, 1,1,1
  cgCENTERTLB,ks_mb
  WIDGET_CONTROL, ks_mb, /realize,group_leader=ks_mb


  im_def = Rebin(Dist(sz[1].x/2), sz[1].x, sz[1].y)
  Widget_Control, ks_cube_disp.obj, Get_Value=wid
  wset,wid
  tv,im_def
  Widget_Control, ks_res_disp.obj, Get_Value=wid
  wset,wid
  tv,im_def

  XMANAGER,'KS_GO', ks_mb,no_block=1
  
END



PRO KINESCOPE
   
   KS_DEF
   RESOLVE_ROUTINE,"ks_fitting"
   RESOLVE_ROUTINE,"ks_res_manager"
   RESOLVE_ROUTINE,"ks_profile_manager"
   RESOLVE_ROUTINE,"ks_analysis_tuner"
   RESOLVE_ROUTINE,"ks_curprof_fit_manager"
   KS_GO
   ks_analysis_tuner
   ks_res_manager
   ks_profile_manager
   ks_curprof_fit_manager
   
END
