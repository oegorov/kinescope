PRO KS_CHANGE_PVTABLE_VAL,event
  COMMON KS_PV_DIAG_MANAGER
  if n_pv_table eq 0 then return
  case event.type OF
    8: pvtab_edit.state=0
    0: BEGIN
      pvtab_edit.state=1
      pvtab_edit.pv=event.y
    END
    4: BEGIN  ; Cell selection
      if pvtab_edit.state eq 1 then begin
        pvtab_edit.state=0
        KS_pvMAN_UPD_SELECTION,upd=pvtab_edit.pv
      endif
      if (event.SEL_LEFT eq -1 and event.SEL_TOP eq -1 and event.SEL_RIGHT eq -1 and event.SEL_BOTTOM eq -1 ) then pv_selected=-1 else begin
        IF (event.SEL_RIGHT ne (n_pv_options-1) or event.SEL_LEFT ne 0) then begin
          IF event.SEL_TOP ne event.SEL_BOTTOM then pv_selected=-1 else begin
              pv_selected=event.sel_top
          endelse
        endif
        IF (event.SEL_RIGHT eq n_pv_options-1 and event.SEL_LEFT eq 0) then pv_selected=indgen(abs(event.sel_top-event.sel_bottom)+1)+min([event.sel_bottom,event.sel_top])
      ENDELSE
      IF n_elements(pv_selected) eq 1 then begin
        if pv_selected ne -1 then begin
          WIDGET_CONTROL,ks_table_pv,set_table_select=[-1,pv_selected,n_pv_options-1,pv_selected]
          KS_PVMAN_TRIGGER_SENS_SHOWRES,1
        endif else KS_PVMAN_TRIGGER_SENS_SHOWRES,0
        KS_PVMAN_UPD_PARAMS
        KS_PVMAN_SHOW_PV
      ENDIF else begin
        KS_PVMAN_TRIGGER_SENS_SHOWRES,0
;        WIDGET_CONTROL,ks_table_profcomp,sens=0
;        WIDGET_CONTROL,ks_pvman_setcomp1,sens=0
;        WIDGET_CONTROL,ks_pvman_setcomp2,sens=0
;        WIDGET_CONTROL,ks_pvman_setcomp3,sens=0
        WIDGET_CONTROL,ks_pv_disp.obj, GET_VALUE = index
        WSet, index
        Erase, color=cgcolor('white')
      endelse
    END
    else:
  endcase
END



PRO KS_PV_manager_event, event
  COMMON KS_FLAGS_AND_PARAMS
  COMMON KS_RES_MANAGER_WIDGET
  COMMON KS_DATA
  COMMON KS_ANALYSIS
  COMMON KS_SIZES
  COMMON KS_PV_DIAG_MANAGER
  
  WIDGET_CONTROL,event.ID,get_uvalue=ev


END

PRO KS_PV_manager
  COMMON KS_WIDGET_ELEMENTS
  COMMON KS_FLAGS_AND_PARAMS
  COMMON KS_SIZES
  COMMON KS_PV_DIAG_MANAGER
    
  ks_pvman_b=WIDGET_BASE(TITLE="KINEScope: PV Manager",/row,GROUP_LEADER=ks_mb, map=0)

  if (!VERSION.OS_FAMILY eq "Windows") then titfont="Sans Serif" else titfont="-misc-fixed-bold-r-normal--13-120-75-75-c-70-iso8859-1"
  
  pvman_buttons=[{obj_par,'Load PV','load_pv',0,0,1},$
                  {obj_par,'Save PV','save_pv',0,0,1},$
                  {obj_par,'Close','close',0,0,1}]
  
  
  
  ;===== Селектор PV, параметров и дисплей

  
  ks_pvman_pvparams_b=WIDGET_BASE(ks_pvman_b,/col)
  ;Селектор
  pv_options=["Name","Color","Expand"]
  n_pv_options=n_elements(pv_options)
  pv_cwidth=replicate(75,n_pv_options)
  pv_options_def={name:'',color:'red', expand:0L}
  
  ks_pvman_allpv_b=WIDGET_BASE(ks_pvman_pvparams_b,/column,/frame)
      lab=WIDGET_LABEL(ks_pvman_allpv_b, VAL="All PV diagrams:", font=titfont)
      ks_tmp_b=WIDGET_BASE(ks_pvman_allpv_b,/col,/frame)
          ks_table_pv=WIDGET_TABLE(ks_tmp_b,/edit,ROW_LABELS=string(indgen(5)+1,format="(I2)"),column_width=prof_cwidth, $
                                  COLUMN_LABELS=pv_options,value=replicate(pv_options_def,5),/all_event,EVENT_PRO="KS_CHANGE_PVTABLE_VAL")
          ks_tmp0_b=WIDGET_BASE(ks_tmp_b,/row)
          but=WIDGET_BUTTON(ks_tmp0_b,val="Errase All",uval="prof_err",xs=sz[2].x,ys=sz[2].y)
          but=WIDGET_BUTTON(ks_tmp0_b,val="Delete Cur.",uval="prof_del",xs=sz[2].x,ys=sz[2].y)
          tb=WIDGET_BASE(ks_tmp0_b,xs=20)
          KS_Buttons_Cre,ks_tmp0_b, pvman_buttons[0:1].name,pvman_buttons[0:1].uval,output,sens=pvman_buttons[0:1].sens,xs=sz[2].x,ys=sz[2].y;
          pvman_buttons[0:1].obj=output
          WIDGET_CONTROL,ks_table_pv,/delete_rows,use_table_select=[-1,0,n_pv_options-1,4]
    
  
  

  
  ;Дисплей
  ks_pv_disp={disp_par,0,'disppv',0}
  
  ks_pvman_out_and_but_b=WIDGET_BASE(ks_profman_b,/col)
  ks_profman_out_b=WIDGET_BASE(ks_profman_out_and_but_b,/row)
  
  ks_prof_disp_base=WIDGET_BASE(ks_profman_out_b,/col)
  
       ks_prof_disp.obj=WIDGET_DRAW(ks_prof_disp_base,uvalue=ks_prof_disp.uval, xsize=sz[3].x,ysize=sz[3].y,$
                                              /motion_event,/button_event,/frame)
  
  
  ks_prof_disp_setup_and_mon_b=WIDGET_BASE(ks_prof_disp_base,/row,/frame,ypad=0,yoffset=0)
  
  ks_prof_disp_mon_b=WIDGET_BASE(ks_prof_disp_setup_and_mon_b,/col,ypad=40,yoffset=0)
  output=lonarr(2)
  KS_Monitor_Cre,ks_prof_disp_mon_b,ks_profman_monitors[0:1].name,output,/col,xs=[40,sz[4].y]
      ks_profman_monitors[0:1].obj=output
  
  
  ks_prof_disp_setup_b=WIDGET_BASE(ks_prof_disp_setup_and_mon_b,/col,ypad=0,yoffset=0)
  
  lab=WIDGET_LABEL(ks_prof_disp_setup_b,val="Show on display:",font=titfont)
  
  ks_prof_showres=[{obj_par,'Sum of selected','show_fit_res_a',0,0,0},$
                   {obj_par,'Continuum','show_fit_res_c',0,0,0},$
                   {obj_par,'Residual','show_fit_res_r',0,0,0},$
                   {obj_par,'1-st component','show_fit_res_1',0,0,0},$
                   {obj_par,'2-nd component','show_fit_res_2',0,0,0},$
                   {obj_par,'3-rd component','show_fit_res_3',0,0,0}]
  
  tmp=WIDGET_BASE(ks_prof_disp_setup_b,/row)
  tmp0=WIDGET_BASE(tmp,/col,/frame)
  i0=0
  KS_Buttons_Cre,tmp0,ks_prof_showres[i0:i0+2].name,ks_prof_showres[i0:i0+2].uval,output,sens=ks_prof_showres[i0:i0+2].sens,/nonexclusive,/frame
  ks_prof_showres[i0:i0+2].obj=output 
  
  tmp0=WIDGET_BASE(tmp,/col,/frame)
  i0=3
  KS_Buttons_Cre,tmp0,ks_prof_showres[i0:i0+2].name,ks_prof_showres[i0:i0+2].uval,output,sens=ks_prof_showres[i0:i0+2].sens,/nonexclusive,/frame
  ks_prof_showres[i0:i0+2].obj=output 
  
  
  
  
  ; Результаты фиттинга
  ks_prof_res_base=WIDGET_BASE(ks_profman_out_b,/row,xoffset=0,xpad=0)
  ks_prof_res_fullbase=WIDGET_BASE(ks_prof_res_base,/col,xoffset=0,xpad=0,/align_center);,xsize=sz[3].x)
  lab=WIDGET_LABEL(ks_prof_res_fullbase,val="Fitting results:",font=titfont,/align_cent)
  
  
  
  ks_prof_resbase=WIDGET_BASE(ks_prof_res_fullbase,/col,xpad=0,xoffset=0)
  
  tit=["=== Total ===","=== Comp1 ===","=== Comp2 ===","=== Comp3 ==="]
  FOR bb=0,3 do begin
    tmp=WIDGET_BASE(ks_prof_resbase,/col,/frame)
    lab=WIDGET_LABEL(tmp,val=tit[bb])
    i0=bb*4+2
    output=lonarr(1)
    for i=i0,i0+3 do begin
      KS_Monitor_Cre,tmp,ks_profman_monitors[i].name,output,/row,xs=[80,sz[4].y+10];,ys=[10,10]
      ks_profman_monitors[i].obj=output
    endfor
  ENDFOR
  
  
  
  
  
  ks_profman_butbase=WIDGET_BASE(ks_profman_out_and_but_b,/row, /frame)
  
  but=WIDGET_BUTTON(ks_profman_butbase,val="Save PS",uval="save_ps",xs=sz[2].x,ys=sz[2].y)
  tmp=WIDGET_BASE(ks_profman_butbase,xs=20)
  KS_Buttons_Cre,ks_profman_butbase, profman_buttons[2:4].name,profman_buttons[2:4].uval,output,sens=profman_buttons[2:4].sens,xs=sz[2].x,ys=sz[2].y,break_arr=[1]
  profman_buttons[2:4].obj=output
  
    
    
  cgCENTERTLB,ks_profman_b
  WIDGET_CONTROL, ks_profman_b, /realize,group_leader=ks_mb
  Widget_Control, ks_prof_disp.obj, Get_Value=wid
  WSet, wid
  Erase, color=cgcolor('white')
  XMANAGER,'KS_PROFILE_MANAGER', ks_profman_b,no_block=1
  

END