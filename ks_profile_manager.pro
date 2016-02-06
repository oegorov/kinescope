PRO KS_PROFMAN_SHOWCOMPS_ADJUST, index
  COMMON KS_PROF_MANAGER
  IF total(*prof_storage[index].fitted_isset,/nan) eq 0 then begin
    *prof_storage[index].show_on=intarr(6)
    return
  ENDIF
  (*prof_storage[index].show_on)[0]=1
  (*prof_storage[index].show_on)[1]=0
  (*prof_storage[index].show_on)[2]=1
  IF (*prof_storage[index].fitted_isset)[0] eq 1 then (*prof_storage[index].show_on)[3]=1 else (*prof_storage[index].show_on)[3]=0
  IF (*prof_storage[index].fitted_isset)[1] eq 1 then (*prof_storage[index].show_on)[4]=1 else (*prof_storage[index].show_on)[4]=0
  IF (*prof_storage[index].fitted_isset)[2] eq 1 then (*prof_storage[index].show_on)[5]=1 else (*prof_storage[index].show_on)[5]=0 
END

PRO KS_PROFMAN_TRIGGER_SENS_SHOWRES, state
  COMMON KS_PROF_MANAGER
  for i=0,5 do WIDGET_CONTROL,ks_prof_showres[i].obj,sens=state
END

PRO KS_PROFMAN_SAVE_PRF
  ; Save selected profiles into fits and txt with res. Also log-file will be saved
  COMMON KS_PROF_MANAGER
  COMMON KS_DATA
  
  if prof_selected[0] eq -1 then return 
      win_title="Choose TXT-file to save line profile"
      nprf=n_elements(prof_selected)
      if nprf gt 1 then win_title=win_title+"s"
      logfile=DIALOG_PICKFILE(title=win_title,default_extension='txt',filter="*txt",/write)
          if (logfile eq '') then return
          fdecomp,logfile,disk,logfile_dir,logfile_name,qual
          if (logfile_name eq '') then return
          cd,disk+logfile_dir
          fitsfile_dir=slash(disk+logfile_dir+logfile_name)
      
      
      ; Writing log_file and fits
      first=1
      indexes=prof_selected+1
      names=prof_storage[indexes].name
      colors=prof_storage[indexes].color
      types=string(profile_selection[indexes].type,format="(I1)")
      isfit=strarr(nprf)
      for i=0,nprf-1 do isfit[i]= string(0 > total(*prof_storage[indexes[i]].fitted_isset,/nan) < 1,format="(I1)")
      
      
      nz=sxpar(header_cub,"NAXIS3")
      refpix=sxpar(header_cub,"CRPIX3")
      if refpix eq 0 then refpix=1
      mkhdr,prf_head,fltarr(nz)
      sxaddpar,prf_head,"BUNIT",sxpar(header_cub,"BUNIT"),after="NAXIS1"
      sxaddpar,prf_head,"CTYPE1",sxpar(header_cub,"CTYPE3"),after="NAXIS1"
      sxaddpar,prf_head,"CRPIX1",sxpar(header_cub,"CRPIX3"),after="NAXIS1"
      sxaddpar,prf_head,"CDELT1",sxpar(header_cub,"CDELT3"),after="NAXIS1"
      sxaddpar,prf_head,"CRVAL1",sxpar(header_cub,"CRVAL3"),after="NAXIS1"
      sxaddhist,"KINESCOPE: Profile saved from data cube "+filenames[0],prf_head
      sxaddhist,"KINESCOPE: See "+logfile+" for description",prf_head
      
      
      prf_fit=fltarr(nz,6)
      mkhdr,prf_head_fit,prf_fit
      sxaddpar,prf_head_fit,"BUNIT",sxpar(header_cub,"BUNIT"),after="NAXIS2"
      sxaddpar,prf_head_fit,"CRPIX2",1,after="NAXIS2"
      sxaddpar,prf_head_fit,"CDELT2",1,after="NAXIS2"
      sxaddpar,prf_head_fit,"CRVAL2",0,after="NAXIS2"
      sxaddpar,prf_head_fit,"CTYPE1",sxpar(header_cub,"CTYPE3"),after="NAXIS2"
      sxaddpar,prf_head_fit,"CRPIX1",sxpar(header_cub,"CRPIX3"),after="NAXIS2"
      sxaddpar,prf_head_fit,"CDELT1",sxpar(header_cub,"CDELT3"),after="NAXIS2"
      sxaddpar,prf_head_fit,"CRVAL1",sxpar(header_cub,"CRVAL3"),after="NAXIS2"
      sxaddhist,"KINESCOPE: FIT RESULTS of profile saved from data cube "+filenames[0],prf_head_fit
      sxaddhist,"KINESCOPE: See "+logfile+" for description",prf_head_fit
      
      FOR i=0, nprf - 1 do begin
        ok=0
        if first then begin  
          ff=file_info(fitsfile_dir)
          if not ff.exists then file_mkdir,fitsfile_dir
            openw,u,logfile,/get_lun
            printf,u,"SIMPLE  =                            T / KINESCOPE INT.PROFILES PARAMETERS"
            printf,u,"CUBE    = '"+filenames[0]+"'  / Data cube"
            printf,u,"FITS_DIR= '"+fitsfile_dir+"'  / DIR containing results"  
            printf,u,"ROOTNAME= '"+logfile_name+"'  / ROOT of the NAME of ALL profiles (ROOTNAME_PROFNAME1, ROOTNAME_PROFNAME2, ...)"
            printf,u,"PROFNAME= '"+strjoin(names,",")+"' / NAME of EACH profile"
            printf,u,"COLORS  = '"+strjoin(colors,",")+"' / COLOR of EACH profile to be shown in the image"
            printf,u,"SH_TYPES= '"+strjoin(types,",")+"' / SHAPE TYPE of EACH profile"
            printf,u,"FITTED  = '"+strjoin(isfit,",")+"' / IS THIS PROFILE FITTED?"
            first=0
          endif
          printf,u,"PTS_"+string(i,format="(I03)")+" = '"+strjoin(string(*profile_selection[indexes[i]].points,format="(F0.7)"),",")+"' / POINTS of THIS SHAPE"
          
      ENDFOR
      if first eq 0 then begin
        printf,u,"END"
        close,u
        free_lun,u
      endif
      
      FOR i=0,nprf-1 do begin
          writefits, fitsfile_dir+logfile_name+"_"+names[i]+".fits",*prof_storage[indexes[i]].y,prf_head
          if isfit[i] eq '1' then begin
            ;Saving fit results to fits
              prf_fit[*,0]=total((*prof_storage[indexes[i]].fitted_comps),1,/nan)+prof_storage[indexes[i]].fitted_cont
              prf_fit[*,1]=(*prof_storage[indexes[i]].fitted_comps)[0,*]
              prf_fit[*,2]=(*prof_storage[indexes[i]].fitted_comps)[1,*]
              prf_fit[*,3]=(*prof_storage[indexes[i]].fitted_comps)[2,*]
              prf_fit[*,4]=replicate(prof_storage[indexes[i]].fitted_cont,nz)
              prf_fit[*,5]=(*prof_storage[indexes[i]].fitted_resid)
            writefits, fitsfile_dir+logfile_name+"_"+names[i]+"_fitres.fits",prf_fit,prf_head_fit
          endif
      ENDFOR
      
      rec=where(isfit eq '1', nr)
      if nr eq 0 then return
      
      ;Saving fit results to txt
       openw,u,fitsfile_dir+logfile_name+"_fitres.txt",/get_lun

       printf,u,"Profiles fitting result (see "+logfile+" for description)"
       printf,u,"##############################################################################################################################################"
       printf,u,"Prof#  Name   S/N    FLUX_TOT     CENT1   SIGMA1   AMPL1    FLUX1    CENT2     SIGMA2     AMPL2     FLUX2    CENT3   SIGMA3   AMPL3   FLUX3    CONT   Resid/Noise Disp_subtracted"
       
       frm="(G0.4)"
       FOR i=0,nr-1 do begin
          flux=total(*prof_storage[indexes[rec[i]]].fitted_flux,/nan)
          txtstr=string(i+1,format="(I0)")+"      "+names[rec[i]]+"    "+string(prof_storage[indexes[rec[i]]].snr, format="(F0.2)")+"   "+string(flux,format=frm)+"  "+$
              string((*prof_storage[indexes[rec[i]]].fitted_cent)[0],format=frm)+"  "+string((*prof_storage[indexes[rec[i]]].fitted_fwhm)[0]/2.35482,format=frm)+$
              "  "+string((*prof_storage[indexes[rec[i]]].fitted_ampl)[0],format=frm)+"  "+string((*prof_storage[indexes[rec[i]]].fitted_flux)[0],format=frm)+"  "+$
              string((*prof_storage[indexes[rec[i]]].fitted_cent)[1],format=frm)+"  "+string((*prof_storage[indexes[rec[i]]].fitted_fwhm)[1]/2.35482,format=frm)+$
              "  "+string((*prof_storage[indexes[rec[i]]].fitted_ampl)[1],format=frm)+"  "+string((*prof_storage[indexes[rec[i]]].fitted_flux)[1],format=frm)+"  "+$
              string((*prof_storage[indexes[rec[i]]].fitted_cent)[2],format=frm)+"  "+string((*prof_storage[indexes[rec[i]]].fitted_fwhm)[2]/2.35482,format=frm)+$
              "  "+string((*prof_storage[indexes[rec[i]]].fitted_ampl)[2],format=frm)+"  "+string((*prof_storage[indexes[rec[i]]].fitted_flux)[2],format=frm)+"  "+$
              string(prof_storage[indexes[rec[i]]].fitted_cont,format=frm)+"   "+$
              string(total(abs(*prof_storage[indexes[rec[i]]].fitted_resid),/nan)/flux*prof_storage[indexes[rec[i]]].snr,format="(F0.2)")+"    "+ $
              string(extern_disp.vel,format="(F0.2)")
          printf,u,txtstr 
       ENDFOR
       close,u
       free_lun,u
END


PRO KS_PROFMAN_LOAD_PRF
  ; Load profiles from previously saved log-file.
  COMMON KS_PROF_MANAGER
  win_title="Choose TXT-file with log to load line profiles"
      logfile=DIALOG_PICKFILE(title=win_title,default_extension='txt',filter="*txt",/write)
      inf=FILE_INFO(logfile)
      IF NOT (inf.exists) OR (logfile EQ '')  THEN return
      fdecomp,logfile,disk,logfile_dir,logfile_name,qual
      if (logfile_name eq '') then return
      cd,disk+logfile_dir
      

     ;Reading LOG
     par=ks_read_par(logfile)
     fitsdir=ks_sxpar(par,"FITS_DIR")
     names=ks_sxpar(par,"PROFNAME")
     rootname=ks_sxpar(par,"ROOTNAME")
     cubname=ks_sxpar(par,"CUBE")
     colors=ks_sxpar(par,"COLORS")
     types=fix(ks_sxpar(par,"SH_TYPES"))
     isfitted=fix(ks_sxpar(par,"FITTED"))
     
     ;WHAT??? WHY NAMES == rootname???? Check ks_sxpar later
     nprf=n_elements(names)
     print,names,colors,fitsdir

END



PRO KS_PROFMAN_FIT_RUN,indexes=indexes
  ; Running fitting of integral profiles
  COMMON KS_PROF_MANAGER
  COMMON KS_DATA
  COMMON KS_ANALYSIS
   
  
  nfit=n_elements(indexes)
  
  FOR i=0,nfit-1 do begin
   xscale=*prof_storage[indexes[i]].x
   prof=*prof_storage[indexes[i]].y
   
   params={comps: *prof_storage[indexes[i]].comps,ampl: *prof_storage[indexes[i]].ampl,cent: *prof_storage[indexes[i]].cent, fwhm: *prof_storage[indexes[i]].fwhm,$
           setmax_ampl: *prof_storage[indexes[i]].setmax_ampl,setmax_cent: *prof_storage[indexes[i]].setmax_cent, setmax_fwhm: *prof_storage[indexes[i]].setmax_fwhm,$
           setmin_ampl: *prof_storage[indexes[i]].setmin_ampl,setmin_cent: *prof_storage[indexes[i]].setmin_cent, setmin_fwhm: *prof_storage[indexes[i]].setmin_fwhm,$
           fixampl: *prof_storage[indexes[i]].fixampl,fixcent: *prof_storage[indexes[i]].fixcent, fixfwhm: *prof_storage[indexes[i]].fixfwhm,$
           max_ampl: *prof_storage[indexes[i]].max_ampl,max_cent: *prof_storage[indexes[i]].max_cent, max_fwhm: *prof_storage[indexes[i]].max_fwhm,$
           min_ampl: *prof_storage[indexes[i]].min_ampl,min_cent: *prof_storage[indexes[i]].min_cent, min_fwhm: *prof_storage[indexes[i]].min_fwhm,$
           cont: prof_storage[indexes[i]].cont,setmax_cont: prof_storage[indexes[i]].setmax_cont,setmin_cont: prof_storage[indexes[i]].setmin_cont,$
           max_cont: prof_storage[indexes[i]].max_cont,min_cont: prof_storage[indexes[i]].min_cont,fixcont: prof_storage[indexes[i]].fixcont} 
   KS_FITTING, xscale, prof, contin=contin, out_models=out_models,manual_params=params, $
              out_maps=out_maps, inst_vel=inst_fwhm.vel, prof_type=fit_proftype, method=ks_profman_method
   
   *prof_storage[indexes[i]].fitted_ampl=[out_maps.i1,out_maps.i2,out_maps.i3]
   *prof_storage[indexes[i]].fitted_cent=[out_maps.v1,out_maps.v2,out_maps.v3]
   *prof_storage[indexes[i]].fitted_fwhm=sqrt([(out_maps.sigma1^2-extern_disp.vel^2) > 0,(out_maps.sigma2^2-extern_disp.vel^2) > 0,(out_maps.sigma3^2-extern_disp.vel^2) > 0])*2.35482
   *prof_storage[indexes[i]].fitted_flux=[out_maps.f1,out_maps.f2,out_maps.f3]
   *prof_storage[indexes[i]].fitted_isset=[out_maps.is_set1,out_maps.is_set2,out_maps.is_set3]
    prof_storage[indexes[i]].fitted_cont=out_maps.contin
   *prof_storage[indexes[i]].fitted_resid=out_models.resid
   (*prof_storage[indexes[i]].fitted_comps)[0,*]=out_models.c1
   (*prof_storage[indexes[i]].fitted_comps)[1,*]=out_models.c2
   (*prof_storage[indexes[i]].fitted_comps)[2,*]=out_models.c3
   KS_PROFMAN_SHOWCOMPS_ADJUST, indexes[i]
  ENDFOR
  KS_PROFMAN_SHOW_PROF
END


PRO KS_PROFMAN_READ_COMPSVAL
  ; Считываем значения параметров фиттинга компонент
  COMMON KS_PROF_MANAGER
  WIDGET_CONTROL, ks_profman_comp1_a, get_value=p0
  WIDGET_CONTROL, ks_profman_comp2_a, get_value=p1
  WIDGET_CONTROL, ks_profman_comp3_a, get_value=p2
  *prof_storage[prof_selected+1].ampl=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_profman_comp1_f, get_value=p0
  WIDGET_CONTROL, ks_profman_comp2_f, get_value=p1
  WIDGET_CONTROL, ks_profman_comp3_f, get_value=p2
  *prof_storage[prof_selected+1].fwhm=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_profman_comp1_c, get_value=p0
  WIDGET_CONTROL, ks_profman_comp2_c, get_value=p1
  WIDGET_CONTROL, ks_profman_comp3_c, get_value=p2
  *prof_storage[prof_selected+1].cent=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_profman_comp1_amin, get_value=p0
  WIDGET_CONTROL, ks_profman_comp2_amin, get_value=p1
  WIDGET_CONTROL, ks_profman_comp3_amin, get_value=p2
  *prof_storage[prof_selected+1].min_ampl=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_profman_comp1_fmin, get_value=p0
  WIDGET_CONTROL, ks_profman_comp2_fmin, get_value=p1
  WIDGET_CONTROL, ks_profman_comp3_fmin, get_value=p2
  *prof_storage[prof_selected+1].min_fwhm=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_profman_comp1_cmin, get_value=p0
  WIDGET_CONTROL, ks_profman_comp2_cmin, get_value=p1
  WIDGET_CONTROL, ks_profman_comp3_cmin, get_value=p2
  *prof_storage[prof_selected+1].min_cent=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_profman_comp1_amax, get_value=p0
  WIDGET_CONTROL, ks_profman_comp2_amax, get_value=p1
  WIDGET_CONTROL, ks_profman_comp3_amax, get_value=p2
  *prof_storage[prof_selected+1].max_ampl=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_profman_comp1_fmax, get_value=p0
  WIDGET_CONTROL, ks_profman_comp2_fmax, get_value=p1
  WIDGET_CONTROL, ks_profman_comp3_fmax, get_value=p2
  *prof_storage[prof_selected+1].max_fwhm=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_profman_comp1_cmax, get_value=p0
  WIDGET_CONTROL, ks_profman_comp2_cmax, get_value=p1
  WIDGET_CONTROL, ks_profman_comp3_cmax, get_value=p2
  *prof_storage[prof_selected+1].max_cent=[p0,p1,p2]
  
  WIDGET_CONTROL, ks_profman_cont, get_value=p0
  WIDGET_CONTROL, ks_profman_contmin, get_value=p1
  WIDGET_CONTROL, ks_profman_contmax, get_value=p2
  prof_storage[prof_selected+1].cont=p0
  prof_storage[prof_selected+1].min_cont=p1
  prof_storage[prof_selected+1].max_cont=p2
END

PRO KS_PROFMAN_SAVE_PS
        ; Сохраняет профиль в eps-файл
        COMMON KS_PROF_MANAGER
        COMMON KS_DATA
        if prof_selected[0] eq -1 then return 
        win_title="Choose EPS-file to save line profile"
        nprf=n_elements(prof_selected)
        if nprf gt 1 then win_title=win_title+"s"
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
          
          
          
          xlay=1
          ylay=1
          if nprf ge 4 and nprf le 6 then xlay=2
          if nprf ge 7 then xlay=3
          if nprf eq 2 or nprf eq 4 then ylay=2
          if nprf eq 3 or (nprf ge 5 and nprf le 9) then ylay=3
          if nprf ge 10 then ylay=4
          
          npp=12
          npages=fix(nprf)/npp+1
          FOR page=0, npages-1 do begin
            if page gt 0 then ERASE
            
            if nprf ge (page+1)*npp then nprf_onpage=npp else nprf_onpage=nprf-page*npp
            
            FOR i=page*npp,page*npp+nprf_onpage-1 do BEGIN
              cgplot,*prof_storage[prof_selected[i]+1].x,*prof_storage[prof_selected[i]+1].y, $
              title=prof_storage[prof_selected[i]+1].name,$
               ytit = "Intensity, "+ axes_info.I, xtit = xtit, xr=*prof_storage[prof_selected[i]+1].xr, yr=*prof_storage[prof_selected[i]+1].yr,$
               xst=1,yst=1, psym=10,layout=[xlay,ylay,i+1-page*npp],charsize=1.2
             
              if total(*prof_storage[prof_selected[i]+1].fitted_isset,/nan) gt 0 then fit_done=1 else fit_done=0 
      
              IF fit_done then begin
                 if finite(prof_storage[prof_selected[i]+1].fitted_cont) then this_contin=prof_storage[prof_selected[i]+1].fitted_cont else this_contin=0
                 if (*prof_storage[prof_selected[i]+1].fitted_isset)[0] eq 1 and (*prof_storage[prof_selected[i]+1].show_on)[3] eq 1 then cgoplot,*prof_storage[prof_selected[i]+1].x,$
                    ((*prof_storage[prof_selected[i]+1].fitted_comps)[0,*]+this_contin),color="green"
                 if (*prof_storage[prof_selected[i]+1].fitted_isset)[1] eq 1 and (*prof_storage[prof_selected[i]+1].show_on)[4] eq 1 then cgoplot,*prof_storage[prof_selected[i]+1].x,$
                    ((*prof_storage[prof_selected[i]+1].fitted_comps)[1,*]+this_contin),color="blue"
                 if (*prof_storage[prof_selected[i]+1].fitted_isset)[2] eq 1 and (*prof_storage[prof_selected[i]+1].show_on)[5] eq 1 then cgoplot,*prof_storage[prof_selected[i]+1].x,$
                    ((*prof_storage[prof_selected[i]+1].fitted_comps)[2,*]+this_contin),color="red"
                 if (*prof_storage[prof_selected[i]+1].show_on)[2] eq 1 then cgoplot,*prof_storage[prof_selected[i]+1].x,(*prof_storage[prof_selected[i]+1].fitted_resid+this_contin),color="yellow"
                 
                 nx=n_elements(*prof_storage[prof_selected[i]+1].x)
                 cnt=replicate(this_contin,nx)
                 if (*prof_storage[prof_selected[i]+1].show_on)[1] eq 1 then  cgoplot,*prof_storage[prof_selected[i]+1].x,cnt,color="brown"
                 if (*prof_storage[prof_selected[i]+1].show_on)[0] eq 1 then  begin
                   tot=cnt
                   if (*prof_storage[prof_selected[i]+1].show_on)[3] eq 1 and (*prof_storage[prof_selected[i]+1].fitted_isset)[0] eq 1 then tot+= (*prof_storage[prof_selected[i]+1].fitted_comps)[0,*]
                   if (*prof_storage[prof_selected[i]+1].show_on)[4] eq 1 and (*prof_storage[prof_selected[i]+1].fitted_isset)[1] eq 1 then tot+= (*prof_storage[prof_selected[i]+1].fitted_comps)[1,*]
                   if (*prof_storage[prof_selected[i]+1].show_on)[5] eq 1 and (*prof_storage[prof_selected[i]+1].fitted_isset)[2] eq 1 then tot+= (*prof_storage[prof_selected[i]+1].fitted_comps)[2,*]
                    cgoplot,*prof_storage[prof_selected[i]+1].x,tot,color="magenta"
                 endif
              ENDIF
           
            ENDFOR
          ENDFOR 
        ps_end
    
  END  


PRO KS_FREE_PROF_POINTERS, indexes=indexes
  ;Удаляем не нужные более указатили в хранилище профилей
  COMMON KS_PROF_MANAGER
  if n_elements(indexes) eq 0 then return
  for i=0,n_elements(indexes)-1 do ptr_free,profile_selection[indexes[i]].points
  for i=0,n_elements(indexes)-1 do ptr_free,prof_storage[indexes[i]].xr,prof_storage[indexes[i]].yr,prof_storage[indexes[i]].x,prof_storage[indexes[i]].y,$
               prof_storage[indexes[i]].x, prof_storage[indexes[i]].cent, prof_storage[indexes[i]].fwhm, prof_storage[indexes[i]].ampl, $
               prof_storage[indexes[i]].max_cent,prof_storage[indexes[i]].max_fwhm,prof_storage[indexes[i]].max_ampl,prof_storage[indexes[i]].comps,$
               prof_storage[indexes[i]].min_cent, prof_storage[indexes[i]].min_fwhm, prof_storage[indexes[i]].min_ampl,$
               prof_storage[indexes[i]].setmax_cent,prof_storage[indexes[i]].setmax_fwhm,prof_storage[indexes[i]].setmax_ampl,$
               prof_storage[indexes[i]].setmin_cent, prof_storage[indexes[i]].setmin_fwhm, prof_storage[indexes[i]].setmin_ampl,$
               prof_storage[indexes[i]].fixcent, prof_storage[indexes[i]].fixfwhm, prof_storage[indexes[i]].fixampl,$
               prof_storage[indexes[i]].link_cent, prof_storage[indexes[i]].link_fwhm, prof_storage[indexes[i]].link_ampl,prof_storage[indexes[i]].fitted_comps,$
               prof_storage[indexes[i]].fitted_isset,prof_storage[indexes[i]].fitted_cent, prof_storage[indexes[i]].fitted_fwhm,$
               prof_storage[indexes[i]].fitted_ampl,prof_storage[indexes[i]].fitted_flux,prof_storage[indexes[i]].fitted_resid,prof_storage[indexes[i]].show_on
END

PRO KS_PROFMAN_UPD_COMPS
  COMMON KS_PROF_MANAGER
  ;Обновляем таблицу с начальными параметрами компонент фиттинга под текущий профиль
  ; НЕ ЗАБЫВАТЬ О ТОМ, ЧТО prof_storage[0] СОДЕРЖИТ ДЕФОЛТНОЕ ЗНАЧЕНИЕ, А НЕ РЕАЛЬНЫЙ ПРОФИЛЬ!!!
  ; СООТВЕТСТВЕННО, РЕАЛЬНОМУ ПРОФИЛЮ СООТВЕТСТВУЕТ prof_selected+1
  WIDGET_CONTROL,ks_table_profcomp,sens=(1-ks_profman_method)*((1+prof_selected[0]) < 1)
  WIDGET_CONTROL,ks_profman_setcomp1,sens=(1-ks_profman_method)*((1+prof_selected[0]) < 1)
  WIDGET_CONTROL,ks_profman_setcomp2,sens=(1-ks_profman_method)*((1+prof_selected[0]) < 1)
  WIDGET_CONTROL,ks_profman_setcomp3,sens=(1-ks_profman_method)*((1+prof_selected[0]) < 1)
  IF prof_selected gt -1 then begin
    WIDGET_CONTROL,ks_profman_setcomp1,set_button=0 > (*prof_storage[prof_selected+1].comps)[0] < 1
    WIDGET_CONTROL,ks_profman_setcomp2,set_button=0 > (*prof_storage[prof_selected+1].comps)[1] < 1
    WIDGET_CONTROL,ks_profman_setcomp3,set_button=0 > (*prof_storage[prof_selected+1].comps)[2] < 1
    
    WIDGET_CONTROL,ks_profman_comp1_asetmax,set_button=0 > (*prof_storage[prof_selected+1].setmax_ampl)[0] < 1
    WIDGET_CONTROL,ks_profman_comp2_asetmax,set_button=0 > (*prof_storage[prof_selected+1].setmax_ampl)[1] < 1
    WIDGET_CONTROL,ks_profman_comp3_asetmax,set_button=0 > (*prof_storage[prof_selected+1].setmax_ampl)[2] < 1
    WIDGET_CONTROL,ks_profman_comp1_asetmin,set_button=0 > (*prof_storage[prof_selected+1].setmin_ampl)[0] < 1
    WIDGET_CONTROL,ks_profman_comp2_asetmin,set_button=0 > (*prof_storage[prof_selected+1].setmin_ampl)[1] < 1
    WIDGET_CONTROL,ks_profman_comp3_asetmin,set_button=0 > (*prof_storage[prof_selected+1].setmin_ampl)[2] < 1
    WIDGET_CONTROL,ks_profman_comp1_afix,set_button=0 > (*prof_storage[prof_selected+1].fixampl)[0] < 1
    WIDGET_CONTROL,ks_profman_comp2_afix,set_button=0 > (*prof_storage[prof_selected+1].fixampl)[1] < 1
    WIDGET_CONTROL,ks_profman_comp3_afix,set_button=0 > (*prof_storage[prof_selected+1].fixampl)[2] < 1
    WIDGET_CONTROL,ks_profman_comp1_fsetmax,set_button=0 > (*prof_storage[prof_selected+1].setmax_fwhm)[0] < 1
    WIDGET_CONTROL,ks_profman_comp2_fsetmax,set_button=0 > (*prof_storage[prof_selected+1].setmax_fwhm)[1] < 1
    WIDGET_CONTROL,ks_profman_comp3_fsetmax,set_button=0 > (*prof_storage[prof_selected+1].setmax_fwhm)[2] < 1
    WIDGET_CONTROL,ks_profman_comp1_fsetmin,set_button=0 > (*prof_storage[prof_selected+1].setmin_fwhm)[0] < 1
    WIDGET_CONTROL,ks_profman_comp2_fsetmin,set_button=0 > (*prof_storage[prof_selected+1].setmin_fwhm)[1] < 1
    WIDGET_CONTROL,ks_profman_comp3_fsetmin,set_button=0 > (*prof_storage[prof_selected+1].setmin_fwhm)[2] < 1
    WIDGET_CONTROL,ks_profman_comp1_ffix,set_button=0 > (*prof_storage[prof_selected+1].fixfwhm)[0] < 1
    WIDGET_CONTROL,ks_profman_comp2_ffix,set_button=0 > (*prof_storage[prof_selected+1].fixfwhm)[1] < 1
    WIDGET_CONTROL,ks_profman_comp3_ffix,set_button=0 > (*prof_storage[prof_selected+1].fixfwhm)[2] < 1
    WIDGET_CONTROL,ks_profman_comp1_csetmax,set_button=0 > (*prof_storage[prof_selected+1].setmax_cent)[0] < 1
    WIDGET_CONTROL,ks_profman_comp2_csetmax,set_button=0 > (*prof_storage[prof_selected+1].setmax_cent)[1] < 1
    WIDGET_CONTROL,ks_profman_comp3_csetmax,set_button=0 > (*prof_storage[prof_selected+1].setmax_cent)[2] < 1
    WIDGET_CONTROL,ks_profman_comp1_csetmin,set_button=0 > (*prof_storage[prof_selected+1].setmin_cent)[0] < 1
    WIDGET_CONTROL,ks_profman_comp2_csetmin,set_button=0 > (*prof_storage[prof_selected+1].setmin_cent)[1] < 1
    WIDGET_CONTROL,ks_profman_comp3_csetmin,set_button=0 > (*prof_storage[prof_selected+1].setmin_cent)[2] < 1
    WIDGET_CONTROL,ks_profman_comp1_cfix,set_button=0 > (*prof_storage[prof_selected+1].fixcent)[0] < 1
    WIDGET_CONTROL,ks_profman_comp2_cfix,set_button=0 > (*prof_storage[prof_selected+1].fixcent)[1] < 1
    WIDGET_CONTROL,ks_profman_comp3_cfix,set_button=0 > (*prof_storage[prof_selected+1].fixcent)[2] < 1
    
    WIDGET_CONTROL,ks_profman_comp1_a,set_value=string((*prof_storage[prof_selected+1].ampl)[0],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp2_a,set_value=string((*prof_storage[prof_selected+1].ampl)[1],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp3_a,set_value=string((*prof_storage[prof_selected+1].ampl)[2],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp1_f,set_value=string((*prof_storage[prof_selected+1].fwhm)[0],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp2_f,set_value=string((*prof_storage[prof_selected+1].fwhm)[1],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp3_f,set_value=string((*prof_storage[prof_selected+1].fwhm)[2],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp1_c,set_value=string((*prof_storage[prof_selected+1].cent)[0],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp2_c,set_value=string((*prof_storage[prof_selected+1].cent)[1],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp3_c,set_value=string((*prof_storage[prof_selected+1].cent)[2],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp1_amin,set_value=string((*prof_storage[prof_selected+1].min_ampl)[0],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp2_amin,set_value=string((*prof_storage[prof_selected+1].min_ampl)[1],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp3_amin,set_value=string((*prof_storage[prof_selected+1].min_ampl)[2],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp1_fmin,set_value=string((*prof_storage[prof_selected+1].min_fwhm)[0],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp2_fmin,set_value=string((*prof_storage[prof_selected+1].min_fwhm)[1],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp3_fmin,set_value=string((*prof_storage[prof_selected+1].min_fwhm)[2],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp1_cmin,set_value=string((*prof_storage[prof_selected+1].min_cent)[0],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp2_cmin,set_value=string((*prof_storage[prof_selected+1].min_cent)[1],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp3_cmin,set_value=string((*prof_storage[prof_selected+1].min_cent)[2],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp1_amax,set_value=string((*prof_storage[prof_selected+1].max_ampl)[0],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp2_amax,set_value=string((*prof_storage[prof_selected+1].max_ampl)[1],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp3_amax,set_value=string((*prof_storage[prof_selected+1].max_ampl)[2],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp1_fmax,set_value=string((*prof_storage[prof_selected+1].max_fwhm)[0],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp2_fmax,set_value=string((*prof_storage[prof_selected+1].max_fwhm)[1],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp3_fmax,set_value=string((*prof_storage[prof_selected+1].max_fwhm)[2],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp1_cmax,set_value=string((*prof_storage[prof_selected+1].max_cent)[0],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp2_cmax,set_value=string((*prof_storage[prof_selected+1].max_cent)[1],format="(G0.4)")
    WIDGET_CONTROL,ks_profman_comp3_cmax,set_value=string((*prof_storage[prof_selected+1].max_cent)[2],format="(G0.4)")
    
    WIDGET_CONTROL,ks_profman_cont,set_value=string((prof_storage[prof_selected+1].cont),format="(G0.4)")
    WIDGET_CONTROL,ks_profman_contmin,set_value=string((prof_storage[prof_selected+1].min_cont),format="(G0.4)")
    WIDGET_CONTROL,ks_profman_contmax,set_value=string((prof_storage[prof_selected+1].max_cont),format="(G0.4)")
    
    WIDGET_CONTROL,ks_profman_contsetmin,set_button=0 > (prof_storage[prof_selected+1].setmin_cont) < 1
    WIDGET_CONTROL,ks_profman_contsetmax,set_button=0 > (prof_storage[prof_selected+1].setmax_cont) < 1
    WIDGET_CONTROL,ks_profman_contfix,set_button=0 > (prof_storage[prof_selected+1].fixcont) < 1
  ENDIF
END

PRO KS_PROFMAN_SHOW_PROF
  ;Рисуем выбранный профиль на дисплее
  COMMON KS_PROF_MANAGER
  COMMON KS_DATA
    WIDGET_CONTROL,ks_prof_disp.obj, GET_VALUE = index
    WSet, index
  IF prof_selected[0] eq -1 then Erase, color=cgcolor('white') ELSE BEGIN
   
    if axes_info.ztype eq -1 then xtit = "X (unrecognized units)"
    if axes_info.ztype eq 0 or axes_info.ztype eq 1 then xtit = "Velocity, km s$\up-1$"
    if axes_info.ztype ge 2 and axes_info.ztype le 4 then xtit = "Wavelength, "+axes_info.zunit[axes_info.ztype+1]
    if axes_info.ztype ge 5 then xtit = "Frequency, "+axes_info.zunit[axes_info.ztype+1]
    
    cgplot,*prof_storage[prof_selected+1].x,*prof_storage[prof_selected+1].y,$
           ytit = "Intensity, "+ axes_info.I, xtit = xtit, xr=*prof_storage[prof_selected+1].xr, yr=*prof_storage[prof_selected+1].yr,$
           xst=1,yst=1, background="white",psym=10,title=prof_storage[prof_selected+1].name
           ks_profman_pos_on_disp=[!x.window,!y.window]
    if total(*prof_storage[prof_selected+1].fitted_isset,/nan) gt 0 then fit_done=1 else fit_done=0 
    
    
    WIDGET_CONTROL,ks_profman_monitors[2].obj,set_value=string(prof_storage[prof_selected+1].snr,format="(F0.1)")
    format="(F0.2)"
    IF fit_done then begin
       KS_PROFMAN_TRIGGER_SENS_SHOWRES,1
       if finite(prof_storage[prof_selected+1].fitted_cont) then begin
        WIDGET_CONTROL,ks_prof_showres[1].obj,set_but=(*prof_storage[prof_selected+1].show_on)[1]
        ks_prof_showres[1].val=(*prof_storage[prof_selected+1].show_on)[1]
        this_contin=prof_storage[prof_selected+1].fitted_cont
       endif else begin
        this_contin=0
        WIDGET_CONTROL,ks_prof_showres[1].obj,set_but=0
        WIDGET_CONTROL,ks_prof_showres[1].obj,sens=0
        ks_prof_showres[1].val=0
       endelse
       
       if total(*prof_storage[prof_selected+1].fitted_isset,/nan) ge 1 then begin
       
         if (*prof_storage[prof_selected+1].fitted_isset)[0] eq 1 then begin
           WIDGET_CONTROL,ks_prof_showres[3].obj,set_but=(*prof_storage[prof_selected+1].show_on)[3]
           ks_prof_showres[3].val=(*prof_storage[prof_selected+1].show_on)[3]
           if ks_prof_showres[3].val then cgoplot,*prof_storage[prof_selected+1].x,((*prof_storage[prof_selected+1].fitted_comps)[0,*]+this_contin),color="green"
         endif else begin
           WIDGET_CONTROL,ks_prof_showres[3].obj,set_but=0
           ks_prof_showres[3].val=0
           WIDGET_CONTROL,ks_prof_showres[3].obj,sens=0
         endelse
         
         if (*prof_storage[prof_selected+1].fitted_isset)[1] eq 1 then begin
            WIDGET_CONTROL,ks_prof_showres[4].obj,set_but=(*prof_storage[prof_selected+1].show_on)[4]
           ks_prof_showres[4].val=(*prof_storage[prof_selected+1].show_on)[4]
           if ks_prof_showres[4].val then cgoplot,*prof_storage[prof_selected+1].x,((*prof_storage[prof_selected+1].fitted_comps)[1,*]+this_contin),color="blue"
         endif else begin
           WIDGET_CONTROL,ks_prof_showres[4].obj,set_but=0
           ks_prof_showres[4].val=0
           WIDGET_CONTROL,ks_prof_showres[4].obj,sens=0
         endelse
         
         if (*prof_storage[prof_selected+1].fitted_isset)[2] eq 1 then begin
           WIDGET_CONTROL,ks_prof_showres[5].obj,set_but=(*prof_storage[prof_selected+1].show_on)[5]
           ks_prof_showres[5].val=(*prof_storage[prof_selected+1].show_on)[5]
           if ks_prof_showres[5].val then cgoplot,*prof_storage[prof_selected+1].x,((*prof_storage[prof_selected+1].fitted_comps)[2,*]+this_contin),color="red"
         endif else begin
           WIDGET_CONTROL,ks_prof_showres[5].obj,set_but=0
           ks_prof_showres[5].val=0
           WIDGET_CONTROL,ks_prof_showres[5].obj,sens=0
         endelse
         
       endif
       nx=n_elements(*prof_storage[prof_selected+1].x)
       if ks_prof_showres[1].val eq 1 then cgoplot,*prof_storage[prof_selected+1].x,replicate(this_contin,nx),color="brown"
       
        WIDGET_CONTROL,ks_prof_showres[2].obj,set_but=(*prof_storage[prof_selected+1].show_on)[2]
        ks_prof_showres[2].val=(*prof_storage[prof_selected+1].show_on)[2]
        WIDGET_CONTROL,ks_prof_showres[0].obj,set_but=(*prof_storage[prof_selected+1].show_on)[0]
        ks_prof_showres[0].val=(*prof_storage[prof_selected+1].show_on)[0]
        
        
        if ks_prof_showres[2].val eq 1 then cgoplot,*prof_storage[prof_selected+1].x,(*prof_storage[prof_selected+1].fitted_resid+this_contin),color="gold"
        if ks_prof_showres[0].val eq 1 then begin
          tot=replicate(this_contin,nx)
          
          if ks_prof_showres[3].val eq 1 then tot+=(*prof_storage[prof_selected+1].fitted_comps)[0,*]
          if ks_prof_showres[4].val eq 1 then tot+=(*prof_storage[prof_selected+1].fitted_comps)[1,*]
          if ks_prof_showres[5].val eq 1 then tot+=(*prof_storage[prof_selected+1].fitted_comps)[2,*]
          cgoplot,*prof_storage[prof_selected+1].x,tot,color="magenta"
       endif
       
       ; Отображение результатов фиттинга (текстовых)
       flux=total(*prof_storage[prof_selected+1].fitted_flux,/nan)
       resid=total(abs(*prof_storage[prof_selected+1].fitted_resid),/nan)/flux*prof_storage[prof_selected+1].snr
       WIDGET_CONTROL,ks_profman_monitors[3].obj,set_value=string(flux,format=format)
       WIDGET_CONTROL,ks_profman_monitors[4].obj,set_value=string(prof_storage[prof_selected+1].fitted_cont,format=format)
       WIDGET_CONTROL,ks_profman_monitors[5].obj,set_value=string(resid,format=format)
       if (*prof_storage[prof_selected+1].fitted_isset)[0] eq 1 then begin
         WIDGET_CONTROL,ks_profman_monitors[6].obj,set_value=string((*prof_storage[prof_selected+1].fitted_cent)[0],format=format)
         WIDGET_CONTROL,ks_profman_monitors[7].obj,set_value=string((*prof_storage[prof_selected+1].fitted_fwhm)[0]/2.35482,format=format)+$
         " ("+string((*prof_storage[prof_selected+1].fitted_fwhm)[0],format=format)+")"
         WIDGET_CONTROL,ks_profman_monitors[8].obj,set_value=string((*prof_storage[prof_selected+1].fitted_ampl)[0],format=format)
         WIDGET_CONTROL,ks_profman_monitors[9].obj,set_value=string((*prof_storage[prof_selected+1].fitted_flux)[0],format=format)
       endif else begin
        for i=0,3 do WIDGET_CONTROL,ks_profman_monitors[6+i].obj,set_value="none"
       endelse
       if (*prof_storage[prof_selected+1].fitted_isset)[1] eq 1 then begin
         WIDGET_CONTROL,ks_profman_monitors[10].obj,set_value=string((*prof_storage[prof_selected+1].fitted_cent)[1],format=format)
         WIDGET_CONTROL,ks_profman_monitors[11].obj,set_value=string((*prof_storage[prof_selected+1].fitted_fwhm)[1]/2.35482,format=format)+$
         " ("+string((*prof_storage[prof_selected+1].fitted_fwhm)[1],format=format)+")"
         WIDGET_CONTROL,ks_profman_monitors[12].obj,set_value=string((*prof_storage[prof_selected+1].fitted_ampl)[1],format=format)
         WIDGET_CONTROL,ks_profman_monitors[13].obj,set_value=string((*prof_storage[prof_selected+1].fitted_flux)[1],format=format)
       endif else begin
        for i=0,3 do WIDGET_CONTROL,ks_profman_monitors[10+i].obj,set_value="none"
       endelse
       if (*prof_storage[prof_selected+1].fitted_isset)[2] eq 1 then begin
         WIDGET_CONTROL,ks_profman_monitors[14].obj,set_value=string((*prof_storage[prof_selected+1].fitted_cent)[2],format=format)
         WIDGET_CONTROL,ks_profman_monitors[15].obj,set_value=string((*prof_storage[prof_selected+1].fitted_fwhm)[2]/2.35482,format=format)+$
         " ("+string((*prof_storage[prof_selected+1].fitted_fwhm)[2],format=format)+")"
         WIDGET_CONTROL,ks_profman_monitors[16].obj,set_value=string((*prof_storage[prof_selected+1].fitted_ampl)[2],format=format)
         WIDGET_CONTROL,ks_profman_monitors[17].obj,set_value=string((*prof_storage[prof_selected+1].fitted_flux)[2],format=format)
       endif else begin
        for i=0,3 do WIDGET_CONTROL,ks_profman_monitors[14+i].obj,set_value="none"
       endelse
    ENDIF ELSE BEGIN
      KS_PROFMAN_TRIGGER_SENS_SHOWRES,0
      WIDGET_CONTROL,ks_profman_monitors[3].obj,set_value="none"
      for i=0,12 do WIDGET_CONTROL,ks_profman_monitors[5+i].obj,set_value="none"
      WIDGET_CONTROL,ks_profman_monitors[4].obj,set_value=string(prof_storage[prof_selected+1].cont,format=format)
    ENDELSE
  ENDELSE
END


PRO KS_CHANGE_TABLE_VAL,event
  COMMON KS_PROF_MANAGER
  if n_prof_table eq 0 then return
  case event.type OF
    8: proftab_edit.state=0
    0: BEGIN
      proftab_edit.state=1
      proftab_edit.prof=event.y
    END
    4: BEGIN  ; Cell selection
      if proftab_edit.state eq 1 then begin
        proftab_edit.state=0
        KS_PROFMAN_UPD_SELECTION,upd=proftab_edit.prof
      endif
      if (event.SEL_LEFT eq -1 and event.SEL_TOP eq -1 and event.SEL_RIGHT eq -1 and event.SEL_BOTTOM eq -1 ) then prof_selected=-1 else begin
        IF (event.SEL_RIGHT ne (n_prof_options-1) or event.SEL_LEFT ne 0) then begin
          IF event.SEL_TOP ne event.SEL_BOTTOM then prof_selected=-1 else begin
              prof_selected=event.sel_top
          endelse
        endif
        IF (event.SEL_RIGHT eq n_prof_options-1 and event.SEL_LEFT eq 0) then prof_selected=indgen(abs(event.sel_top-event.sel_bottom)+1)+min([event.sel_bottom,event.sel_top])
      ENDELSE
      IF n_elements(prof_selected) eq 1 then begin
        if prof_selected ne -1 then begin
          WIDGET_CONTROL,ks_table_prof,set_table_select=[-1,prof_selected,n_prof_options-1,prof_selected]
          KS_PROFMAN_TRIGGER_SENS_SHOWRES,1
        endif else KS_PROFMAN_TRIGGER_SENS_SHOWRES,0
        KS_PROFMAN_UPD_COMPS
        KS_PROFMAN_SHOW_PROF
      ENDIF else begin
        KS_PROFMAN_TRIGGER_SENS_SHOWRES,0
        WIDGET_CONTROL,ks_table_profcomp,sens=0
        WIDGET_CONTROL,ks_profman_setcomp1,sens=0
        WIDGET_CONTROL,ks_profman_setcomp2,sens=0
        WIDGET_CONTROL,ks_profman_setcomp3,sens=0
        WIDGET_CONTROL,ks_prof_disp.obj, GET_VALUE = index
        WSet, index
        Erase, color=cgcolor('white')
      endelse
    END
    else:
  endcase
END


PRO KS_PROFMAN_UPD_SELECTION,del=del, upd=upd
   ; Обновляем выделение профиля на изображении в соответствии с изменением в таблице
   COMMON KS_PROF_MANAGER
   COMMON KS_DISPLAY
   if n_elements(upd) gt 0 then begin
    ; Обновляем выбранные
    WIDGET_CONTROL,ks_table_prof,get_value=table
    bad=0
    for i=0,n_elements(upd)-1 do begin
      if upd[i] ge n_prof_table then continue
      prof_storage[upd[i]+1].name=table[upd[i]].name
      prof_storage[upd[i]+1].color=KS_CHECK_COLOR(table[upd[i]].color, default='red')
      (*prof_storage[upd[i]+1].xr)[0]=table[upd[i]].v0
      (*prof_storage[upd[i]+1].xr)[1]=table[upd[i]].v1
      (*prof_storage[upd[i]+1].yr)[0]=table[upd[i]].I0
      (*prof_storage[upd[i]+1].yr)[1]=table[upd[i]].I1
      prof_storage[upd[i]+1].amp_norm=0 < fix(table[upd[i]].norm) <1
      profile_selection[upd[i]+1].color=prof_storage[upd[i]+1].color
      profile_selection[upd[i]+1].name=prof_storage[upd[i]+1].name
      if table[upd[i]].color ne prof_storage[upd[i]+1].color or prof_storage[upd[i]+1].amp_norm ne table[upd[i]].norm then bad=1
    endfor
    KS_SHOW_IMAGE,mode="cube"
    if curprof_monitor eq 0 then KS_SHOW_IMAGE,mode="result"
    if bad then KS_PROFMAN_UPD_TABLE, /noselect
   endif
   if n_elements(del) gt 0 then begin
     ;Удаляем выбранные
     mask=intarr(n_prof_table+1)
     for i=0,n_elements(del)-1 do begin
      if del[i] lt n_prof_table then mask[del[i]+1]=1
     endfor
     rec=where(mask eq 0,nr)
     rec1=where(mask eq 1,nr1)
     if nr1 gt 0 then KS_FREE_PROF_POINTERS, indexes=rec1
     
     if nr gt 0 then profile_selection= profile_selection[rec] else profile_selection=profile_selection[0]
     if nr gt 0 then prof_storage=prof_storage[rec] else prof_storage=prof_storage[0]
     n_prof_table=nr-1
     if n_prof_table gt 0 then begin
       prof_selected= min(del) < (n_prof_table-1)
       WIDGET_CONTROL,ks_table_prof,set_table_select=[-1,prof_selected,n_prof_options-1,prof_selected]
     endif else begin
      prof_selected=-1
      KS_PROFMAN_TRIGGER_SENS_SHOWRES,0
     endelse
     KS_SHOW_IMAGE,mode="cube"
     if curprof_monitor eq 0 then KS_SHOW_IMAGE,mode="result"
     KS_PROFMAN_UPD_COMPS
     KS_PROFMAN_SHOW_PROF
   endif
END




PRO KS_PROFMAN_UPD_TABLE, noselect=noselect
  COMMON KS_PROF_MANAGER
  nprof=n_elements(prof_storage)-1
  if nprof gt 0 then begin
    if n_prof_table gt 0 then WIDGET_CONTROL,ks_table_prof,/delete_rows,use_table_select=[0,0,n_prof_options-1,n_prof_table-1]
    new_table=replicate({name:'',color:'', V0:0E,V1:0E,I0:0E,I1:0E, norm:0L},nprof)
    
    for i=0,nprof-1 do begin
      new_table[i].name=prof_storage[i+1].name
      new_table[i].color=prof_storage[i+1].color
      new_table[i].v0=(*prof_storage[i+1].xr)[0]
      new_table[i].v1=(*prof_storage[i+1].xr)[1]
      new_table[i].i0=(*prof_storage[i+1].yr)[0]
      new_table[i].i1=(*prof_storage[i+1].yr)[1]
      new_table[i].norm=prof_storage[i+1].amp_norm
      WIDGET_CONTROL,ks_table_prof,/Insert_rows
    endfor
    n_prof_table=nprof
    WIDGET_CONTROL,ks_table_prof,row_labels=[string(indgen(nprof)+1,format="(I0)")]
    WIDGET_CONTROL,ks_table_prof,set_value=new_table
    if not keyword_set(noselect) then begin prof_selected=n_prof_table-1
      WIDGET_CONTROL,ks_table_prof,set_table_select=[-1,prof_selected,n_prof_options-1,prof_selected]
    endif
    KS_PROFMAN_TRIGGER_SENS_SHOWRES, 1
  endif else KS_PROFMAN_TRIGGER_SENS_SHOWRES, 0
  
  KS_PROFMAN_UPD_COMPS
  KS_PROFMAN_SHOW_PROF
END

pro KS_PROFILE_MANAGER_EVENT, event
  COMMON KS_FLAGS_AND_PARAMS
  COMMON KS_RES_MANAGER_WIDGET
  COMMON KS_DATA
  COMMON KS_ANALYSIS
  COMMON KS_SIZES
  COMMON KS_PROF_MANAGER
  
  
  WIDGET_CONTROL,event.ID,get_uvalue=ev
  
 
  IF strpos(STRLOWCASE(ev),"show_fit_res_") ne -1 then begin
     mode=strmid(STRUPCASE(ev),13,1)
    CASE mode of
     '1': ks_prof_showres[3].val=1-ks_prof_showres[3].val
     '2': ks_prof_showres[4].val=1-ks_prof_showres[4].val
     '3': ks_prof_showres[5].val=1-ks_prof_showres[5].val
     'A': ks_prof_showres[0].val=1-ks_prof_showres[0].val
     'C': ks_prof_showres[1].val=1-ks_prof_showres[1].val
     'R': ks_prof_showres[2].val=1-ks_prof_showres[2].val
     ELSE:
    ENDCASE
    if n_elements(prof_selected) eq 1 and prof_selected[0] ne -1 then begin
      for i=0,5 do (*prof_storage[prof_selected+1].show_on)[i]=ks_prof_showres[i].val
      KS_PROFMAN_SHOW_PROF
    endif
   return 
  ENDIF
 
 
  IF strpos(STRUPCASE(ev),"PROF_COM_SET_") ne -1 then begin
   if prof_selected[0] eq -1 or n_elements(prof_selected) gt 1 or n_prof_table le 0 then return
   mode=strmid(STRUPCASE(ev),13,4)
   comp=(strmid(STRUPCASE(ev),17,1))
   if comp ne 'E' then comp=fix(comp) else comp=0
   CASE mode of
   
   'VALU': KS_PROFMAN_READ_COMPSVAL
   
   'COMP': (*prof_storage[prof_selected+1].comps)[comp]=1-(*prof_storage[prof_selected+1].comps)[comp]
   
   'CFIX': (*prof_storage[prof_selected+1].fixcent)[comp]=1-(*prof_storage[prof_selected+1].fixcent)[comp]
   
   'CSMI': (*prof_storage[prof_selected+1].setmin_cent)[comp]=1-(*prof_storage[prof_selected+1].setmin_cent)[comp]
   
   'CSMA': (*prof_storage[prof_selected+1].setmax_cent)[comp]=1-(*prof_storage[prof_selected+1].setmax_cent)[comp]
   
   'FFIX': (*prof_storage[prof_selected+1].fixfwhm)[comp]=1-(*prof_storage[prof_selected+1].fixfwhm)[comp]
   
   'FSMI': (*prof_storage[prof_selected+1].setmin_fwhm)[comp]=1-(*prof_storage[prof_selected+1].setmin_fwhm)[comp]
   
   'FSMA': (*prof_storage[prof_selected+1].setmax_fwhm)[comp]=1-(*prof_storage[prof_selected+1].setmax_fwhm)[comp]
   
   'AFIX': (*prof_storage[prof_selected+1].fixampl)[comp]=1-(*prof_storage[prof_selected+1].fixampl)[comp]
   
   'ASMI': (*prof_storage[prof_selected+1].setmin_ampl)[comp]=1-(*prof_storage[prof_selected+1].setmin_ampl)[comp]
   
   'ASMA': (*prof_storage[prof_selected+1].setmax_ampl)[comp]=1-(*prof_storage[prof_selected+1].setmax_ampl)[comp]
   
   'CTFX': prof_storage[prof_selected+1].fixcont=1-prof_storage[prof_selected+1].fixcont
   
   'CTMI': prof_storage[prof_selected+1].setmin_cont=1-prof_storage[prof_selected+1].setmin_cont
   
   'CTMA': prof_storage[prof_selected+1].setmax_cont=1-prof_storage[prof_selected+1].setmax_cont
    ELSE:
    
    ENDCASE
   return 
  ENDIF
 
 
  Case ev OF
  
  'fit_cur': BEGIN
    if prof_selected[0] ne -1 and n_prof_table gt 0 then KS_PROFMAN_FIT_RUN, indexes=prof_selected+1
  END
  
  'fit_all': BEGIN
    if n_prof_table gt 0 then KS_PROFMAN_FIT_RUN, indexes=indgen(n_prof_table)+1
  END
  
  'prof_err':BEGIN
    if n_prof_table eq 0 then return 
    WIDGET_CONTROL,ks_table_prof,/delete_rows,use_table_select=[-1,0,n_prof_options-1,n_prof_table-1]
    KS_PROFMAN_UPD_SELECTION,del=indgen(n_prof_table)
  END
  
  'prof_del':BEGIN
    if n_prof_table eq 0 or prof_selected[0] eq -1 then return 
    WIDGET_CONTROL,ks_table_prof,/delete_rows,/use_table_select
    KS_PROFMAN_UPD_SELECTION, del=prof_selected
    if n_prof_table gt 0 then WIDGET_CONTROL,ks_table_prof,row_labels=[string(indgen(n_prof_table)+1,format="(I0)")]
  END
  
  'close': BEGIN
    WIDGET_CONTROL,ks_profman_b, map=0
  END
  
  'save_ps': KS_PROFMAN_SAVE_PS
  
  'save_prof': KS_PROFMAN_SAVE_PRF
  
  'load_prof': KS_PROFMAN_LOAD_PRF
  
  'set_fit_method': BEGIN
    WIDGET_CONTROL, ks_profman_method_but,get_val=tmp
    ks_profman_method=tmp
    WIDGET_CONTROL, ks_profman_setcomp1,sens=(1-ks_profman_method)*((1+prof_selected) < 1)
    WIDGET_CONTROL, ks_profman_setcomp2,sens=(1-ks_profman_method)*((1+prof_selected) < 1)
    WIDGET_CONTROL, ks_profman_setcomp3,sens=(1-ks_profman_method)*((1+prof_selected) < 1)
    WIDGET_CONTROL, ks_table_profcomp,sens=(1-ks_profman_method)*((1+prof_selected) < 1)
  END
  
  
  
  'dispprof': BEGIN
    ; === События, генерируемые дисплеем профилей
    IF prof_selected[0] ne -1 and n_elements(prof_selected) eq 1 THEN BEGIN
       Xcur=event.x
       Ycur=event.y
       IF (event.TYPE LT 3) THEN BEGIN
          ; --- Мониторинг профиля
          
          start_pix=[ks_profman_pos_on_disp[0]*sz[3].x,ks_profman_pos_on_disp[2]*sz[3].y]
          fin_pix=[ks_profman_pos_on_disp[1]*sz[3].x,ks_profman_pos_on_disp[3]*sz[3].y]
          xs=(fin_pix[0]-start_pix[0])
          ys=(fin_pix[1]-start_pix[1])
          
          IF xcur lt start_pix[0] or xcur gt fin_pix[0] or ycur lt start_pix[1] or ycur gt fin_pix[1] then begin
            xcur_data_show="none"
            ycur_data_show="none"
          ENDIF ELSE BEGIN
            xcur_data=float(xcur-start_pix[0])/xs*((*prof_storage[prof_selected+1].xr)[1]-(*prof_storage[prof_selected+1].xr)[0])+(*prof_storage[prof_selected+1].xr)[0]
            ycur_data=float(ycur-start_pix[1])/ys*((*prof_storage[prof_selected+1].yr)[1]-(*prof_storage[prof_selected+1].yr)[0])+(*prof_storage[prof_selected+1].yr)[0]
            xcur_data_show=string(xcur_data,format="(F0.2)")
            ycur_data_show=string(ycur_data,format="(F0.2)")            
          ENDELSE
          WIDGET_CONTROL,ks_profman_monitors[0].obj,set_value=xcur_data_show
          WIDGET_CONTROL,ks_profman_monitors[1].obj,set_value=ycur_data_show
                
       ENDIF
    ENDIF
  END
  
  
  
  
  
  ELSE: BEGIN
  
    print,"Not ready yet!"
  
  ENDELSE
  
  ENDCASE

END



PRO KS_PROFILE_MANAGER
  COMMON KS_WIDGET_ELEMENTS
  COMMON KS_FLAGS_AND_PARAMS
  COMMON KS_SIZES
  COMMON KS_PROF_MANAGER
  
  ks_profman_b=WIDGET_BASE(TITLE="KINEScope: Profiles Manager",/row,GROUP_LEADER=ks_mb, map=0)

  if (!VERSION.OS_FAMILY eq "Windows") then titfont="Sans Serif" else titfont="-misc-fixed-bold-r-normal--13-120-75-75-c-70-iso8859-1"
  
  profman_buttons=[{obj_par,'Load Profs.','load_prof',0,0,1},$
                  {obj_par,'Save Profs.','save_prof',0,0,1},$
                  {obj_par,'Fit Current','fit_cur',0,0,1},$
                  {obj_par,'Fit All','fit_all',0,0,1},$
                  {obj_par,'Close','close',0,0,1}]
  
  
  
  ;===== Селектор профилей, параметров фиттинга и дисплей
;  ks_profman_selbase=WIDGET_BASE(ks_profman_b,/row)
  
  
  ks_profman_profandcomp_b=WIDGET_BASE(ks_profman_b,/col)
  ;Селектор
  prof_options=["Name","Color","Vmin","Vmax","Imin","Imax", "Norm."]
  n_prof_options=n_elements(prof_options)
  prof_cwidth=replicate(75,n_prof_options)
  prof_options_def={name:'',color:'red', V0:0E,V1:0E,I0:0E,I1:0E, norm:0L }
  
  ks_profman_allprofs_b=WIDGET_BASE(ks_profman_profandcomp_b,/column,/frame)
      lab=WIDGET_LABEL(ks_profman_allprofs_b, VAL="All Profiles:", font=titfont)
      ks_tmp_b=WIDGET_BASE(ks_profman_allprofs_b,/col,/frame)
          ks_table_prof=WIDGET_TABLE(ks_tmp_b,/edit,ROW_LABELS=string(indgen(5)+1,format="(I2)"),column_width=prof_cwidth, $
                                  COLUMN_LABELS=prof_options,value=replicate(prof_options_def,5),/all_event,EVENT_PRO="KS_CHANGE_TABLE_VAL")
          ks_tmp0_b=WIDGET_BASE(ks_tmp_b,/row)
          but=WIDGET_BUTTON(ks_tmp0_b,val="Errase All",uval="prof_err",xs=sz[2].x,ys=sz[2].y)
          but=WIDGET_BUTTON(ks_tmp0_b,val="Delete Cur.",uval="prof_del",xs=sz[2].x,ys=sz[2].y)
          tb=WIDGET_BASE(ks_tmp0_b,xs=20)
          KS_Buttons_Cre,ks_tmp0_b, profman_buttons[0:1].name,profman_buttons[0:1].uval,output,sens=profman_buttons[0:1].sens,xs=sz[2].x,ys=sz[2].y;
          profman_buttons[0:1].obj=output
          WIDGET_CONTROL,ks_table_prof,/delete_rows,use_table_select=[-1,0,n_prof_options-1,4]
    
  
  
  
  
  ; Настройки фиттинга
    
  
  
    ks_profman_comps_mb=WIDGET_BASE(ks_profman_profandcomp_b,/col,/frame)
    tmpb=widget_base(ks_profman_comps_mb,/row)
    lab_prof_indicator=WIDGET_LABEL(tmpb,val='Prof. components parameters:',xs=300, font=titfont)
    
    prof_storage=ks_new_profile(nprof=1)
    
    ks_profman_method_b=WIDGET_BASE(tmpb,/row)
    lab=WIDGET_LABEL(ks_profman_method_b,val="Fitting Method:",xs=90)
    ks_profman_method_but=CW_BGROUP(ks_profman_method_b,["MPFIT", "GENFIT"],/frame, uval='set_fit_method', /row, /EXCLUSIVE,SET_VALUE=ks_profman_method,/NO_RELEASE)
    
    
    ks_comp_b=WIDGET_BASE(ks_profman_comps_mb,/row,/frame)
    
    comp_select_base=WIDGET_BASE(ks_comp_b,/column,xpad=0,ypad=3)
    lab=WIDGET_LABEL(comp_select_base,val='Comp.')
    comp_vt_base=WIDGET_BASE(comp_select_base,/column,/nonexcl)
    ks_profman_setcomp1=WIDGET_BUTTON(comp_vt_base,value='1 (Cent)',uvalue="PROF_COM_SET_COMP0",ys=97,sens=(1-ks_profman_method)*((1+prof_selected) < 1))
    ks_profman_setcomp2=WIDGET_BUTTON(comp_vt_base,value='2 (Blue)',uvalue="PROF_COM_SET_COMP1",ys=87,sens=(1-ks_profman_method)*((1+prof_selected) < 1))
    ks_profman_setcomp3=WIDGET_BUTTON(comp_vt_base,value='3 (Red)',uvalue="PROF_COM_SET_COMP2",ys=57,sens=(1-ks_profman_method)*((1+prof_selected) < 1))
  
    
    ks_table_profcomp=WIDGET_TAB(ks_comp_b,uvalue='tabu',sens=(1-ks_profman_method)*((1+prof_selected) < 1) )
    
    max_decim=9
    
      ;###### Center
      ks_tmp_b=WIDGET_BASE(ks_table_profcomp,/column,tit="Shift from line center")
        
        ;### COMP1
        ks_comp1_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
              ks_profman_comp1_c=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(*prof_storage.cent)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_profman_comp1_cfix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_CFIX0")
        ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_profman_comp1_cmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(*prof_storage.min_cent)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_comp1_csetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_CSMI0")
        ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_profman_comp1_cmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(*prof_storage.max_cent)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_comp1_csetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_CSMA0")
        
        ;### COMP2
        ks_comp2_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
              ks_profman_comp2_c=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(*prof_storage.cent)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_profman_comp2_cfix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_CFIX1")
        ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
          ks_profman_comp2_cmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(*prof_storage.min_cent)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_comp2_csetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_CSMI1")
        ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
          ks_profman_comp2_cmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(*prof_storage.max_cent)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_comp2_csetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_CSMA1")

        ;### COMP3
        ks_comp3_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
              ks_profman_comp3_c=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(*prof_storage.cent)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_profman_comp3_cfix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_CFIX2")
        ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
          ks_profman_comp3_cmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(*prof_storage.min_cent)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_comp3_csetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_CSMI2")
        ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
          ks_profman_comp3_cmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(*prof_storage.max_cent)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_comp3_csetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_CSMA2")
          
      ;###### FWHM
      ks_tmp_b=WIDGET_BASE(ks_table_profcomp,/column,tit="FWHM")
        
        ;### COMP1
        ks_comp1_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
              ks_profman_comp1_f=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(*prof_storage.fwhm)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_profman_comp1_ffix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_FFIX0")
        ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_profman_comp1_fmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(*prof_storage.min_fwhm)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_comp1_fsetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_FSMI0")
        ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_profman_comp1_fmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(*prof_storage.max_fwhm)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_comp1_fsetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_FSMA0")
        
        ;### COMP2
        ks_comp2_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
              ks_profman_comp2_f=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(*prof_storage.fwhm)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_profman_comp2_ffix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_FFIX1")
        ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
          ks_profman_comp2_fmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(*prof_storage.min_fwhm)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_comp2_fsetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_FSMI1")
        ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
          ks_profman_comp2_fmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(*prof_storage.max_fwhm)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_comp2_fsetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_FSMA1")

        ;### COMP3
        ks_comp3_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
              ks_profman_comp3_f=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(*prof_storage.fwhm)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_profman_comp3_ffix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_FFIX2")
        ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
          ks_profman_comp3_fmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(*prof_storage.min_fwhm)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_comp3_fsetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_FSMI2")
        ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
          ks_profman_comp3_fmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(*prof_storage.max_fwhm)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_comp3_fsetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_FSMA2")


      ;###### AMPLITUDE
      ks_tmp_b=WIDGET_BASE(ks_table_profcomp,/column,tit="Amplitude")
        
        ;### COMP1
        ks_comp1_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
              ks_profman_comp1_a=FSC_FIELD(ks_tmp1_b, decimal=max_decim, title='Value',value=(*prof_storage.ampl)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_profman_comp1_afix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_AFIX0")
        ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_profman_comp1_amin=FSC_FIELD(ks_tmp1_b, title='Min.', decimal=max_decim, value=(*prof_storage.min_ampl)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_comp1_asetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_ASMI0")
        ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_profman_comp1_amax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(*prof_storage.max_ampl)[0],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_comp1_asetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_ASMA0")
        
        ;### COMP2
        ks_comp2_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
              ks_profman_comp2_a=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(*prof_storage.ampl)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_profman_comp2_afix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_AFIX1")
        ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
          ks_profman_comp2_amin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(*prof_storage.min_ampl)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_comp2_asetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_ASMI1")
        ks_tmp1_b=WIDGET_BASE(ks_comp2_b,/column)
          ks_profman_comp2_amax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(*prof_storage.max_ampl)[1],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_comp2_asetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_ASMA1")

        ;### COMP3
        ks_comp3_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
              ks_profman_comp3_a=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(*prof_storage.ampl)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_profman_comp3_afix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_AFIX2")
        ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
          ks_profman_comp3_amin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(*prof_storage.min_ampl)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_comp3_asetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_ASMI2")
        ks_tmp1_b=WIDGET_BASE(ks_comp3_b,/column)
          ks_profman_comp3_amax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(*prof_storage.max_ampl)[2],xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_comp3_asetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_ASMA2")

      ;###### CONTINUUM
      ks_tmp_b=WIDGET_BASE(ks_table_profcomp,/column,tit="Continuum")
        
        ks_comp1_b=WIDGET_BASE(ks_tmp_b,/row,/frame,ys=85)
          ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
              ks_profman_cont=FSC_FIELD(ks_tmp1_b, title='Value',decimal=max_decim,value=(prof_storage.cont),xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
              ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)
                ks_profman_contfix=WIDGET_BUTTON(ks_tmp2_b,value='Fix',uvalue="PROF_COM_SET_CTFX0")
        ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_profman_contmin=FSC_FIELD(ks_tmp1_b, title='Min.',decimal=max_decim, value=(prof_storage.min_cont),xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_contsetmin=WIDGET_BUTTON(ks_tmp2_b,value='Set min.',uvalue="PROF_COM_SET_CTMI0")
        ks_tmp1_b=WIDGET_BASE(ks_comp1_b,/column)
          ks_profman_contmax=FSC_FIELD(ks_tmp1_b, title='Max.',decimal=max_decim, value=(prof_storage.max_cont),xsize=10,uvalue="PROF_COM_SET_VALUE",event_pro="KS_PROFILE_MANAGER_event")
          ks_tmp2_b=WIDGET_BASE(ks_tmp1_b,/nonexclusive,/row)          
          ks_profman_contsetmax=WIDGET_BUTTON(ks_tmp2_b,value='Set max.',uvalue="PROF_COM_SET_CTMA0")
  
  
  lab=WIDGET_LABEL(ks_profman_profandcomp_b,val="Note: Params. of axes and inst. contour will be taken from ANALYSIS menu!", font=titfont)
  
  
  
  ks_profman_monitors=[{obj_par,'X: ','',0,0,1},$
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
  ks_prof_disp={disp_par,0,'dispprof',0}
  
  ks_profman_out_and_but_b=WIDGET_BASE(ks_profman_b,/col)
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
  
  
  
  lab=WIDGET_LABEL(ks_profman_out_and_but_b,font=titfont,val="Note: Disp. is corrected for inst., therm. and nat. broadening (see ANALYSIS menu).")
  
  
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