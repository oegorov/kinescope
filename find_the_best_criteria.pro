PRO lin_moms, x,p, ymod, dp
  
  ymod=x[0,*]*0
  n=n_elements(p)
  for i=0,n-1 do ymod+=p[i]*x[i,*]
;  ymod+=p[n-1]*0
  ymod=reform(ymod)
  ;ymod=reform(p[0]*0+p[1]*x[0,*]+p[2]*x[1,*]);+p[3]*x[2,*]+p[4]*x[3,*])
  if n_params() GE 4  then begin
;       ; Create derivative and compute derivative array
       dp = make_array(n_elements(x), n_elements(p), value=x[0]*0)

       ; Compute derivative if requested by caller
;       for i = 0, n_elements(p)-1 do dp(*,i) = FGRAD(x, p, i)
     endif
  
END


PRO find_the_best_criteria
COMMON KS_FOR_MPFIT
    ;Пытаемся найти оптимальное соотношение между моментами, задающее число линий
    
    
    ; Параметры куба
      nchan=40 ;40
      inst_vel=22.;22.
      dv=12. ;12.
      vscale=findgen(nchan)*dv-dv*nchan/2.
      vscale_interp=findgen(nchan*10)*dv/10.-dv*nchan/2.
      hist_bin=[0.1,10.,1.5,0.015]
      
      niter=1000 ; число итераций
      
      disp_range=[18.,42.]
      int_range=[0.2,1.]
      cent_range=[-70.,70.]
      sn_range=[50,200]
      
      
      mom0=fltarr(niter)
      mom1=fltarr(niter)
      mom2=fltarr(niter)
      mom3=fltarr(niter)
      mom4=fltarr(niter)
      sn_used=fltarr(niter)
      ncomps=fltarr(niter)
      
    
    
      ;Сохраняем максимум 100 профилей в ps для визуального анализа. Туда же - производные. 
      IF NOT KEYWORD_SET(NO_PLOT) THEN BEGIN
        ps_start,file="~/Science/IDLWorkspace/KINESCOPE/comp_sep_test.ps"
        npr=2
        npc=3
        nplot_max=100
        nplot_cur=0
        npage=1
        plt_xr=[vscale[0],vscale[nchan-1]]
        plt_yr=[-int_range[1]/10.,int_range[1]*2.]
      ENDIF
    
    
    
    
     ;Метод 1 - сравнение с распределением вероятностей
        dir="/Users/mors/Science/IDLWorkspace/KINESCOPE/moments/"
        m3_m4_1=readfits(dir+"m3_m4_1comp.fits",h11)
        m3_m4_2=readfits(dir+"m3_m4_2comp.fits",h12)
        m3_m4_3=readfits(dir+"m3_m4_3comp.fits",h13)
        m3_m3xm4_1=readfits(dir+"m3_m3xm4_1comp.fits",h21)
        m3_m3xm4_2=readfits(dir+"m3_m3xm4_2comp.fits",h22)
        m3_m3xm4_3=readfits(dir+"m3_m3xm4_3comp.fits",h23)
        
        m3_ax=findgen(sxpar(h11,"NAXIS1"))*sxpar(h11,"CDELT1")+sxpar(h11,"CRVAL1")
        m4_ax=findgen(sxpar(h11,"NAXIS2"))*sxpar(h11,"CDELT2")+sxpar(h11,"CRVAL2")
        m3xm4_ax=findgen(sxpar(h21,"NAXIS2"))*sxpar(h21,"CDELT2")+sxpar(h21,"CRVAL2")
      
        ncomps_imgcrit=fltarr(niter)+1
        ncomps_curcrit=fltarr(niter)+1
        ncomps_out=fltarr(niter)+1
        c1_prob=fltarr(niter)
        c2_prob=fltarr(niter)
        c3_prob=fltarr(niter)
    
        cf_poly=[3.82897,     -1.85430]
    
    
    
      FOR i=0L,niter-1 DO BEGIN
        IF NOT KEYWORD_SET(NO_PLOT) THEN BEGIN
         nplot_cur+=1
         IF nplot_cur le nplot_max THEN BEGIN 
           if nplot_cur gt npage*npr*npc then begin
            erase
            npage+=1
           endif
           cgplot,[0],[0],/nodata,xr=plt_xr, yr=plt_yr,$
           xst=1,yst=1,xtit="Center",ytit="Intens",layout=[npr,npc,nplot_cur-(npage-1)*npr*npc]
         ENDIF 
        ENDIF
        
        ;Симмулируем
        
        ;вычисляем число линий
        r=randomu(seed)
        if r lt 1/2. then ncomps[i]=1. else ncomps[i]=2;begin
;          if r lt 2/3. then ncomps[i]=2. else ncomps[i]=3.
;        endelse
        
        cur_intens=fltarr(ncomps[i])
        cur_disp=fltarr(ncomps[i])
        cur_cent=fltarr(ncomps[i])
        cur_sn=randomu(seed)*(sn_range[1]-sn_range[0])+sn_range[0]
        prof=fltarr(nchan)
        sn_used[i]=cur_sn
        FOR j=0, ncomps[i]-1 do begin 
          if j gt 0 then begin
            ok=0
            while ok ne 1 do begin
              ok=1
              cur_intens[j]=randomu(seed)*(int_range[1]-int_range[0])+int_range[0]
              cur_disp[j]=randomu(seed)*(disp_range[1]-disp_range[0])+disp_range[0]
              cur_cent[j]=randomu(seed)*(cent_range[1]-cent_range[0])+cent_range[0]
              if sqrt(abs(cur_disp[j]^2-cur_disp[0]^2)) le 2.5*inst_vel and abs(cur_cent[j]-cur_cent[0]) le inst_vel then ok=0
              if cur_intens[j] le 5.*1./cur_sn then ok=0
    ;          if ncomps[i] ne 1 and sqrt(cur_disp[0]^2+inst_vel^2) lt 26 then ok=0
              if j eq 2 then begin
                if abs(cur_disp[j]-cur_disp[j-1]) le 2.5*inst_vel and abs(cur_cent[j]-cur_cent[j-1]) le inst_vel then ok=0
              endif
            endwhile
          endif else begin
            cur_disp[j]=randomu(seed)*(disp_range[1]-disp_range[0])+disp_range[0]
            cur_cent[j]=randomu(seed)*(cent_range[1]-cent_range[0])+cent_range[0]
            cur_intens[j]=1.
          endelse
          this_comp=ks_voigt(vscale,[cur_intens[j],cur_cent[j],cur_disp[j]*2.35482],inst_vel=inst_vel)
          prof+=this_comp
          IF NOT KEYWORD_SET(NO_PLOT) and nplot_cur le nplot_max THEN cgoplot,vscale, this_comp, color="red"
          
        ENDFOR
          
        
        s=reverse(sort(cur_intens))
        cur_intens=cur_intens[s]
        cur_cent=cur_cent[s]
        cur_disp=cur_disp[s]
        
        noise=(randomn(seed,nchan))*cur_intens[0]/cur_sn
        prof+=noise
        der1=deriv(vscale,prof)
        der2=deriv(vscale,der1)
       
;        
;        rec=where(prof lt stddev(noise)*1.5, nr)
;        if nr gt 0 then prof[rec]=0
        
        
        
        moms=KS_moments(vscale,prof,contin=0, snr=snr,iter_done=iter_done)
        mom0[i]=moms[0]
        mom1[i]=moms[1]
        mom2[i]=moms[2]
        mom3[i]=moms[3]
        mom4[i]=moms[4]
        
        
        
        
        
        
        ;##### Apply Method 1 (PDF) #####
        
        m=min(abs(m3_ax-mom3[i]),mpos1)
        m=min(abs(m4_ax-mom4[i]),mpos2)
        m=min(abs(m3xm4_ax-mom4[i]*mom3[i]),mpos3)
        c1_prob[i]=(m3_m4_1[mpos1,mpos2])
        c2_prob[i]=(m3_m4_2[mpos1,mpos2])
;        c3_prob[i]=(m3_m4_3[mpos1,mpos2])
        m=max([c1_prob[i],c2_prob[i]],mpos);c3_prob[i]],mpos)
        s_prob=sort([c1_prob[i],c2_prob[i]]);,c3_prob[i]])
        
        
        
;        second=1.-[c1_prob[i],c2_prob[i],c3_prob[i]]/m
;        rec=where(second lt 0.1,nr)
;        if nr gt 1 and mpos ne 0 then begin
;          for pi=0,nr-1 do begin
;            if rec[pi] lt mpos then mpos=rec[pi]
;          endfor
;        endif
        ncomps_imgcrit[i]=mpos+1
        
        ;###### Apply Метод 2 - задание границ
        
;        if ((abs(mom3[i]) gt 0.15) or (mom4[i] lt -0.75)) then ncomps_curcrit[i]=2
;        if ((abs(mom3[i]) gt 0.5) and (mom4[i] gt -0.5)) then ncomps_curcrit[i]=3
;        if (abs(mom3[i]*mom4[i]) gt 0.7) then ncomps_curcrit[i]=2
;        if (mom4[i] gt 0.1) then ncomps_curcrit[i]=1
;      
        
        y=(mom4[i]+2.)/0.05
        x=(mom3[i]+1.)/0.05
        
       ncomps_curcrit[i]=0
       
;       if y ge 43 and (x-20.3)^2+(y-56.5)^2 le 17.5^2 then ncomps_curcrit[i]=1 
;       if y lt (-2.05*x+53.8) or y lt 2.25*x-35.53 or y le 16 then  ncomps_curcrit[i]=2
;       if y gt 31 and (x-20.44)^2+(y-28.31)^2 gt 18.7^2 and (x-20.3)^2+(y-56.5)^2 gt 17.5^2 then ncomps_curcrit[i]=3
;       
;       if ncomps_curcrit[i] eq 0 then begin
;       ncomps_curcrit[i]=1
;        if ((abs(mom3[i]) gt 0.15) or (mom4[i] lt -0.75)) then ncomps_curcrit[i]=2
;        if ((abs(mom3[i]) gt 0.5) and (mom4[i] gt -0.5)) then ncomps_curcrit[i]=3
;        if (abs(mom3[i]*mom4[i]) gt 0.7) then ncomps_curcrit[i]=2
;        if (mom4[i] gt 0.1) then ncomps_curcrit[i]=1        
       
       got=0
       
       if (mom4[i] gt 0.2) and got eq 0 then begin
          ncomps_curcrit[i]=1
          got=1
       endif
       if ((abs(mom3[i]) gt 0.3) or mom4[i] le -0.8 or abs(mom3[i]*mom4[i]) gt 0.15) and got eq 0 then begin
          ncomps_curcrit[i]=2
          got=1
       endif
;       if got eq 0 then ncomps_curcrit[i]=1 
       
       
       
       
       if ncomps_curcrit[i] eq 0 then begin
         if y ge 43 and (x-20.3)^2+(y-56.5)^2 le 17.5^2 then ncomps_curcrit[i]=1 
         if y lt (-2.05*x+53.8) or y lt 2.25*x-35.53 or y le 16 then  ncomps_curcrit[i]=2
         if y gt 31 and (x-20.44)^2+(y-28.31)^2 gt 18.7^2 and (x-20.3)^2+(y-56.5)^2 gt 17.5^2 then ncomps_curcrit[i]=2
         
         ncomps_curcrit[i]=1
        if ((abs(mom3[i]) gt 0.2) or (mom4[i] lt -0.75)) then ncomps_curcrit[i]=2
        if ((abs(mom3[i]) gt 0.5) and (mom4[i] gt -0.5)) then ncomps_curcrit[i]=2
        if (abs(mom3[i]*mom4[i]) gt 0.2) then ncomps_curcrit[i]=2
        if (mom4[i] gt 0.2) then ncomps_curcrit[i]=1     
       
        
        if (x-20.33)^2+(y-19.45)^2 le 7.48^2 and got eq 0 then begin 
          m=min(abs(m3_ax-mom3[i]),mpos1)
          m=min(abs(m4_ax-mom4[i]),mpos2)
          c1_prob[i]=(m3_m4_1[mpos1,mpos2])
          c2_prob[i]=(m3_m4_2[mpos1,mpos2])
          c3_prob[i]=(m3_m4_3[mpos1,mpos2])
          m=max([c1_prob[i],c2_prob[i]],mpos);,c3_prob[i]],mpos)
          s_prob=sort([c1_prob[i],c2_prob[i]]);,c3_prob[i]])
          ncomps_curcrit[i]=mpos+1
        endif
;       
       endif
;       endif 
        
        
        ;######## Apply method 3 (derivatives) ###
        
        
        rec=where((vscale lt moms[1]-3.*moms[2]) or (vscale gt moms[1]+3.*moms[2]), nr)
        if nr lt 5 then rec=where((vscale lt moms[1]-1.5*moms[2]) or (vscale gt moms[1]+1.5*moms[2]), nr)
        if nr lt 5 then rec=where((vscale lt moms[1]-1.*moms[2]) or (vscale gt moms[1]+1.*moms[2]), nr)
        if nr lt 5 then rec=indgen(nchan)
        
        mx=max(der2,/nan)
        if mx le 0 then der2=der2-mx
        noise_der=stddev(der2[rec],/nan)
        rec=where(abs(der2) lt noise_der or der2 gt noise_der, nr)
        if nr ne 0 then der2[rec]=0
        der2[0]=0
        der2[nchan-1]=0

        
        
        ncomps_out[i]=1
        
        
;        well_done=0
;        ncmp=0
;        
;        rec=where(der2 eq 0)
;        while well_done eq 0 do begin
;          m=min(der2,mpos)
;          if abs(m) le noise_der then begin
;            well_done=1
;            continue
;          endif
;          rpos=rec-mpos
;          rr=where(rpos gt 0)
;          mm=min(rpos[rr],rbord)
;          rbord=rec[rr[rbord]]
;          lpos=mpos-rec
;          rr=where(lpos gt 0)
;          mm=min(lpos[rr],lbord)
;          lbord=rec[rr[lbord]]
;          der2[lbord:rbord]=0
;          if rbord-lbord ge 4 then ncmp+=1
;        endwhile
;        
;        ncomps_out[i]=1
;        if ncmp eq 2 then ncomps_out[i]=2
;        if ncmp ge 3 then ncomps_out[i]=3



;        for kk=0,nchan-1 do der2[kk]+=randomu(seed)*noise_der/50.
;        moms_der=KS_moments(vscale,-der2/max(abs(der2)),contin=0, snr=snr,iter_done=iter_done)
;        
;        if moms_der[2] gt 0 then begin
;          if ((abs(moms_der[3]) gt 0.1) or (moms_der[4] lt -0.7)) then ncomps_out[i]=2
;          if (abs(moms_der[3]*moms_der[4]) gt 0.7) then ncomps_out[i]=2
;          if ((abs(moms_der[3]) gt 0.5) and (moms_der[4] gt -0.5)) then ncomps_out[i]=3
;           if (moms_der[4] gt 0.1) then ncomps_out[i]=1
;        endif



         IF NOT KEYWORD_SET(NO_PLOT) and nplot_cur le nplot_max THEN begin
          cgoplot,vscale, prof, color="black"
          der2_plt=-der2/max(abs(der2),/nan)+1.
          der1_plt=der1/max(abs(der1),/nan)+1.
          cgoplot,vscale, der1_plt, color="blue"
          cgoplot,vscale, der2_plt, color="green"
          cgoplot,vscale,prof*0+1.,linest=2,col="gray"
        endif
        
         

        
        
;        ;##### Apply method 4 - fitting ####
;        
;        moments_struct={mom1:moms[1],mom2:moms[2],mom3:moms[3],mom4:moms[4],snr:snr}
;        contin=0
;        
;        resid=[-1]
;        good=0
;        nciter=0
;        
;        while nciter le 2 and good eq 0 do begin 
;            N_lines=s_prob[nciter]+1
;            
;            
;            ysort=prof[sort(abs(vscale-moments_struct.mom1))]
;            intens=(max(ysort[0:5]))-contin
;            ampl=[1.,0.3,0.3]*intens
;            fw=replicate(moments_struct.mom2,3)/sqrt(n_lines)*2.35482
;            cent=[moments_struct.mom1,moments_struct.mom1+moments_struct.mom3/abs(moments_struct.mom3)*(abs(moments_struct.mom3)*sqrt(abs(moments_struct.mom2^2-inst_vel^2))^3)^0.33,$
;            moments_struct.mom1-moments_struct.mom3/abs(moments_struct.mom3)*(abs(moments_struct.mom3)*sqrt(abs(moments_struct.mom2^2-inst_vel^2))^3)^0.33]
;            rec=where(~finite(intens),nr)
;            if nr gt 0 then intens[rec]=0
;            rec=where(~finite(fw),nr)
;            if nr gt 0 then fw[rec]=0
;            rec=where(~finite(cent),nr)
;            if nr gt 0 then cent[rec]=0
;            
;            s=sort(cent[1:2]-cent[0])
;            comps=[1,0,0]
;            if n_lines eq 3 then comps=[1,1,1]
;            if n_lines eq 2 then begin
;              rec=where(s eq 0)
;              comps[rec+1]=1
;            endif
;            
;            
;            
;            ;##### Define borders
;            smncnt=1
;            smxcnt=1
;            mncnt=min(prof,/nan)
;            mxcnt=max(prof,/nan)
;            
;            smna=[1,1,1]
;            mna=[replicate(mncnt,3)]
;            smxa=[1,1,1]
;            mxa=[replicate(mxcnt,3)]
;            
;            smnc=[1,1,1]
;            smxc=[1,1,1]
;            nx=n_elements(vscale)
;            mnc=[replicate(vscale[fix(nx/10)],3)]
;            mxc=[replicate(vscale[fix(nx*9/10)],3)]
;            
;            smnf=[1,1,1]
;            smxf=[1,1,1]
;            mnf=[replicate(23.5482,3)]
;            mxf=[replicate(moments_struct.mom2*2.35482*2,3)]
;            
;            cent=double(cent)
;            ampl=double(ampl)
;            fw=double(fw)
;            contin=double(contin)
;            
;            
;            ini_comps={prf_ini_str, ampl: [ampl[0],(ampl[1:2])[s]], cent: [cent[0],(cent[1:2])[s]], comps: comps, fwhm: [fw[0],(fw[1:2])[s]], $
;                      min_ampl: [mna[0],(mna[1:2])[s]], max_ampl: [mxa[0],(mxa[1:2])[s]],min_fwhm: [mnf[0],(mnf[1:2])[s]], max_fwhm: [mxf[0],(mxf[1:2])[s]],$
;                      min_cent: [mnc[0],(mnc[1:2])[s]], max_cent: [mxc[0],(mxc[1:2])[s]], setmin_ampl: [smna[0],(smna[1:2])[s]], setmax_ampl: [smxa[0],(smxa[1:2])[s]], $
;                      setmin_fwhm: [smnf[0],(smnf[1:2])[s]], setmax_fwhm: [smxf[0],(smxf[1:2])[s]],setmin_cent: [smnc[0],(smnc[1:2])[s]], setmax_cent: [smxc[0],(smxc[1:2])[s]],$
;                      setmin_cont: smncnt, setmax_cont: smxcnt, min_cont: mncnt, max_cont: mxcnt, cont: contin}
;            
;            
;            
;            
;            tmp=[0,0,0]
;            params_struct={comps: ini_comps.comps,ampl: ini_comps.ampl,cent: ini_comps.cent, fwhm: ini_comps.fwhm,$
;               setmax_ampl: ini_comps.setmax_ampl,setmax_cent: ini_comps.setmax_cent, setmax_fwhm: ini_comps.setmax_fwhm,$
;               setmin_ampl: ini_comps.setmin_ampl,setmin_cent: ini_comps.setmin_cent, setmin_fwhm: ini_comps.setmin_fwhm,$
;               fixampl: tmp,fixcent: tmp, fixfwhm: tmp,$
;               max_ampl: ini_comps.max_ampl,max_cent: ini_comps.max_cent, max_fwhm: ini_comps.max_fwhm,$
;               min_ampl: ini_comps.min_ampl,min_cent: ini_comps.min_cent, min_fwhm: ini_comps.min_fwhm,$
;               cont: ini_comps.cont,setmax_cont: ini_comps.setmax_cont,setmin_cont: ini_comps.setmin_cont,$
;               max_cont: ini_comps.max_cont,min_cont: ini_comps.min_cont,fixcont: 0}
;                
;            param=fltarr(N_lines*3+1) ; 1 - cont
;            parinfo=replicate({parinfo_str,fixed:0L,limits:fltarr(2),limited:lonarr(2)},N_lines*3+1)
;            rec=where(params_struct.comps eq 1)
;            for ind=0,N_lines-1 do begin
;              param[ind*3]=double((params_struct.ampl)[rec[ind]])
;              param[ind*3+1]=double((params_struct.cent)[rec[ind]])
;              param[ind*3+2]=double((params_struct.fwhm)[rec[ind]])
;              
;              parinfo[ind*3].limited=[(params_struct.setmin_ampl)[rec[ind]],(params_struct.setmax_ampl)[rec[ind]]]
;              parinfo[ind*3].limits=[(params_struct.min_ampl)[rec[ind]],(params_struct.max_ampl)[rec[ind]]]
;              parinfo[ind*3].fixed=(params_struct.fixampl)[rec[ind]]
;              parinfo[ind*3+1].limited=[(params_struct.setmin_cent)[rec[ind]],(params_struct.setmax_cent)[rec[ind]]]
;              parinfo[ind*3+1].limits=[(params_struct.min_cent)[rec[ind]],(params_struct.max_cent)[rec[ind]]]
;              parinfo[ind*3+1].fixed=(params_struct.fixcent)[rec[ind]]
;              parinfo[ind*3+2].limited=[(params_struct.setmin_fwhm)[rec[ind]],(params_struct.setmax_fwhm)[rec[ind]]]
;              parinfo[ind*3+2].limits=[(params_struct.min_fwhm)[rec[ind]],(params_struct.max_fwhm)[rec[ind]]]
;              parinfo[ind*3+2].fixed=(params_struct.fixfwhm)[rec[ind]]
;            endfor
;            param[N_lines*3]=double(params_struct.cont)
;            parinfo[N_lines*3].fixed=params_struct.fixcont
;            parinfo[N_lines*3].limited=[params_struct.setmin_cont,params_struct.setmax_cont]
;            parinfo[N_lines*3].limits=[params_struct.min_cont,params_struct.max_cont]
;            contin=params_struct.cont
;                       
;            err = sqrt(1000.D + abs(prof-contin))
;          
;            inst_fwhm_vel = inst_vel
;            current_method = 0
;            res=mpfitfun('KS_FIT_FUNC',vscale,prof,err,param,yfit=yfit,parinfo=parinfo,/quiet)
;            
;            resid=[resid,total(abs(prof-yfit),/nan)]
;            if resid[nciter+1]/stddev(noise)/nchan gt 5 then good=0 else good=1 
;            nciter+=1
;        ENDWHILE
;        
;        nres=n_elements(resid)
;        resid=resid[1:nres-1]
;        m=min(resid,mpos)
;        if good eq 0 then ncomps_out[i]=s_prob[mpos]+1 else ncomps_out[i]=n_lines 
        
        
        
        IF NOT KEYWORD_SET(NO_PLOT) and nplot_cur le nplot_max THEN  cgtext,(plt_xr[1]-plt_xr[0])/20.+plt_xr[0],plt_yr[1]-(plt_yr[1]-plt_yr[0])/20.,"N_gen = "+string(ncomps[i],format="(I0)"),charsize=0.5
        IF NOT KEYWORD_SET(NO_PLOT) and nplot_cur le nplot_max THEN  cgtext,(plt_xr[1]-plt_xr[0])/20.+plt_xr[0],plt_yr[1]-(plt_yr[1]-plt_yr[0])*2/20.,"N_pdf = "+string(ncomps_imgcrit[i],format="(I0)"),charsize=0.5
        IF NOT KEYWORD_SET(NO_PLOT) and nplot_cur le nplot_max THEN  cgtext,(plt_xr[1]-plt_xr[0])/20.+plt_xr[0],plt_yr[1]-(plt_yr[1]-plt_yr[0])*3/20.,"N_reg = "+string(ncomps_curcrit[i],format="(I0)"),charsize=0.5
        IF NOT KEYWORD_SET(NO_PLOT) and nplot_cur le nplot_max THEN  cgtext,(plt_xr[1]-plt_xr[0])/20.+plt_xr[0],plt_yr[1]-(plt_yr[1]-plt_yr[0])*4/20.,"N_der = "+string(ncomps_out[i],format="(I0)"),charsize=0.5
        
      ENDFOR
      
      IF NOT KEYWORD_SET(NO_PLOT)  THEN ps_end
      
      
        
        
;        ncomps_imgcrit=c1_prob+c2_prob*2.+c3_prob*3/
      
      
      
      
      
      
;      ;Метод 3 - полином
;      ncomps_out=mom3*0
;      rec=where(ncomps_out eq 0, nr)
;      weight = (fltarr(nr)+1./nr);*sn_used/70.
;      
;      cf=[1.,1.]
;      x=fltarr(n_elements(cf),nr)
;      x0=fltarr(n_elements(cf),niter)
;      x[0,*]=abs(mom3[rec])
;      x[1,*]=mom4[rec]
;      
;      x0[0,*]=abs(mom3)
;      x0[1,*]=mom4
;  
;      y=ncomps[rec]
;      res=mpcurvefit(x,y,weight,cf,function_name="lin_moms",NODERIVATIVE=1,/quiet)
;      
;      
;      for i=0,n_elements(cf)-1 do ncomps_out[rec]+=cf[i]*x0[i,*]
;      ncomps_out[rec]=round(ncomps_out[rec])
;      
;      rec=where(ncomps_out gt 3,nr)
;      if nr gt 0 then ncomps_out[rec]=3
;      rec=where(ncomps_out lt 1,nr)
;      if nr gt 0 then ncomps_out[rec]=1
;      
;      
;      rec=where(ncomps_out eq 3 and mom4 gt -0.4,nr)
;      if nr gt 0 then ncomps_out[rec]=2
;      rec=where(ncomps_out eq 1 and abs(mom3*mom4) gt 0.2 or abs(mom3) gt 0.5 or mom4 lt -1. ,nr)
;      if nr gt 0 then ncomps_out[rec]=2
;      
;      rec=where((abs(mom3) gt 0.5) or (mom4 lt -1.5) or abs(mom3*mom4) gt 0.7, nr)
;      if nr gt 0 then ncomps_out[rec]=2
;      rec=where((mom4) gt 0.1,nr)
;      if nr gt 0 then ncomps_out[rec]=1
      
      
      
      check=intarr(niter)
      check_curcrit=intarr(niter)
      check_imgcrit=intarr(niter)
      lie=intarr(niter)
      lie_curcrit=intarr(niter)
      lie_imgcrit=intarr(niter)
      for i=0,niter-1 do begin
        if ncomps_out[i] eq round(ncomps[i]) then check[i] = 1 
        if ncomps_curcrit[i] eq round(ncomps[i]) then check_curcrit[i] = 1
        if ncomps_imgcrit[i] eq round(ncomps[i]) then check_imgcrit[i] = 1 
        if ncomps_out[i] ne round(ncomps[i]) then lie[i] = ncomps_out[i] 
        if ncomps_curcrit[i] ne round(ncomps[i]) then lie_curcrit[i] = ncomps_curcrit[i]
        if ncomps_imgcrit[i] ne round(ncomps[i]) then lie_imgcrit[i] = ncomps_imgcrit[i]
      endfor

    
    
      print,"******************"
      rec1=where(ncomps eq 1,nr1)
      rec2=where(ncomps eq 2,nr2)
      rec3=where(ncomps eq 3,nr3)
      
      print, "Generated Distribution: "+string(nr1*100./niter,format="(F0.2)")+"%; "+string(nr2*100./niter,format="(F0.2)")+"%; "+string(nr3*100./niter,format="(F0.2)")+"%"
      
  
  
      print,"########## Method 1 (PDF) ######
      
      rec1=where(ncomps_imgcrit eq 1,nr1)
      rec2=where(ncomps_imgcrit eq 2,nr2)
      rec3=where(ncomps_imgcrit eq 3,nr3)
    
      print,"For 1 comps:"
      rec=where(ncomps eq 1,nr)
      rec_l=where(lie_imgcrit eq 1, nrl)
      print,"Correct: "+string(float(total(check_imgcrit[rec]))/nr*100.,format="(F0.2)")+"%;  Liers: "+string(float(nrl)/nr1*100, format="(F0.2)")+"%"
      print,"For 2 comps:"
      rec=where(ncomps eq 2,nr)
      rec_l=where(lie_imgcrit eq 2, nrl)
      print,"Correct: "+string(float(total(check_imgcrit[rec]))/nr*100.,format="(F0.2)")+"%;  Liers: "+string(float(nrl)/nr2*100, format="(F0.2)")+"%"
      print,"For 3 comps:"
      rec=where(ncomps eq 3,nr)
      rec_l=where(lie_imgcrit eq 3, nrl)
      if nr gt 0 and nrl gt 0 then print,"Correct: "+string(float(total(check_imgcrit[rec]))/nr*100.,format="(F0.2)")+"%;  Liers: "+string(float(nrl)/nr3*100, format="(F0.2)")+"%"
      
      
      
      print,"Distribution: "+string(nr1*100./niter,format="(F0.2)")+"%; "+string(nr2*100./niter,format="(F0.2)")+"%; "+string(nr3*100./niter,format="(F0.2)")+"%"
      
  
  
    
    
      print,"######### Method 2 (deriv) ######## 
      
;      print,"Coeffs:", cf
      
      rec1=where(ncomps_out eq 1,nr1)
      rec2=where(ncomps_out eq 2,nr2)
      rec3=where(ncomps_out eq 3,nr3)
      
      print,"For 1 comps:"
      rec=where(ncomps eq 1,nr)
      rec_l=where(lie eq 1, nrl)
      print,"Correct: "+string(float(total(check[rec]))/nr*100.,format="(F0.2)")+"%;  Liers: "+string(float(nrl)/nr1*100, format="(F0.2)")+"%"
      print,"For 2 comps:"
      rec=where(ncomps eq 2,nr)
      rec_l=where(lie eq 2, nrl)
      print,"Correct: "+string(float(total(check[rec]))/nr*100.,format="(F0.2)")+"%;  Liers: "+string(float(nrl)/nr2*100, format="(F0.2)")+"%"
      print,"For 3 comps:"
      rec=where(ncomps eq 3,nr)
      rec_l=where(lie eq 3, nrl)
      if nr gt 0 and nrl gt 0 then print,"Correct: "+string(float(total(check[rec]))/nr*100.,format="(F0.2)")+"%;  Liers: "+string(float(nrl)/nr3*100, format="(F0.2)")+"%"
      
      
      
      print,"Distribution: "+string(nr1*100./niter,format="(F0.2)")+"%; "+string(nr2*100./niter,format="(F0.2)")+"%; "+string(nr3*100./niter,format="(F0.2)")+"%"
      
      print,"########## Method 3 (regions) ######
      
      rec1=where(ncomps_curcrit eq 1,nr1)
      rec2=where(ncomps_curcrit eq 2,nr2)
      rec3=where(ncomps_curcrit eq 3,nr3)
  
    print,"For 1 comps:"
      rec=where(ncomps eq 1,nr)
      rec_l=where(lie_curcrit eq 1, nrl)
      print,"Correct: "+string(float(total(check_curcrit[rec]))/nr*100.,format="(F0.2)")+"%;  Liers: "+string(float(nrl)/nr1*100, format="(F0.2)")+"%"
      print,"For 2 comps:"
      rec=where(ncomps eq 2,nr)
      rec_l=where(lie_curcrit eq 2, nrl)
      print,"Correct: "+string(float(total(check_curcrit[rec]))/nr*100.,format="(F0.2)")+"%;  Liers: "+string(float(nrl)/nr2*100, format="(F0.2)")+"%"
      print,"For 3 comps:"
      rec=where(ncomps eq 3,nr)
      rec_l=where(lie_curcrit eq 3, nrl)
      if nr gt 0 and nrl gt 0 then print,"Correct: "+string(float(total(check_curcrit[rec]))/nr*100.,format="(F0.2)")+"%;  Liers: "+string(float(nrl)/nr3*100, format="(F0.2)")+"%"
      

      
      print,"Distribution: "+string(nr1*100./niter,format="(F0.2)")+"%; "+string(nr2*100./niter,format="(F0.2)")+"%; "+string(nr3*100./niter,format="(F0.2)")+"%"
      
    
      
  
;  FUNCTION KS_GET_NLINES, moments_struct
;  ;Computes the number of lines to be used in fitting
;  N_lines=fix(moments_struct.mom1)*0
;  rec=where(moments_struct.snr gt 1,nr)
;  if nr gt 0 then begin
;    n_lines[rec]=1
;    rec=where((abs(moments_struct.mom3) gt 0.1) or (moments_struct.mom4 lt -0.7), nr)
;    if nr gt 0 then n_lines[rec]=2
;    rec=where( (abs(moments_struct.mom3) gt 0.5) and (moments_struct.mom4 gt -0.5), nr)
;    if nr gt 0 then N_lines[rec]=3
;  endif
;  ;if (excess+0.55)^2+asymmetry^2 le (2*0.04)^2 then N_lines=1 else N_lines=2 tested for gaussian, good enough
;  return,N_lines
;END
;  




END