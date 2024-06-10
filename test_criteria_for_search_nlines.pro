PRO TEST_CRITERIA_FOR_SEARCH_NLINES

  ; Пытаемся подобрать с помощью Монте-Карло оптимальный критерий для поиска числа линий
  
  
  ; Параметры куба
  nchan=40
  inst_vel=22.
  dv=12.
  vscale=findgen(nchan)*dv-dv*nchan/2.
  hist_bin=[0.1,10.,1.5,0.05,0.05]
  
  niter=30000 ; число итераций
  
  disp_range=[15.,22.]
  int_range=[0.3,1.]
  cent_range=[-140.,140.]
  sn_range=[40,100]
  
  
  mom0=fltarr(niter)
  mom1=fltarr(niter)
  mom2=fltarr(niter)
  mom3=fltarr(niter)
  mom4=fltarr(niter)
  sn_used=fltarr(niter)
  ncomps=fltarr(niter)
  
  
  FOR i=0L,niter-1 DO BEGIN
    ;Симмулируем
    
    ;вычисляем число линий
    r=randomu(seed)
    if r lt 1/2. then ncomps[i]=1 else ncomps[i]=2;begin
;      if r lt 2/3. then ncomps[i]=2 else ncomps[i]=3
;    endelse
    
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
          if cur_intens[j] le 10.*1./cur_sn then ok=0
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
      prof+=ks_voigt(vscale,[cur_intens[j],cur_cent[j],cur_disp[j]*2.35482],inst_vel=inst_vel)
    ENDFOR
    
    s=reverse(sort(cur_intens))
    cur_intens=cur_intens[s]
    cur_cent=cur_cent[s]
    cur_disp=cur_disp[s]
    
    noise=(randomn(seed,nchan))*cur_intens[0]/cur_sn
    prof+=noise
    
    moms=KS_moments(vscale,prof,contin=0, snr=snr,iter_done=iter_done)
    mom0[i]=moms[0]
    mom1[i]=moms[1]
    mom2[i]=moms[2]
    mom3[i]=moms[3]
    mom4[i]=moms[4]
    
;    cgdisplay,wid=13
;    cgplot,vscale,prof,tit=string(mom1[i],format="(F0.2)")
;    wait,0.5
    
  ENDFOR
  
  ;cgdisplay,1500,900,wid=15
  rec=where(ncomps eq 1,nr)
  i=0
  col=['sky blue','royal blue','navy']
  nc=[" (1 comp)"," (2 comp)"," (3 comp)"]
  
  
  addcmd=0
  FOR k=0,1 do begin
    rec=where(ncomps eq k+1,nr)
;    if k ne 0 then begin
;      rec1=where((mom3[rec]*mom4[rec] lt -0.1 or mom3[rec]*mom4[rec] gt 0.1),nr)
;      ;rec1=where((mom3[rec] lt -0.2 or mom3[rec] gt 0.2) and (mom4[rec] gt -0.15 or mom4[rec] lt -0.75),nr)
;      rec=rec[rec1]
;    endif
    i=5*k
     cgwindow,"cgHistoplot", mom1[rec], BINSIZE=hist_bin[1], /FILL, POLYCOLOR=col[k],layout=[5,2,i+1],MININPUT=cent_range[0],maxinp=cent_range[1], tit="MOM1"+nc[k],addcmd=addcmd
    addcmd=1
     cgwindow,"cgHistoplot", mom2[rec], BINSIZE=hist_bin[2], /FILL, POLYCOLOR=col[k],layout=[5,2,i+2],MININPUT=disp_range[0],maxinp=disp_range[1], tit="MOM2"+nc[k],addcmd=addcmd
     cgwindow,"cgHistoplot", mom3[rec], BINSIZE=hist_bin[3], /FILL, POLYCOLOR=col[k],layout=[5,2,i+3],MININPUT=-1,maxinp=1, tit="MOM3"+nc[k],addcmd=addcmd
     cgwindow,"cgHistoplot", mom4[rec], BINSIZE=hist_bin[4], /FILL, POLYCOLOR=col[k],layout=[5,2,i+4],MININPUT=-2,maxinp=1, tit="MOM4"+nc[k],addcmd=addcmd
     cgwindow,"cgHistoplot", mom4[rec]*mom3[rec], BINSIZE=0.05, /FILL, POLYCOLOR=col[k],layout=[5,2,i+5],MININPUT=-1,maxinp=1, tit="MOM4*MOM3"+nc[k],addcmd=addcmd
  ENDFOR
  
   
   
   dir="/Users/mors/Science/IDLWorkspace/KINESCOPE/moments/"
   
   addcmd=0
   plotsym,0,0.6,/fil
; FOR k=0,2 do begin
;    rec=where(ncomps eq k+1,nr)
;    i=k
;      res1=hist_2d( mom3[rec],mom4[rec],bin1=hist_bin[3],bin2=hist_bin[4],max1=1,max2=1,min1=-1,min2=-2)
;      ;res1=float(res1)/max(res1);*100.
;      cgwindow,"cgimage",res1,layout=[3,1,i+1],tit=nc[k],addcmd=addcmd,xtit="MOM3",ytit="MOM4",/axes,maxval=1.,xr=[-1,1],yr=[-2,2],wxs=1200,wys=400
;    addcmd=1
;    
;;    mkhdr,h,res1
;;    sxaddpar,h,"CRVAL1",-1
;;    sxaddpar,h,"CDELT1",hist_bin[3]
;;    sxaddpar,h,"CRVAL2",-2
;;    sxaddpar,h,"CDELT2",hist_bin[4]
;;    writefits,dir+'m3_m4_'+string(i+1,format="(I0)")+'comp.fits',float(res1)/nr,h
;  ENDFOR
; 
; addcmd=0
; FOR k=0,2 do begin
;    rec=where(ncomps eq k+1,nr)
;    i=k
;        res2=hist_2d( mom3[rec],(mom4[rec]*mom3[rec]),bin1=hist_bin[3],bin2=0.05,max1=1,max2=1,min1=-1,min2=-1)
;        ;res2=float(res2)/max(res2);*100.
;     cgwindow,"cgimage",res2,layout=[3,1,i+1],tit=nc[k],addcmd=addcmd,xtit="MOM3",ytit="MOM3*MOM4",/axes,maxval=1.,xr=[-1,1],yr=[-2,2],wxs=1200,wys=400
;    addcmd=1
;;    mkhdr,h,res2
;;    sxaddpar,h,"CRVAL1",-1
;;    sxaddpar,h,"CDELT1",hist_bin[3]
;;    sxaddpar,h,"CRVAL2",-1
;;    sxaddpar,h,"CDELT2",0.05
;;    writefits,dir+'m3_m3xm4_'+string(i+1,format="(I0)")+'comp.fits',float(res2)/nr,h
;  ENDFOR
  
  
;  addcmd=0
;  FOR k=0,2 do begin
;  rec=where(ncomps eq k+1,nr)
;  if k gt 0 then addcmd=1
;  cgwindow,"cgHistoplot", ((mom4+0.55)^2+mom3^2)[rec], BINSIZE=.1, /FILL, POLYCOLOR=col[k],layout=[3,1,k+1],MININPUT=-4,maxinp=4, tit="bla"+nc[k],addcmd=addcmd
;  endfor
  
  
;  plotsym,0,0.3,/fil
;  cgplot,(mom4^2+mom3^2)/mom4, ncomps,psym=8,yr=[0.8,3.2],yst=1,xr=[-50,50],xst=1
  
  
;  
;  
;cgdisplay,wid=16
;cgHistoplot, sn_used, BINSIZE=1., /FILL, POLYCOLOR=col,MININPUT=5,maxinp=300, tit="S/N"

END