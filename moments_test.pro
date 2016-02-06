;FUNCTION KS_VOIGT, x, param, inst_vel=inst_vel
;  vel=inst_vel/2.35482
;  param2=param[2]/2.35482
;  b=(x-param[1])/(sqrt(2.)*param2)
;  y=vel/(sqrt(2.)*param[2])
;  res=voigt(y,b)/(sqrt(2.*!pi)*param2)
;  norm=max(res)
;  res=param[0]*res/norm
;RETURN, res
;END

PRO moments_test


  nchan=40
  inst_vel=22.
  disp=18.
  cent=0.
  intens=1.
  dv=12.
  sn=findgen(96)+5

  cent1=65.
  disp1=60.
  intens1=0.6

;######################
  
  
  vscale=findgen(nchan)*dv+cent-dv*nchan/2.
  noise_ini=(randomn(seed,nchan)+1.)
  fwhm=2.35482*disp
  param=[intens,cent,fwhm]
  prof_ini=ks_voigt(vscale,param,inst_vel=inst_vel)
  
  param1=[intens1,cent1,2.35482*disp1]
  prof_ini1=ks_voigt(vscale,param1,inst_vel=inst_vel)
  
  npoints=n_elements(sn)
  noise=noise_ini
  
  
  cgdisplay,1200,300,wid=17
  cgplot,vscale,prof_ini,col="red",layout=[3,1,1]
  cgplot,vscale,prof_ini+prof_ini1,col="red",layout=[3,1,2]
  
  
  moms_ini_1comp=KS_moments(vscale,prof_ini,contin=contin, snr=snr,iter_done=iter_done)
  moms_ini_2comp=KS_moments(vscale,prof_ini+prof_ini1,contin=contin, snr=snr,iter_done=iter_done)
  
  mom0=fltarr(npoints)
  mom1=fltarr(npoints)
  mom2=fltarr(npoints)
  mom3=fltarr(npoints)
  mom4=fltarr(npoints)
  
  for i=0,npoints-1 do begin
    noise=noise_ini*intens/sn[i]
    prof=prof_ini+noise
    moms=KS_moments(vscale,prof,contin=contin, snr=snr,iter_done=iter_done)
    mom0[i]=moms[0]
    mom1[i]=moms[1]
    mom2[i]=moms[2]
    mom3[i]=moms[3]
    mom4[i]=moms[4]
  endfor


  mom0_2comp=fltarr(npoints)
  mom1_2comp=fltarr(npoints)
  mom2_2comp=fltarr(npoints)
  mom3_2comp=fltarr(npoints)
  mom4_2comp=fltarr(npoints)
  
  for i=0,npoints-1 do begin
    noise=noise_ini*intens/sn[i]
    prof=prof_ini+prof_ini1+noise
    moms=KS_moments(vscale,prof,contin=contin, snr=snr,iter_done=iter_done)
    mom0_2comp[i]=moms[0]
    mom1_2comp[i]=moms[1]
    mom2_2comp[i]=moms[2]
    mom3_2comp[i]=moms[3]
    mom4_2comp[i]=moms[4]
  endfor


;1comp
  cgdisplay,800,900,wid=18
  plotsym,0,0.7,/fil
  cgplot,sn,mom0,psym=8,layout=[2,3,1],xtit="SN",ytit="Mom0",xr=[5,100],xst=1,yst=1,col="red",yr=[0.92,1.08]*moms_ini_1comp[0],charsize=2
  cgoplot,[sn[0],sn[n_elements(sn)-1]],[moms_ini_1comp[0],moms_ini_1comp[0]],col="blue
  
  cgplot,sn,mom1,psym=8,layout=[2,3,2],xtit="SN",ytit="Mom1",xr=[5,100],xst=1,yst=1,col="red",charsize=2;,yr=[0.92,1.08]*(moms_ini[1]-cent)
  cgoplot,[sn[0],sn[n_elements(sn)-1]],[moms_ini_1comp[1],moms_ini_1comp[1]],col="blue
  
  cgplot,sn,mom2,psym=8,layout=[2,3,3],xtit="SN",ytit="Mom2",xr=[5,100],xst=1,yst=1,col="red",charsize=2;,yr=[0.8,1.2]*(moms_ini[2]-disp)
  cgoplot,[sn[0],sn[n_elements(sn)-1]],[moms_ini_1comp[2],moms_ini_1comp[2]],col="blue
  
  cgplot,sn,mom3,psym=8,layout=[2,3,4],xtit="SN",ytit="Mom3",xr=[5,100],xst=1,yst=1,col="red",charsize=2;,yr=[0.92*moms_ini[4],1.08*moms_ini[3]]
  cgoplot,[sn[0],sn[n_elements(sn)-1]],[moms_ini_1comp[3],moms_ini_1comp[3]],col="blue
  
  cgplot,sn,mom4,psym=8,layout=[2,3,5],xtit="SN",ytit="Mom4",xr=[5,100],xst=1,yst=1,col="red",charsize=2;,yr=[0.8*moms_ini[4],1.2*moms_ini[4]]
  cgoplot,[sn[0],sn[n_elements(sn)-1]],[moms_ini_1comp[4],moms_ini_1comp[4]],col="blue


;2comp

 cgdisplay,800,900,wid=19
  plotsym,0,0.7,/fil
  cgplot,sn,mom0_2comp,psym=8,layout=[2,3,1],xtit="SN",ytit="Mom0",xr=[5,100],xst=1,yst=1,col="red",yr=[0.92,1.08]*moms_ini_2comp[0],charsize=2
  cgoplot,[sn[0],sn[n_elements(sn)-1]],[moms_ini_2comp[0],moms_ini_2comp[0]],col="blue
  
  cgplot,sn,mom1_2comp,psym=8,layout=[2,3,2],xtit="SN",ytit="Mom1",xr=[5,100],xst=1,yst=1,col="red",charsize=2;,yr=[0.92,1.08]*(moms_ini[1]-cent)
  cgoplot,[sn[0],sn[n_elements(sn)-1]],[moms_ini_2comp[1],moms_ini_2comp[1]],col="blue
  
  cgplot,sn,mom2_2comp,psym=8,layout=[2,3,3],xtit="SN",ytit="Mom2",xr=[5,100],xst=1,yst=1,col="red",charsize=2;,yr=[0.8,1.2]*(moms_ini[2]-disp)
  cgoplot,[sn[0],sn[n_elements(sn)-1]],[moms_ini_2comp[2],moms_ini_2comp[2]],col="blue
  
  cgplot,sn,mom3_2comp,psym=8,layout=[2,3,4],xtit="SN",ytit="Mom3",xr=[5,100],xst=1,yst=1,col="red",charsize=2;,yr=[0.92*moms_ini[4],1.08*moms_ini[3]]
  cgoplot,[sn[0],sn[n_elements(sn)-1]],[moms_ini_2comp[3],moms_ini_2comp[3]],col="blue
  
  cgplot,sn,mom4_2comp,psym=8,layout=[2,3,5],xtit="SN",ytit="Mom4",xr=[5,100],xst=1,yst=1,col="red",charsize=2;,yr=[0.8*moms_ini[4],1.2*moms_ini[4]]
  cgoplot,[sn[0],sn[n_elements(sn)-1]],[moms_ini_2comp[4],moms_ini_2comp[4]],col="blue




END