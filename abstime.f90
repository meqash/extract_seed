!subroutine abstime(n1,nnh,nzhour,nzmin,nzsec,nzmsec,npts,dt,sig,sigo)
subroutine abstime(nloc,nzhour,nzmin,nzsec,sachead,sig,sigo)
use sacio
implicit none
type(sac_head) :: sachead
real deltat
real sig(sachead%npts),sigo(sachead%npts),dt
integer nloc,nzhour,nzmin,nzsec,nzmsec,npts
integer n1temp
dt=sachead%delta
if(nzhour.ge.sachead%nzhour)then
   deltat=mod(nzmsec,int(dt*1000))/1000.0
else
   deltat=mod(1000-nzmsec,int(dt*1000))/1000.0
endif
nzmsec=int(nzmsec/dt/1000)*dt*1000
!n1temp=t1/dt
!do i=1,npts
!   sigo(i)=sig(i)+deltat*(sig(i+1)-sig(i))/dt
!enddo
sigo=sig
nloc=int(((sachead%nzhour-nzhour)*60.0*60.0+(sachead%nzmin-nzmin)*60.0+(sachead%nzsec-nzsec)*1.0+sachead%nzmsec*1.0/1000.0)/dt)+1
!write(*,'("kztime=",4i,1x,"nnh=",1i)')nzhour,nzmin,nzsec,nzmsec,nnh
end subroutine
