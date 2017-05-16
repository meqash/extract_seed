!subroutine abstime(n1,nnh,nzhour,nzmin,nzsec,nzmsec,npts,dt,sig,sigo)
subroutine abstime(n1,sacheado,sachead,sig,sigo)
use sacio
implicit none
integer,parameter :: nn=4000000
type(sac_head) :: sachead,sacheado
real deltat
real sig(nn),sigo(nn),dt
integer n1,nnh,nzhour,nzmin,nzsec,nzmsec,npts
integer n1temp
dt=sachead%delta
nzhour=sachead%nzhour
nzmin=sachead%nzmin
nzsec=sachead%nzsec
nzmsec=sachead%nzmsec
write(*,*)'hello in abstime'
if(nzhour.ge.sacheado%nzhour)then
   deltat=mod(nzmsec,int(dt*1000))/1000.0
else
   deltat=mod(1000-nzmsec,int(dt*1000))/1000.0
endif
nzmsec=int(nzmsec/dt/1000)*dt*1000
!n1temp=t1/dt
!do i=1,npts
!       sigo(i)=sig(i)+deltat*(sig(i+1)-sig(i))/dt
!enddo
sigo(1:nn)=sig(1:nn)
n1=int(((sachead%nzhour-sacheado%nzhour)*60.0*60.0+(nzmin-sacheado%nzmin)*60.0+&
(nzsec-sacheado%nzsec)*1.0+(nzmsec-sacheado%nzmsec)*1.0/1000.0)/dt)+1
!write(*,'("kztime=",4i,1x,"nnh=",1i)')nzhour,nzmin,nzsec,nzmsec,nnh
end subroutine
