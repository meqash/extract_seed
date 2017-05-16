function av_sig(sig, i, npts, nwin )
parameter(nmax=4000000)
integer n1, n2, j, nav, nwin, npts, i
real av_sig
real sig(nmax)
nav=0
if(nwin>npts)nwin=npts
n1=i-nwin/2
if(n1.lt.1)n1=1
n2=n1+nwin-1
if(n2.gt.npts)n2=npts
n1=n2-nwin+1
av_sig=0
do j=n1,n2 
    if(sig(j).lt.1.e29)then
        av_sig=av_sig+sig(j)
        nav=nav+1
    endif
enddo
if(nav.lt.2)then 
     av_sig=1.e30
else 
     av_sig=av_sig/real(nav)
endif
end function
