! extract three components of sac file from seeds file or miniseed file
! 2016-10-26
program extract_earthquake_seed
use sacio
implicit none
type(sac_head)::     sachead,sachead1,sacheado
integer,parameter::  nn=4000000,nstmax=1000,nfmax=1000,neqmax=1000
integer::            seed_type ! 1 for seed; 0 for miniseed
integer::            nh1,nh2,npart,nnh1,n1
integer::            i,j,nsta,error,ifile,ic,ieq
integer year_b,day_b,year_e,day_e,dhour
integer do_rm_response,do_decimate,imark,npts
integer year,month,day,hour,mmin,sec,msec,dsec
integer nlen,nzhour,nzmin,nzsec,nzmsec,nerr,nzero
integer is,ih,iy,id,dayb,daye,jday,nh,nnh,nhh1,icom
character(80)sta(nstmax)
character(80)net(nstmax)
character(3)com(3),co
character(10)nd,year_day
character(80)str,tra,bash,resp,mdata
character(180)command,saclist,file
character(80)para,dirin,dirout,sta_list,eq_list
character(80)seed,sac,dir,sac1,sac2,sac_tmp
character(80)dir_seed,dir_sac,dir_resp,dir_mdata
character(8)kh,kho
character(8)khole
real sig(nn),sigall(nn),dt,beg,stla,stlo,sigo(nn)
real sigout(nn),evla,evlo,mag,evdp
real f1,f2,dt0,mean,avg
logical ext,ext1,ext2

interface
   function av_sig(sig, i, npts, nwin )
   integer,intent(in) :: i,npts,nwin
   real,dimension(4000000):: sig
   end function
end interface

str='extract.bash'
if(iargc().ne.1)then
   write(*,*)'Usage: extract_seed param.dat'
   write(*,'("param.dat:")')
   write(*,'("station list")')
   write(*,'("earthquake list")')
   write(*,'("seed_type dsec icom component (data type,length of find in seconds, &
        number of components;component &
        :seed_type (1 for seed, 0 for miniseed)")')
   write(*,'("f1   f2  do_rm_response do_decimate(do remove response (1) or not (0))")')
   write(*,'("dir_of_seed")')
   write(*,'("dir_of_resp")')
   write(*,'("dir_of_mdata")')
   write(*,'("dir_of_output")')
   call exit(-1)
endif
i=1
call getarg(1,para)
open(9,file=para)
read(9,'(a80)')sta_list
read(9,'(a80)')eq_list
read(9,*)seed_type,dsec,icom,co
read(9,*)f1,f2,do_rm_response,do_decimate
read(9,'(a80)')dir_seed
read(9,'(a80)')dir_resp
read(9,'(a80)')dir_mdata
read(9,'(a80)')dir_sac
close(9)
if(icom.eq.1)then
   com(1)=co
else
   com(1)=trim(co)//'Z'
   com(2)=trim(co)//'N'
   com(3)=trim(co)//'E'
endif
open(10,file=sta_list)
do i=1,nstmax
   read(10,*,end=12,err=12)sta(i),net(i)
enddo
12  close(10)
nsta=i-1
!write(*,*)'Number of station is ',nsta
open(12,file=eq_list)
write(command,'("mkdir -p",1x,1a,1x,"2>/dev/null")')trim(dir_sac)
do ieq=1,neqmax                                  ! loop over year
   read(12,*,end=14,err=14)year,month,day,hour,mmin,sec,msec,evla,evlo,evdp,mag
   write(seed,'("seed/",i4.4,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i3.3,".seed")')&
   year,month,day,hour,mmin,sec,msec
   inquire(file=seed,exist=ext)
   if (.not.ext)cycle                        ! if the seed file exists
   !write(*,*)"extract seed from ",trim(seed)
   call system(command)            ! make output directory
   if(seed_type.eq.1)then          ! if it is the seed file
      do is=1,nsta                 ! loop over station
         do ic=1,icom              ! loop over component
            sigall=1e30
            !write(saclist,'(i4.4,"_",1a,"_",1a,"_",1a,".list")')year,trim(net(is)),trim(sta(is)),com(ic)
            write(saclist,'(i4.4,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i3.3,"_",1a,"_",1a,"_",1a,".list")')&
            year,month,day,hour,mmin,sec,msec,trim(net(is)),trim(sta(is)),trim(com(ic))
            write(*,*)'saclist=',trim(saclist)
            write(sac1,'(i4.4,".*.",1a,".",1a,".*.",1a,".*.SAC")')year,trim(net(is)),trim(sta(is)),trim(com(ic))
            write(*,*)'sac1=',trim(sac1)
            write(sac,'(1a,"/",i4.4,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i3.3,"_",1a,"_",1a,"_",1a,".SAC")')&
            trim(dir_sac),year,month,day,hour,mmin,sec,msec,trim(net(is)),trim(sta(is)),trim(com(ic))
            write(command,'("rm",1x,1a," 2>/dev/null")')trim(bash)
            call system(command)     ! rmove the bash
            inquire(file=sac,exist=ext)
            if (ext)cycle
            !write(bash,'(i4.4,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i3.3,"_",1a,".",1a)')&
            !year,month,day,hour,mmin,sec,msec,trim(sta(is)),trim(str)
            write(bash,'(i4.4,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i3.3,"_",1a,"_",1a,"_",1a,".bash")')&
            year,month,day,hour,mmin,sec,msec,trim(net(is)),trim(sta(is)),trim(com(ic))
            open(20,file=bash)
            write(20,'("#!/bin/bash")')
            write(20,'("rdseed.linux <<eof 1>/dev/null 2>&1")')
            write(20,'(1a)')trim(seed)
            write(20,'("")')
            write(20,'("")')
            write(20,'("d")')
            write(20,'("")')
            write(20,*)trim(sta(is))
            write(20,'(a3)')com(ic)
            write(20,'("")')
            write(20,'("")')
            write(20,'("")')
            write(20,'("")')
            write(20,'("")')
            write(20,'("")')
            write(20,'("")')
            write(20,'("")')
            write(20,'("")')
            write(20,'("")')
            write(20,'("Y")')
            write(20,'("Quit")')
            write(20,'("eof")') 
            close(20)
            write(command,'("bash",1x,1a)')trim(bash)
            call system(command) ! extract sac file
            write(command,'("rm",1x,1a," 2>/dev/null")')trim(bash)
            call system(command)     ! rmove the bash
            open(20,file=bash) 
            write(20,'("#!/bin/bash")')
            write(20,'("rm",1x,1a,1x,"2>/dev/null")')trim(saclist)
            write(20,'("n=`ls",1x,1a,1x,"2>/dev/null | wc -l`")')trim(sac1)
            write(20,'("if [ $n -ge 1 ];then")')
            write(20,'("     ls",1x,1a,1x,">",1a)')trim(sac1),trim(saclist)
            write(20,'("fi")')
            close(20)
            write(command,'("bash",1x,1a)')trim(bash)
            call system(command)  ! write all sac in to saclist
            write(command,'("rm",1x,1a," 2>/dev/null")')trim(bash)
            call system(command)     ! rmove the bash
            inquire(file=trim(saclist),exist=ext)  
            imark=0;nlen=0;dt=0;beg=0
            sig=0;stla=0;stlo=0;nzhour=0;nzsec=0;nzmsec=0
            if(.not.ext)cycle! if the sac file list exists
            open(30,file=trim(saclist))
            do ifile=1,nfmax
               read(30,'(1a180)',err=20,end=20)file
               write(*,*)'Read file ',trim(file)
               call read_sachead(trim(file),sachead,nerr)
               if(nerr.eq.-1)cycle              ! read file incorrectly
               if(imark==0)then
                  kho=trim(sachead%khole(1:2))
                  imark=imark+1
                  dt0=sachead%delta
                  sacheado=sachead
                  sacheado%evla=evla
                  sacheado%evlo=evlo
                  sacheado%evdp=evdp
                  sacheado%mag=mag
               endif
               if(sachead%khole(1:2)/=kho)cycle
               if(dt0.ne.sachead%delta)cycle
               write(*,*)'Read file correctly1'
               call read_sac(trim(file),sig,sachead,nerr)
               !write(*,*)'Read file correctly2'
               if(nerr.eq.-1)cycle              ! read file incorrectly
               write(*,*)'Read file correctly'
               call abstime(n1,sachead,sachead,sig,sigo)
               write(*,*)'Read file correctly'
               if(do_rm_response.eq.1)then
                  write(resp,'("RESP.",1a,".",1a,".",1a,".",1a)')trim(net(is)),trim(sta(is)),trim(kho),trim(com(ic))
                  if(kho.eq."-1")write(resp,'("RESP.",1a,".",1a,"..",1a)')trim(net(is)),trim(sta(is)),trim(com(ic))
                  write(*,*)'remove response ',trim(resp), " from ",trim(file)
                  call rm_resp(trim(bash),trim(file),trim(resp),f1,f2,error)
                  if(error.eq.-1)then !  response wrong
                     write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(file)
                     write(*,'("Please redownload the response file for file ",1a)') seed
                     call system(command)
                     stop
                     cycle
                  endif
               endif  ! remove response
               do i=1,sachead%npts
                  if(n1.ge.1.and.sigall(i+n1-1).ge.1e29)sigall(i+n1-1)=sigo(i)
               enddo
            enddo               ! end loop over each sac file
         20 close(30)
            npts=int(dsec/dt0)           ! number of points of each segments 
            sacheado%npts=npts
            nzero=0 
            do i=1,npts
               if(sigall(i).gt.1e29)nzero=nzero+1 
            enddo
            if(nzero.gt.npts/2)cycle     ! ignore the data with number of zeros bigger than half of the npts
            do i=1,npts
               npart=16
               avg=sigall(i)
               do while (avg.gt.1.e29.and.npart.gt.1)
                  avg=av_sig(sigall,i,npts,npts/npart)
                  npart = npart/2
               enddo
               sigall(i) = avg
               if(npart.eq.1 )then
                  avg = 0.
               endif 
            enddo                       ! end loop over points
            sigout(1:npts)=sigall(1:npts)
            write(*,*)'Write to file ',trim(sac)
            call write_sac(trim(sac),sigout,sacheado,nerr)
            if(do_decimate.eq.1)call decimate(trim(sac),dt0) ! decimate the data to 10Hz
            write(command,'("rm",1x,1a,1x,"2>/dev/null")') trim(saclist)
            call system(command)
            write(command,'("rm",1x,1a,1x,"2>/dev/null")') trim(bash)
            call system(command)
            write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(sac1)
            call system(command)
         enddo      ! loop over comopnonet   
      enddo         ! loop over station
   else             ! if the file is miniseed
      write(mdata,'("mdat/",i4.4,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i3.3,".mdata")')&
      year,month,day,hour,mmin,sec,msec
      inquire(file=mdata,exist=ext)
      if(.not.ext)cycle
      write(command,'("mseed2sac ",1a,1x,"-m",1x,1a,1x,"1>/dev/null 2>&1")')trim(seed),trim(mdata) 
      call system(command)  ! extract miniseed file
      do is=1,nsta          ! loop over station
         do ic=1,icom       ! loop over component
            write(saclist,'(i4.4,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i3.3,"_",1a,"_",1a,"_",1a,".list")')&
            year,month,day,hour,mmin,sec,msec,trim(net(is)),trim(sta(is)),trim(com(ic))
            write(bash,'(i4.4,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i3.3,"_",1a,"_",1a,"_",1a,".bash")')&
            year,month,day,hour,mmin,sec,msec,trim(net(is)),trim(sta(is)),trim(com(ic))
            write(sac_tmp,'(1a,".",1a,".*.",1a,".*.",i4.4,".*.SAC")')&
            trim(net(is)),trim(sta(is)),trim(com(ic)),year
            write(sac,'(1a,"/",i4.4,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i3.3,"_",1a,"_",1a,"_",1a,".SAC")')&
            trim(dir_sac),year,month,day,hour,mmin,sec,msec,trim(net(is)),trim(sta(is)),trim(com(ic))
            inquire(file=sac,exist=ext)
            if(ext)cycle
            open( 20,file=bash) 
            write(20,'("#!/bin/bash")')
            write(20,'("rm",1x,1a,1x,"2>/dev/null")')trim(saclist)
            write(20,'("n=`ls",1x,1a,1x,"2>/dev/null | wc -l`")')trim(sac_tmp)
            write(20,'("if [ $n -ge 1 ];then")')
            write(20,'("     ls",1x,1a,1x,">",1a)')trim(sac_tmp),trim(saclist)
            write(20,'("fi")')
            close(20)
            write(command,'("bash",1x,1a)')trim(bash)
            call system(command)     ! write sac file to saclist
            write(command,'("rm",1x,1a," 2>/dev/null")')trim(bash)
            call system(command)     ! rmove the bash
            inquire(file=trim(saclist),exist=ext) 
            if(.not.ext)cycle ! if the sac file list exists
            imark=0
            sigall=1e30
            open(100,file=trim(saclist))
            call initial_sachead(sacheado)
            do ifile=1,nfmax
               read(100,'(1a180)',err=16,end=16)file !read sac file list  
               write(*,*)"Read file: ",trim(file)
               call read_sachead(trim(file),sachead,nerr)
               if(nerr.eq.-1)cycle
               if(imark==0)then
                  kho=sachead%khole(1:2)
                  dt0=sachead%delta
                  imark=imark+1
                  sacheado=sachead
                  sacheado%evla=evla
                  sacheado%evlo=evlo
                  sacheado%evdp=evdp
                  sacheado%mag=mag
               endif
               if(sachead%delta.ne.dt0)cycle
               if(sachead%khole(1:2).ne.kho)cycle
!      ******************************************** remove response before paste
               if(do_rm_response.eq.1)then
                  write(resp,'(1a,"/RESP.",1a,".",1a,".",1a,".",1a)')&
                  trim(dir_resp),trim(net(is)),trim(sta(is)),trim(kho),trim(com(ic))
                  if(kho.eq."-1")write(resp,'(1a,"/RESP.",1a,".",1a,"..",1a)')trim(dir_resp),&
                  trim(net(is)),trim(sta(is)),trim(com(ic))
                  write(*,*)'remove response ',trim(resp), " from ",trim(file)
                  call rm_resp(bash,trim(file),trim(resp),f1,f2,error)
                  if(error.eq.-1)then !  response wrong
                     write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(file)
                     write(*,'("Please redownload the response file for file ",1a)') seed
                     call system(command)
                     stop
                     cycle
                  endif
               endif
!      ******************************************** remove response done
               call read_sac(trim(file),sig,sachead,nerr)
               if(nerr.eq.-1)cycle
               call abstime(n1,sacheado,sachead,sig,sigo) 
               !write(*,*)'n1=',n1
               do i=1,sachead%npts
                  if(i+n1-1.gt.0.and.sigall(i).gt.1e29)then
                     sigall(i+n1-1)=sigo(i)
                  endif
               enddo          ! loop over points
            enddo             ! loop over sacfile 
         16 close(100)         ! done reading the temporary sac files
            npts=int(dsec/dt0) ! number of points of each segments 
            sacheado%npts=npts
            nzero=0
            do i=1,npts
               if (sigall(i).gt.1.e29) nzero=nzero+1
            enddo
!            if(real(nzero).gt.real(npts/2))cycle
            do i=1,npts
               avg=sigout(i)
               npart=16
               do while(avg.gt.1.e29.and.npart.gt.1)
                  avg=av_sig(sigout,i,npts,npts/npart)
                  npart = npart/2
               enddo
               if(npart.eq.1)avg = 0.
               sigout(i) = avg
            enddo        ! loop over points
            sigout(1:npts)=sigall(1:npts)
            write(*,*)'Write to file ',trim(sac)
            call write_sac(trim(sac),sigout,sacheado,nerr)
            if(do_decimate.eq.1)call decimate(trim(sac),dt0) 
         enddo       ! loop over  component
         write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(saclist)
         call system(command) ! remove sac file list
         write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(bash)
         call system(command) ! remove command scripts
         write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(sac_tmp)
         call system(command) ! remove temporary file
      enddo                      ! loop over station
   endif                         ! different seed type
enddo                            ! loop over year
14 close(12)
end program
