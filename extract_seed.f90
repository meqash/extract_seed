! extract three components of sac file from seeds file or miniseed file
program main
use sacio
implicit none
integer nn,nstmax,nfmax
type(sac_head):: sachead,sachead1
parameter (nn=4000000,nstmax=1000,nfmax=1000)
integer seed_type ! 1 for seed; 0 for miniseed
integer nh1,nh2,npart,nnh1,n1
integer i,j,nsta,error,ifile,ic
integer year_b,day_b,year_e,day_e,dhour
integer do_rm_response,do_decimate,imark,npts
integer nlen,nzhour,nzmin,nzsec,nzmsec,nerr,nzero
integer is,ih,iy,id,dayb,daye,jday,nh,nnh,nhh1,icom
character(80)sta(nstmax)
character(80)net(nstmax)
character(3)com(3),co
character(10)nd,year_day
character(80)str,tra,bash,resp,mdata
character(180)command,saclist,file
character(80)para,dirin,dirout,list
character(80)seed,sac,dir,sac1,sac2,sac_tmp
character(80)dir_seed,dir_sac,dir_resp,dir_mdata
character(8)kh,kho
character(8)khole
real sig(nn),sigall(nn),dt,beg,stla,stlo,sigo(nn)
real sigout(nn)
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
   write(*,'("year_b day_b year_e day_e")')
   write(*,'("seed_type dhour icom component (length of output SAC &
        file in hour; number of components;component &
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
read(9,'(a80)')list
read(9,*)year_b,day_b,year_e,day_e
read(9,*)seed_type,dhour,icom,co
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
open(10,file=list)
do i=1,nstmax
   read(10,*,end=12,err=12)sta(i),net(i)
enddo
12  close(10)
nsta=i-1
write(*,*)'Number of station is ',nsta
nh=24/dhour ! number of segments
!write(*,*)'nh=',nh
do iy=year_b,year_e                                  ! loop over year
   write(*,*)'extract data from ',year_b, 'to', year_e
   jday=365
   if(mod(iy,4).eq.0.and.mod(iy,100).ne.0.or.mod(iy,400).eq.0)jday=366  !leap year
   dayb=day_b
   if(iy.ne.year_b)dayb=1
   daye=day_e
   if(iy.ne.year_e)daye=jday
   do id=dayb,daye                                ! loop over day
      write(year_day,'(i4.4,"_",i3.3)')iy,id
      seed=trim(dir_seed)//'/'//trim(year_day)//'.seed'
      write(*,'(i4.4,"_",i3.3," seed:",1a)')iy,id,trim(seed)
      inquire(file=seed,exist=ext)
      if (.not.ext)cycle                        ! if the seed file exists
      write(command,'("mkdir -p",1x,1a,1x,"2>/dev/null")')trim(dir_sac)//'/'//trim(year_day)
      call system(command)            ! make output directory
      if(seed_type.eq.1)then          ! if it is the seed file
         do ih=1,nh                  ! loop over daily segments
            write(*,*)'deal with ',iy, id
            nnh=(ih-1)*dhour
            nnh1=nnh+dhour-1
            write(nd,'("_",i2.2)')nnh
            do is=1,nsta            ! loop over station
               do ic=1,icom    ! loop over component
                  sigall=1e30
                  write(saclist,'(1a,"_",1a,"_",1a,".list")')trim(year_day),trim(sta(is)),com(ic)
                  write(sac1,'(i0,".",i3.3,"*",1a,"*",1a,"*SAC")')iy,id,trim(sta(is)),com(ic)
                  write(*,*)'sac1=',trim(sac1)
                  !sac=trim(dir_sac)//'/'//trim(year_day)//'/'//trim(year_day)//trim(nd)//'_'//trim(sta(is))//'_'//trim(com(ic))//'.SAC'
                  write(sac,'(1a,"/",i4.4,"_",i3.3,"/",i4.4,"_",i3.3,"_",i2.2,"_",1a,"_",1a,"_",1a,".SAC")')&
                  trim(dir_sac),iy,id,iy,id,nnh,trim(net(is)),trim(sta(is)),com(ic)
                  inquire(file=sac,exist=ext)
                  if (ext)cycle
                  bash=trim(year_day)//"_"//trim(sta(is))//trim(str)
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
                  write(20,'(i0,",",i0,",",i0,":00:00.0000")')iy,id,nnh
                  write(20,'(i0,",",i0,",",i0,":59:59.9999")')iy,id,nnh1
                  write(20,'("")')
                  write(20,'("Y")')
                  write(20,'("Quit")')
                  write(20,'("eof")') 
                  close(20)
                  write(command,'("bash",1x,1a)')trim(bash)
                  !write(*,*)'extracting file'
                  call system(command) ! extract sac file
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
                  inquire(file=trim(saclist),exist=ext)  
                  imark=0;nlen=0;dt=0;beg=0
                  sig=0;stla=0;stlo=0;nzhour=0;nzsec=0;nzmsec=0
                  if(.not.ext)cycle! if the sac file list exists
                  open(10,file=trim(saclist))
                  do ifile=1,nfmax
                     read(10,'(1a180)',err=20,end=20)file
                     !write(*,*)'Read file ',trim(file)
                     !call readsac(trim(file),sig,nlen,dt,beg,stla,stlo,&
                     !nzhour,nzmin,nzsec,nzmsec,khole,nerr) 
                     if(imark==0)then
                        call read_sachead(file,sachead,nerr)
                        kho=trim(sachead%khole(1:2))
                        imark=imark+1
                        dt0=sachead%delta
                        write(*,'("dt0=",f9.5)')dt0
                     else   
                        call read_sachead(file,sachead1,nerr)
                     endif
                     kh=sachead1%khole(1:2)
                     if(nerr.ne.0)exit ! read file correctly
                     if(kh.ne.kho)exit
                     if(dt0.ne.dt)exit
                     write(*,*)"read file ",trim(file)," khole=",trim(kh)
                     call read_sachead(file,sig,sachead,nerr)
                     call abstime(n1,nnh,sachead,sig,sigo)
                     write(*,'("dt0=",f9.5)')dt0
                     write(*,'("n1=",1i7)')n1
                     write(*,*)"hello 0,dt0=",dt0
                     do i=1,nlen 
                        if(n1.ge.1.and.sigall(i+n1-1).ge.1e29)sigall(i+n1-1)=sig(i)
                     enddo
                  enddo ! loop over file
               20 close(10)
                  write(*,*)"hello 1.5,dt0=",dt0
                  !npts=int(dhour*3600/dt0)+1
                  npts=int(dhour*3600/dt0)+1
                  nzero=0 
                  do i=1,npts
                     if(sigall(i).gt.1e29)nzero=nzero+1 
                  enddo
                  write(*,'("nzero=",1i7)')nzero
                  write(*,*)"hello 2,dt0=",dt0
                  write(*,'("dt0=",f9.5)')dt0
                  if(nzero.gt.npts/2)cycle ! ignore the data with number of zeros bigger than half of the npts
                  do i=1,npts
                     npart=16
                     avg=sigall(i)
                     do while (avg.gt.1.e29.and.npart.gt.1)
                        avg=av_sig(sigall,i,npts,npts/npart)
                        npart = npart/2
                     enddo
                     sigall(i) = avg
                     if ( npart.eq.1 )then
                        avg = 0.
                     endif 
                  enddo ! loop over points
                  write(*,*)'write to file ',trim(sac)," khole=",kh
                  !call wrsac(trim(sac),dt0,sigall,npts,stla,&
                  !stlo,iy,id,nnh,0,0,0,trim(sta(is)),&
                  !trim(net(is)),trim(com(ic)),kho,nerr)
                  !call write_ext_sac(trim(sac),sigall,sachead,npts)
                  call write_ext_sac(trim(sac),sigout,sachead,iy,id,nnh,0,0,0,kho,nerr)
                  write(*,'("dt=",f9.5)')dt
                  if(do_decimate.eq.1)call decimate(trim(sac),dt0) ! decimate the data to 10Hz
                  if(do_rm_response.eq.1)then
                     write(resp,'("RESP.",1a,".",1a,".",1a,".",1a)')&
                     trim(net(is)),trim(sta(is)),trim(kho),trim(com(ic))
                     write(*,'("resp:",1x,1a)')trim(resp)
                     if(trim(kho).eq."a")&
                     write(resp,'("RESP.",1a,".",1a,"..",1a)')&
                     trim(net(is)),trim(sta(is)),trim(com(ic))
                     write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(bash)
                     call system(command)
                     call rm_resp(trim(bash),trim(sac),trim(resp),f1,f2,error)
                  endif
                  write(*,'("dt=",f9.5)')dt0
                  write(*,'("station:",1x,1a)')trim(sta(is))
                  write(*,'("network:",1x,1a)')trim(net(is))
                  write(*,'("component:",1x,1a)')trim(com(ic))
                  if(error.eq.-1)then
                     write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(sac)
                     call system(command)
                  endif
                  write(command,'("rm",1x,1a,1x,"2>/dev/null")') trim(saclist)
                  call system(command)
                  write(command,'("rm",1x,1a,1x,"2>/dev/null")') trim(bash)
                  call system(command)
                  write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(sac1)
                  call system(command)
               enddo      ! loop over comopnonet             
            enddo         ! loop over station
         enddo            ! loop over daily segments
      else                ! if the file is miniseed
         write(mdata,'(1a,"/",i4.4,"_",i3.3,".mdata")')trim(dir_mdata),iy,id
         write(*,'(1a,"/",i4.4,"_",i3.3,".mdata")')trim(dir_mdata),iy,id
         inquire(file=mdata,exist=ext1)
         if(.not.ext1)cycle
         write(command,'("mseed2sac ",1a,1x,"-m",1x,1a,1x,"1>/dev/null 2>&1")')trim(seed),trim(mdata) 
         call system(command)  ! extract miniseed file
         do is=1,nsta          ! loop over station
            do ic=1,icom       ! loop over component
               write(saclist,'(1a,"_",1a,"_",1a,".list")')trim(year_day),trim(sta(is)),com(ic)
               write(bash,'(1a,"_",1a,"_",1a,".bash")')trim(year_day),trim(sta(is)),trim(com(ic))
               write(sac_tmp,'(1a,".",1a,".*.",1a,".*.",i4.4,".",i3.3,".*.SAC")')&
               trim(net(is)),trim(sta(is)),trim(com(ic)),iy,id
               
               open(20,file=bash) 
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
               open(30,file=trim(saclist))
               do ifile=1,nfmax
                  read(30,'(1a180)',err=16,end=16)file !read sac file list  
                  if(imark==0)then
                     call read_sachead(trim(file),sachead,nerr)
                     if(nerr.eq.-1)cycle
                     kho=sachead%khole(1:2)
                     dt0=sachead%delta
                     imark=imark+1
                     sachead1=sachead
                  else
                     call read_sachead(trim(file),sachead1,nerr)
                     if(nerr.eq.-1)cycle
                  endif
                  if(sachead1%delta.ne.dt0)cycle
                  if(sachead1%khole(1:2).ne.kho)cycle
!      ******************************************** remove response before paste
                  if(do_rm_response.eq.1)then
                     write(resp,'(1a,"/resp_",1a,"/RESP.",1a,".",1a,".",1a,".",1a)')&
                     trim(dir_resp),trim(year_day),trim(net(is)),trim(sta(is)),trim(kho),trim(com(ic))
                     if(kho.eq."-1")write(resp,'(1a,"/resp_",1a,"/RESP.",1a,".",1a,"..",1a)')trim(dir_resp),&
                     trim(year_day),trim(net(is)),trim(sta(is)),trim(com(ic))
                     !write(command,'("cp",1x,1a,1x,"./")')trim(resp)
                     !write(*,*)'Response file is ',trim(resp)
                     !call system(command)
                     write(*,*)'remove response ',trim(resp), " from ",trim(file)
                     call rm_resp(bash,trim(file),trim(resp),f1,f2,error)
                     if(error.eq.-1)then !  response wrong
                        write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(file)
                        call system(command)
                        cycle
                     endif
                     !write(command,'("rm",1x,1a)')trim(resp)
                     !call system(command)
                  endif
!      ******************************************** remove response done
                  call read_sac(trim(file),sig,sachead1,nerr)
                  if (nerr.eq.-1)then
                     write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(file)
                     call system(command)
                     cycle
                  endif
                  call abstime(n1,0,0,0,sachead1,sig,sigo) 
                  !find absolute location of the first dot
                  do i=1,sachead1%npts
                     if(i+n1-1.gt.0.and.sigall(i).gt.1e29)then
                        sigall(i+n1-1)=sigo(i)
                     endif
                  enddo ! loop over points
               enddo    ! loop over sacfile 
            16 close(30) ! done reading the temporary sac files
               !write(*,'("kho=",1a)')trim(kho)
               !npts=int(dhour*3600/dt0)+1 ! number of points of each segments 
               npts=int(dhour*3600/dt0) ! number of points of each segments 
               do ih=1,nh
                  nnh=(ih-1)*dhour
                  write(sac,'(1a,"/",1a,"/",1a,"_",i2.2,"_",1a,"_",1a,"_",1a,".SAC")')&
                  trim(dir_sac),trim(year_day),trim(year_day),nnh,trim(net(is)),trim(sta(is)),trim(com(ic))
                  inquire(file=sac,exist=ext)
                  if (ext)cycle
                  nh1=(ih-1)*npts+1 
                  nh2=nh1+npts-1
                  nzero=0
                  do i=nh1,nh2
                     if (sigall(i).gt.1.e29) nzero=nzero+1
                  enddo
                  if(real(nzero).lt.real(npts/2))then
                     write(*,*)'Write to file ',trim(sac)
                     do i=1,npts
                        sigout(i)=sigall(nh1+i-1)
                     enddo
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
                     call write_ext_sac(trim(sac),sigout,sachead,npts,iy,id,nnh,0,0,0,kho,nerr)
                     if(do_decimate.eq.1)call decimate(trim(sac),dt0) 
                     ! decimate the data to 10Hz
                  endif    ! if the number of zeros is small
               enddo       ! loop over  every hour segments
               write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(saclist)
               call system(command) ! remove sac file list
               write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(bash)
               call system(command) ! remove command scripts
               write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(sac_tmp)
               call system(command) ! remove temporary file
            enddo                   ! loop over components
         enddo                      ! loop over station
      endif                         ! different seed type
   enddo                            ! loop over day
enddo                               ! loop over year
end program
