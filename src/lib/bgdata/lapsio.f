      subroutine get_laps_lat_lon(dir,ext,nx,ny,lat,lon,istatus)
c
c *** Modified from code generated by gennet (B. Schwartz, FSL)
c
      implicit none
c
      include 'netcdf.inc'
c
      integer nx,ny
      real   lat(nx,ny),
     .       lon(nx,ny)
c
      integer len,elen,i,istatus
      integer start(10),count(10)
     .       ,ncid,ntp,nvdim,nvs,lenstr,ndsize,rcode
      integer vdims(10)
c
      character*(*) dir
      character*(*) ext
      character*31  dummy
c
      logical exists
c_______________________________________________________________________________
c
c *** Create netcdf file name and check for existence.
c
      len=index(dir,' ')-1
      if (dir(len:len) .ne. '/') then
          dir(len+1:len+1)='/'
          len=len+1
      endif
      elen=index(ext,' ')-1
      inquire(file=dir(1:len)//'static.'//ext(1:elen),exist=exists)
      if (.not. exists) then
         print *,' *** LAPS static file does not exist:  ',
     .           dir(1:len)//'static.'//ext(1:elen)
         istatus=0
         return
      endif
c
c *** Open netcdf file.
c
      ncid=ncopn(dir(1:len)//'static.'//ext(1:elen),ncnowrit,rcode)
c
c *** Statements to fill lat, lon.
c
      call ncvinq(ncid,1,dummy,ntp,nvdim,vdims,nvs,rcode)
      lenstr=1
      do i=1,nvdim
         call ncdinq(ncid,vdims(i),dummy,ndsize,rcode)
         lenstr=lenstr*ndsize
         start(i)=1
         count(i)=ndsize
      enddo
      call ncvgt(ncid,1,start,count,lat,rcode)
c
      call ncvinq(ncid,2,dummy,ntp,nvdim,vdims,nvs,rcode)
      lenstr=1
      do i=1,nvdim
         call ncdinq(ncid,vdims(i),dummy,ndsize,rcode)
         lenstr=lenstr*ndsize
         start(i)=1
         count(i)=ndsize
      enddo
      call ncvgt(ncid,2,start,count,lon,rcode)
c
c *** Close netcdf file.
c
      call ncclos(ncid,rcode)
c
      istatus=1
      return
      end
c
c===============================================================================
c
      subroutine get_laps_wind(dir,i4time,ext,nx,ny,nz,uw,vw,istatus)
c
c *** Modified from code generated by gennet (B. Schwartz, FSL)
c
      implicit none
c
      include 'netcdf.inc'
c
      integer nx,ny,nz
      real   uw(nx,ny,nz)   !LAPS 3d u-wind (m/s)
     .      ,vw(nx,ny,nz)   !LAPS 3d v-wind (m/s)
     .      ,gd(nx,ny,nz)
c
      integer len,elen,i4time,i,j,k,istatus
      integer start(10),count(10),
     .        ncid,ntp,nvdim,nvs,lenstr,ndsize,rcode
      integer vdims(10)
c
      character*(*) dir,ext
      character*500 ldir,lext,fname
      character*31  dummy
      character*9   atime
c
      logical exists
c_______________________________________________________________________________
c
c *** Create netcdf file name and check for existence.
c
      call make_fnam_lp(i4time,atime,istatus)
      ldir=dir
      lext=ext
      len=index(ldir,' ')-1
      if (ldir(len:len) .ne. '/') then
         ldir(len+1:len+1)='/'
         len=len+1
      endif
      fname=ldir(1:len)//atime
      len=len+9
c
      elen=index(lext//' ',' ')-1
      if (elen .gt. 0) then
         fname=fname(1:len)//'.'//lext(1:elen)
         len=len+elen+1
      endif
      inquire(file=fname(1:len),exist=exists)
      if (.not. exists) then
         print *,' *** LAPS 3d wind file does not exist:  ',
     .           fname(1:len)
         istatus=0
         return
      endif
c
c *** Open netcdf file.
c
      ncid=ncopn(fname(1:len),ncnowrit,rcode)
c
c *** Statements to fill 3d u-wind (uw).
c
      call ncvinq(ncid,1,dummy,ntp,nvdim,vdims,nvs,rcode)
      lenstr=1
      do i=1,nvdim
         call ncdinq(ncid,vdims(i),dummy,ndsize,rcode)
         lenstr=lenstr*ndsize
         start(i)=1
         count(i)=ndsize
      enddo
      call ncvgt(ncid,1,start,count,gd,rcode)
      do k=1,nz
      do j=1,ny
      do i=1,nx
         uw(i,j,k)=gd(i,j,nz+1-k)
      enddo
      enddo
      enddo
c
c *** Statements to fill 3d v-wind (vw).
c
      call ncvinq(ncid,2,dummy,ntp,nvdim,vdims,nvs,rcode)
      lenstr=1
      do i=1,nvdim
         call ncdinq(ncid,vdims(i),dummy,ndsize,rcode)
         lenstr=lenstr*ndsize
         start(i)=1
         count(i)=ndsize
      enddo
      call ncvgt(ncid,2,start,count,gd,rcode)
      do k=1,nz
      do j=1,ny
      do i=1,nx
         vw(i,j,k)=gd(i,j,nz+1-k)
      enddo
      enddo
      enddo
c
c *** Close netcdf file.
c
      call ncclos(ncid,rcode)
c
      istatus=1
      return
      end
c
c===============================================================================
c
      subroutine get_laps_2d_sfc(dir,i4time,ext,nx,ny,nfld,fldid
     .                      ,grid,istatus)
c
c *** Modified from code generated by gennet (B. Schwartz, FSL)
c
      implicit none
c
      include 'netcdf.inc'
c
      integer nx,ny,nfld
      real   grid(nx,ny,nfld)
      integer fldid(nfld)
c
      integer len,elen,i4time,i,n,istatus
      integer start(10),count(10)
     .       ,ncid,ntp,nvdim,nvs,lenstr,ndsize,rcode
      integer vdims(10)
c
      character*(*) dir,ext
      character*500 ldir,lext,fname
      character*31  dummy
      character*9   atime
c
      logical exists
c_______________________________________________________________________________
c
c *** Create netcdf file name and check for existence.
c
      call make_fnam_lp(i4time,atime,istatus)
      ldir=dir
      len=index(ldir,' ')-1
      if (ldir(len:len) .ne. '/') then
         ldir(len+1:len+1)='/'
         len=len+1
      endif
      fname=ldir(1:len)//atime
      len=len+9
c
      lext=ext
      elen=index(lext,' ')-1
      if (elen .gt. 0) then
         fname=fname(1:len)//'.'//lext(1:elen)
         len=len+elen+1
      endif
      inquire(file=fname(1:len),exist=exists)
      if (.not. exists) then
         print *,' *** LAPS surface file does not exist:  '
     .          ,fname(1:len)
         istatus=0
         return
      endif
c
c *** Open netcdf file.
c
      ncid=ncopn(fname(1:len),ncnowrit,rcode)
c
      do n=1,nfld
c
c *** Statements to fill requested 2d field.
c
         call ncvinq(ncid,fldid(n),dummy,ntp,nvdim,vdims,nvs,rcode)
         lenstr=1
         do i=1,nvdim
            call ncdinq(ncid,vdims(i),dummy,ndsize,rcode)
            lenstr=lenstr*ndsize
            start(i)=1
            count(i)=ndsize
         enddo
         call ncvgt(ncid,fldid(n),start,count,grid(1,1,n),rcode)
c
      enddo
c
c *** Close netcdf file.
c
      call ncclos(ncid,rcode)
c
      istatus=1
      return
      end
c
c===============================================================================
c
      subroutine get_laps_sfc_elev(dir,ext,nx,ny,sht,istatus)
c
c *** Modified from code generated by gennet (B. Schwartz, FSL)
c
      implicit none
c
      include 'netcdf.inc'
c
      integer nx,ny
      real   sht(nx,ny)
c
      integer len,elen,i,istatus
      integer start(10),count(10)
     .       ,ncid,ntp,nvdim,nvs,lenstr,ndsize,rcode
      integer vdims(10)
c
      character*(*) dir
      character*500 ldir
      character*(*) ext
      character*500 lext
      character*31  dummy
c
      logical exists
c_______________________________________________________________________________
c
c *** Create netcdf file name and check for existence.
c
      ldir=dir
      len=index(ldir,' ')-1
      if (ldir(len:len) .ne. '/') then
         ldir(len+1:len+1)='/'
         len=len+1
      endif
      lext=ext
      elen=index(lext,' ')-1
      inquire(file=ldir(1:len)//'static.'//lext(1:elen),exist=exists)
      if (.not. exists) then
         print *,' *** LAPS static file does not exist:  '
     .          ,ldir(1:len)//'static.'//lext(1:elen)
         istatus=0
         return
      endif
c
c *** Open netcdf file.
c
      ncid=ncopn(ldir(1:len)//'static.'//lext(1:elen),ncnowrit,rcode)
c
c *** Statements to fill sht.
c
      call ncvinq(ncid,3,dummy,ntp,nvdim,vdims,nvs,rcode)
      lenstr=1
      do i=1,nvdim
         call ncdinq(ncid,vdims(i),dummy,ndsize,rcode)
         lenstr=lenstr*ndsize
         start(i)=1
         count(i)=ndsize
      enddo
      call ncvgt(ncid,3,start,count,sht,rcode)
c
c *** Close netcdf file.
c
      call ncclos(ncid,rcode)
c
      istatus=1
      return
      end
c
c ============================================================
c
      subroutine get_laps_3d_analysis_data(i4time,nx,ny,nz
     +,phi,t,u,v,sh,omo,istatus)
c
      implicit none

      integer   nx,ny,nz
      integer   i4time
      integer   istatus
      integer   lendlco
      integer   lends
      integer   lendt
      integer   lendw
      integer   lendsh
      integer   i,j,k
      real    r_missing_data
      real    phi(nx,ny,nz),t(nx,ny,nz)
     .       ,u(nx,ny,nz),v(nx,ny,nz),sh(nx,ny,nz)
     .       ,omo(nx,ny,nz)

      real  , allocatable :: om(:,:,:)


      character*255 tempdir,winddir,sfcdir,shdir,lcodir
      character*125 comment
      character*31  tempext,windext,sfcext,shext,lcoext
      character*10  units
      logical found_lowest

      call get_r_missing_data(r_missing_data,istatus)
      if (istatus .ne. 1) then
         print *,'Error getting r_missing_data...Abort.'
         return
      endif

      shext='lq3'
      tempext='lt1'
      windext='lw3'
      sfcext='lsx'
      lcoext='lco'

      call get_directory(tempext,tempdir,lendt)
      call get_directory(windext,winddir,lendw)
      call get_directory(sfcext,sfcdir,lends)
      call get_directory(shext,shdir,lendsh)
      call get_directory(lcoext,lcodir,lendlco)

      call get_laps_3d(i4time,nx,ny,nz
     1  ,tempext,'ht ',units,comment,phi,istatus)

      if (istatus .ne. 1) then
         print *,'Error getting LAPS height data...Abort.'
         return
      endif
c
c *** Get laps temps
c
      call get_laps_3d(i4time,nx,ny,nz
     1  ,tempext,'t3 ',units,comment,t,istatus)

      if (istatus .ne. 1) then
         print *,'Error getting LAPS temp data...Abort.'
         return
      endif
c
c *** Get laps spec humidity
c
      call get_laps_3d(i4time,nx,ny,nz
     1  ,shext,'sh ',units,comment,sh,istatus)

      if(istatus .ne. 1)then
         print*,'Error getting LAPS sh  data ... Abort.'
         return
      endif
C    The specific humidity field uses the missing value
c    for below ground points, so we need to fill those in by
c    replicating the lowest valid value downward (upward in array
c    space).

      DO j = 1, ny
        DO i = 1, nx
          k = 1
          found_lowest = .false.
      
          DO WHILE (.NOT. found_lowest) 
            IF (sh(i,j,k) .lt. 1.e37) THEN
              found_lowest = .true.
              sh(i,j,1:k) = sh(i,j,k)
            ELSE
              k = k + 1
              IF (k .ge. nz) THEN
                PRINT *, 'No valid SH found in column!'
                PRINT *, 'I/J = ', i,j
                STOP
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO

c    Make sure we got it right!
c      print *, 'Min/Max/Center values of SH:'  
c      DO k = 1, nz
c        print *, minval(sh(:,:,k)),maxval(sh(:,:,k)),
c     +           sh(nx/2,ny/2,k)
c      ENDDO
c
c *** Get laps cloud omega
c
      call get_laps_3d(i4time,nx,ny,nz
     1  ,lcoext,'com',units,comment,omo,istatus)

      if(istatus .ne. 1)then
         print*,'No LAPS Cld Omega data ....'
         print*,'Initializing omo array with zero'
         call zero3d(omo,nx,ny,nz)
      endif
c
c *** Get laps wind data.
c
      call get_laps_3d(i4time,nx,ny,nz
     1  ,windext,'u3 ',units,comment,u,istatus)

      if (istatus .ne. 1) then
         print *,'Error getting LAPS time 0 u3 data...Abort.'
         return
      endif

      call get_laps_3d(i4time,nx,ny,nz
     1  ,windext,'v3 ',units,comment,v,istatus)

      if (istatus .ne. 1) then
         print *,'Error getting LAPS time 0 v3 data...Abort.'
         return
      endif

      allocate (om(nx,ny,nz))

      call get_laps_3d(i4time,nx,ny,nz
     1	,windext,'om ',units,comment,om,istatus)

      if (istatus .ne. 1) then
	 print *,'Error getting LAPS time 0 v3 data...Abort.'
	 return
      endif
      ! BLS commented this out as a test on 11/19/02
      !where(omo .eq. r_missing_data)omo=om

      deallocate(om)
      return
      end