      program bv_atm_norm
!     implicit none

!     generate the bv_norm for coupled model

      PARAMETER (IM = 180)
      PARAMETER (JM =1080)
      PARAMETER (LM =  72)

      REAL*8  ak(lm+1)        
      REAL*8  bk(lm+1)        
      REAL*8 q1(im,jm), qo1(im,jm)
      REAL*8 q2(im,jm), qo2(im,jm)
      REAL*8 qq(im,jm)
      REAL*8 pertcoef
      integer headr1(6)
      integer headr2(5)
      character(len=80)  bfile1, bfile2

c     bfile1='fvcore_internal_rst11' 
c     bfile2='fvcore_internal_rst12' 
c     bfile='fvcore_internal_rst'
c     
c     open(11,file=trim(bfile1),status='old')
c     open(12,file=trim(bfile2),status='old')
c     open(13,file=trim(bfile),status='old')
      pertcoef=8.0
c     pertcoef=248.0

      DO K = 1, 3
         read (10+K) headr1
         read (10+K) headr2
         read (10+K) ak
         read (10+K) bk
      ENDDO

         write(31) headr1
         write(31) headr2
         write(31) ak
         write(31) bk

         write(32) headr1
         write(32) headr2
         write(32) ak
         write(32) bk

         write(6,*) headr1
         write(6,*) headr2

! U
      do l = 1, lm
         read (11)  q1
         read (12)  q2
         read (13)  qq
            do j = 1, jm
            do i = 1, im
               qo1(i,j) = qq(i,j) + (q2(i,j)-q1(i,j))/pertcoef
               qo2(i,j) = qq(i,j) - (q2(i,j)-q1(i,j))/pertcoef
            enddo
            enddo
            write(31) qo1
            write(32) qo2
      enddo
      write(6,*) lm
! V
      do l = 1, lm
         read (11)  q1
         read (12)  q2
         read (13)  qq
            do j = 1, jm
            do i = 1, im
               qo1(i,j) = qq(i,j) + (q2(i,j)-q1(i,j))/pertcoef
               qo2(i,j) = qq(i,j) - (q2(i,j)-q1(i,j))/pertcoef
            enddo
            enddo
            write(31) qo1
            write(32) qo2
      enddo
      write(6,*) lm
! PT
      do l = 1, lm
         read (11)  q1
         read (12)  q2
         read (13)  qq
            do j = 1, jm
            do i = 1, im
               qo1(i,j) = qq(i,j) + (q2(i,j)-q1(i,j))/pertcoef
               qo2(i,j) = qq(i,j) - (q2(i,j)-q1(i,j))/pertcoef
            enddo
            enddo
            write(31) qo1
            write(32) qo2
      enddo
      write(6,*) lm

! PE, PKZ, DZ and W

      do l = 1, lm*4+1
         read (11)  q1
         read (12)  q2
         read (13)  qq
         write(31)  qq
         write(32)  qq
      enddo
      write(6,*) lm
      
      end

