      program bv_atm_norm
!     implicit none

!     generate the bv_norm for coupled model
!     for 180x1080CF there are 648 (slices)=9x72
!     for MERRA2 there are 6x72 slices

      PARAMETER (IM = 180)
      PARAMETER (JM =1080)
      PARAMETER (LM =  72)

      REAL*4 q1(im,jm), qo1(im,jm)
      REAL*4 q2(im,jm), qo2(im,jm)
      REAL*4 qq(im,jm)
      REAL*4 pertcoef

      pertcoef=8.0
!     pertcoef=248.0

      do l = 1, lm
         read (21)  q1
         read (22)  q2
         read (23)  qq
         do j = 1, jm
         do i = 1, im
            qo1(i,j) = qq(i,j) + (q2(i,j)-q1(i,j))/pertcoef
            qo2(i,j) = qq(i,j) - (q2(i,j)-q1(i,j))/pertcoef
            if(qo1(i,j) .lt. 0.0) qo1(i,j) = 0.0
            if(qo2(i,j) .lt. 0.0) qo2(i,j) = 0.0
         enddo
         enddo
         write(41) qo1
         write(42) qo2
      enddo

      do l = 1, lm*6
!        write(6,*) l
         read (21) q1
         read (22) q2
         read (23) qq
         write(41) qq
         write(42) qq
      enddo

      end

