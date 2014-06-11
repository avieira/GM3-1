      subroutine LU(A, b, n)
      implicit none
      double precision A(n,n), b(n), C
      integer i,j,k,n


      do i=1,n-1 
       do k=i+1,n
        C=A(k,i)/A(i,i)
        b(k)=b(k)-C*b(i);
        do j=i,n
         A(k,j)=A(k,j)-C*A(i,j)
        enddo
       enddo
      enddo

      return
      end


