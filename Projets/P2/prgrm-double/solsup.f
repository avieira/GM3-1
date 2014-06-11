      subroutine solsup(n, A, b, x)
      implicit none
      integer i,j,n
      double precision A(n,n),b(n),x(n),S

      do i=n,1,-1
        S=0
        do j=i+1,n
          S=S+A(i,j)*x(j)
        enddo
        x(i)=(b(i)-S)/A(i,i)
      enddo

      return
      end
