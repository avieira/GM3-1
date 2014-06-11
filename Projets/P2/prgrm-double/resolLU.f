      program resolLU
      implicit none
      double precision, allocatable::A(:,:)
      double precision, allocatable::b(:)
      double precision, allocatable::y(:)
      double precision, allocatable::x(:)
      integer n
      
      read(*,*) n
      allocate(A(n,n),y(n),x(n),b(n))

      read(*,*) A
      read(*,*) b

      call LU(A,b,n) !Transformation du système de Ax=b en Ux=y, avec U supérieur et y=L^-1.b
      call solsup(n,A,b,x) !Resolution de Ux=y avec U=A supérieure
      
      write(*,*) x
      end
