      program relax
      implicit none
      external relaxation
      real, allocatable::A(:,:)
      real, allocatable::b(:)
      real, allocatable::x(:)
      real omega,tol
      integer n,nmax,sortie

      read(*,*) n
      allocate (A(n,n), b(n), x(n))
      read(*,*) A
      read(*,*) b
      read(*,*) x
      read(*,*) omega, nmax, tol
      
      call relaxation(A,b,x,n,nmax,omega,tol,sortie)
      if(sortie==1)then
       write(*,*) x
      else
       write(*,*) 'La methode n a pas converge dans le nombre d iterati
     ons donne'
       write(*,*) x
      endif
      
      end
