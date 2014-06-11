      subroutine solsup(n, A, b, x)
      implicit none
      integer i,j,n
      real A(n,n),b(n),x(n),S

      do i=n,1,-1 !calcul par remontée
        S=0
        do j=i+1,n
          S=S+A(i,j)*x(j) !calcul de la somme des coefficients avec les composantes déjà calculées
        enddo
        x(i)=(b(i)-S)/A(i,i) !calcul de la ième composante
      enddo

      return
      end
