c ---------------------------------------------------------------------------------------------------------
c subroutine pour la méthode de relaxation, cherchant à résoudre le système A*x1=b, avec un paramètre omega
c ---------------------------------------------------------------------------------------------------------

      subroutine relaxation(A,b,x1,n,nmax,omega,tol,sortie)
      implicit none
      real A(n,n),b(n),x1(n),x2(n),resid(n),s,omega,tol,norme2
      integer i,j,k,n,nmax,sortie

      sortie=0
      do k=1,nmax
       do i=1,n
        s=0
        do j=1,i-1
         s=s+A(i,j)*x2(j) !calcul de la somme sur les éléments de x^k+1 entre 1 et i-1
        enddo
        do j=i+1,n
         s=s+A(i,j)*x1(j) !calcul de la somme sur les éléments de x^k entre i+1 et n
        enddo
        x2(i)=(1-omega)*x1(i)+(omega/A(i,i))*(b(i)-s) !calcul de la ième composante de x^k+1 qui sera de suite réutilisée
       enddo
       x1=x2 !x^k+1 devient le nouveau x^k
       resid=b-matmul(A,x1) !calcul du résidu
       if((norme2(resid,n)/norme2(b,n))<tol) then !test d'arrêt
        sortie=1
        exit
       endif
      enddo
      return
      end
