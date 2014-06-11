      subroutine LU(A, b, n)
      implicit none
      real A(n,n), b(n), C
      integer i,j,k,n


      do i=1,n-1 
       do k=i+1,n
        C=A(k,i)/A(i,i) !utilisation du pivot, calcul du coefficient pour les calculs suivants
        b(k)=b(k)-C*b(i); !transformation du vecteur de résultat
        do j=i,n
         A(k,j)=A(k,j)-C*A(i,j) !transformation de la matrice pour arriver à une matrice supérieure
        enddo
       enddo
      enddo

      return
      end


