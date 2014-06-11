       double precision function norme2(x,n)
       implicit none
       double precision x(n)
       integer i,n

       norme2=0
       do i=1,n
         norme2=norme2+x(i)**2
       enddo
       end
