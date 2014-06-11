FC=gfortran
OPT=-g

#Méthode de relaxation
#Objets
relaxation.o : relaxation.f
	$(FC) $(OPT) relaxation.f -c

norme2.o : norme2.f
	$(FC) $(OPT) norme2.f -c

relax.o : relax.f
	$(FC) $(OPT) relax.f -c

#Edition des liens
relax : relax.o relaxation.o norme2.o
	$(FC) $(OPT) relax.o relaxation.o norme2.o -o relax

#Méthode de décomposition QR
#Objets
lu.o : lu.f
	$(FC) $(OPT) lu.f -c

solsup.o : solsup.f
	$(FC) $(OPT) solsup.f -c

resolLU.o : resolLU.f
	$(FC) $(OPT) resolLU.f -c

#Edition des liens
resolLU : resolLU.o solsup.o lu.o
	$(FC) $(OPT) resolLU.o solsup.o lu.o -o resolLU

clean :
	rm *.o *~
