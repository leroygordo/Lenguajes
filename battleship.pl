% -*- Mode: Prolog -*-
tableroinicial(0,_,[]).
tableroinicial(F,C,[L|Ls]):-
	llenarfila(F,C,L),
	Fnew is F -1,
	tableroinicial(Fnew,C,Ls).

llenarfila(_,0,[]).
llenarfila(F,C,[L|Ls]):-
	L = 'a',
	Cnew is C -1,
	llenarfila(F,Cnew,Ls).

mostrartablero([]).
mostrartablero([L|Ls]):-
	mostrartableroaux(L),
	mostrartablero(Ls).

mostrartableroaux([]):-
	nl.
mostrartableroaux([L|Ls]):-
	write(L),
	mostrartableroaux(Ls).
	
jugar:-
	write('Num. de Filas: '),
	read(NFilas),
	write('Num. de Columnas: '),
	read(NColumnas),
	write('Cant. de Barcos: '),
	read(NBarcos),
	assert(tamano(NFilas,NColumnas)),
	tableroinicial(NFilas,NColumnas,Tdp),
	%ponerbarcos(NBarcos,Tdp,Tbp),
	mostrartablero(Tdp),
	%ciclopricipal(),
	retractall(tamano(X,Y)).