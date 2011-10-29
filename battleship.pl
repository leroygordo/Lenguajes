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

colocarBarcos(0).
colocarBarcos(B):-
	B > 0,
	write('Informacion de Barco:'),
	nl,
	write('Tamano: '),
	read(T),
	write('Direccion: '),
	read(D),
	write('Fila Inicial: '),
	read(FilaI),
	write('Columna Inicial: '),
	read(ColumnaI),
	assert(barco(pos(filaI,columnaI),tam(t),dir(d))),
	B1 is B - 1,
	colocarBarcos(B1).

ataque(_,_,0,0).
ataque(_,_,_,0).
ataque(_,_,0,_).
ataque(T0,T1,F,C):-
       write('Fila a atacar: '),
       read(Fa),
       write('Columna a atacar: '),
       read(Ca),
       ataqueaux(T0,T1,Fa,Ca).

ataqueaux([T0|T0s],[T1|T0s],0,C):-
       ataqueaux2(T0,T1,C).
ataqueaux([T0|T0s],[T0|T1],F,C):-
       Fnew is F -1,
       ataqueaux(T0s,T1,Fnew,C).

ataqueaux2(['b'|T0s],['g'|T0s],0).
ataqueaux2(['a'|T0s],['f'|T0s],0).
ataqueaux2([T0|T0s],[T0|T1],C):-
       Cnew is C -1,
       ataqueaux2(T0s,T1,Cnew).
	
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
	colocarBarcos(NBarcos),
	%ciclopricipal(),
	retractall(tamano(X,Y)).