% -*- Mode: Prolog -*-
isin(_,[],'f').
isin(_,[],'g').
isin(Elem,[L|Ls],S):-
	isinaux(Elem,L,S),
	isin(Elem,Ls,S).
	
isinaux(pos(X,Y),[],S).
isinaux(pos(X,Y),[pos(X,Y)|Ls],'g').
isinaux(pos(X,Y),[L|Ls],S):-
	isinaux(pos(X,Y),Ls,S).

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

ataquesposibles(F,C,[],[]).
ataquesposibles(F,C,[[]|Ts],L):-
       Fnew is F +1,
       Cnew is 0,
       ataquesposibles(Fnew,Cnew,Ts,L).
ataquesposibles(F,C,[['a'|Es]|Ts],[pos(F,C)|Ls]):-
       Cnew is C +1,
       ataquesposibles(F,Cnew,[Es|Ts],Ls).
ataquesposibles(F,C,[[_|Es]|Ts],L):-
       Cnew is C +1,
       ataquesposibles(F,Cnew,[Es|Ts],L).

barcoGolpeado(F,C,[],[]).
barcoGolpeado(F,C,[[]|Ts],L):-
       Fnew is F +1,
       Cnew is 0,
       barcoGolpeado(Fnew,Cnew,Ts,L).
barcoGolpeado(F,C,[['g'|Es]|Ts],[pos(F,C)|Ls]):-
       Cnew is C +1,
       barcoGolpeado(F,Cnew,[Es|Ts],Ls).
barcoGolpeado(F,C,[[_|Es]|Ts],L):-
       Cnew is C +1,
       barcoGolpeado(F,Cnew,[Es|Ts],L).

ataque(_,_,0,0).
ataque(_,_,_,0).
ataque(_,_,0,_).
ataque(T0,T1,F,C):-
	barcoGolpeado(0,0,T0,[Lg|Lgs]),
	ataquesposibles(0,0,T0,[La|Las]),
	write([Lg|Lgs]),nl,
	numbarcos(Num),
	barcos(L),
	buscarbarco(Num,Lt,L),
	isin(La,Lt,Hit),
	ataqueaux(T0,T1,La,Hit).

ataqueaux([T0|T0s],[T1|T0s],pos(0,C),Hit):-
	ataqueaux2(T0,T1,C,Hit).
ataqueaux([T0|T0s],[T0|T1],pos(F,C),Hit):-
	Fnew is F -1,
	ataqueaux(T0s,T1,pos(Fnew,C),Hit).

ataqueaux2(['g'|T0s],['g'|T0s],0,_).
ataqueaux2(['f'|T0s],['f'|T0s],0,_).
ataqueaux2(['h'|T0s],['h'|T0s],0,_).
ataqueaux2(['a'|T0s],[Hit|T0s],0,Hit).
ataqueaux2([T0|T0s],[T0|T1],C,Hit):-
	Cnew is C -1,
	ataqueaux2(T0s,T1,Cnew,Hit).

colocarBarcos(0,[]).
colocarBarcos(B,[L,Ls]):-
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
	L = barco(pos(FilaI,ColumnaI),tam(T),dir(D)),
	B1 is B - 1,
	colocarBarcos(B1,Ls).

buscarbarco(0,[],[]).
buscarbarco(N,[L|Ls],[barco(pos(FilaI,ColumnaI),tam(T),dir(D))|[Lsb]]):-
	agregarbarco(FilaI,ColumnaI,T,D,L),
	NNew is N -1,
	buscarbarco(NNew,Ls,Lsb).

agregarbarco(_,_,0,'v',[]).
agregarbarco(_,_,0,'h',[]).
agregarbarco(F,C,T,'v',[L|Ls]):-
	Fnew is F +1,
	Tnew is T -1,
	L = pos(F,C),
	agregarbarco(Fnew,C,Tnew,'v',Ls).
agregarbarco(F,C,T,'h',[L|Ls]):-
	Cnew is C +1,
	Tnew is T -1,
	L = pos(F,C),
	agregarbarco(F,Cnew,Tnew,'h',Ls).
	
ciclo(_,0,_,_).
ciclo(T,Num,F,C):-
	Numnew is Num -1,
	numbarcos(NumB),
	ataque(T,Tnew,F,C),
	mostrartablero(Tnew),nl,
	ciclo(Tnew,Numnew,F,C).
	
jugar:-
	write('Num. de Filas: '),
	read(NFilas),
	write('Num. de Columnas: '),
	read(NColumnas),
	write('Cant. de Barcos: '),
	read(NBarcos),
	assert(tamano(NFilas,NColumnas)),
	assert(numbarcos(NBarcos)),
	tableroinicial(NFilas,NColumnas,Tp),
	colocarBarcos(NBarcos,Lb),
	assert(barcos(Lb)),
	mostrartablero(Tp),nl,
	ciclo(Tp,4,NFilas,NColumnas),
	retractall(tamano(X,Y)),
	retractall(barcos(P)),
	retractall(numbarcos(Z)),
	retractall(barco(W,V,O)).