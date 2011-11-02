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

barcogolpeado(F,C,[],[]).
barcogolpeado(F,C,[[]|Ts],L):-
       Fnew is F +1,
       Cnew is 0,
       barcogolpeado(Fnew,Cnew,Ts,L).
barcogolpeado(F,C,[['g'|Es]|Ts],[pos(F,C)|Ls]):-
       Cnew is C +1,
       barcogolpeado(F,Cnew,[Es|Ts],Ls).
barcogolpeado(F,C,[[_|Es]|Ts],L):-
       Cnew is C +1,
       barcogolpeado(F,Cnew,[Es|Ts],L).

adyacente(_,[],_):-!,fail.
adyacente(pos(X,Y),[pos(X,_)|Ls],pos(U,V)):-
	W is Y + 1,
	adyacenteaux(pos(X,Y),[pos(X,W)|Ls],pos(U,V)).
adyacente(pos(X,Y),[pos(X,_)|Ls],pos(U,V)):-
	W is Y - 1,
	adyacenteaux(pos(X,Y),[pos(X,W)|Ls],pos(U,V)).
adyacente(pos(X,Y),[pos(_,Y)|Ls],pos(U,V)):-
	W is X + 1,
	adyacenteaux(pos(X,Y),[pos(W,Y)|Ls],pos(U,V)).
adyacente(pos(X,Y),[pos(_,Y)|Ls],pos(U,V)):-
	W is X - 1,
	adyacenteaux(pos(X,Y),[pos(W,Y)|Ls],pos(U,V)).
adyacente(pos(X,Y),[_|Ls],pos(U,V)):-
	adyacente(pos(X,Y),Ls,pos(U,V)).

adyacenteaux(pos(X,Y),[pos(Z,W)|Ls],pos(U,V)):-
	U is Z,
	V is W,
	!.
	%adyacente(pos(X,Y),Ls,Lg).

ataque(_,_,0,0).
ataque(_,_,_,0).
ataque(_,_,0,_).
ataque(T0,T1,F,C):-
	ataquesposibles(0,0,T0,La),
	barcogolpeado(0,0,T0,[Lg|Lgs]),
	adyacente(Lg,La,A),
	numbarcos(Num),
	barcos(L),
	buscarbarco(Num,Lt,L),
	isin(La,Lt,Hit),
	ataqueaux(T0,T1,A,Hit).

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

posicionGolpeado(T,pos(X,Y)):-
	posicionGolpeadoF(0,T,pos(X,Y)).

posicionGolpeadoF(F,[T0|_],pos(F,Y)):-!,
	posicionGolpeadoC(0,T0,pos(F,Y)).
posicionGolpeadoF(F,[_|Ts],pos(X,Y)):-
	FNew is F + 1,
	posicionGolpeadoF(FNew,Ts,pos(X,Y)).

posicionGolpeadoC(C,['g'|Ts],pos(X,C)):-!.
posicionGolpeadoC(C,['a'|Ts],pos(X,C)):-fail.
posicionGolpeadoC(C,[_|Ts],pos(X,Y)):-
	CNew is C + 1,
	posicionGolpeadoC(CNew,Ts,pos(X,Y)).

hundirBarcos(T0,T1):-
	numbarcos(Num),
	barcos(L),
	buscarbarco(Num,Lt,L),
	hundirBarcosAux(T0,T1,Lt).

hundirBarcosAux(_,_,[]):-!.
hundirBarcosAux(T0,T1,[Lb|Lbs]):-
	barcoHundido(T0,Lb),
	hundirBarco(T0,T1,Lb),
	hundirBarcosAux(T0,T1,Lbs).

hundirBarco(_,_,[]):-!.
hundirBarco(T0,T1,[B|Bs]):-
	hundirBarcoAux(T0,T1,B),
	mostrartablero(T1),
	[T0|_] = [T1],
	hundirBarco(T0,T1,Bs).

barcoHundido(_,[]):-!.
barcoHundido(T0,[L|Ls]):-
	posicionGolpeado(T0,L),
	barcoHundido(T0,Ls).

hundirBarcoAux(T0,T1,pos(X,Y)):-
	hundirBarcoAuxF(0,T0,T1,pos(X,Y)).

hundirBarcoAuxF(F,[T0|T0s],[T1|T0s],pos(F,Y)):-!,
	hundirBarcoAuxC(0,T0,T1,pos(F,Y)).
hundirBarcoAuxF(F,[T0|T0s],[T0|T1s],pos(X,Y)):-
	FNew is F + 1,
	hundirBarcoAuxF(FNew,T0s,T1s,pos(X,Y)).

hundirBarcoAuxC(C,[T0|T0s],[L|T1s],pos(X,C)):-!,
	L = 'h'.
hundirBarcoAuxC(C,[T0|T0s],[T0|T1s],pos(X,Y)):-
	CNew is C + 1,
	hundirBarcoAuxC(CNew,T0s,T1s,pos(X,Y)).

	
estadofinal(T):-
       numbarcos(Num),
       barcos(L),
       buscarbarco(Num,Lt,L),!,
       estadofinalaux(Lt,T).

estadofinalaux([],_).
estadofinalaux([L|Ls],T):-
       estadofinalaux2(L,T),
       estadofinalaux(Ls,T).

estadofinalaux2([],_).
estadofinalaux2([pos(X,Y)|Ls],T):-
       estadofinalaux3(X,Y,T),
       estadofinalaux2(Ls,T).
       
estadofinalaux3(0,0,[['h'|Ls]|Ts]).
estadofinalaux3(0,Y,[[L|Ls]|Ts]):-
       Y > 0,
       Ynew is Y -1,
       estadofinalaux3(0,Ynew,[Ls|Ts]).
estadofinalaux3(X,Y,[[L|Ls]|Ts]):-
       X > 0,
       Xnew is X -1,
       estadofinalaux3(Xnew,Y,Ts).

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
	mostrartablero(Tp),
	nl,
	barcoshundidos(Tp,T1,NFilas,NColumnas),
	%ciclo(Tp,4,NFilas,NColumnas),
	retractall(tamano(X,Y)),
	retractall(barcos(P)),
	retractall(numbarcos(Z)),
	retractall(barco(W,V,O)).