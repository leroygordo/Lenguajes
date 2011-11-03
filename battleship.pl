% -*- Mode: Prolog -*-

%Autores: Erick Dos Ramos
%         Hancel Gonzalez

% isin(+E,+L,S) :- Se satisface si S corresponde  a un disparo exitoso 'g'  o
%     fallido 'f' en la posicion E del tablero. L es un subconjuntos de las
%     posiciones del tablero.
isin(_,[],'f').
isin(_,[],'g').
isin(Elem,[L|Ls],S):-
    isinaux(Elem,L,S),
    isin(Elem,Ls,S).

% isinaux(+pos(X,Y),+L,S) :- Se satisface si S corresponde a un disparo exito-
%    so en la posicion pos(X,Y) al alguna parte de un barco ubicada en  esa
%    posicion.
isinaux(pos(X,Y),[],S).
isinaux(pos(X,Y),[pos(X,Y)|Ls],'g').
isinaux(pos(X,Y),[L|Ls],S):-
    isinaux(pos(X,Y),Ls,S).

% tableroinicial(+F,+C,T) :- Se satisface si T  corresponde a un tablero de  F
%     filas & C columnas.
tableroinicial(0,_,[]).
tableroinicial(F,C,[L|Ls]):-
    llenarfila(F,C,L),
    Fnew is F -1,
    tableroinicial(Fnew,C,Ls).

% llenarfila(+F,+C,+L) :- Se satisface si cada elemento la fila L de tamano  C 
%    es igual a 'a'.
llenarfila(_,0,[]).
llenarfila(F,C,[L|Ls]):-
    L = 'a',
    Cnew is C -1,
    llenarfila(F,Cnew,Ls).

% mostrartablero(+T) :- Se satisface si un tablero T es mostrado en pantalla.
mostrartablero([]).
mostrartablero([L|Ls]):-
    mostrartableroaux(L),
    mostrartablero(Ls).

% mostrartableroaux(+L) :- Se satisface si los elementos de una lista L es mos
%    trada en pantalla dispuestos en fila.
mostrartableroaux([]):-
    nl.
mostrartableroaux([L|Ls]):-
    write(L),
    mostrartableroaux(Ls).

% ataquesposibles(+F,+C,+T,L) :- Se satisface si L corresponde a las posiciones
%    posibles de ataques,es decir, aquellas cuyas casillas en T (de tamano FxC)
%    son iguales a 'a'.
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

% barcogolpeado(+F,+C,+T,L) :- Se satisface si L corresponde a las posiciones
%    atacadas pero no hundidas de los barcos dentro del tablero T (de  tamano
%    TxC), es decir, casillas iguales a 'g'.
barcogolpeado(F,C,[],_).
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

% adyacente(+G,+L,pos(U,V)) :- Se satisface si pos(U,V) es una posicion adyacente a G 
%    ademas de pertenecer a la lista L.
adyacente([],_,_):-!.
adyacente(_,[],_):-!.
adyacente(pos(X,Y),[pos(X,_)|Ls],pos(U,V)):-
    Z0 is Y + 1,
    adyacenteaux(pos(X,Y),[pos(X,Z0)|Ls],pos(U,V)).
adyacente(pos(X,Y),[pos(X,_)|Ls],pos(U,V)):-
    Z1 is Y - 1,
    adyacenteaux(pos(X,Y),[pos(X,Z1)|Ls],pos(U,V)).
adyacente(pos(X,Y),[pos(_,Y)|Ls],pos(U,V)):-
    Z2 is X + 1,
    adyacenteaux(pos(X,Y),[pos(Z2,Y)|Ls],pos(U,V)).
adyacente(pos(X,Y),[pos(_,Y)|Ls],pos(U,V)):-
    Z3 is X - 1,
    adyacenteaux(pos(X,Y),[pos(Z3,Y)|Ls],pos(U,V)).
adyacente(pos(X,Y),[pos(_,_)|Ls],pos(U,V)):-
    adyacente(pos(X,Y),Ls,pos(U,V)).

adyacenteaux(pos(X,Y),[pos(Z,W)|Ls],pos(U,V)):-!,
    U is Z,
    V is W.

% ataque(+T0,T1,+F,+C) :- se satisface si T0, T1 son tableros de F filas C columnas
%    donde T1 corresponde a disparar o hundir un barco en T0.
ataque(_,_,0,0).
ataque(_,_,_,0).
ataque(_,_,0,_).
ataque(T0,T1,F,C):-
    ataquesposibles(0,0,T0,[La|Las]),
    barcogolpeado(0,0,T0,[Lg|Lgs]),!,
    adyacente(Lg,[La|Las],A),
    numbarcos(Num),
    barcos(L),
    buscarbarco(Num,Lt,L),!,
    isin(A,Lt,Hit),
    ataqueaux(T0,T2,A,Hit),
	hundirBarcos(T2,T1).

ataqueaux([T0|T0s],[T1|T0s],pos(0,C),Hit):-
    ataqueaux2(T0,T1,C,Hit).
ataqueaux([T0|T0s],[T0|T1],pos(F,C),Hit):-
    Fnew is F -1,
	Fnew > 0,
    ataqueaux(T0s,T1,pos(Fnew,C),Hit).

ataqueaux2(['g'|T0s],['g'|T0s],0,_).
ataqueaux2(['f'|T0s],['f'|T0s],0,_).
ataqueaux2(['h'|T0s],['h'|T0s],0,_).
ataqueaux2(['a'|T0s],[Hit|T0s],0,Hit).
ataqueaux2([T0|T0s],[T0|T1],C,Hit):-
    Cnew is C -1,
	Cnew > 0,
    ataqueaux2(T0s,T1,Cnew,Hit).

% posicionGolpeado(+T,G) :- se satisface si G corresponde a una posicion  en  T
%   donde algun barco ha sido golpeado.
posicionGolpeado(T,pos(X,Y)):-
    posicionGolpeadoF(0,T,pos(X,Y)).

% posicionGolpeadoF(+F,+T,+U) :- se satisface si la coordenada X de U es igual a
%    F.
posicionGolpeadoF(F,[T0|_],pos(F,Y)):-!,
    posicionGolpeadoC(0,T0,pos(F,Y)).
posicionGolpeadoF(F,[_|Ts],pos(X,Y)):-
    FNew is F + 1,
    posicionGolpeadoF(FNew,Ts,pos(X,Y)).

% posicionGolpeadoC(C,T,G) :- se satisface si la coordenada X de G es igual a C
%    ademas de G corresponder a una posicion golpeada en el tablero T.
posicionGolpeadoC(C,['g'|Ts],pos(X,C)):-!.
posicionGolpeadoC(C,['a'|Ts],pos(X,C)):-fail.
posicionGolpeadoC(C,[_|Ts],pos(X,Y)):-
    CNew is C + 1,
    posicionGolpeadoC(CNew,Ts,pos(X,Y)).

% hundirBarcos(+T0,T1) :- se satisface si T1 corresponde a un tablero donde los
%    barcos en T0 estan hundidos (marcados con h).
hundirBarcos(T0,T1):-
    numbarcos(Num),
    barcos(L),
    buscarbarco(Num,Lt,L),!,
    hundirBarcosAux(T0,T1,Lt).

% hundirBarcosAux(+T0,+T1,+L) :- se satisface si T1 es el tablero que refleja los
%    barcos L hundidos de T0
hundirBarcosAux(_,_,[]):-!.
hundirBarcosAux(T0,T1,[Lb|Lbs]):-
    barcoHundido(T0,Lb),!,
    hundirBarco(T0,T1,Lb),!,
    hundirBarcosAux(T2,T1,Lbs).

% hundirBarco(+T0,+T1,+B) :- se satisface si T1 refleja que un barco B (posiciones
%    del barco) fue hundido.
hundirBarco(L,L,[]):-!.
hundirBarco(T0,T1,[B|Bs]):-
    hundirBarcoAux(T0,T2,B),
    hundirBarco(T2,T1,Bs).

% barcoHundido(T,B) :- se satisface si una barco B ha sido golpeado completamente.
barcoHundido(_,[]):-!.
barcoHundido(T0,[L|Ls]):-
    posicionGolpeado(T0,L),
    barcoHundido(T0,Ls).

% hundirBarcoAux(T0,T1,pos(X,Y)):- se satisface si T1 es el tablero cuya posicion 
%    (X,Y) es igual a 'h', es decir, se hunde una posicion de barco.
hundirBarcoAux(T0,T1,pos(X,Y)):-
    hundirBarcoAuxF(0,T0,T1,pos(X,Y)).

% hundirBarcoAuxF(F,T0,T1,U) :- se satisface si la coordenada X de U es igual a F.
hundirBarcoAuxF(F,[T0|T0s],[T1|T0s],pos(F,Y)):-!,
    hundirBarcoAuxC(0,T0,T1,pos(F,Y)).
hundirBarcoAuxF(F,[T0|T0s],[T0|T1s],pos(X,Y)):-
    FNew is F + 1,
    hundirBarcoAuxF(FNew,T0s,T1s,pos(X,Y)).

% hundirBarcoAuxC(C,T0,T1,H) :- se satisface si la coordenada X de H es igual a C
%    T1 es el tablero cuya posicion H es igual a 'h'.
hundirBarcoAuxC(C,[T0|T0s],[L|T0s],pos(X,C)):-!,
    L = 'h'.
hundirBarcoAuxC(C,[T0|T0s],[T0|T1s],pos(X,Y)):-
    CNew is C + 1,
    hundirBarcoAuxC(CNew,T0s,T1s,pos(X,Y)).
    
% estadofinal(T) :- se satisface si los barcos ocultos en el tablero T han sido 
%    hundidos todos.
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

% colocarBarcos(+N,L) :- se satisface si L almacena la informacion de N barcos.
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

% buscarbarco(+N,B,+L) :- se satisface si B corresponde a una lista con posiciones
%    ocupada por cada barco, esta informacion es extraida de L.
buscarbarco(0,[],[]).
buscarbarco(N,[L|Ls],[barco(pos(FilaI,ColumnaI),tam(T),dir(D))|[Lsb]]):-
    agregarbarco(FilaI,ColumnaI,T,D,L),
    NNew is N -1,
    buscarbarco(NNew,Ls,Lsb).

% agregarbarco(+F,+C,+T,+D,L) :- se satisface si L es una  lista de las posiciones 
%    ocupadas por un barco cuya posicion inicial es (F,C) de tamano T direccion D.
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
    
ciclo(_,_,_,0).
ciclo(T,F,C,Balas):-
	Balas > 0,
	BalasNew is Balas - 1,
	\+(estadofinal(T)),
    numbarcos(NumB),
    ataque(T,Tnew,F,C),
    mostrartablero(Tnew),
	nl,
    ciclo(Tnew,F,C,BalasNew).
    
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
	write('Cant. de proyectiles disponibles: '),
	read(NBalas),
    mostrartablero(Tp),
    nl,
    ciclo(Tp,NFilas,NColumnas,NBalas),
    retractall(tamano(X,Y)),
    retractall(barcos(P)),
    retractall(numbarcos(Z)),
    retractall(barco(W,V,O)).
