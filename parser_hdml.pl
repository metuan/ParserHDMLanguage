:- module(parser_hdml, [parse/3]).


% --------------------------------------- LEKSER ---------------------------------------- %
/* 
  Opierajac sie na while_parser wstawionym na skos stworzylem lekser, ktorego struktura wyglada nastepujaco:

   - slowa kluczowe : def else if in let then _
   - opratory i znaki przestankowe:  ( ) [ ] .. , = <> < > <= >= ^ | + - & * / % @ # ~
   - identyfikatory: niepuste identyfikatory skladajace sie z malych i wielkich liter ASCII, cyfr 0-9
i znakow podkreslenie i apostrof) zaczynajace sie od liter lub podkresleniem i rozne odslow kluczowych,
   - literaly calkowitoliczbowe: niepuste ciagi cyfr 0-9.

   Nie bylo tu wielkiej filozofii, jedynie podazanie za schematem tj. znak jako token i jego znaczenie.
*/

lexer(Tokens) -->
    white_space,
   (  ( "(*",       !, { Token = tokLewyKomentarz }
      ; "*)",       !, { Token = tokPrawyKomentarz }
      ;  "(",       !, { Token = tokLewyNawias }
      ;  ")",       !, { Token = tokPrawyNawias }
      ;  "[",       !, { Token = tokLewyKwadratowyNawias }
      ;  "]",       !, { Token = tokPrawyKwadratowyNawias }
      ;  "..",      !, { Token = tokPodwojnaKropka }
      ;  ",",       !, { Token = tokPrzecinek }
      ;  "=",       !, { Token = tokPrzypisanie }
      ;  "<>",      !, { Token = tokNeq }
      ;  "<=",      !, { Token = tokMniejszyRowny }
      ;  ">=",      !, { Token = tokWiekszyRowny }
      ;  "<",       !, { Token = tokMniejszy }
      ;  ">",       !, { Token = tokWiekszy }
      ;  "^",       !, { Token = tokDaszek }
      ;  "|",       !, { Token = tokPalka }
      ;  "+",       !, { Token = tokPlus }
      ;  "-",       !, { Token = tokMinus }
      ;  "&",       !, { Token = tokAmpersand }
      ;  "*",       !, { Token = tokGwiazdka }
      ;  "/",       !, { Token = tokSlash }
      ;  "%",       !, { Token = tokProcent }
      ;  "@",       !, { Token = tokMalpa }
      ;  "#",       !, { Token = tokHash }
      ;  "~",       !, { Token = tokTylda }
      ;  digit(D),  !,
            number(D, N),
            { Token = tokNumber(N) }
      ;  letter(L), !, identifier(L, Id),
        {  member((Id, Token), [ (def, tokDef),
                                 ('_', tokUnderscore), 
                                 (if, tokIf), 
                                 (in, tokIn), 
                                 (else, tokElse),
                                 (then, tokThen),
                                 (let, tokLet)]),
               !;  Token = tokId(Id) }
      ;  [_],
            { Token = tokUnknown }
      ),
      !,
    { Tokens = [Token | TokList] },
    lexer(TokList)
    ;  [],
    { Tokens = [] }
    ).

/* 
  Dalej podazajac za praserem jezyka while, definiujemy wyglad spacji, literlu calkowitoliczbowego,
  litery, znaku oraz identyfikatorow.
*/
white_space --> [Char], { code_type(Char, space) }, !, white_space.
white_space --> [].

digit(D) -->
   [D],
      { code_type(D, digit) }.

digits([D|T]) -->
   digit(D),
   !,
   digits(T).
digits([]) -->
   [].

number(D, N) -->
   digits(Ds),
      { number_chars(N, [D|Ds]) }.

letter(L) -->
   [L], { code_type(L, csymf) }.

alphanum([A|T]) -->
   [A], { A == 39 ; code_type(A, csym)  }, !, alphanum(T).
alphanum([]) -->
   [].

identifier(L, Id) -->
   alphanum(As),
      { atom_codes(Id, [L|As]) }.

% --------------------------------------- KOMENTARZE ---------------------------------------- %
% CO ZROBIC Z KOMENTARZAMI? ODPOWIEDZIA JEST ZADANIE Z LIST ZADAN Z CWICZEN, GDZIE DOWODZILISMY
% POPRAWNOSCI NAWIASOWANIA GRAMATYKI  

/* 
 Wiemy, ze komentarze nie odgrywaja roli w sparsowanym programie, zatem
 stosujac sztuczke z list zadan, gdzie mielismy stwierdzic czy wyrazenie
 jest poprawnie nawiasowane. Za kazdym razem kiedy napotkamy token, ktory
 jest otwarciem komentarza zwiekszamy liczbe nawiasow o 1, jesli zamykajacy
 to dekrementujemy ta liczbe. Kiedy dojdziemy do sytuacji w ktorej jest liczba 0, 
 to wiemy, ze to komentarz. Jest to zalozenie latwe do udowodnienia indukcyjnie wzgledem
 dlugosci wyrazenia (w odpowiedniej gramatyce wyprowadzen, tak jak na cwiczeniach). 
*/
% Tworzymy predykat, ktory usunie nam komentarze, a wiec liczba komentarzy bedzie rowna 0.

usuwanieKomentarzy(Tokens, NewTokens) :- usuwanieKomentarzy(Tokens, NewTokens, 0).

usuwanieKomentarzy(_,_, A) :-
  A < 0, !,
  fail.
usuwanieKomentarzy([],[], A) :- 
  A =\= 0, !,
  fail.
usuwanieKomentarzy([],[],_).
usuwanieKomentarzy([tokLCom|RT], T1, LiczbaKomentarzy1) :-
  N_LiczbaKomentarzy is LiczbaKomentarzy + 1, !,
  usuwanieKomentarzy(RT, T1, N_LiczbaKomentarzy).
usuwanieKomentarzy([tokRCom|RT], T1, LiczbaKomentarzy1) :-
  N_LiczbaKomentarzy is LiczbaKomentarzy - 1, !, 
  usuwanieKomentarzy(RT, T1, N_LiczbaKomentarzy).
usuwanieKomentarzy([T|RT], T1, LiczbaKomentarzy) :- 
  LiczbaKomentarzy > 0, !, 
  usuwanieKomentarzy(RT,T1,LiczbaKomentarzy).
usuwanieKomentarzy([T|RT], [T|T1], 0) :- 
  usuwanieKomentarzy(RT,T1,0).

% --------------------------------------- PARSER ---------------------------------------- %
/*

  Wlasciwy program tj. zdefiniowana gramatyka bezkonstekstowa oraz reguly wychodzace z tej gramatyki.
  Jesli chodzi o reguly to sa one wprost zdefiniowane z pliku skos, gdzie pokazane jest w jaki sposob
  tworzymy jezyk reprezentowany przez ta gramatyke.

*/


% Staramy sie rozbic nasza gramatyke na najmniejsze przypadki tj. sprawdzenie kazdej mozliwej drogi, 
% wowczas zapewimy jej jednoznacznosc oraz lacznosc operatorow. Najwiecej zabawy mamy z wyrazeniami.
% Mozemy zatem stworzyc sobie grupy operatorow (podobnie dzieje sie w jezyku while) i na tej podstawie 
% podzielic wyrazenie na wyrazenia nalezace do danych grup: addytywnych, multiplikatywnych, unarnych itp.



%--------------------------------------------------%
% Definicja programu w jezyku HDML %
%--------------------------------------------------%

program(ProgramWJezykuHDML) -->
  definicje(ProgramWJezykuHDML).

definicje([]) --> [].
definicje([H|T]) --> 
  definicja(H),
  definicje(T).

definicja(def(Id,P,E)) -->
  [tokDef],[tokId(Id)],[tokLewyNawias],wzorzec(P),
  [tokPrawyNawias],[tokPrzypisanie],wyrazenie(E).

%--------------------------------------------------%
% Wzorce %

%--------------------------------------------------%
wzorzec(P) --> prostyWzorzec(P).
wzorzec(pair(no,P1,P2)) -->
  prostyWzorzec(P1),[tokPrzecinek],wzorzec(P2).

prostyWzorzec(wildcard(no)) --> 
  [tokUnderscore], !.
prostyWzorzec(var(no,Z)) --> 
  [tokId(Z)], !.
prostyWzorzec(P) -->
  [tokLewyNawias], !, 
  wzorzec(P),
  [tokPrawyNawias].

%--------------------------------------------------%
% Gramatyka wyrazen %
%--------------------------------------------------%

% Wyrazenie proste %
wyrazenieProste(E,E) --> [].
wyrazenieProste(E) --> 
  wyrazenieAtomowe(E1),
  wyrazenieProste(E1,E).
wyrazenieProste(E) -->
  [tokLewyNawias], wyrazenie(E), [tokPrawyNawias].
wyrazenieProste(AC,E) -->
  selectBit(E1),
  {AC1 = bitsel(no, AC, E1)},
  wyrazenieProste(AC1, E).
wyrazenieProste(AC, E) -->
  selectBits(E1,E2),
  {AC1 = bitsel(no, AC, E1, E2)},
  wyrazenieProste(AC1, E).

%--------------------------------------------------%
% Wyrazenia operatow binarnych %
%--------------------------------------------------%

wyrazenie(W) --> binarneWyrazeniePrzecinek(W).
wyrazenie(if(no,E1,E2,E3)) -->
  [tokIf], !, wyrazenie(E1),
  [tokThen],wyrazenie(E2),
  [tokElse],wyrazenie(E3).
wyrazenie(let(no,P,E1,E2)) --> 
  [tokLet], !, wzorzec(P),
  [tokPrzypisanie],wyrazenie(E1),
  [tokIn],wyrazenie(E2).

binarneWyrazeniePrzecinek(E1) -->
  wyrazenieBinarne(E1).
binarneWyrazeniePrzecinek(pair(no, E1, E2)) --> 
	wyrazenieBinarne(E1),
  [tokPrzecinek], !,
  binarneWyrazeniePrzecinek(E2).

wyrazenieBinarne(E1) -->
  wyrazenieBinarneMalpa(E1).
wyrazenieBinarne(op(no, Op, E1, E2)) -->
	wyrazenieBinarneMalpa(E1), 
  operatoryRelacyjne(Op),
  wyrazenieBinarneMalpa(E2).


wyrazenieBinarneMalpa(E1) -->
  wyrazeniegrupyAddytywnej(E1).
wyrazenieBinarneMalpa(op(no, Op, E1, E2)) -->
	wyrazeniegrupyAddytywnej(E1),
  operatorMalpa(Op),
  wyrazenieBinarneMalpa(E2).
wyrazeniegrupyAddytywnej(E,E) --> [].
wyrazeniegrupyAddytywnej(A) -->
	wyrazenieGrupyMultiplikatywnej(A1),
	wyrazeniegrupyAddytywnej(A1,A).
wyrazeniegrupyAddytywnej(AC, A) -->
	operatoryAddytywne(Op), !,
	wyrazenieGrupyMultiplikatywnej(A1),
	{AC1 = op(no, Op, AC, A1)},
	wyrazeniegrupyAddytywnej(AC1, A).

wyrazenieGrupyMultiplikatywnej(A,A) --> [].
wyrazenieGrupyMultiplikatywnej(A) -->
	wyrazenieUnarne(A1),
	wyrazenieGrupyMultiplikatywnej(A1,A).
wyrazenieGrupyMultiplikatywnej(AC, A) -->
	operatoryMultiplikatywne(Op), !, 
	wyrazenieUnarne(A1), 
	{AC1 = op(no, Op, AC, A1)}, 
	wyrazenieGrupyMultiplikatywnej(AC1, A).

%--------------------------------------------------%
% Wyrazenia unarne %
%--------------------------------------------------%

wyrazenieUnarne(E1) --> wyrazenieProste(E1).
wyrazenieUnarne(op(no, Op, E1)) -->
	operatoryUnarne(Op),
  wyrazenieUnarne(E1).

%--------------------------------------------------%
% Wyrazenia Atomowe %
%--------------------------------------------------%

wyrazenieAtomowe(var(no, X)) --> [tokId(X)].
wyrazenieAtomowe(num(no, N)) --> [tokNumber(N)].
wyrazenieAtomowe(empty(no)) -->
  [tokLewyKwadratowyNawias],
  [tokPrawyKwadratowyNawias].
wyrazenieAtomowe(bit(no, E)) --> 
  [tokLewyKwadratowyNawias], !,
  wyrazenie(E),
  [tokPrawyKwadratowyNawias].
wyrazenieAtomowe(call(no, N, E)) --> 
  [tokId(N)], 
  [tokLewyNawias], !, 
  wyrazenie(E), 
  [tokPrawyNawias].
wyrazenieAtomowe(E) --> 
  [tokLewyNawias], !, 
  wyrazenie(E), 
  [tokPrawyNawias].

%--------------------------------------------------%
% Select Bits %
%--------------------------------------------------%

selectBit(E2) --> 
  [tokLewyKwadratowyNawias], 
  wyrazenie(E2), 
  [tokPrawyKwadratowyNawias]. 
selectBits(E2,E3) --> 
  [tokLewyKwadratowyNawias], 
  wyrazenie(E2), 
  [tokPodwojnaKropka], 
  wyrazenie(E3), 
  [tokPrawyKwadratowyNawias].


%--------------------------------------------------%
/* Opratory, ustawione od najslabszych priorytetow */
%--------------------------------------------------%

%--------------------------------------------------%
% Grupa operatorow relacyjnych: mniejszy, rowny itd.
%--------------------------------------------------%
operatoryRelacyjne('=') --> 
  [tokPrzypisanie], !.
operatoryRelacyjne('<>') -->
  [tokNeq], !.
operatoryRelacyjne('<=') -->
  [tokMniejszyRowny], !.
operatoryRelacyjne('>=') -->
  [tokWiekszyRowny], !.
operatoryRelacyjne('<') -->
  [tokMniejszy ], !.
operatoryRelacyjne('>') -->
  [tokWiekszy], !.

%--------------------------------------------------%
% Oprator malpa: @.
%--------------------------------------------------%
operatorMalpa('@') --> 
  [tokMalpa], !.

%--------------------------------------------------%
% Grupa opertaorow o priorytecie takim samym, jak grupa addytywna
% gramatyki: +, -, ^.
%--------------------------------------------------%
operatoryAddytywne('|') -->
  [tokPalka], !.
operatoryAddytywne('^') --> 
  [tokDaszek], !.
operatoryAddytywne('+') --> 
  [tokPlus], !.
operatoryAddytywne('-') -->
  [tokMinus], !.

%--------------------------------------------------%
% Grupa operatorow o priorytecie takim samym, jak grupa multiplikatywna
% gramatyki: %, *, /.
%--------------------------------------------------%
operatoryMultiplikatywne('&') -->
  [tokAmpersand], !.
operatoryMultiplikatywne('*') --> 
  [tokGwiazdka], !.
operatoryMultiplikatywne('/') -->
  [tokSlash], !.
operatoryMultiplikatywne('%') -->
  [tokProcent], !.

%--------------------------------------------------%
% Grupa operatorow unarnych, ktore wedlug zalozen gramatyki wiaza silniej
% takie jak: -, #.
%--------------------------------------------------%
operatoryUnarne('-') -->
  [tokMinus], !.
operatoryUnarne('#') --> 
  [tokHash], !.
operatoryUnarne('~') -->
  [tokTylda], !.
%--------------------------------------------------%

%----------------------- GLOWNY PREDYKAT PARSE ---------------------------%

% Glowny predykat, ktory obsluguje powyzsze predykaty i zwraca abstrakcyjne drzewo rozbioru
% wyrazenia w jezyku HDML podanego na wejsciu.

parse(Path, ProgramWJezykuHDML, AbstrakcyjneDrzewRozbioru) :-
   string_to_list(ProgramWJezykuHDML, CharCodeList),
   phrase(lexer(ListaTokenowProgramu), CharCodeList),
   usuwanieKomentarzy(ListaTokenowProgramu, N_ListaTokenowProgramu),
   phrase(program(AbstrakcyjneDrzewRozbioru), N_ListaTokenowProgramu).
