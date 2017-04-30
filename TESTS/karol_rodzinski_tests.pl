:- module(karol_rodzinski_tests, [tests/3]).

tests(empty_program, input(""), program([])).
tests(pustyMain, input("def main()"), no).
tests(sumatorJednocyfrowy, file('sumatorJednocyfrowy.hdml'), yes).
tests(sumatorJednocyfrowyZDrzewemRozbioru, file('sumatorJednocyfrowy.hdml'), 
	program([ def(half_adder,
	pair(no, var(no, 'A'), var(no, 'B')),
	pair(no, op(no, '&', var(no, 'A'), var(no, 'B')), op(no, '^', var(no, 'A'), var(no, 'B')))) ])).
tests(najprostszyTest, input("def main(X, Y) = Z"), program([ def(fun,
	pair(file(test, 1, 10, 9, 4),
	var(file(test, 1, 10, 9, 1), 'X'), 
	var(file(test, 1, 13, 12, 1), 'Y')), 
	var(file(test, 1, 18, 17, 1), 'Z')) ])).
tests(sumatorJednocyfrowyZDrzewemRozbioruPozycje, file('sumatorJednocyfrowy.hdml'), program([ def(half_adder, pair(file('sumatorJednocyfrowy.hdml', 1, 16, 15, 4),
var(file('sumatorJednocyfrowy.hdml', 1, 16, 15, 1), 'A'),
var(file('sumatorJednocyfrowy.hdml', 1, 18, 17, 1), 'B')),
pair(file('sumatorJednocyfrowy.hdml', 2, 3, 26, 12),
op(file('sumatorJednocyfrowy.hdml', 2, 3, 26, 5), '&',
var(file('sumatorJednocyfrowy.hdml', 2, 3, 26, 1), 'A'),
var(file('sumatorJednocyfrowy.hdml', 2, 7, 30, 1), 'B')),
op(file('sumatorJednocyfrowy.hdml', 2, 10, 33, 5), '^',
var(file('sumatorJednocyfrowy.hdml', 2, 10, 33, 1), 'A'),
var(file('sumatorJednocyfrowy.hdml', 2, 14, 37, 1), 'B')))) ])).
tests(wyrazenieLetInZDrzewem, input("def main(xyz) = let _ = X in Y"), program([ def(main,
	var(file(test, 1, 10, 9, 1), 'xyz'), 
    let(file(test, 1, 15, 14, 14),
    wildcard(file(test, 1, 19, 18, 1)), 
    var(file(test, 1, 23, 22, 1), 'X'),
    var(file(test, 1, 28, 27, 1), 'Y'))) ])).
tests(programBezSensu, file('programBezSensu.hdml'), program([ def(suma,
	pair(file('programBezSensu.hdml', 1, 10, 9, 14),
	pair(file('programBezSensu.hdml', 1, 11, 10, 4),
	var(file('programBezSensu.hdml', 1, 11, 10, 1), 'A'),
	var(file('programBezSensu.hdml', 1, 14, 13, 1), 'B')),
	pair(file('programBezSensu.hdml', 1, 19, 18, 4),
	var(file('programBezSensu.hdml', 1, 19, 18, 1), 'C'),
	var(file('programBezSensu.hdml', 1, 22, 21, 1), 'D'))),
 	if(file('programBezSensu.hdml', 2, 1, 28, 29),
	op(file('programBezSensu.hdml', 2, 4, 31, 7), '>=',
	num(file('programBezSensu.hdml', 2, 4, 31, 2), 23),
	var(file('programBezSensu.hdml', 2, 10, 37, 1), 'E')),
    op(file('programBezSensu.hdml', 2, 17, 44, 2), '#', 
	var(file('programBezSensu.hdml', 2, 18, 45, 1), 'F')),
	op(file('programBezSensu.hdml', 2, 25, 52, 5), '+',
	var(file('programBezSensu.hdml', 2, 25, 52, 1), 'G'),
	var(file('programBezSensu.hdml', 2, 29, 56, 1), 'H')))) ])).
tests(sumator, file('sumator.hdml'), yes).
tests(bramkaOrOf, file('bramkaOrOf.hdml'), yes).
tests(sumatorLogGlebokosc, file('sumatorLogGlebokosc.hdml'), yes).
tests(sumatorLogGlebokoscAux, file('sumatorLogGlebokoscAux.hdml'), yes).
tests(sumatorRownolegly, file('sumatorRownoleglySzeregowy.hdml'), yes).
tests(mainSumatorow, file('prostyMainSumatorow.hdml'), yes).
tests(mainTestZPozycjami, input("def main(_) = 1"), program([def(main, wildcard(file(test, 1, 10, 9, 1)), num(no, 1))])).
tests(definicjaOdApostrofa, input("def '(A, B)) = A^B"), no).
tests(definicjaOdSlowaKluczowego, input("def else()"), no).
tests(operatorBinarnyZJednymArgumentem, input("def abc (A,B) = A +"), no). 
tests(kwadratSumyLiczb, input("def _KWSumy(a, b) = (a + b) ^ 2"), yes).
tests(prostaRekurencja, file('rekurencja.hdml'), yes).
tests(zlyArgumentFunkcji, input("def main(+A, +B) = A"), no).
tests(podwojneFunkcje, input("def dod(A,B) = A + B
							  def odej(A, B) = A - B
							  def pot(A, B) = A ^ B"), yes).
tests(zagniezdzoneNawiasy, input("def naw(x, (y,z)) = 0+((y+z)+(x-z))"), yes).
tests(brakNawiasu, input("def main(a,b) = ((a) + (b)"), no).
tests(zleWyrazenieLet, input("def main(a,b) = let #x = 5"), no).
tests(chceBycSkryptem, input("if e > 0 then 1 else 0"), no).
tests(kilkaRoznychFunkcjiIdentyfikatory, input("def (A,B) = A + B
												def (C, D)  = ~C
												def (X, Y, Z) = if X > Y then #Err else (X+Y)+Z"), yes).
tests(dzialanieModulo, input("def mod(A, B) = A % B"), yes).
tests(wyrazanieLetIn, input("def _letIN(A1, A2) = 
							let A1 = A3 % A4 in
							 if A4 > 3 then A1 = 0 else A1 = 1"), yes).
tests(invalid_comment,file('adderwithcomments.hdml'),no).
