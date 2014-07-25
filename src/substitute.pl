% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

%% Nella libreria "lists" di sicstus manca la funzione "substitute"

:-use_module(library(lists)).

substitute(X,XL,Y,YL):-
	substitute1(X,XL,Y,[],YL).

substitute1(_,[],_,Temp,YL):-reverse(Temp,YL).

substitute1(X,[X|XL1],Y,Temp,YL):-
	cons(Y,Temp,Temp1), substitute1(X,XL1,Y,Temp1,YL).

substitute1(X,[First|XL1],Y,Temp,YL):-
	cons(First,Temp,Temp1), substitute1(X,XL1,Y,Temp1,YL).
