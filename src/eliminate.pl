% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

estrai_token_ext_le(C,Tok,Tok1):-name(C,L),estrai_token_ext_le(L,Tok,Tok1).


estrai_token_ext_le([],_,Tok1):-findall(X,clause(extract_tok_le_term(X),_),L),
                                name(Tok1,L),retractall(extract_tok_le_term(_)).
estrai_token_ext_le([A|B],Tok,_):-not(clause(found_le_E,_)),A=69,findall(X,clause(extract_tok_le_term(X),_),L),
                                name(Tok,L),assert(found_le_E),retractall(extract_tok_le_term(_)).
estrai_token_ext_le([A|B],Tok,Tok1):-not(clause(found_le_E,_)),not(A=69),assert(extract_tok_le_term(A)),estrai_token_ext_le(B,Tok,Tok1).
estrai_token_ext_le([A|B],Tok,Tok1):-clause(found_le_E,_),assert(extract_tok_le_term(A)),estrai_token_ext_le(B,Tok,Tok1).

