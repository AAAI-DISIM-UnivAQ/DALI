%--****utils****--%
:-use_module(library(pillow)).
:-use_module(library(lists)).
:-use_module(library(xml)).

:-dynamic elementlist/1.
:-dynamic binding/3.
:-dynamic result/1.

charColon(58). %% ANSI code of ':'

take_result(Var, Result, Value):-
        Result=result(BindList),         
        member(binding([name=Var], _, ValueCodes), BindList),
		  name(Value, ValueCodes).

%% La funzione seguente serve per reintrodurre lo spazio bianco negli atomi che fanno matching con letterali
%% atomo_prolog -> "atomo prolog".
denormalize(Var, DenormalizedValue):-
   atom_codes(Var, VarCodes),
   substitute(95,VarCodes,32,DenormalizedValue). %%95='_' 32=' '

normalize(Var, Result, NormalizedValue):-
        Result=result(BindList),         
        member(binding([name=Var], Type, Value), BindList),
        if(Type=uri,
                take_name_uri(Value, NormalizedValue), 
                if(Type=literal, take_name(Value,NormalizedValue), Value=NormalizedValue) %% ultimo caso significa che c'è un nodo blank.
        ),!.

fetch_ask(Response,Bool):-
        member(content(C),Response), xml_parse(C,XmlParsed),
        xml_subterm(XmlParsed, element(boolean,[],[pcdata(Bool)])),!,
        if(Bool="true",true,false).

fetch_select(Response, ResultsList):-
        member(content(C),Response), xml_parse(C,XmlParsed), 
        xml_subterm(XmlParsed, element(results,[],L)), L\=[],
        assert(elementlist([])),
        last(L,Lu),
        repeat,
                member(Result,L),
                if(Result=element(result,_,RawBindList),
                (assert_bindings(RawBindList), %%side effect           
                 setof(binding(V,T,Vl),binding(V,T,Vl),BindList),
                 retractall(binding(_,_,_)),
                 clause(elementlist(ElementList),_),
                 retractall(elementlist(_)),
                 append(ElementList,[result(BindList)], ElementList1),
                 assert(elementlist(ElementList1))
                ),true),
        Result=Lu,
        !,
        clause(elementlist(ResultsList),_),
        retractall(elementlist(_)).

assert_bindings(RawBindList):-
        last(RawBindList,Lu1),
        repeat,
                member(ElementBinding,RawBindList),
                ElementBinding=element(binding,Var,[Bind]),
                Bind=element(Type, _, [pcdata(Value)]),
                assert(binding(Var, Type, Value)),
         ElementBinding=Lu1,!.

create_query_url(Query,Prefixes,Repository,QueryCodes):-
   name(Prefixes, PrefixesC), name(Repository, RepositoryC),
   append(PrefixesC,Query,PQCodes),name(PQ,PQCodes),
   url_query([query=PQ],UrlArgs),
   append(RepositoryC,UrlArgs,QueryCodes).

do_query(QueryCodes,HostPort,Response):-
		  charColon(Colon),
		  name(HostPort,HostPortC),append(HostC,[Colon|PortC],HostPortC),		  
		  name(Host,HostC), name(Port,PortC),
        fetch_url( http(Host,Port,QueryCodes),[host(HostPort),accept('application/sparql-results+xml, */*;q=0.5')],Response).

query(Query,ArgList,Mode):-
        see('owl/query.conf'),
        search_label(Mode),
        read(Q),
        seen,
        next_marker(Q,ArgList,[],QueryR),reverse(QueryR,Query).

search_label(Mode):-
        read(T),
        if(T=Mode,true,if(T=end_of_file,(seen,false),search_label(Mode))).


next_marker([],[],Temp,Temp).
next_marker(Q,ArgList,Temp,Query):-
        Q=[A|Q1], [A]="%", 
        append(ArgList1,[X],ArgList),
        name(X,XC), reverse(XC,XCR),
        append(XCR,Temp,Temp1),
        next_marker(Q1,ArgList1,Temp1,Query).
next_marker(Q,List,Temp,Query):-
        Q=[A|Q1],
        Temp1=[A|Temp],
        next_marker(Q1,List,Temp1,Query).


%take name torna la stringa in lowerCase e senza spazi, sostituendoli con degli underscore
take_name(X,Z):-
        clean_blanks(X,Y),
        to_lower(Y,K),
        name(Z,K).   
     
%take_name_uri la stringa dell'uri, in lowerCase
take_name_uri(UriCodes,Z):-
        %%Se l'indirizzo termina con '/', allora ignora il '/'.
        if(last(UriCodes,47),
           last(UriCodes1,47,UriCodes),UriCodes=UriCodes1),
        remove_uri(UriCodes1,X),
        to_lower(X,K),
        name(Z,K).

to_lower(Y,Z):-
        to_lower1(Y,[],Z).

to_lower1([],Temp,Z):-reverse(Temp,Z).
to_lower1(Y,Temp,Z):-
        Y=[A|Rest],
        if((A>64,A<91),
           ( B is A+32, Temp1=[B|Temp], to_lower1(Rest,Temp1,Z) ),
           ( Temp1=[A|Temp], to_lower1(Rest,Temp1,Z) )
        ).

clean_blanks(X,Y):-%%32 blank e 95 underscore
        substitute(32,X,95,Y).

remove_uri(UriCodes,X):-
        search_backslash(UriCodes,[],X).

search_backslash(UriCodes,X,X):-
   last(UriCodes,47);last(UriCodes,35). %%47 = '/', 35= '#""'

search_backslash(UriCodes,X,Z):-
        last(UriCodes,Last),
        append(UriCodes1,[Last],UriCodes),
        append([Last],X,Y),
        search_backslash(UriCodes1,Y,Z).

%%Funzioni per la composizione di query
compose_triple_pattern_uri([], [], Object, Delimiter, Result):-
   Object\=[],
   acute_brackets(Object, Triple),
   append(Triple, Delimiter, Result).

compose_triple_pattern_uri([], Predicate, Object, Delimiter, Result):-
   Predicate\=[],
   acute_brackets(Predicate,Predicate1),
   acute_brackets(Object, Object1),
   append(Predicate1, " ", Predicate2),
   append(Predicate2, Object1, Triple),
   append(Triple, Delimiter, Result).

compose_triple_pattern_uri(Subject, Predicate, Object, Delimiter, Result):-
   Subject\=[],
   acute_brackets(Subject, Subject1),
   acute_brackets(Predicate, Predicate1),
   acute_brackets(Object, Object1),
   append(Subject1, " ", Subject2),
   append(Predicate1, " ", Predicate2),
   append(Subject2, Predicate2, SubPred),
   append(SubPred, Object1, Triple),
   append(Triple, Delimiter, Result).

acute_brackets(Uri, Result):-
        name(Uri, UriCodes),
        append("<",UriCodes,UriCodes1),
        append(UriCodes1, ">", Result).
