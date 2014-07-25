:-['communication_onto_utils.pl'].

%%Se si vuole sfruttare solo la PROPRIA ONTOLOGIA
subproperty(Y,X):-
	if(var(X),false,true),
	clause(agent(A),_), 
	clause(ontology(Prefixes,[Repository,Host],A),_);clause(ontology(Prefixes,[Repository,Host],AgM),_),
	query(Query,[X,Y,Y],"SUBPROPERTY"),
	create_query_url(Query,Prefixes,Repository,QueryCodes),
   do_query(QueryCodes,Host,Response),
	fetch_ask(Response,Bool), %% C''era un cut 
   if(Bool="true",true,false).

%%Se si vuole usare anche la capacità dell''ontologia dell''agente mittente
subproperty(Y,X,AgM):-
	if(var(X),false,true),
	clause(agent(A),_), 
	clause(ontology(Prefixes,[Repository,Host],A),_);clause(ontology(Prefixes,[Repository,Host],AgM),_),
	query(Query,[X,Y,Y],"SUBPROPERTY"),
	create_query_url(Query,Prefixes,Repository,QueryCodes),
   do_query(QueryCodes,Host,Response),
	fetch_ask(Response,Bool),
   if(Bool="true",true,false).

%%Se si vuole sfruttare solo la PROPRIA ONTOLOGIA
subclass(Y,X):-
	if(var(X),false,true),
	clause(agent(A),_), 
	clause(ontology(Prefixes,[Repository,Host],A),_);clause(ontology(Prefixes,[Repository,Host],AgM),_),
	query(Query,[X,Y,Y],"SUBCLASS"),
	create_query_url(Query,Prefixes,Repository,QueryCodes),
   do_query(QueryCodes,Host,Response),
	fetch_ask(Response,Bool),
   if(Bool="true",true,false).

%%Se si vuole usare anche la capacità dell''ontologia dell''agente mittente
subclass(Y,X,AgM):-
	if(var(X),false,true),
	clause(agent(A),_), 
	clause(ontology(Prefixes,[Repository,Host],A),_);clause(ontology(Prefixes,[Repository,Host],AgM),_),
	query(Query,[X,Y,Y],"SUBCLASS"),
	create_query_url(Query,Prefixes,Repository,QueryCodes),
   do_query(QueryCodes,Host,Response),
	fetch_ask(Response,Bool),
   if(Bool="true",true,false).

eq_property(Y,X,Prefixes,[Repository,Host]):-
        query(Query,[Y],"EQ_PROPERTY"),
		  create_query_url(Query,Prefixes,Repository,QueryCodes),
   	  do_query(QueryCodes,Host,Response),         
		  fetch_select(Response,ResultsList),!,
        member(Result, ResultsList),
        normalize("O",Result, X).

same_as(Y,X,Prefixes,[Repository,Host]):-
        query(Query,[Y],"SAME_AS"),
		  create_query_url(Query,Prefixes,Repository,QueryCodes),
   	  do_query(QueryCodes,Host,Response),
        fetch_select(Response,ResultsList),!,
        member(Result, ResultsList),
        normalize("O",Result, X).

eq_class(Y,X,Prefixes,[Repository,Host]):-
        query(Query,[Y],"EQ_CLASS"),
        create_query_url(Query,Prefixes,Repository,QueryCodes),
   	  do_query(QueryCodes,Host,Response),
        fetch_select(Response,ResultsList),!,
        member(Result, ResultsList),
        normalize("O",Result, X).

symmetric(Y):-
   clause(agent(A),_), clause(ontology(Prefixes,[Repository,Host],A),_),
   query(Query,[Y],"SYMMETRIC"),
   create_query_url(Query,Prefixes,Repository,QueryCodes),
   do_query(QueryCodes,Host,Response),
   fetch_ask(Response,Bool),
   if(Bool="true",true,false).

symmetric(Y, AgM):-
      clause(agent(A),_), 
		( clause(ontology(Prefixes,[Repository,Host],A),_); clause(ontology(Prefixes,[Repository,Host],AgM),_) ),
      query(Query,[Y],"SYMMETRIC"),
   	create_query_url(Query,Prefixes,Repository,QueryCodes),
   	do_query(QueryCodes,Host,Response),
   	fetch_ask(Response,Bool),
   	if(Bool="true",true,false).        

:-dynamic result_l/1.
:-dynamic vini/1.

cerca_abbinamenti(Pasto,ListaPropNamed):-
        clause(agent(A),_), clause(ontology(Prefixes,[Repository,Host],A),_),
        query(Query,[Pasto],"CERCA_VINI"),
        create_query_url(Query, Prefixes, Repository, QueryCodes),
        do_query(QueryCodes, Host, Response),
        fetch_select(Response,ResultsList),!,
        assert(result_l([])),
        last(ResultsList, Last),
        repeat,
                member(Result, ResultsList),
                take_result("Value1",Result, Uri),
                take_result("Property",Result, Prop),
                if(Result=Last, 
                        compose_triple_pattern_uri([], Prop, Uri, ".", GraphPattern1),
                        compose_triple_pattern_uri([], Prop, Uri, ";", GraphPattern1)
                ),
                clause(result_l(List),_),                
                append(List, GraphPattern1, List1), 
                retractall(result_l(List)), assert(result_l(List1)),
                Result=Last,
        !,
        clause(result_l(ListaProp), _),retractall(result_l(_)), 
        append("'", ListaProp, ListaProp1),
        append(ListaProp1, "'", ListaProp2),
        atom_codes(ListaPropNamed,ListaProp2).

cerca_vini(ListaPropNamed, ListaViniFinale):-
        clause(agent(A),_), clause(ontology(Prefixes,[Repository,Host],A),_),
        query(QueryPropVini, [ListaPropNamed], "PROP_VINI2"), 
        create_query_url(QueryPropVini, Prefixes, Repository, QueryPropViniCodes),
        do_query(QueryPropViniCodes, Host, ResponseVini),
        fetch_select(ResponseVini, ResultsViniList), !, 
        assert(vini([])),
        last(ResultsViniList, LastVino),
        repeat,
                member(Result1, ResultsViniList),
                normalize("Wine", Result1, Vino),
                clause(vini(ListaVino),_), retractall(vini(ListaVino)),
                ListaVino1=[Vino|ListaVino],
                assert(vini(ListaVino1)),
                Result1=LastVino,
        !, clause(vini(ListaViniFinale),_), retractall(vini(_)).
