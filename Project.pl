% 107489 Duarte Ponce
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- ['dados.pl'], ['keywords.pl']. % ficheiros a importar.
/*
*/

/*
As tres funcoes a seguir tem como objetivo iterar e encontrar todos os eventos referentes a
uma certa condicao
1- eventos sem sala
2- eventos sem sala num certo dia da semana
3- eventos sem sala que pertencem a um dos periodos da lista dada
*/
eventosSemSalas(Eventos):-
    findall(ID, evento(ID, _, _, _, semSala), Eventos).

eventosSemSalasDiaSemana(DiaDaSemana, EventosSemSala):-
    findall(ID, (evento(ID, _, _, _, semSala), horario(ID, DiaDaSemana, _, _, _, _)), EventosSemSala).

eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala):-
    findall(ID, (evento(ID, _, _, _, semSala), length(ListaPeriodos, Len), 
    Len > 0, aux(ListaPeriodos, ID) ), EventosSemSala).  % Meche na ordem

% De seguida esta uma recursiva que criei para fazer a verificacao dos periodos do predicato anterior

aux([], _).
aux([H | T], ID):-

    (H == p1),!,
    horario(ID, _, _, _, _, Periodo),
    length(T, Len), 

    (\+ member(Periodo, [p1, p1_2]), Len > 0 -> 
        aux(T, ID);
    member(Periodo, [p1, p1_2]));

    (H == p2),!,
    horario(ID, _, _, _, _, Periodo),
    length(T, Len),

    (\+ member(Periodo, [p2, p1_2]), Len > 0 -> 
    aux(T, ID);
    member(Periodo, [p2, p1_2]));

    (H == p3),!,
    horario(ID, _, _, _, _, Periodo),
    length(T, Len),

    (\+ member(Periodo, [p3, p3_4]), Len > 0 -> 
    aux(T, ID);
    member(Periodo, [p3, p3_4]));
    
    (H == p4),!,
    horario(ID, _, _, _, _, Periodo),
    length(T, Len),

    (\+ member(Periodo, [p4, p3_4]), Len > 0 -> 
    aux(T, ID);
    member(Periodo, [p4, p3_4])).


/*
O proximo predicado avalia recursivamente se os eventos que veem na ListaE
pertencem ao periodo indicado em P
*/
organizaEventos(ListE, P, Res):-
    organizaEventos(ListE, P, [], Res2),!,
    sort(Res2, Res).

organizaEventos([], _, Res, Res).

organizaEventos([H | T], P, Accu, Res):-
    aux([P], H),
    organizaEventos(T, P, [H | Accu], Res).

organizaEventos([_ | T], P, Accu, Res):-
    organizaEventos(T, P, Accu, Res).

/*
Os proximos dois predicados avaliao primeiro os eventos com duracao menor ou igual a dada e adiciona os numa lista
o outro avalia em true or false se um evento especifico respeita a mesma condicao do anterior
*/
eventosMenoresQue(Duracao, ListaEventosMenoresQue):-
    findall(ID, (horario(ID, _, _, _, Time, _), Time =< Duracao), ListaEventosMenoresQue).

eventosMenoresQueBool(ID, Duracao):-
    horario(ID, _, _, _, Duracaoe, _), Duracaoe =< Duracao.

/*
Predicado que procura todas as cadeiras de um curso e adiciona numa lista
*/
procuraDisciplinas(Curso, List2):-
    findall(ID, turno(ID, Curso, _, _), Accu),
    findall(Cadeiras, (evento(ID, Cadeiras, _, _, _), member(ID, Accu)), ListD),
    sort(ListD, List2).

/*
Predicado recursivo que organiza as diciplinas dadas em duas listas uma referente ao primeiro semestre e outra ao segundo de um mesmo curso
*/
organizaDisciplinas(ListD, Curso, Res):-
    organizaDisciplinas(ListD, Curso, [], [], Res).

organizaDisciplinas([], _, Res1, Res2, [Res1n, Res2n]):-
    sort(Res1, Res1n), sort(Res2, Res2n).

organizaDisciplinas([Disc | Resto], Curso, Accu1, Accu2, Res):- 
    turno(ID, Curso, _, _), 
    evento(ID, Disc, _, _, _),
    horario(ID, _, _, _, _, Periodo),
    member(Periodo, [p1, p2, p1_2]),

    organizaDisciplinas(Resto, Curso, [Disc | Accu1], Accu2, Res).

organizaDisciplinas([Disc | Resto], Curso, Accu1, Accu2, Res):- 
    turno(ID, Curso, _, _), 
    evento(ID, Disc, _, _, _),
    horario(ID, _, _, _, _, Periodo),
    member(Periodo, [p3, p4, p3_4]),

    organizaDisciplinas(Resto, Curso, Accu1, [Disc | Accu2], Res).

% Predicado que descobre a soma de horas totais de um curso num ano especifico e num periodo desse ano

horasCurso(P, Curso, Ano, Horas):-
    findall(ID, (turno(ID, Curso, Ano, _), aux([P], ID)), IDs),
    sort(IDs, IDS),
    findall(Tempo, (member(ID, IDS), horario(ID, _, _, _, Tempo, _)), TempoLista),
    sumlist(TempoLista, Horas).

/*
Predicado evolucaoHorasCurso e a sua auxiliar recursiva respetiva que criam uma lista de tuplos 
que criam a soma de horas desse curso em todos os anos e periodos dele
*/
auxEvo(Ano, Per, Curso, Res):-
    auxEvo(Ano, Per, Curso, [], Res).

auxEvo([], [], _, Res, Res).

auxEvo([A | Resto1], [P | Resto2], Curso, Accu, Res):-
    horasCurso(P, Curso, A, Horas),
    auxEvo(Resto1, Resto2, Curso, [(A, P, Horas) | Accu], Res).

evolucaoHorasCurso(Curso, Evolucao):-
    Ano = [1,1,1,1,2,2,2,2,3,3,3,3],
    Per = [p1,p2,p3,p4,p1,p2,p3,p4,p1,p2,p3,p4],
    auxEvo(Ano, Per, Curso, EvolucaoPre),
    reverse(EvolucaoPre, Evolucao).

% Predicado que verifica a intercecao entre o intervalo de tempo dado e o de um evento

ocupaSlot(InicioD, FimD, InicioE, FimE, Horas):-
   
    (InicioD =< InicioE, FimD >= FimE) ->
        Horas is FimE - InicioE,
        Horas > 0; 
        
    (InicioD >= InicioE, FimD =< FimE) ->
        Horas is FimD - InicioD,
        Horas > 0;
    
    (InicioD =< InicioE, FimD =< FimE, FimD >= InicioE) ->
        Horas is FimD - InicioE,
        Horas > 0;
    
    (InicioD >= InicioE, FimD >= FimE, InicioD =< FimE) ->
        Horas is FimE - InicioD,
        Horas > 0.
/*
Predicaado que soma todas as intercecoes de horas (usanso o ocupaSlot) de todos 
os eventos de um tipo de sala e periopdo especifico
*/

numHorasOcupadas(P, TipoSala, DiaSemana, HoraI, HoraF, Soma):-

    salas(TipoSala, LstSalas),

    findall(ID, (evento(ID, _, _, _, Sala), member(Sala, LstSalas),
    horario(ID, DiaSemana, _, _, _, _), aux([P], ID)), LstIDs),

    findall(ID, (member(ID, LstIDs), horario(ID, _, HI, HF, _, _),
    ocupaSlot(HoraI, HoraF, HI, HF, _)), LstIDs2),

    findall(Tempo, (member(ID, LstIDs2), horario(ID, _, HI, HF, _, _), 
    ocupaSlot(HoraI, HoraF, HI, HF, Tempo)), TempoLista),

    sumlist(TempoLista, Soma).

/*
Predicado que multilplica o delta do tempo das horas dadas e multilplica
pelo numero de salas de um certo tipo dado
*/
ocupacaoMax(TipoSala, HoraI, HoraF, Max):-
    salas(TipoSala, LstSalas),
    length(LstSalas, Len),
    Max is (HoraF - HoraI) * Len.

% Calcula a percentagem entre a soma de horas e a ocupacao maxima 
percentagem(SomaH, Max, Percentagem):-
    Percentagem is (SomaH / Max) * 100.

auxSalas(Sala, Tipo):-
    salas(grandesAnfiteatros, Lst), member(Sala, Lst) -> salas(Tipo, Lst);   
    salas(pequenosAnfiteatros, Lst), member(Sala, Lst) -> salas(Tipo, Lst);
    salas(salasAula, Lst), member(Sala, Lst) -> salas(Tipo, Lst);
    salas(labsPC, Lst), member(Sala, Lst) -> salas(Tipo, Lst);
    salas(labsElectro, Lst), member(Sala, Lst) -> salas(Tipo, Lst);
    salas(labsQuimica, Lst), member(Sala, Lst) -> salas(Tipo, Lst);
    salas(labsFisica, Lst), member(Sala, Lst) -> salas(Tipo, Lst);
    salas(labsRedes, Lst), member(Sala, Lst) -> salas(Tipo, Lst);
    salas(labsJogos, Lst), member(Sala, Lst) -> salas(Tipo, Lst);
    salas(videoConf, Lst), member(Sala, Lst) -> salas(Tipo, Lst).
/*
Predicado que descobre a lista de predicados de casos criticos referentes a uma percentagem num tipo de sala
num dia de semana maiores do que o threshold tolerado
*/

ocupacaoCritica(HoraI, HoraF, Threshold, Res):-

    findall(casosCriticos(DiaS, TipoSala, Percentagem), (evento(ID, _, _, _, Sala),
    horario(ID, DiaS, _, _, _, Periodo), auxSalas(Sala, TipoSala),
    numHorasOcupadas(Periodo, TipoSala, DiaS, HoraI, HoraF, SomaH), ocupacaoMax(TipoSala, HoraI, HoraF, Max),
    percentagem(SomaH, Max, Per), Per > Threshold, Percentagem is ceiling(Per)), ResPre),
    
    sort(ResPre, Res).

/*
Este predicado auxiliar premite verificar recursivamente se uma certa disposicao de mesas verifica
todas as restricoes impostas caso nao respeite da false
*/
auxMesas([], _).
auxMesas([H | T], Mesas):-

    H = cab1(P),!, 
    nth1(4, Mesas, El), El == P,!, 
    auxMesas(T, Mesas);

    H = cab2(P),!,
    nth1(5, Mesas, El), El == P,!,
    auxMesas(T, Mesas);

    H = honra(P1, P2) ->
        (nth1(I, Mesas, P1), I = 4 ->
            nth1(6, Mesas, El), El = P2,
            auxMesas(T, Mesas);

        nth1(I, Mesas, P1), I = 5 ->
            nth1(3, Mesas, El), El = P2,
            auxMesas(T, Mesas));
    
    H = frente(P1, P2) ->
       (nth1(I, Mesas, P1), I = 1 ->
            nth1(6, Mesas, El), El = P2,
            auxMesas(T, Mesas);

        nth1(I, Mesas, P1), I = 2 ->
            nth1(7, Mesas, El), El = P2,
            auxMesas(T, Mesas);

        nth1(I, Mesas, P1), I = 3 ->
            nth1(8, Mesas, El), El = P2,
            auxMesas(T, Mesas);
            
        nth1(I, Mesas, P1), I = 6 ->
            nth1(1, Mesas, El), El = P2,
            auxMesas(T, Mesas);

        nth1(I, Mesas, P1), I = 7 ->
            nth1(2, Mesas, El), El = P2,
            auxMesas(T, Mesas);

        nth1(I, Mesas, P1), I = 8 ->
            nth1(3, Mesas, El), El = P2,
            auxMesas(T, Mesas));

    H = naoFrente(P1, P2) ->

        (nth1(I, Mesas, P1), I = 1 ->
            nth1(6, Mesas, El), El \= P2,
            auxMesas(T, Mesas);

        nth1(I, Mesas, P1), I = 2 ->
            nth1(7, Mesas, El), El \= P2,
            auxMesas(T, Mesas);

        nth1(I, Mesas, P1), I = 3 ->
            nth1(8, Mesas, El), El \= P2,
            auxMesas(T, Mesas);
            
        nth1(I, Mesas, P1), I = 6 ->
            nth1(1, Mesas, El), El \= P2,
            auxMesas(T, Mesas);

        nth1(I, Mesas, P1), I = 7 ->
            nth1(2, Mesas, El), El \= P2,
            auxMesas(T, Mesas);

        nth1(I, Mesas, P1), I = 8 ->
            nth1(3, Mesas, El), El \= P2,
            auxMesas(T, Mesas));

    H = lado(P1, P2) ->

        (nth1(I, Mesas, P1), I = 1 ->
            nth1(2, Mesas, El), El = P2,
            auxMesas(T, Mesas);
        
        nth1(I, Mesas, P1), I = 3 ->
            nth1(2, Mesas, El), El = P2,
            auxMesas(T, Mesas);
            
        nth1(I, Mesas, P1), I = 2 ->
            nth1(1, Mesas, El1), nth1(3, Mesas, El2),
            member(P2,[El1, El2]),
            auxMesas(T, Mesas);
        
        nth1(I, Mesas, P1), I = 6 ->
            nth1(7, Mesas, El), El = P2,
            auxMesas(T, Mesas);
        
        nth1(I, Mesas, P1), I = 8 ->
            nth1(7, Mesas, El), El = P2,
            auxMesas(T, Mesas);
            
        nth1(I, Mesas, P1), I = 7 ->
            nth1(6, Mesas, El1), nth1(8, Mesas, El2),
            member(P2,[El1, El2]),
            auxMesas(T, Mesas));

    H = naoLado(P1, P2) ->

        (nth1(I, Mesas, P1), I = 1 ->
            nth1(2, Mesas, El), El \= P2,
            auxMesas(T, Mesas);
        
        nth1(I, Mesas, P1), I = 3 ->
            nth1(2, Mesas, El), El \= P2,
            auxMesas(T, Mesas);
            
        nth1(I, Mesas, P1), I = 2 ->
            nth1(1, Mesas, El1), nth1(3, Mesas, El2),
            \+ member(P2,[El1, El2]),
            auxMesas(T, Mesas);
        
        nth1(I, Mesas, P1), I = 6 ->
            nth1(7, Mesas, El), El \= P2,
            auxMesas(T, Mesas);
        
        nth1(I, Mesas, P1), I = 8 ->
            nth1(7, Mesas, El), El \= P2,
            auxMesas(T, Mesas);
            
        nth1(I, Mesas, P1), I = 7 ->
            nth1(6, Mesas, El1), nth1(8, Mesas, El2),
            \+ member(P2,[El1, El2]),
            auxMesas(T, Mesas)).
/*
predicado que cria e testa exaustivamente e aleatoriamente qual sera a mesa que respeitara todas as restricoes impostas 
*/
ocupacaoMesa(LstPessoas, LstRestricoes, Mesa):-
    findall(LstMesa, (permutation(LstPessoas, LstMesa), auxMesas(LstRestricoes, LstMesa)), ResTemporario),
    ResTemporario = [[A,B,C,D,E,F,G,H]],            % Fiz estas substituicoes menos eficientes para facilitar no findall a aleatoprizacao e testagem das condicoes
    [[A,B,C],[D,E],[F,G,H]] = Mesa.  