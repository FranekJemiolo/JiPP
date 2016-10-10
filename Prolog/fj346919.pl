% Franciszek Jemioło, index number 346919
:- ensure_loaded(library(lists)).
% Defining operator for ouir program
:- op(700, xfx, <>).

%-------------------------------------------------------------------------------
%---------------------VERIFY----------------------------------------------------
%-------------------------------------------------------------------------------
 
% Main function- verifies if program (in given file) run in N processes is safe.
verify(N, ProgramFile) :-
    (verifyNumProc(N) ->
	    readFile(ProgramFile, Vars, Arrays, Program),
        initState(Vars, Arrays, Program, N, InitialState),
        dfs(N, Program, [InitialState], [], MoreThanAllStates),
        removeDuplicates(MoreThanAllStates, AllStates),
        tagStates(AllStates, 1, TaggedStates),
        distinctStates(AllStates, DistinctStates),
        getAllSections(Program, 1, [], Sections),
        findWrongState(DistinctStates, Sections, [], WrongState),
        (WrongState = [SomeState|_] ->
            get(SomeState, TaggedStates, NumStmt),
            displayResult(WrongState, NumStmt, Sections)
        ;
            displayResult([], -1, Sections)),        
        % We do not look for more results
        !
    ;
	    (write('Error: parametr '), 
        write(N), 
        write(' powinien byc liczba > 0'),
        fail)).

% Just checking if number is greater than 0.
verifyNumProc(N) :-
    N > 0.

% Just reading the file an returning list of variables, list of arrays and a 
% list of instructions (program)
readFile(ProgramFile, Vars, Arrays, Program) :-
    set_prolog_flag(fileerrors, off),
    see(ProgramFile),
    read(Vars),
    read(Arrays),
    read(Program),
    seen.

readFile(ProgramFile, _, _, _) :-
    format('Error: brak pliku o nazwie - ~p.~n', [ProgramFile]), abort.

%-------------------------------------------------------------------------------
%---------------------FINDING-WRONG-STATES--------------------------------------
%-------------------------------------------------------------------------------

% Get all the number of lines in code with sekcja statement.
getAllSections(program([]), _, Sections, Sections).

getAllSections(program([sekcja|RestStmts]), LineNum, CurrSections, Sections) :-
    NextLineNum is LineNum + 1,
    getAllSections(program(RestStmts), NextLineNum, [LineNum|CurrSections], 
        Sections).

getAllSections(program([_|RestStmts]), LineNum, CurrSections, Sections) :-
    NextLineNum is LineNum + 1,
    getAllSections(program(RestStmts), NextLineNum, CurrSections, Sections).

% Finding wrong state - we look only for one, where two or more proccesses are
% on sekcja stmt.
findWrongState([], _, WrongStates, WrongStates).

findWrongState([State|RestStates], Sections, CurrWrongStates, WrongStates) :-
    State = state(_, _, ProcessCounters, _),
    countProcessesInSection(ProcessCounters, Sections, 0, ProcessesInSection),
    (ProcessesInSection > 1 ->
        findWrongState([], [], [State|CurrWrongStates], WrongStates)
    ;
        findWrongState(RestStates, Sections, CurrWrongStates, WrongStates)).

% We are counting here how many processes got into sekcja statement.
countProcessesInSection([], _, ProcessesInSection, ProcessesInSection).

countProcessesInSection([item(_, LineNum)|RestProcessCounters], Sections, 
    CurrInSection, ProcessesInSection) :-
        (member(LineNum, Sections) ->
            NewInSection is CurrInSection + 1,
            countProcessesInSection(RestProcessCounters, Sections, 
                NewInSection, ProcessesInSection)
        ;
            countProcessesInSection(RestProcessCounters, Sections, 
                CurrInSection, ProcessesInSection)).

% Almost identical to countProcessesInSection but here we waste more space
% to find the list of the processes in sekcja statement.
getProcessesInSection([], _, ProcessesInSection, ProcessesInSection).

getProcessesInSection([item(PID, LineNum)|RestProcessCounters], Sections, 
    CurrInSection, ProcessesInSection) :-
        (member(LineNum, Sections) ->
            add(PID, CurrInSection, NewInSection),
            getProcessesInSection(RestProcessCounters, Sections, NewInSection, 
                ProcessesInSection)
        ;
            getProcessesInSection(RestProcessCounters, Sections, CurrInSection,
                ProcessesInSection)).


% First parameter is the wrong state, second is the number of the state
% that is not safe, and the third is list of all sekcja stmt's in program
displayResult([], _, _) :-
    write('Program jest poprawny (bezpieczny).').

displayResult(WrongStates, N, Sections) :-
    WrongStates = [state(_,_,ProcessCounters, StmtHistory)|_],
    format('Program jest niepoprawny: stan nr ~d jest niebezpieczny ~n', [N]),
    reverse(StmtHistory, ReversedStmtHistory),
    % Now display how we achieved this state
    displayHistory(ReversedStmtHistory),
    getProcessesInSection(ProcessCounters, Sections, [], ProcessesInSection),
    write('Procesy w sekcji: '),
    displayProcesses(ProcessesInSection).

% Just pretty printing processes
displayProcesses([]) :-
    nl.

displayProcesses([PID]) :-
    write(PID),
    write('.'),
    nl.

displayProcesses([PID|[ANOTHERPID|Rest]]) :-
    write(PID),
    write(', '),
    displayProcesses([ANOTHERPID|Rest]).

% Just printing how we achieved this state.
displayHistory([]) :-
    nl.

displayHistory([moved(PID,LineNum)|RestMoves]) :-
    format('Proces ~d:  ~d ~n', [PID, LineNum]),
    displayHistory(RestMoves).
    
%-------------------------------------------------------------------------------
%---------------------FINDING-ALL-STATES----------------------------------------
%-------------------------------------------------------------------------------

% We perform here just simple DFS search to find every possible state
dfs(_, _, [], _, []).

dfs(N, Program, ToVisit, Visited, AllStates) :-
    % ToVisit is a list of states to visit so we get the first state to visit    
    ToVisit = [State|RestToVisit],
    % Check if the state was not already visited
    (\+ containsState(State, Visited) ->
        (getChildStates(Program, State, 0, N, ChildStates) ->
            % Add to visited        
            add(State, Visited, NewVisited),
            % Add the child states to begining of the list
            append(ChildStates, RestToVisit, NewToVisit),
            % Recursive call (just like in dfs as it is dfs)
            dfs(N, Program, NewToVisit, NewVisited, RestAllStates),
            % Also add the child states to list of all states, because when
            % we check for children we ignore all states without them       
            append(ChildStates, RestAllStates, AllStatesWithoutParent),
            % Yes this is sometimes unnecessary but we remove them if 
            % they're not needed
            add(State, AllStatesWithoutParent, AllStates)
        ;
            AllStates = [State])
            
    ;
        dfs(N, Program, RestToVisit, Visited, AllStates)).

getChildStates(_, _, Start, Start, []). 

getChildStates(Program, State, Start, Stop, ChildStates) :-
    Start < Stop,
    Next is Start + 1,
    step(Program, State, Start, StateOut),
    getChildStates(Program, State, Next, Stop, RestChildStates),
    add(StateOut, RestChildStates, ChildStates).
    
%-------------------------------------------------------------------------------
%---------------------INIT-STATE------------------------------------------------
%-------------------------------------------------------------------------------
% State consists of three dictionaries and a list. I will write them just below.
% VarsDict -> A dictionary with mapped variable names to their values
% ArraysDict -> A dictionary with mapped array names to array values(dicts)
% ProcessCounters -> A dictionary with mapped processes id's to current line
%   in code.
% StmtHistory -> This is just a list of pairs - (Process id, number of the line 
% of code that was executed). We use it to display how we achieved the wrong
% state where two or more processes achieve 'sekcja' line simultaneously.
% Initialize state, all variables and arrays are set to 0, and processes
% instructions counters are set to 1. The +Program is not used that's why it is
% _.

initState(vars(Vars), arrays(Arrays), _, N, state(VarsDict, ArraysDict, 
    ProcessCounters, [])) :-
        dictFromList(Vars, 0, VarsDict),
        length(Arrays, ArraysLen),
        initArrays(N, Arrays, ArraysLen, ArraysDict),
        createList(N, 0, ListProcessCounters),
        dictFromList(ListProcessCounters, 1, ProcessCounters).        


initArrays(_, [], 0, []).

initArrays(N, [Array|RestArrays], ArraysLen, 
    [item(Array, ArrayDict)|RestDict]) :-
        ArraysLen > 0,
        RestLen is ArraysLen - 1,
        createList(N, 0, ListForArray),
        dictFromList(ListForArray, 0, ArrayDict),
        initArrays(N, RestArrays, RestLen, RestDict).

%-------------------------------------------------------------------------------
%---------------------STEP------------------------------------------------------
%-------------------------------------------------------------------------------

step(Program, StateIn, PrId, StateOut) :-
    Program = program(Stmts),
    StateIn = state(_, _, ProcessCounters, _),
    get(PrId, ProcessCounters, NumStmt),
    getFromList(NumStmt, Stmts, Stmt),
    interpret(Stmt, NumStmt, StateIn, PrId, StateOut).

%-------------------------------------------------------------------------------
%---------------------INTERPRET-------------------------------------------------
%-------------------------------------------------------------------------------

addStepToHistory(PrId, NumStmt, StmtHistory, NewStmtHistory) :-
    PerformedStep = moved(PrId, NumStmt),
    add(PerformedStep, StmtHistory, NewStmtHistory).
    
% assign statement 
interpret(assign(Var, Exp), NumStmt, StateIn, PrId, StateOut) :-
    StateIn = state(VarsDict, ArraysDict, ProcessCounters, StmtHistory),
    NextNumStmt is NumStmt + 1,
    (contains(Var, VarsDict) ->
        eval(Exp, Evaluated, PrId, StateIn),
        replace(Var, Evaluated, VarsDict, NewVarsDict),
        addStepToHistory(PrId, NumStmt, StmtHistory, NewStmtHistory),
        replace(PrId, NextNumStmt, ProcessCounters, NewProcessCounters),
        % Creating new state
        StateOut = state(NewVarsDict, ArraysDict, NewProcessCounters, 
            NewStmtHistory)
    ;
        % We know that the program is correct so this must be array that exists.
        Var = arr(Ident, Position),
        % Evaluating both the expression to be bounded, and the position
        % in the array.
        eval(Exp, Evaluated, PrId, StateIn),
        eval(Position, EvaluatedPosition, PrId, StateIn),
        % Get the array
        get(Ident, ArraysDict, Array),
        % Replace the value in the array
        replace(EvaluatedPosition, Evaluated, Array, NewArray),
        % Replace the array in list of arrays.
        replace(Ident, NewArray, ArraysDict, NewArraysDict), 
        addStepToHistory(PrId, NumStmt, StmtHistory, NewStmtHistory),
        replace(PrId, NextNumStmt, ProcessCounters, NewProcessCounters),
        % Creating new State.
        StateOut = state(VarsDict, NewArraysDict, NewProcessCounters, 
            NewStmtHistory)).

% goto statement
interpret(goto(NextNumStmt), NumStmt, StateIn, PrId, StateOut) :-
    StateIn = state(VarsDict, ArraysDict, ProcessCounters, StmtHistory),
    addStepToHistory(PrId, NumStmt, StmtHistory, NewStmtHistory),
    replace(PrId, NextNumStmt, ProcessCounters, NewProcessCounters),
    % Creating new state
    StateOut = state(VarsDict, ArraysDict, NewProcessCounters, 
        NewStmtHistory).

% condGoto statement
interpret(condGoto(BExp, GotoNumStmt), NumStmt, StateIn, PrId, StateOut) :-
    StateIn = state(VarsDict, ArraysDict, ProcessCounters, StmtHistory),
    eval(BExp, Evaluated, PrId, StateIn),
    (Evaluated = true ->
        NextNumStmt is GotoNumStmt
    ;
        NextNumStmt is NumStmt + 1),
    addStepToHistory(PrId, NumStmt, StmtHistory, NewStmtHistory),
    replace(PrId, NextNumStmt, ProcessCounters, NewProcessCounters),
    % Creating new State.
    StateOut = state(VarsDict, ArraysDict, NewProcessCounters, 
        NewStmtHistory).

% sekcja statement
interpret(sekcja, NumStmt, StateIn, PrId, StateOut) :-
    StateIn = state(VarsDict, ArraysDict, ProcessCounters, StmtHistory),
    NextNumStmt is NumStmt + 1,
    addStepToHistory(PrId, NumStmt, StmtHistory, NewStmtHistory),
    replace(PrId, NextNumStmt, ProcessCounters, NewProcessCounters),
    % Creating new State.
    StateOut = state(VarsDict, ArraysDict, NewProcessCounters, 
        NewStmtHistory).

%-------------------------------------------------------------------------------
%---------------------EVAL------------------------------------------------------
%-------------------------------------------------------------------------------

% Evaluating boolean expressions
eval(Exp1<Exp2, Evaluated, PrId, State) :- 
    eval(Exp1, Evaluated1, PrId, State), 
    eval(Exp2, Evaluated2, PrId, State),
    (Evaluated1 < Evaluated2 ->
        Evaluated = true
    ;
        Evaluated = false).

eval(Exp1=Exp2, Evaluated, PrId, State) :- 
    eval(Exp1, Evaluated1, PrId, State), 
    eval(Exp2, Evaluated2, PrId, State),
    (Evaluated1 = Evaluated2 ->
        Evaluated = true
    ;
        Evaluated = false).

eval(Exp1<>Exp2, Evaluated, PrId, State) :- 
    eval(Exp1, Evaluated1, PrId, State), 
    eval(Exp2, Evaluated2, PrId, State),
    (Evaluated1 \= Evaluated2 ->
        Evaluated = true
    ;
        Evaluated = false).

% Evaluating number expressions
eval(Exp1+Exp2, Evaluated, PrId, State) :-
    eval(Exp1, Evaluated1, PrId, State),
    eval(Exp2, Evaluated2, PrId, State),
    Evaluated is Evaluated1 + Evaluated2.

eval(Exp1-Exp2, Evaluated, PrId, State) :-
    eval(Exp1, Evaluated1, PrId, State),
    eval(Exp2, Evaluated2, PrId, State),
    Evaluated is Evaluated1 - Evaluated2.

eval(Exp1*Exp2, Evaluated, PrId, State) :-
    eval(Exp1, Evaluated1, PrId, State),
    eval(Exp2, Evaluated2, PrId, State),
    Evaluated is Evaluated1 * Evaluated2.

eval(Exp1/Exp2, Evaluated, PrId, State) :-
    eval(Exp1, Evaluated1, PrId, State),
    eval(Exp2, Evaluated2, PrId, State),
    Evaluated is Evaluated1 / Evaluated2.

% Evaluating everything 'else'
eval(Exp, Evaluated, PrId, State) :-
    State = state(VarsDict, ArraysDict, _, _),
    (contains(Exp, VarsDict) ->
        get(Exp, VarsDict, Evaluated)
    ;
        Exp = arr(Ident, Position) ->
            eval(Position, EvaluatedPosition, PrId, State),
            % Get the array from dict, then the value from array
            get(Ident, ArraysDict, Array),
            get(EvaluatedPosition, Array, Evaluated)
        ;
            Exp = pid ->
                Evaluated = PrId
            ;
                number(Exp) ->
                    Evaluated = Exp).

%-------------------------------------------------------------------------------
%---------------------HELPER-FUNCTIONS------------------------------------------
%-------------------------------------------------------------------------------

% Creating dict from list, and setting values to X
dictFromList([], _, []).

dictFromList([Elem|Rest], X, [item(Elem,X)|RestDict]) :-
    dictFromList(Rest, X, RestDict).

% Returns pair at given position.
getAt(Position, [item(Elem, X)|Rest], Result) :-
    (Position is 0 ->
        Result = item(Elem, X)
    ;
        NewPosition is Position - 1,
        getAt(NewPosition, Rest, Result)).

% Check if dict contains some key
contains(Key, [item(Key, _)|_]).

contains(Key, [item(_, _)|Rest]) :-
    contains(Key, Rest).

% Get the value from dict under some key.
get(Key, [item(Key, Value)|_], Value).

get(Key, [item(_, _)|Rest], Result) :-
    get(Key, Rest, Result). 

% Replace value in dict under some key.
replace(Key, RepValue, [item(Key, _)|Rest], [item(Key, RepValue)|Rest]).

replace(Key, RepValue, [item(SKey, SValue)|Rest], 
    [item(SKey, SValue)|Result]) :-
        replace(Key, RepValue, Rest, Result).

% Creating list from Starting index. like [0,1,2,3].
createList(0, _, []).

createList(Len, Idx, [Idx | Rest]) :-
    Len > 0,
    NewLen is Len - 1,
    NewIdx is Idx + 1,
    createList(NewLen, NewIdx, Rest).

% Just adding element to list
add(Element, List, [Element|List]).

% Get element from list at some position.
getFromList(Position, [Element|Rest], Result) :-
    (Position is 1 ->
        Result = Element
    ;
        Position > 1,
        NewPosition is Position -1,
        getFromList(NewPosition, Rest, Result)).

% Two states are the same if Variables and Arrays have the same values,
% end every process is at the same line of code. We do not take history in to 
% the account when comparing.
containsState(state(VarsDict, ArraysDict, ProcessCounters, _), 
    state(VarsDict, ArraysDict, ProcessCounters, _)).

containsState(state(VarsDict, ArraysDict, ProcessCounters, _), 
    [state(VarsDict, ArraysDict, ProcessCounters, _)|_]).

containsState(State, [_|RestStates]) :-
    containsState(State, RestStates).

% Check for TRULY THE SAME states
containsTheSame(Same, Same).

containsTheSame(Same, [Same|_]).

containsTheSame(State, [_,RestStates]) :-
    containsTheSame(State, RestStates).


% Remove duplicate states
removeDuplicates([], []).

removeDuplicates([State|RestStates], WithoutDuplicates) :-
    (containsTheSame(State, RestStates) ->
        removeDuplicates(RestStates, WithoutDuplicates)
    ;
        WithoutDuplicates = [State|RestWithoutDuplicates],
        removeDuplicates(RestStates, RestWithoutDuplicates)).

% Finding distinct states
distinctStates([], []).

distinctStates([State|RestStates], DistinctStates) :-
    (containsState(State, RestStates) ->
        distinctStates(RestStates, DistinctStates)
    ;
        DistinctStates = [State|RestDistinctStates],
        distinctStates(RestStates, RestDistinctStates)).


% Tagging states with unique numbers
tagStates([], _, []).

tagStates([State|RestStates], Index, [item(State, Index)|RestLabeledStates]) :-
    NewIndex is Index + 1,    
    tagStates(RestStates, NewIndex, RestLabeledStates).
    
