% #!/usr/bin/env swipl

% :- initialization(main).

% main(Argv) :-

one(Left,Right,Scope,Left,Right,NewScope) :- plus(Scope,1,NewScope).

height(Left,Right,Scope,Left,Right,NewScope) :-
	length(Left,Len),
	plus(Scope,Len,NewScope).

pop([],Right,Scope,[],Right,Scope).
pop([Popend|Left],Right,Scope,Left,Right,NewScope) :- plus(Scope,Popend,NewScope).

swap(Left,Right,Scope,Right,Left,Scope).

run_contents([],Left,Right,Scope,Left,Right,Scope).
run_contents([H|T],LeftS,RightS,ScopeS,LeftF,RightF,ScopeF) :-
	call(H,LeftS,RightS,ScopeS,LeftM,RightM,ScopeM),
	run_contents(T,LeftM,RightM,ScopeM,LeftF,RightF,ScopeF).

push(Contents,LeftS,RightS,ScopeS,[ScopeM|LeftF],RightF,ScopeF) :-
	run_contents(Contents,LeftS,RightS,ScopeS,LeftF,RightF,ScopeF),
	plus(ScopeS,ScopeM,ScopeF).

negative(Contents,LeftS,RightS,ScopeS,LeftF,RightF,ScopeF) :-
	run_contents(Contents,LeftS,RightS,0,LeftF,RightF,ScopeM),
	plus(ScopeF,ScopeM,ScopeS).

loop(_,[],Right,Scope,[],Right,Scope) :- !.
loop(_,[0|Left],Right,Scope,[0|Left],Right,Scope) :- !.
loop(Contents,LeftS,RightS,ScopeS,LeftF,RightF,ScopeF) :-
	run_contents(Contents,LeftS,RightS,ScopeS,LeftM,RightM,ScopeM),
	loop(Contents,LeftM,RightM,ScopeM,LeftF,RightF,ScopeF).

zero(Contents,LeftS,RightS,Scope,LeftF,RightF,Scope) :-
	run_contents(Contents,LeftS,RightS,0,LeftF,RightF,_).
