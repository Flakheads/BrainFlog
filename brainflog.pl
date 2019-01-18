:- consult('grammar.dcg').

opt_spec([
	[opt(ascii_out), shortflags(['A']), longflags(['ascii-out']),
	 type(boolean), default(false), help('Outputs as ASCII characters')],
	[opt(help), shortflags(['h']), longflags(['help']), type(boolean),
	 default(false), help('Prints this menu and exits')],
	[opt(execute), shortflags(['e']), longflags(['execute']), type(boolean),
	 default(false), help('Reads source from the first command line argument instead of file')]
	]).

main :-
	opt_spec(OptSpec),
	opt_arguments(OptSpec, Opts, Args),
	(member(help(true),Opts)->
	opt_help(OptSpec,HelpText),
	write(HelpText);
	Args=[File|Argv],
	get_src(File,SrcText,Opts),
	include(brace,SrcText,Src),
	phrase(head(SrcTree),Src),
	maplist(read_arg,Argv,RaggedArgs),
	append(RaggedArgs,Arguments),
	run_contents(SrcTree,Arguments,[],0,Out,_,_),
	format_output(Out,Formattedoutput,Opts),
	write(Formattedoutput),nl).

get_src(File,SrcText,Opts) :- 
	member(execute(true),Opts),
	atom_to_chars(File,SrcText).
get_src(File,SrcText,_) :- 
	open(File,read,SrcFile),
	read_stream_to_codes(SrcFile,SrcText),
	close(SrcFile).

format_output(Out,Formattedoutput,Opts) :-
	member(ascii_out(true),Opts),
	string_codes(Formattedoutput,Out).
format_output(Out,Formattedoutput,_) :- atomic_list_concat(Out,' ',Formattedoutput).

read_arg(Arg,[X]):-atom_number(Arg,X).
read_arg(Arg,X):-append([39|X],[39],Y),atom_to_chars(Arg,Y).
read_arg(Arg,X):-append([34|X],[34],Y),atom_to_chars(Arg,Y).

brace(Code) :- member(Code, `()[]{}<>`).

run_contents([],Left,Right,Scope,Left,Right,Scope).
run_contents([H|T],LeftS,RightS,ScopeS,LeftF,RightF,ScopeF) :-
	call(H,LeftS,RightS,ScopeS,LeftM,RightM,ScopeM),
	run_contents(T,LeftM,RightM,ScopeM,LeftF,RightF,ScopeF).


one(Left,Right,Scope,Left,Right,NewScope) :- plus(Scope,1,NewScope).

height(Left,Right,Scope,Left,Right,NewScope) :-
	length(Left,Len),
	plus(Scope,Len,NewScope).

pop([],Right,Scope,[],Right,Scope).
pop([Popend|Left],Right,Scope,Left,Right,NewScope) :- plus(Scope,Popend,NewScope).

swap(Left,Right,Scope,Right,Left,Scope).


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
