% Define a non-deterministic Finite State Automata simulator in Prolog.

fsa(Input) :- initial(Q), config(Q, Input).

% Move
config(State, [S|Input]) :- delta(State, S, NewState), config(NewState, Input).

%Final accepting state
config(Q,[]) :- final(State).
