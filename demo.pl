:- use_module(regex).
:- use_module(library(http/dcg_basics)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
1 ?- time(forall(between(1, 100000, _), t2(C, "Content-length: 42", []))).
% 4,600,039 inferences, 1.000 CPU in 1.026 seconds (98% CPU, 4600039 Lips)
true.

2 ?-
|    time(forall(between(1, 100000, _), test(C, "Content-length: 42", []))).
% 11,000,001 inferences, 2.320 CPU in 2.387 seconds (97% CPU, 4741380 Lips)
true.

9 ?- time(t3(100000)).
% 300,003 inferences, 3.570 CPU in 3.606 seconds (99% CPU, 84034 Lips)
true.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

test(Count) -->
	regex('Content-length:\\s*(\\d+)', [number(Count)]).


t2(Count) -->
	"Content-length:", whites, number(Count).

t3(Times) :-
	new(Re, regex('Content-length: *([0-9]+)')),
	Text = 'Content-length: 42',
	forall(between(1, Times, _),
	       (   send(Re, match, Text),
		   get(Re, register_value, Text, 1, int, V),
		   V == 42
	       )).
