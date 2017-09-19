-module(horner2).
-export([server/1, client/2]).

%Given Coeffs list [C[N], C[N-1], ..., C[1], C[0]], set up a server
%to evaluate the polynomial
%
%   C[N]*X^(N) + C[N-1]]*X^(N-1) + ... + C[1]*X^(1) + C[0]
%
%at some (yet unspecified) X.
%
%The server must consist of multiple Erlang processes with one Erlang 
%process per polynomial coefficient (there may be additional processes too).
%The processes for the coefficients should store their respective
%coefficients and communicate among themselves to evaluate the
%polynomial at a specified X using Horner's rule.
%
%The return value of this function should be the PID of the server.
%
%The details of the messages exchanged between the processes for
%evaluating the polynomial can be chosen by the implementation.
%However, the server must respond to a 'stop' message by stopping
%all the processes as well as itself.
%
%When a process for a coefficient is assisting in evaluating the
%polynomial, it must log to standard error it's stored coeffient
%and the value accumulated so far in the Horner evaluation.
%When a process for a coefficient is being stopped, it must
%log to standard error it's stored coefficient and the 'stop'
%message.  

server(Coeffs) ->
PID = spawns(Coeffs,[]),
PID.

process(L) ->
PID = spawn(L,[]),
receive
	{R, Msg} ->
do_log(Coeff, Msg) ->
io:format(standard_error, "coeff = ~w; ~w~n", [Coeff, Msg]).
createprocessids([A|B])
createprocess(B)
spawnprocesses([A|B])
hornerevaluate(PIDS)
patternmatch1 - This clause evaluates the horner method till the last but one coefficient
patternmatch2 - This clause evaluates last coefficient and client pid
%PIDS :It pattern matches and call itself with another processid and the last process returns the result to the client.
spawnprocesses([H|T], ListofPIDs)->
list = pid ++ [spawn(horner2, loop, [Head])],
spawnprocesses(T,sample);
spawnprocesses([], ListofPIDs) ->
ListofPIDs.
end

client(Pid, X) ->
PID ! {self(), X}
	receive
		{R, Msg} ->
		R
	end.


createprocessids() 
%Its spawns number of processes which are equal to number of coefficient
%Return the value at X of the polynomial stored within the polynomial server
%specified by PID.


%client will call a function which starts execution of horner

