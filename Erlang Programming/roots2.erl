-module(roots2).
-export([server/0, client/2, loop/0]).
-import(roots1, [roots/3]).
%Return the PID of a quadratic-solver Erlang process which when given
%a message containing the coefficients A, B, C responds with a message
%giving the roots of the quadratic equation A*X^2 + B*X + C.  It is
%assumed that the return'd roots are not complex.
server()-> SPID = spawn(roots2, loop, []),
	SPID.


loop()->
receive
{ClientPid, {A, B, C}}->
ClientPid ! {self(), roots1:roots(A,B,C)},	
loop();
stop -> true
end.

%Given the PID of a quadratic-solver and Coeffs a 3-tuple {A, B, C}, 
%this function uses that quadratic-solver to return the roots of the
%quadratic equation A*X^2 + B*X + C.  The roots are return'd as a
%2-tuple, with the first element of the return'd tuple using the
%positive square-root of the discriminant and the second element of
%the return'd tuple using the negative square-root of the
%discriminant.  It is assumed that the return'd roots are not complex.
client(PID, Coeffs)->
PID!{self(), Coeffs},
receive
{PID, Response}->
Response
end.