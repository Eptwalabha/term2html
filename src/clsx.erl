-module(clsx).

-export([run/1]).

-include_lib("../include/term2html.hrl").

run(Proplists) ->
    Classes = lists:uniq(?REV(lists:map(fun to_str/1, run(Proplists, [])))),
    string:join(Classes, " ").

to_str(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_str(List) when is_list(List) -> List.

run([], Acc) ->
    Acc;
run([{_, False} | Tail], Acc)
  when False =:= false; False =:= null; False =:= undefined;
       False =:= 0; False =:= []; False =:= '' ->
    run(Tail, Acc);
run([{Key, Predicate} | Tail], Acc) when is_function(Predicate, 0) ->
    case Predicate() of
        true -> run(Tail, [Key | Acc]);
        {true, Key2} -> run(Tail, [Key2 | Acc]);
        _ -> run(Tail, Acc)
    end;
run([{Key, _} | Tail], Acc) ->
    run(Tail, [Key | Acc]);
run([False | Tail], Acc)
  when False =:= false; False =:= null; False =:= undefined;
       False =:= 0; False =:= []; False =:= '' ->
    run(Tail, Acc);
run([Atom | Tail], Acc) when is_atom(Atom) ->
    run(Tail, [atom_to_list(Atom) | Acc]);
run([List | Tail], Acc) when is_list(List) ->
    case catch string:trim(List) of
        [] -> run(Tail, Acc);
        {'EXIT', _} -> run(Tail, run(List, []) ++ Acc);
        Key -> run(Tail, [Key | Acc])
    end;
run([{true, If_true, _} | Tail], Acc) ->
    run(Tail, [If_true | Acc]);
run([{false, _, If_false} | Tail], Acc) ->
    run(Tail, [If_false | Acc]);
run([_ | Tail], Acc) ->
    run(Tail, Acc).

