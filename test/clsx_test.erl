-module(clsx_test).

-include_lib("eunit/include/eunit.hrl").

clsx_test_() ->
    Suite = [{"tokens",
              [{["a", b, {c, true}, {d, false}, {e, anything}], "a b c e"},
               {[{a, false}, {b, null}, false, undefined, {c, 0}], ""},
               {[{false, true}], "false"},
               {[{a, false}, a], "a"}]},
             {"tokens are uniq with preserved order",
              [{["b", a, c, a, {b, true}], "b a c"}]},
             {"trim and discard empty token",
              [{["", " a   ", "     "], "a"}]},
             {"deep lists",
              [{[a, [b, [a, c], [[d], e]], [f]], "a b c d e f"}]},
             {"function predicates",
              [{[{predicate, fun() -> true end}], "predicate"},
               {[{key_a, fun() -> {true, "key_b"} end}], "key_b"},
               {[{key_a, fun() -> other end}], ""},
               {[{key, fun() -> false end}], ""}]},
             {"ternary structure",
              [{[{true, "a", "b"}], "a"},
               {[{false, "a", "b"}], "b"},
               {[{other, "a", "b"}], ""}]},
             {"other structures are ignored",
              [{[{ignored}], ""}]}
            ],
    [{Comment, ?_assertEqual(Expected, clsx:run(Input))}
     || {Comment, Sub_suite} <- Suite,
        {Input, Expected} <- Sub_suite].
