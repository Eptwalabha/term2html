-module(term2html_test).


-include_lib("eunit/include/eunit.hrl").

empty_expand_returns_empty_string_test() ->
    ?assertEqual("", term2html:expand([])).

tag_only_test() ->
    ?assertEqual("<toto />", term2html:expand({toto})).

tag_with_attributes_test() ->
    Term = {toto, [{key, value}, attribute]},
    ?assertEqual("<toto key=\"value\" attribute />", term2html:expand(Term)).

tag_with_attributes_and_children_test() ->
    Term = {toto, [{key, value}, attribute],
            [{tata},
             {tutu, [{a, b}, selected, "checked"]},
             {titi, [{c, d}, {"test", 1}], "super"}]},
    ?assertEqual("<toto key=\"value\" attribute>"
                 "<tata />"
                 "<tutu a=\"b\" selected checked />"
                 "<titi c=\"d\" test=\"1\">super</titi>"
                 "</toto>",
                 term2html:expand(Term)).
