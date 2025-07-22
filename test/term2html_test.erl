-module(term2html_test).


-include_lib("eunit/include/eunit.hrl").

empty_expand_returns_empty_string_test() ->
    ?assertEqual("", term2html:expand([])).

tag_only_test() ->
    ?assertEqual("<toto></toto>", term2html:expand({toto})),
    ?assertEqual("<br>", term2html:expand({br})).

tag_with_attributes_test() ->
    Term = {toto, [{key, value}, attribute]},
    ?assertEqual("<toto key=\"value\" attribute></toto>", term2html:expand(Term)).

void_tag_with_attributes_test() ->
    Term = {img, [{src, <<"https://img.com/penguin.png">>},
                  {alt, "superbe image of a penguin"}]},
    ?assertEqual("<img src=\"https://img.com/penguin.png\" "
                 "alt=\"superbe image of a penguin\">",
                 term2html:expand(Term)).

tag_with_attributes_and_children_test() ->
    Term = {toto, [{key, value}, attribute],
            [{tata},
             {tutu, [{a, b}, selected, "checked"]},
             {br},
             {titi, [{c, d}, {"test", 1}, {<<"tutu">>, math:pi()}], "super"}]},
    ?assertEqual("<toto key=\"value\" attribute>"
                 "<tata></tata>"
                 "<tutu a=\"b\" selected checked></tutu>"
                 "<br>"
                 "<titi c=\"d\" test=\"1\" tutu=\"3.141592653589793\">super</titi>"
                 "</toto>",
                 term2html:expand(Term)).

tag_escape_attribute_test() ->
    Term = {button,
            [{onclick, "alert(\"hello <USER_NAME> & tata\");"}],
            "click-me"},
    ?assertEqual("<button onclick=\"alert(&quot;hello &lt;USER_NAME&gt; &amp; tata&quot;);\">"
                 "click-me"
                 "</button>", term2html:expand(Term)).

clsx_attribute_test() ->
    Term = {'div', [{class, [a, {b, true}, {c, false}, "d"]}],
            <<"test clsx">>},
    ?assertEqual("<div class=\"a b d\">test clsx</div>",
                 term2html:expand(Term)).
