-module(term2html_test).


-include_lib("eunit/include/eunit.hrl").

empty_expand_returns_empty_string_test() ->
    ?assertEqual("", term2html:expand([])).

tag_only_test() ->
    ?assertEqual("<toto></toto>", term2html:expand({toto})),
    ?assertEqual("<br>", term2html:expand({br})).

tag_with_attributes_test() ->
    Term = {toto, [{key, value}, attribute]},
    ?assertEqual("<toto key=\"value\" attribute></toto>",
                 term2html:expand(Term)).

void_tag_with_attributes_test() ->
    Term = {img, [{src, <<"https://img.com/penguin.png">>},
                  {alt, "superbe image of a penguin"}]},
    ?assertEqual("<img src=\"https://img.com/penguin.png\" "
                 "alt=\"superbe image of a penguin\">",
                 term2html:expand(Term)).

all_void_tags_should_quietly_discard_content_test_() ->
    Test = fun (Tag) ->
                   Term = {Tag, [{class, ~"test"}], ~"content"},
                   ExpectedHtml = lists:concat(["<", Tag, " class=\"test\">"]),
                   ?_assertEqual(ExpectedHtml, term2html:expand(Term))
           end,
    [Test(Tag) || Tag <- term2html:void_elements()].

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
                 "<titi c=\"d\" test=\"1\" "
                 "tutu=\"3.14159265358979\">super</titi>"
                 "</toto>",
                 term2html:expand(Term)).

tag_escape_attribute_test() ->
    Term = {button,
            [{onclick, "alert(\"hello <USER_NAME> & tata\");"}],
            "click-me"},
    ?assertEqual("<button onclick=\"alert(&quot;hello &lt;"
                 "USER_NAME&gt; &amp; tata&quot;);\">"
                 "click-me"
                 "</button>", term2html:expand(Term)).

html_list_test() ->
    Term = {ul, [],
            [{li, [], I} || I <- lists:seq(1, 3)]},
    ?assertEqual("<ul>"
                 "<li>1</li>"
                 "<li>2</li>"
                 "<li>3</li>"
                 "</ul>",
                 term2html:expand(Term)).

html_deep_list_test() ->
    Term = {'div', [], [[{p}], {span}]},
    ?assertEqual("<div><p></p><span></span></div>",
                 term2html:expand(Term)).

clsx_with_class_attribute_test() ->
    Term = {'div', [{class, [a, {b, true}, {c, false}, "d", <<"töp"/utf8>>]}],
            <<"tëst clsx"/utf8>>},
    ?assertEqual("<div class=\"a b d töp\">tëst clsx</div>",
                 term2html:expand(Term)).

no_clsx_with_other_attribute_test() ->
    Term = {form, [{action, [<<"ä"/utf8>>, "bé", c]}], <<"cöntent"/utf8>>},
    ?assertEqual("<form action=\"äbéc\">cöntent</form>",
                 term2html:expand(Term)).

float_test() ->
    Term = {span, [], 14.7},
    ?assertEqual("<span>14.7</span>",
                 term2html:expand(Term)).
