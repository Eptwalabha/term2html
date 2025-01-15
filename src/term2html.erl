-module(term2html).

-export([expand/1]).

-include_lib("../include/term2html.hrl").

-define(REV(List), lists:reverse(List)).
-define(FLAT(List), lists:flatten(List)).
-define(CONCAT(List), lists:concat(List)).

expand(Items) ->
    ?FLAT(expand(Items, [])).

expand([], Acc) ->
    ?REV(Acc);
expand([Item | Rest], Acc) when is_tuple(Item) ->
    expand(Rest, [expand_item(Item) | Acc]);
expand(Item, Acc) when is_tuple(Item) ->
    ?REV([expand_item(Item) | Acc]);
expand(Item, Acc) when is_list(Item) ->
    expand([], [Item | Acc]).

expand_item({Tag}) ->
    ?CONCAT(["<", Tag, " />"]);
expand_item({Tag, Attributes}) ->
    ?CONCAT(["<", Tag, " ", item_attributes(Attributes), " />"]);
expand_item({Tag, Attributes, Content}) ->
    ?CONCAT(["<", Tag, " ", item_attributes(Attributes), ">",
             expand(Content, []),
             "</", Tag, ">"]).

item_attributes(Attributes) ->
    ?FLAT(lists:join(" ", [attr(Attribute) || Attribute <- Attributes])).

attr({Key, Value}) ->
    ?CONCAT([Key, "=\"", Value, "\""]);
attr(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
attr(Other) ->
    Other.


