-module(term2html).

-export([expand/1]).

-include_lib("../include/term2html.hrl").

expand(Items) ->
    unicode:characters_to_list(expand(Items, [])).

expand([], Acc) ->
    ?REV(Acc);
expand([Item | Rest], Acc) when is_tuple(Item) ->
    expand(Rest, [expand_item(Item) | Acc]);
expand(Item, Acc) when is_binary(Item) ->
    ?REV([Item | Acc]);
expand(Item, Acc) when is_tuple(Item) ->
    ?REV([expand_item(Item) | Acc]);
expand(Item, Acc) when is_list(Item) ->
    case is_string(Item) of
        true -> [Item | Acc];
        _ -> ?REV([[expand(I, []) || I <- Item] | Acc])
    end.

expand_item({Tag}) ->
    expand_item({Tag, []});
expand_item({Tag, Attributes}) ->
    case is_void_element(Tag) of
        true ->
            Tag_str = to_str(Tag),
            case item_attributes(Attributes) of
                [] -> ["<", Tag_str, ">"];
                Attr -> ["<", Tag_str, " ", Attr, ">"]
            end;
        _ ->
            expand_item({Tag, Attributes, ""})
    end;
expand_item({Tag, Attributes, Content}) ->
    InnerHTML = expand(Content, []),
    Tag_str = to_str(Tag),
    case item_attributes(Attributes) of
        [] -> ["<", Tag_str, ">", InnerHTML, "</", Tag_str, ">"];
        Attr -> ["<", Tag_str, " ", Attr, ">", InnerHTML, "</", Tag_str, ">"]
    end.

item_attributes(Attributes) ->
    lists:join(" ", lists:map(fun attr/1, Attributes)).

attr({Key, List}) when is_list(List) ->
    Key_str = to_str(Key),
    case is_string(List) of
        true -> [Key_str, "=\"", escape(List), "\""];
        _ -> [Key_str, "=\"", clsx:run(List), "\""]
    end;
attr({Key, Value}) ->
    [to_str(Key), "=\"", to_str(Value), "\""];
attr(Other) ->
    to_str(Other).

to_str(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_str(Binary) when is_binary(Binary) -> Binary;
to_str(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_str(Float) when is_float(Float) -> io_lib:fwrite("~w", [Float]);
to_str(String) when is_list(String) -> String.

escape(String) ->
    escape(String, []).

escape([], Acc) -> ?REV(Acc);
escape([$& | Tail], Acc) -> escape(Tail, ["&amp;" | Acc]);
escape([$< | Tail], Acc) -> escape(Tail, ["&lt;" | Acc]);
escape([$> | Tail], Acc) -> escape(Tail, ["&gt;" | Acc]);
escape([$" | Tail], Acc) -> escape(Tail, ["&quot;" | Acc]);
escape([L | Tail], Acc) -> escape(Tail, [L | Acc]).

is_string([]) -> true;
is_string([I | Tail]) when is_integer(I) -> is_string(Tail);
is_string(_) -> false.

void_elements() ->
    [area, base, br, col, frame, embed, hr, img, input, isindex,
     link, meta, param, source, track, wbr].

is_void_element(Tag) ->
    lists:member(Tag, void_elements()).
