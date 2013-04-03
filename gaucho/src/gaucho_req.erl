-module(gaucho_req).
-export([qs_val_ignore_case/2, cookie_ignore_case/2, qs_lowercase/1, qs_vals_lowercase/1, qs_vals/1]).

-spec qs_val_ignore_case/2 :: (binary()|atom(), cowboy_req:req()) -> {binary()|undefined, cowboy_req:req()}.
qs_val_ignore_case(Name, Req) ->
    req_item_ignore_case(Name, Req, qs_vals).

    
-spec cookie_ignore_case/2 :: (binary()|atom(), cowboy_req:req()) -> {binary()|undefined, cowboy_req:req()}.
cookie_ignore_case(Name, Req) ->
    req_item_ignore_case(Name, Req, cookies).

-spec req_item_ignore_case/3 :: (binary()|atom(), cowboy_req:req(), any()) -> {binary()|undefined, cowboy_req:req()}.
req_item_ignore_case(Name, Req, Fun) ->
    {Items, Req1} = cowboy_req:Fun(Req),
    case xl_lists:find(fun(Item) -> xl_string:equal_ignore_case(element(1,Item), Name) end, Items) of 
        {ok, {_Key, Value}} ->
            {Value, Req1};
        undefined ->
            {undefined, Req1}
    end.

-spec(qs_lowercase(cowboy_req:req()) -> {binary(), cowboy_req:req()}).
qs_lowercase(Req) ->
    {QueryString, Req1} = cowboy_req:qs(Req),
    {xl_string:to_lower(QueryString), Req1}.

qs_vals(Req) ->
    {QSVals, Req1} = cowboy_req:qs_vals(Req),
    {qs_vals_lowercase(QSVals), Req1}.



qs_vals_lowercase(QSVals) ->
    [{ xl_convert:to(atom, xl_string:to_lower(Key)), Value} || {Key, Value} <- QSVals].
