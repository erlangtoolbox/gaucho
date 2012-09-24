-module(gaucho_req).
-compile(export_all).

-spec qs_val_ignore_case/2 :: (binary()|atom(), cowboy_req:req()) -> {binary()|undefined, cowboy_req:req()}.
qs_val_ignore_case(Name, Req) ->
    {QsVals, Req1} = cowboy_req:qs_vals(Req),
    case xl_lists:find(fun(QS) -> xl_string:equal_ignore_case(element(1,QS), Name) end, QsVals) of 
        {ok, {_Key, Value}} ->
            {Value, Req1};
        undefined ->
            {undefined, Req1}
    end.

    
