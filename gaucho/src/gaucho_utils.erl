-module(gaucho_utils).
-compile(export_all).

-spec get_attribute_types/1 :: (list()) -> tuple().
get_attribute_types([Spec | Specs]) ->
    [get_attribute_type(Spec)| get_attribute_types(Specs)];

get_attribute_types([]) -> [].


-spec get_attribute_type/1 :: (any()) -> tuple().
get_attribute_type({remote_type,_, [{atom,_, option_m},{atom,_,monad},[Type]]}) ->
    {maybe, get_attribute_type(Type)};

get_attribute_type({type, _, SimpleType, []}) when SimpleType =/= list->
    SimpleType;

get_attribute_type({type, _, record, [{atom, _, RecordName}]}) ->
    {record, RecordName};

get_attribute_type(UnsupportedType) ->
    erlang:error(unsupported_type, [UnsupportedType]).


%% process lists and error_m:monad type that are not allowed in attribute specs but allowed in output spec
-spec get_output_type/1 :: (any()) -> tuple().
get_output_type({remote_type, _, [{atom, _, error_m},{atom, _, monad},[Type]]}) ->
    get_output_type(Type);
get_output_type({type, _, list, [Type]}) ->
    {list, get_output_type(Type)};
get_output_type(Type) ->
    get_attribute_type(Type).
