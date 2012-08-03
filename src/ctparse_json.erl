-module(ctparse_json).
-include("ctparse_desctiptor.hrl").
-export([init/1, from/3, to/2]).


-spec init/1 :: (atom()) -> #ctparse_descriptor{}.
init(ParserModuleName) ->
    #ctparse_descriptor{parser_module=ParserModuleName, module=?MODULE}.

-spec from/3 :: (#ctparse_descriptor{}, any(), atom()) -> any().
from(#ctparse_descriptor{parser_module=ParserModuleName}, Value, Type) -> 
    ParserModuleName:from_json(Value, Type).

-spec to/2 :: (#ctparse_descriptor{}, any()) -> any().
to(#ctparse_descriptor{parser_module=ParserModuleName}, Value) ->
    ParserModuleName:to_json(Value).
