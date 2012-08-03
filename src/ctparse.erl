-module(ctparse).
-include("ctparse_desctiptor.hrl").
-export([init/2,from/3, to/2]).

init(ContentType, ModuleName) ->
    Module = get_module(ContentType),
    Module:init(ModuleName).

from(#ctparse_descriptor{module=Module} = Descriptor, Value, Type) ->
    Module:from(Descriptor, Value, Type).

to(#ctparse_descriptor{module=Module} = Descriptor, Value) ->
    Module:to(Descriptor, Value).


get_module("application/json") ->
    ctparse_json;
get_module(ContentType) ->
    erlang:error(cowboy_ext_not_implemented, [ContentType]).
