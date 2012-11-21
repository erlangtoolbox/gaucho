%% Copyright
-module(gaucho_callback).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([error/4, request/2]).

-callback(request(Uri :: binary(), Body :: binary()) -> ok).
-callback(error(Code :: integer(), Message :: binary(), Uri :: binary(), Body :: binary()) -> ok).

request(_Uri, _Body) -> ok.

error(Code, Message, Uri, Body) ->
    error_logger:error_report([Code, Message, Uri, Body]).
