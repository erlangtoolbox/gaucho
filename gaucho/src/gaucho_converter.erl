-module(gaucho_converter).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
        {to, 3},
        {from, 3}
    ];
behaviour_info(_) ->
    undefined.
