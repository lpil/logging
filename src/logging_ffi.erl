-module(logging_ffi).
-export([configure/0, format/2, set_primary_config/2]).

configure() ->
    logger:update_primary_config(#{
        level => info,
        filter_default => log,
        filters => [
            {domain,{fun logger_filters:domain/2, {stop, sub, [otp,sasl]}}},
            {domain,{fun logger_filters:domain/2, {stop, sub, [supervisor_report]}}}
        ],
        metadata => #{}
    }),
    logger:update_handler_config(default, #{
        formatter => {logging_ffi, []}
    }),
    nil.

format(Event, _Config) ->
    format(Event).

format(#{level := Level, msg := Msg, meta := _Meta}) ->
    [format_level(Level), format_msg(Msg), $\n].

format_level(Level) ->
    case Level of
        emergency -> "\x1b[1;41mEMRG\x1b[0m";
        alert -> "\x1b[1;41mALRT\x1b[0m";
        critical -> "\x1b[1;41mCRIT\x1b[0m";
        error -> "\x1b[1;31mEROR\x1b[0m";
        warning -> "\x1b[1;33mWARN\x1b[0m";
        notice -> "\x1b[1;32mNTCE\x1b[0m";
        info -> "\x1b[1;34mINFO\x1b[0m";
        debug -> "\x1b[1;36mDEBG\x1b[0m"
    end.

format_msg(Report0) ->
    case Report0 of
        {string, Msg} ->
            [$\s, Msg];

        {report, Report1} when is_map(Report1) ->
            format_kv(maps:to_list(Report1));

        {report, Report1} when is_list(Report1) ->
            format_kv(Report1);

        _ ->
            [$\s, gleam@string:inspect(Report0)]
    end.

format_kv(Pairs) ->
    case Pairs of
        [] -> [];
        [{K, V} | Rest] when is_atom(K) -> [
            $\s, erlang:atom_to_binary(K), $=, gleam@string:inspect(V) 
            | format_kv(Rest)
        ];
        [{K, V} | Rest]  -> [
            $\s, gleam@string:inspect(K), $=, gleam@string:inspect(V) 
            | format_kv(Rest)
        ];
        Other -> gleam@string:inspect(Other)
    end.

set_primary_config(Key, Level) ->
    case logger:set_primary_config(Key, Level) of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason}
    end.
