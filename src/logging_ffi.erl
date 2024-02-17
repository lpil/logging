-module(logging_ffi).
-export([configure/0, format/2]).

configure() ->
    ok = logger:update_primary_config(#{
        level => info,
        filter_default => log,
        filters => [
            {domain,{fun logger_filters:domain/2, {stop, sub, [otp,sasl]}}},
            {domain,{fun logger_filters:domain/2, {stop, sub, [supervisor_report]}}}
        ],
        metadata => #{}
    }),
    ok = logger:update_handler_config(default, #{
        formatter => {logging_ffi, []}
    }),
    nil.

format(Event, _Config) ->
    format(Event).

format(#{level := Level, msg := Msg, meta := _Meta}) ->
    [format_level(Level), $\s, format_msg(Msg), $\n].


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

format_msg(Report) ->
    case Report of
        {string, Msg} -> Msg;
        {report, Report} when is_map(Report) -> format_msg(maps:to_list(Report));
        {report, Report} when is_list(Report) -> [$\s, format_kv(Report)];
        _ -> gleam@string:inspect(Report)
    end.

format_kv(Pairs) ->
    case Pairs of
        [] -> [];
        [{K, V} | Rest] when is_atom(K) -> [
            erlang:atom_to_binary(K), $=, gleam@string:inspect(V) 
            | format_kv(Rest)
        ];
        [{K, V} | Rest]  -> [
            gleam@string:inspect(K), $=, gleam@string:inspect(V) 
            | format_kv(Rest)
        ];
        Other -> gleam@string:inspect(Other)
    end.
