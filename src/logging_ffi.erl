-module(logging_ffi).
-export([configure/0, format/2]).

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
  {Msg, StartColor, EndColor} = case Level of
      emergency -> {"EMRG", "\x1b[1;41m", "\x1b[0m"};
      alert -> {"ALRT", "\x1b[1;41m", "\x1b[0m"};
      critical -> {"CRIT", "\x1b[1;41m", "\x1b[0m"};
      error -> {"EROR", "\x1b[1;31m", "\x1b[0m"};
      warning -> {"WARN", "\x1b[1;33m", "\x1b[0m"};
      notice -> {"NTCE", "\x1b[1;32m", "\x1b[0m"};
      info -> {"INFO", "\x1b[1;34m", "\x1b[0m"};
      debug -> {"DEBG", "\x1b[1;36m", "\x1b[0m"}
  end,
  case os:getenv("NO_COLOR") of
    "true" -> Msg;
    "True" -> Msg;
    _ -> lists:concat([StartColor, Msg, EndColor])
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
