# Changelog

## v1.5.0 - 2026-04-03

- Progress logs are now excluded.

## v1.4.0 - 2026-04-03

- OTP logs are no longer excluded.

## v1.3.0 - 2024-07-15

- The `NO_COLOUR` or `NO_COLOR` environment variable can now be used to disable
  colourful logging.

## v1.2.0 - 2024-06-20

- Add `set_level` function to allow configuring log visibility.

## v1.1.0 - 2024-04-26

- The `configure` function now no-ops rather than crashing if the default Erlang
  logger handler has been uninstalled, such as when Elixir has been added to a
  project.

## v1.0.1 - 2024-02-17

- Fixed a bug with the formatting of structured reports.

## v1.0.0 - 2024-02-17

- Initial release.
