# Changelog

## Unreleased

- Add `set_level` function to allow configuring log visibility.

## v1.1.0 - 2024-04-26

- The `configure` function now no-ops rather than crashing if the default Erlang
  logger handler has been uninstalled, such as when Elixir has been added to a
  project.

## v1.0.1 - 2024-02-17

- Fixed a bug with the formatting of structured reports.

## v1.0.0 - 2024-02-17

- Initial release.
