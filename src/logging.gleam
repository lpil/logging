pub type LogLevel {
  Emergency
  Alert
  Critical
  Error
  Warning
  Notice
  Info
  Debug
}

type DoNotLeak

@external(erlang, "logging_ffi", "configure")
pub fn configure() -> Nil

pub fn log(level: LogLevel, message: String) -> Nil {
  erlang_log(level, message)
  Nil
}

@external(erlang, "logger", "log")
fn erlang_log(level: LogLevel, message: String) -> DoNotLeak
