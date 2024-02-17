# logging

Configuration for the Erlang logger.

[![Package Version](https://img.shields.io/hexpm/v/logging)](https://hex.pm/packages/logging)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/logging/)

```sh
gleam add logging
```
```gleam
import logging.{Info}

pub fn main() {
  // Run this once at the start of your program
  logging.configure()

  // And get logging!
  logging.log(Info, "Hello, Joe!")
}
```

Further documentation can be found at <https://hexdocs.pm/logging>.
