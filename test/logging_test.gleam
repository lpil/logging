import logging.{Alert, Critical, Debug, Emergency, Error, Info, Notice, Warning}
import gleeunit
import gleeunit/should

pub fn main() {
  logging.configure()
  logging.log(Emergency, "Hello!")
  logging.log(Alert, "Hello!")
  logging.log(Critical, "Hello!")
  logging.log(Error, "Hello!")
  logging.log(Warning, "Hello!")
  logging.log(Notice, "Hello!")
  logging.log(Info, "Hello!")
  logging.log(Debug, "Hello!")
  gleeunit.main()
}

pub fn hello_world_test() {
  1
  |> should.equal(1)
}
