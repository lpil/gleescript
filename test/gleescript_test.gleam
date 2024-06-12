import gleescript
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn arguments_parsed_correctly_test() {
  ["--target-dir", "test"]
  |> gleescript.get_target_dir
  |> should.equal("test/")

  []
  |> gleescript.get_target_dir
  |> should.equal("./")
}
