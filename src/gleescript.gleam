// TODO: Do not include dev deps

import filepath
import gleam/dynamic.{type Dynamic}
import gleam/erlang/charlist.{type Charlist}
import gleam/list
import gleam/io
import gleam/result
import gleam/string
import simplifile
import snag
import tom

const build = "build/dev/erlang"

pub fn main() {
  case run() {
    Ok(_) -> Nil

    Error(error) -> {
      io.println_error(snag.pretty_print(error))
      shutdown(1)
    }
  }
}

type Config {
  Config(package_name: String)
}

pub fn run() -> snag.Result(Nil) {
  use config <- result.try(
    load_config()
    |> snag.context("Failed to load configuration"),
  )

  use files <- result.try(
    locate_beam_files()
    |> snag.context("Failed to scan packages in build directory"),
  )

  let emu_args =
    "-escript main gleescript_main_shim -env GLEESCRIPT_MAIN "
    <> config.package_name

  use files <- result.try(
    list.try_map(files, fn(f) {
      let name = charlist.from_string(filepath.base_name(f))
      use content <- result.map(
        simplifile.read_bits(f)
        |> snag_inspect_error
        |> snag.context("Failed to read " <> f),
      )
      #(name, content)
    }),
  )

  let result =
    erlang_escript_create(charlist.from_string(config.package_name), [
      Shebang,
      Comment(charlist.from_string("")),
      EmuArgs(charlist.from_string(emu_args)),
      Archive(files, []),
    ])

  let assert Ok(result) = dynamic.result(Ok, Ok)(result)
  use _ <- result.try(
    result
    |> snag_inspect_error
    |> snag.context("Failed to build escript"),
  )

  io.println("  \u{001b}[35mGenerated\u{001b}[0m ./" <> config.package_name)

  Ok(Nil)
}

fn locate_beam_files() -> snag.Result(List(String)) {
  use files <- result.try(
    simplifile.get_files(build)
    |> snag_inspect_error
    |> snag.context("Failed to read build directory"),
  )

  files
  |> list.filter(fn(f) {
    // The @@ modules belong to the Gleam build tool
    !string.contains(f, "@@")
    && { string.ends_with(f, ".beam") || string.ends_with(f, ".app") }
  })
  |> Ok
}

fn load_config() -> snag.Result(Config) {
  use text <- result.try(
    simplifile.read("gleam.toml")
    |> snag_inspect_error
    |> snag.context("Failed to read gleam.toml"),
  )

  use config <- result.try(
    tom.parse(text)
    |> snag_inspect_error
    |> snag.context("Failed to parse gleam.toml"),
  )

  use package_name <- result.try(
    tom.get_string(config, ["name"])
    |> snag_inspect_error
    |> snag.context("Failed to get package name from gleam.toml"),
  )

  Ok(Config(package_name: package_name))
}

fn snag_inspect_error(result: Result(t, e)) -> snag.Result(t) {
  case result {
    Ok(value) -> Ok(value)
    Error(error) -> snag.error(string.inspect(error))
  }
}

type ErlangOption {
  Shebang
  Comment(Charlist)
  EmuArgs(Charlist)
  Archive(files: List(#(Charlist, BitArray)), zip_options: List(Never))
}

type Never

@external(erlang, "escript", "create")
fn erlang_escript_create(
  file: Charlist,
  sections: List(ErlangOption),
) -> Dynamic

@external(erlang, "init", "stop")
fn shutdown(status: Int) -> a
