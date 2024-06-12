// TODO: Do not include dev deps

import argv
import filepath
import gleam/dynamic.{type Dynamic}
import gleam/erlang/charlist.{type Charlist}
import gleam/io
import gleam/list
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
  Config(package_name: String, out_dir: String)
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

  use _ <- result.try(
    simplifile.create_directory_all(config.out_dir)
    |> snag_inspect_error
    |> snag.context("Failed to create " <> config.out_dir <> " directory"),
  )

  let result =
    erlang_escript_create(
      charlist.from_string(config.out_dir <> config.package_name),
      [
        Shebang,
        Comment(charlist.from_string("")),
        EmuArgs(charlist.from_string(emu_args)),
        Archive(files, []),
      ],
    )

  let assert Ok(result) = dynamic.result(Ok, Ok)(result)
  use _ <- result.try(
    result
    |> snag_inspect_error
    |> snag.context("Failed to build escript"),
  )

  let name = config.out_dir <> config.package_name
  use _ <- result.try(
    simplifile.set_permissions_octal(name, 0o777)
    |> snag_inspect_error
    |> snag.context("Failed to make " <> name <> " executable"),
  )

  io.println(
    "  \u{001b}[95mGenerated\u{001b}[0m "
    <> config.out_dir
    <> config.package_name,
  )

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
  let out_dir = get_out_dir(argv.load().arguments)

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

  Ok(Config(package_name: package_name, out_dir: out_dir))
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

fn get_out_dir(args: List(String)) -> String {
  let out = case args {
    ["--out", folder] | ["--out=" <> folder] -> folder
    _ -> "./"
  }
  case string.last(out) {
    Ok("/") -> out
    _ -> out <> "/"
  }
}
