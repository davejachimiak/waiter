# waiter

Waiter is an abstract development server for compiled languages. It runs
your server, compiles when you change a specified file, and manages
compilation and server processes.

## Install

### Homebrew
```sh
$ brew install waiter
```

## Usage

```sh
$ waiter SERVER_COMMAND BUILD_COMMAND [-f|--file-name-regex REGEX] [-d|--dir DIR]
```

Example:
```
$ waiter "ENV=development ./blog-server" "cabal build" -f "hs$" -d ./lib
```

That would run your server binary `./blog-server` with the environment
variable `ENV` set to `development`. It would run `cabal build` when
files with the regex `hs$` in the `./lib` tree change. After all build
processes finish, it would kill the previous `./blog-server` process and
starts a new one with the newly built binary.

### Required:
#### SERVER_COMMAND
This is the command that runs your server.

Example:
```
"ENV=developemnt ./blog-server"
```

#### BUILD_COMMAND
Example:
```
"cabal build"
```

### Optional
#### [-f|--file-name-regex REGEX]
Only filenames matching this regex trigger `BUILD_COMMAND`s. Uses POSIX
regular expression interface. Defaults to `.*`.

#### [-d|--dir DIR]
Only changes to files in this directory's tree trigger `BUILD_COMMAND`s.
Defaults to `./src`.

## Build from source

1. `git clone` and `cd` into this repo.
2. `cabal sandbox init`
3. `cabal install`
