-----------------------------------------------------------------------------
-- | Documentation for the miso command line tool (haskell-miso/cli): one
-- binary with MicroHs and MicroCabal built in. The prose follows the
-- tool's manual (docs/manual.md) and README, one page per group of
-- commands.
module Site.Docs.Cli
  ( cliPages
  ) where
-----------------------------------------------------------------------------
import Site.Docs.Types
import Site.Prose
import Site.Route
-----------------------------------------------------------------------------
cliPages :: [DocPage]
cliPages =
  [ overview
  , projects
  , dependencies
  , misoYml
  , tool
  ]
-----------------------------------------------------------------------------
-- Overview
-----------------------------------------------------------------------------
overview :: DocPage
overview = DocPage
  { pageSlug = "cli"
  , pageGroup = Cli
  , pageTitle = "Overview"
  , pageBlurb = "The miso command line tool: build, run and serve miso apps with MicroHs, without GHC."
  , pageKeywords = [ "cli", "miso", "microhs", "mhs", "mcabal", "install", "download", "commands", "emscripten", "emcc" ]
  , pageBody =
    [ lead
      [ c "miso", " is one binary that creates, builds, runs and serves miso applications. It has ", a "https://github.com/augustss/MicroHs" "MicroHs", " (", c "mhs", ") and ", a "https://github.com/augustss/MicroCabal" "MicroCabal", " (", c "mcabal", ") built in, so there is no GHC to install: a project is a ", c "miso.yml", ", and its packages come from Hackage." ]
    , h2 "Install"
    , para [ "On Linux and macOS (Intel and Apple Silicon), and on Windows in an ", a "https://www.msys2.org" "MSYS2", " shell:" ]
    , shCopy "cli-install" """
      $ curl -fsSL https://haskell-miso.org/download | sh
      """
    , para
      [ "The installer downloads the release for your OS and architecture from ", a "https://github.com/haskell-miso/cli/releases" "haskell-miso/cli", ", checks its sha256, unpacks it in ", c "~/.miso", " (or ", c "$MISO_HOME", "), and offers to add ", c "~/.miso/bin", " to your ", c "PATH", ". It shows what it will do and asks first; ", c "sh -s -- --yes", " takes the defaults, and ", c "--no-modify-path", " leaves your shell's startup files alone. ", c "MISO_VERSION=v0.1.0", " installs a given release." ]
    , para [ "With nix, the release is a flake package: ", c "nix profile install github:haskell-miso/cli#release", "." ]
    , h2 "Your first app"
    , shCopy "cli-start" """
      $ miso doctor
      $ miso new counter
      $ cd counter
      $ miso dev
      """
    , para
      [ c "miso doctor", " checks the installation and the tools miso needs, and says how to fix what is missing. ", c "miso new", " makes a project from a template; ", c "miso dev", " builds it, serves ", c "public/", " on ", c "http://localhost:8000", ", and rebuilds and reloads the page on every save." ]
    , note
      [ "A new project builds for the browser (", c "target: browser", ", WebAssembly), which needs ", a "https://emscripten.org" "emscripten", " (", c "emcc", "). Without it, set ", c "target: native", " in ", c "miso.yml", " and use ", c "miso run", "." ]
    , h2 "Requirements"
    , ul
      [ [ "A C compiler (", c "cc", "): mhs compiles programs to C." ]
      , [ "For the browser targets (", c "browser", ", ", c "browser_js", "), ", a "https://emscripten.org" "emscripten", " (", c "emcc", ")." ]
      , [ c "curl", ", ", c "tar", ", ", c "sha256sum", " or ", c "shasum", ", and ", c "git", " for git dependencies." ]
      , [ a "https://github.com/ndmitchell/hlint" "hlint", ", for ", c "miso lint", " only." ]
      ]
    , para [ "On Windows, install the tools in MSYS2 first: ", c "pacman -S gcc make tar curl git", "." ]
    , h2 "Every command"
    , table [ "Command", "What it does" ]
      [ [ [ goto (docsPage "cli-projects") [ c "miso new NAME" ] ], [ "a new project from a template" ] ]
      , [ [ goto (docsPage "cli-projects") [ c "miso templates" ] ], [ "list the templates" ] ]
      , [ [ goto (docsPage "cli-projects") [ c "miso init" ] ], [ "make the current directory a project" ] ]
      , [ [ goto (docsPage "cli-projects") [ c "miso build [NAME...]" ] ], [ "build the project, or the programs named" ] ]
      , [ [ goto (docsPage "cli-projects") [ c "miso run [NAME] [ARGS]" ] ], [ "build a native program and run it" ] ]
      , [ [ goto (docsPage "cli-projects") [ c "miso test [NAME...]" ] ], [ "build the tests and run them" ] ]
      , [ [ goto (docsPage "cli-projects") [ c "miso dev [NAME]" ] ], [ "build, serve, and rebuild and reload on every change" ] ]
      , [ [ goto (docsPage "cli-projects") [ c "miso serve" ] ], [ "serve ", c "public/", " as it is" ] ]
      , [ [ goto (docsPage "cli-projects") [ c "miso watch [NAME]" ] ], [ "type check on every change, and show the first error" ] ]
      , [ [ goto (docsPage "cli-projects") [ c "miso lint [ARGS]" ] ], [ "run hlint on the sources" ] ]
      , [ [ goto (docsPage "cli-projects") [ c "miso repl" ] ], [ "MicroHs, interactively, with the project's modules" ] ]
      , [ [ goto (docsPage "cli-projects") [ c "miso clean" ] ], [ "remove what builds make" ] ]
      , [ [ goto (docsPage "cli-dependencies") [ c "miso install" ] ], [ "install the dependencies of ", c "miso.yml" ] ]
      , [ [ goto (docsPage "cli-dependencies") [ c "miso add PKG..." ] ], [ "add Hackage packages, and install them" ] ]
      , [ [ goto (docsPage "cli-dependencies") [ c "miso remove PKG..." ] ], [ "remove dependencies" ] ]
      , [ [ goto (docsPage "cli-dependencies") [ c "miso list" ] ], [ "the installed packages" ] ]
      , [ [ goto (docsPage "cli-dependencies") [ c "miso lock" ] ], [ "write ", c "miso.lock" ] ]
      , [ [ goto (docsPage "cli-dependencies") [ c "miso update" ] ], [ "lock again at the newest versions, and install" ] ]
      , [ [ goto (docsPage "cli-dependencies") [ c "miso vendor" ] ], [ "copy the locked sources to ", c "vendor/" ] ]
      , [ [ goto (docsPage "cli-dependencies") [ c "miso cabal" ] ], [ "print the ", c ".cabal", " file of the library" ] ]
      , [ [ goto (docsPage "cli-tool") [ c "miso doctor" ] ], [ "check the installation and the tools" ] ]
      , [ [ goto (docsPage "cli-tool") [ c "miso upgrade [VERSION]" ] ], [ "install the latest release, or VERSION" ] ]
      , [ [ goto (docsPage "cli-tool") [ c "miso uninstall" ] ], [ "remove miso" ] ]
      , [ [ goto (docsPage "cli-tool") [ c "miso version" ] ], [ "the versions of miso and MicroHs" ] ]
      , [ [ goto (docsPage "cli-tool") [ c "miso help" ] ], [ "the commands" ] ]
      , [ [ goto (docsPage "cli-tool") [ c "miso mhs ARGS" ] ], [ "the MicroHs compiler built into the tool" ] ]
      , [ [ goto (docsPage "cli-tool") [ c "miso mcabal ARGS" ] ], [ "MicroCabal, compiling with ", c "miso mhs" ] ]
      ]
    , h2 "Conventions"
    , ul
      [ [ "Commands that work on a project read ", c "miso.yml", " in the current directory (not a parent), and fail without one. The others are ", c "new", ", ", c "templates", ", ", c "init", ", ", c "serve", ", ", c "repl", ", ", c "list", " and the commands of the tool itself." ]
      , [ c "==>", " lines say what miso is doing or has done, ", c "->", " lines are steps of it; ", c "warning:", " and ", c "error:", " go to standard error. ", c "NO_COLOR=1", " turns colors off." ]
      , [ "The exit status is 0 when the command worked and 1 when it failed. ", c "miso run", " and ", c "miso lint", " exit with the status of the program or of hlint; Ctrl-C exits with 130." ]
      , [ "Commands that build install the dependencies first, when they are not installed yet." ]
      ]
    , h2 "Environment variables"
    , table [ "Variable", "" ]
      [ [ [ c "MISO_HOME" ], [ "where miso keeps downloads, git checkouts, the package list, the store and dev runtimes (default ", c "~/.miso", "); the installer installs there too" ] ]
      , [ [ c "NO_COLOR" ], [ "set (non-empty) to turn off colors" ] ]
      , [ [ c "MISO_INSTALL_URL" ], [ "where ", c "miso upgrade", " downloads the installer from" ] ]
      , [ [ c "MISO_VERSION" ], [ "for the installer: the release to install (default the latest)" ] ]
      , [ [ c "MISO_RELEASE_URL" ], [ "for the installer: a mirror of the release files (or ", c "file://", ")" ] ]
      , [ [ c "MISO_REPO" ], [ "for the installer: the GitHub repository of the releases (default ", c "haskell-miso/cli", ")" ] ]
      , [ [ c "MHSCPPHS" ], [ "the ", c "cpphs", " mhs runs for modules with CPP (default the one next to ", c "miso", ")" ] ]
      ]
    , h2 "Files and directories"
    , para [ "In a project:" ]
    , table [ "Path", "" ]
      [ [ [ c "miso.yml" ], [ "the project: see ", goto (docsPage "cli-miso-yml") [ "miso.yml" ] ] ]
      , [ [ c "miso.lock" ], [ "the locked dependencies; commit it" ] ]
      , [ [ c "vendor/" ], [ "vendored sources (", c "miso vendor", "); commit it" ] ]
      , [ [ c ".miso/" ], [ "installed packages" ] ]
      , [ [ c "dist/" ], [ "native programs and tests" ] ]
      , [ [ c "public/" ], [ "browser programs, and their static files" ] ]
      , [ [ c ".miso-build.log" ], [ "the log of the last build" ] ]
      ]
    , para [ "In ", c "$MISO_HOME", " (default ", c "~/.miso", "):" ]
    , table [ "Path", "" ]
      [ [ [ c "bin/miso", ", ", c "bin/cpphs", ", ", c "mhs-VERSION/" ], [ "the tool and its package database (when the installer installed it)" ] ]
      , [ [ c "src/" ], [ "downloads and git checkouts" ] ]
      , [ [ c "mcabal/" ], [ "MicroCabal's package list" ] ]
      , [ [ c "store/" ], [ "built packages, shared by projects" ] ]
      , [ [ c "rts/" ], [ "the browser runtimes of ", c "miso dev" ] ]
      ]
    , h2 "In CI"
    , para
      [ "The ", a "https://github.com/haskell-miso/setup-miso" "setup-miso", " GitHub Action installs a release and builds and tests a project with it:" ]
    , pre """
      - uses: haskell-miso/setup-miso@v1
        with:
          version: latest
      """
    ]
  }
-----------------------------------------------------------------------------
-- Project commands
-----------------------------------------------------------------------------
projects :: DocPage
projects = DocPage
  { pageSlug = "cli-projects"
  , pageGroup = Cli
  , pageTitle = "Project commands"
  , pageBlurb = "miso new, init, build, run, test, dev, serve, watch, lint, repl and clean."
  , pageKeywords = [ "new", "templates", "init", "build", "run", "test", "dev", "serve", "watch", "lint", "repl", "clean", "hot reload", "hlint" ]
  , pageBody =
    [ lead [ "Making a project, building it, running it, and the loop of editing and reloading." ]
    , h3 "miso new"
    , pre """
      miso new NAME [--template T]
      """
    , para [ "Makes a directory ", c "NAME", " with a new project from a template (default ", c "counter", "):" ]
    , table [ "File", "" ]
      [ [ [ c "miso.yml" ], [ c "name: NAME", ", ", c "version: 0.1.0", ", ", c "main: Main", ", ", c "source-dirs: [app]", ", ", c "target: browser", ", ", c "dependencies: [miso]" ] ]
      , [ [ c "app/Main.hs" ], [ "the template's program" ] ]
      , [ [ c "static/index.html" ], [ "the page that loads the program (copied to ", c "public/", " by builds)" ] ]
      , [ [ c ".gitignore" ], [ c "public/", ", ", c "dist/", ", ", c ".miso/", ", ", c ".mhscache", ", ", c ".miso-build.log" ] ]
      , [ [ c "README.md" ], [ "how to build it" ] ]
      ]
    , para [ "Fails when ", c "NAME", " exists already, or the template does not (it then lists the templates)." ]
    , h3 "miso templates"
    , pre """
      miso templates
      """
    , para [ "Lists the templates: ", c "counter", " (the smallest miso app, the default), ", c "router", " (pages with URLs, and links between them) and ", c "canvas", " (drawing on a canvas, driven by the model)." ]
    , h3 "miso init"
    , pre """
      miso init [--name NAME] [--version V] [--synopsis S] [--author A]
                [--license L] [--template T] [--yes]
      """
    , para [ "Makes the current directory a project, keeping the files that are there. On a terminal it asks for each value the flags do not give, showing the default:" ]
    , table [ "Question", "Default", "Accepted" ]
      [ [ [ "name" ], [ "the directory's name" ], [ "a letter, then letters, digits and ", c "-" ] ]
      , [ [ "version" ], [ c "0.1.0" ], [ "numbers separated by dots" ] ]
      , [ [ "synopsis" ], [ "none" ], [ "anything" ] ]
      , [ [ "author" ], [ c "git config user.name", " ", c "<user.email>" ], [ "anything" ] ]
      , [ [ "license" ], [ c "BSD-3-Clause" ], [ "an SPDX id, or ", c "none" ] ]
      , [ [ "template" ], [ c "counter" ], [ c "counter", ", ", c "router", ", ", c "canvas" ] ]
      ]
    , para
      [ "With ", c "--yes", ", or when the input is not a terminal, it takes the defaults without asking. It writes the template's files that are not there yet; one that is there is kept, except ", c ".gitignore", ", which gets the lines it lacks. A ", c "LICENSE", " file is written for ", c "MIT", ", ", c "BSD-2-Clause", " and ", c "BSD-3-Clause", ". Fails in a directory that has ", c "miso.yml", " already." ]
    , h3 "miso build"
    , pre """
      miso build [NAME...]
      """
    , para [ "Builds the project: type checks the library, if there is one (", c "mhs -fno-code", "), and builds every program; or, with names, only those programs." ]
    , ul
      [ [ "A ", c "native", " program (a program without ", c "target:", " is one) is built to ", c "dist/NAME", "." ]
      , [ "A browser program (", c "browser", ": WebAssembly, ", c "browser_js", ": JavaScript) is built to ", c "public/index.js", ", next to a copy of its ", c "static-dir", " (default ", c "static/", "); with several browser programs, each to ", c "public/NAME/", "." ]
      , [ "Tests are not built (", c "miso test", " builds them)." ]
      ]
    , para
      [ "Before installing anything it checks that ", c "emcc", " is there when a program needs it, and says where to get it. On a terminal it shows the build as it runs, then what mhs said; the whole log is ", c ".miso-build.log", ". "
      , c "miso build --dev NAME", " is the build ", c "miso dev", " runs: for a ", c "browser", " program it writes the program as combinators (", c "app.comb", ") and a runtime built once with emcc (kept in ", c "$MISO_HOME/rts", "; no emcc is needed when it is there already)." ]
    , h3 "miso run"
    , pre """
      miso run [NAME] [ARGS...]
      miso run [NAME] -- ARGS...
      """
    , para [ "Builds a native program and runs it with ", c "ARGS", ": the program named, when the first argument is the name of one, otherwise the first native program of ", c "miso.yml", ". It exits with the program's exit status, and fails for a browser program (use ", c "miso dev", " or ", c "miso serve", ")." ]
    , h3 "miso test"
    , pre """
      miso test [NAME...] [-- ARGS...]
      """
    , para [ "Builds the tests of ", c "miso.yml", " (the ", c "tests:", " section, or those named) to ", c "dist/NAME", ", and runs each with ", c "ARGS", ". A test passes when it exits with 0. All of them run; then it says how many passed, and fails when one did not (", c "1 of 2 tests failed: fail", ")." ]
    , h3 "miso dev"
    , pre """
      miso dev [NAME] [--port N]
      """
    , para [ "For a browser program (the one named, or the first): serves ", c "public/", " on ", c "http://localhost:PORT/", ", builds the program, and builds it again whenever one of these changes, reloading the page when the build works:" ]
    , ul
      [ [ "the source dirs of the program and of the library," ]
      , [ "its ", c "static-dir", "," ]
      , [ c "cbits/", ", ", c "jsbits/", ", ", c "include/", ", and the dirs of its C and JavaScript files," ]
      , [ c "miso.yml", ", and the directories of ", c "path:", " dependencies." ]
      ]
    , para [ "PORT is N, or the first free port from 8000 to 8099. A build that fails shows its log and keeps the last build in the page. With the browser runtime built once, a save does not run emcc: it only compiles the program to combinators. It runs until Ctrl-C." ]
    , h3 "miso serve"
    , pre """
      miso serve [--port N]
      """
    , para [ "Serves ", c "public/", " as it is (no build, no reload) on N, or the first free port from 8000. Fails when there is no ", c "public/", "." ]
    , h3 "miso watch"
    , pre """
      miso watch [NAME]
      """
    , para [ "Type checks the library, the programs and the tests (or the one named) with ", c "mhs -fno-code", ", then again on every change, as ", a "https://github.com/ndmitchell/ghcid" "ghcid", " does. It shows ", c "All good", " or the first error, as ", c "file:line:col: message", "; mhs stops at the first error, so the next one shows after that one is fixed." ]
    , h3 "miso lint"
    , pre """
      miso lint [ARGS...]
      """
    , para [ "Runs ", a "https://github.com/ndmitchell/hlint" "hlint", " on the source dirs of ", c "miso.yml", " with ", c "ARGS", " (hlint reads ", c ".hlint.yaml", "), and exits with hlint's status. Fails when hlint is not installed." ]
    , h3 "miso repl"
    , pre """
      miso repl
      """
    , para [ "MicroHs, interactively. In a project, with its source dirs and ", c "mhs-options", ", after installing its dependencies: ", c "import", " a module of the project to use it. Outside a project, with the packages that come with the tool. When the input is not a terminal it reads plain lines, so expressions can be piped to it. ", c ":quit", " leaves; ", c ":help", " lists the other commands." ]
    , h3 "miso clean"
    , pre """
      miso clean
      """
    , para [ "Removes what builds make in the project: ", c "dist/", ", ", c "public/", ", ", c ".miso/", " (installed packages), ", c ".mhscache", ", ", c "dist-mcabal/", " and ", c ".miso-build.log", ". It fails outside a project, so that it does not remove another kind of project's ", c "public/", " or ", c "dist/", "." ]
    ]
  }
-----------------------------------------------------------------------------
-- Dependencies
-----------------------------------------------------------------------------
dependencies :: DocPage
dependencies = DocPage
  { pageSlug = "cli-dependencies"
  , pageGroup = Cli
  , pageTitle = "Dependencies"
  , pageBlurb = "miso install, add, remove, list, lock, update, vendor and cabal; where packages come from, and miso.lock."
  , pageKeywords = [ "install", "add", "remove", "list", "lock", "update", "vendor", "cabal", "hackage", "stackage", "miso.lock", "versions", "store", "git", "path" ]
  , pageBody =
    [ lead [ "The dependencies of a project are the ", c "dependencies:", " of its ", c "miso.yml", ": Hackage packages, ", c "git:", " checkouts and local ", c "path:", "s. miso installs them in the project, and locks them in ", c "miso.lock", "." ]
    , h2 "Where packages come from"
    , para [ "Some packages come with the tool: the compiler's (", c "base", ", ", c "ghc-compat", ", ", c "array-mhs", ", ", c "template-haskell", ") and the libraries ", c "containers", ", ", c "mtl", " and ", c "transformers", ". The rest are installed in the project's ", c ".miso/", ", so two projects can use two versions of a package." ]
    , para [ "miso itself is a dependency like any other: ", c "miso new", " writes ", c "dependencies: [miso]", ", which is the miso the release was built from, installed prebuilt. Another miso is a dependency with its own source:" ]
    , pre """
      dependencies:
        - name: miso
          git: https://github.com/dmjio/miso
          rev: master
      """
    , para [ "A project can replace one of the tool's libraries too (with ", c "git:", ", ", c "path:", ", or a ", c "version:", " that is not the tool's); the tool's libraries that need it are built again against it. The compiler's packages cannot be replaced." ]
    , para [ "Building a package takes time, so each one is kept in a store, ", c "~/.miso/store", ", and a project that needs the same one again (same source, flags, compiler, and packages it was built against) copies it from there." ]
    , h2 "miso.lock"
    , para [ c "miso lock", " and ", c "miso update", " write ", c "miso.lock", " next to ", c "miso.yml", "; commit it. It has every package the project needs, in install order: for a Hackage package a version and the sha256 of its tarball, for a git dependency the commit its ", c "rev", " is at." ]
    , ul
      [ [ "The versions are those of mcabal's package list (a Stackage snapshot); a package that is not in it gets Hackage's newest version." ]
      , [ "The version ranges of ", c "build-depends", " are checked: a package gets a version that the packages that need it take, and a version that one does not take is an error before anything is built, saying which package needs what. When a package read later needs an older version of one chosen before, ", c "miso lock", " resolves again with the ranges it found." ]
      , [ "Builds install exactly what ", c "miso.lock", " says, each Hackage tarball checked against its sha256." ]
      ]
    , h3 "miso install"
    , pre """
      miso install
      """
    , para [ "Installs the dependencies of ", c "miso.yml", " in the project's ", c ".miso/", ", each after the packages it needs: with ", c "miso.lock", ", exactly what it locks; without one, locked for this install as ", c "miso lock", " would (without writing it); in a vendored project, from ", c "vendor/", ". Builds run it first, so it is rarely needed by hand." ]
    , h3 "miso add"
    , pre """
      miso add PKG...
      """
    , para [ "Adds Hackage packages to the ", c "dependencies:", " of ", c "miso.yml", " (keeping its comments and layout), and installs them. When the install fails, ", c "miso.yml", " is written back as it was. For ", c "git:", ", ", c "path:", " or a ", c "version:", ", edit ", c "miso.yml", "." ]
    , h3 "miso remove"
    , pre """
      miso remove PKG...
      """
    , para [ "Removes dependencies from ", c "miso.yml", ". Their packages stay installed in ", c ".miso/", " until ", c "miso clean", "." ]
    , h3 "miso list"
    , pre """
      miso list
      """
    , para [ "Lists the packages that come with the tool, and in a project, the packages installed in its ", c ".miso/", " (marked when they replace one of the tool's)." ]
    , h3 "miso lock"
    , pre """
      miso lock
      """
    , para [ "Writes ", c "miso.lock", ": it locks what is not locked yet (a new dependency, one whose ", c "git", " or ", c "rev", " changed, or a package a new one needs), keeps what is, and drops what is no longer needed. It lists the changes, or says ", c "miso.lock is up to date", "." ]
    , h3 "miso update"
    , pre """
      miso update
      """
    , para [ "Gets a new package list (the newest Stackage snapshot), locks every package again at its version and every git dependency at the current commit of its ", c "rev", ", writes ", c "miso.lock", ", and installs." ]
    , h3 "miso vendor"
    , pre """
      miso vendor
      """
    , para [ "Runs ", c "miso lock", ", then copies the source of every locked package to ", c "vendor/NAME", ". Builds then install from ", c "vendor/", " and never download: commit it for builds that work offline." ]
    , h3 "miso cabal"
    , pre """
      miso cabal
      """
    , para [ "Prints the ", c ".cabal", " file of the project's ", c "library:", ": what another project that depends on this one (with ", c "git:", " or ", c "path:", ") builds it with." ]
    ]
  }
-----------------------------------------------------------------------------
-- miso.yml
-----------------------------------------------------------------------------
misoYml :: DocPage
misoYml = DocPage
  { pageSlug = "cli-miso-yml"
  , pageGroup = Cli
  , pageTitle = "miso.yml"
  , pageBlurb = "The project file of the miso CLI: programs, targets, a library, tests and dependencies."
  , pageKeywords = [ "miso.yml", "yaml", "project", "target", "browser", "native", "library", "executables", "tests", "c-sources", "js-sources" ]
  , pageBody =
    [ lead [ "A project is a ", c "miso.yml", ". With one program:" ]
    , pre """
      name: counter
      version: 0.1.0             # required: numbers separated by dots
      synopsis: A counter        # optional: one line about the package
      author: Ada <ada@example.org>
      license: BSD-3-Clause      # optional: an SPDX id
      main: Main                 # the main module (default Main)
      source-dirs: [app]         # default [app]
      target: browser            # browser (WebAssembly) or browser_js (JavaScript),
                                 # which need emcc; or native (the default)
      mhs-options: []            # passed to mhs
      dependencies:
        - miso                   # the miso of the release
        - split                  # from Hackage (Stackage)
        - name: lens-family      # from git, at a revision, with cabal flags
          git: https://github.com/example/lens-family
          rev: v2.1.3
          flags: -foo +bar
        - name: my-lib           # a local checkout, relative to miso.yml
          path: ../my-lib
        - name: aeson            # a version from Hackage, not the package list's
          version: 2.2.3.0
      """
    , para [ "miso reads a subset of YAML: mappings, block lists, ", c "[a, b]", " lists, scalars and comments." ]
    , h2 "A library, and several programs"
    , pre """
      name: todo
      version: 0.1.0
      dependencies: [miso]       # for every component
      library:                   # code the programs share
        source-dirs: [src]       # default [src]
        exposed-modules: [Todo]  # default: every module under source-dirs
      executables:
        web:                     # miso build web, miso dev web, ...
          main: Main
          source-dirs: [app]
          target: browser
        cli:
          main: Cli
          source-dirs: [cli]
          dependencies: [containers]
      """
    , ul
      [ [ "Every program can import the library's modules. The library and each program may also have ", c "dependencies", " and ", c "mhs-options", ", added to the project's." ]
      , [ c "miso build", " type checks the library and builds every program: native ones to ", c "dist/NAME", ", a browser one to ", c "public/", ", or with several browser programs, each to ", c "public/NAME/", "." ]
      , [ "A project with just ", c "library:", " is a library, which another project can depend on with ", c "git:", " or ", c "path:", "." ]
      ]
    , h2 "Tests"
    , pre """
      tests:
        spec:                    # miso test spec
          main: Spec
          source-dirs: [test]    # default [test]
          dependencies: [split]
      """
    , para [ "Tests are programs that run on this machine, and pass when they exit with 0, as Cabal's ", c "exitcode-stdio", " tests." ]
    , h2 "C and JavaScript"
    , para [ "The library, each program, and a project with one program also take:" ]
    , pre """
      c-sources: [cbits/*.c]       # C files (globs)
      js-sources: [jsbits/*.js]    # JavaScript for browser programs (emcc --pre-js)
      include-dirs: [include]      # -I for the C files and for CPP
      cpp-options: [-DDEBUG]       # for modules with {-# LANGUAGE CPP #-}
      extra-libraries: [m]         # -lm for the linker
      cc-options: [-DSCALE=2]      # for the C compiler
      ld-options: []               # for the linker
      static-dir: static           # programs only: copied next to a browser program
      """
    , para [ "Without ", c "c-sources", ", ", c "js-sources", " or ", c "include-dirs", ", ", c "cbits/*.c", ", ", c "jsbits/*.js", " and ", c "include/", " are used when they are there." ]
    , h2 "Targets"
    , table [ "Target", "" ]
      [ [ [ c "native" ], [ "a program for this machine, in ", c "dist/" ] ]
      , [ [ c "browser" ], [ "WebAssembly, with emcc, in ", c "public/" ] ]
      , [ [ c "browser_js" ], [ "JavaScript, with emcc, in ", c "public/" ] ]
      , [ [ c "emscripten", ", ", c "emscripten_js", ", ", c "emscripten_web", ", ", c "browser_rts" ], [ "passed to mhs as ", c "-tTARGET", "; compiled with emcc" ] ]
      ]
    ]
  }
-----------------------------------------------------------------------------
-- The tool
-----------------------------------------------------------------------------
tool :: DocPage
tool = DocPage
  { pageSlug = "cli-tool"
  , pageGroup = Cli
  , pageTitle = "The tool"
  , pageBlurb = "miso doctor, upgrade, uninstall, version, help, mhs and mcabal."
  , pageKeywords = [ "doctor", "upgrade", "uninstall", "version", "help", "mhs", "mcabal", "microhs", "microcabal" ]
  , pageBody =
    [ lead [ "Commands about the tool itself, and the compilers inside it." ]
    , h3 "miso doctor"
    , pre """
      miso doctor
      """
    , para [ "Checks the installation (the package database, ", c "base", ", miso, ", c "cpphs", ", that ", c "$MISO_HOME", " is writable), the tools (", c "cc", ", ", c "curl", ", ", c "tar", ", ", c "sha256sum", " or ", c "shasum", "; and, as notes, ", c "hlint", ", ", c "git", " and ", c "emcc", "), and in a project, ", c "miso.yml", ", its programs and tests, source dirs, ", c "emcc", " for its browser targets, and its dependencies. It says how to fix each problem, and fails when it found one." ]
    , h3 "miso upgrade"
    , pre """
      miso upgrade [VERSION]
      """
    , para [ "Installs the latest release (or ", c "VERSION", ", e.g. ", c "v0.1.0", ") where this one is installed, by running the installer. The packages projects installed are installed again by their next build." ]
    , h3 "miso uninstall"
    , pre """
      miso uninstall [--yes]
      """
    , para [ "Removes miso: its files in ", c "$MISO_HOME", " (packages, caches, the store) and the tool where the installer put it. It lists what it removes, with sizes, and asks first; ", c "--yes", " does not ask, and is needed when the input is not a terminal. A folder that holds other files keeps them, and a tool installed with nix is never removed. It lists the lines of your shell startup files that add miso to ", c "PATH", ", for you to remove." ]
    , h3 "miso version"
    , pre """
      miso version
      miso --version
      """
    , para [ "Prints ", c "miso VERSION (MicroHs VERSION)", "." ]
    , h3 "miso help"
    , pre """
      miso help
      miso --help
      """
    , para [ "Prints the commands. ", c "miso", " with no arguments, or an unknown command, prints them too." ]
    , h3 "miso mhs"
    , pre """
      miso mhs ARGS...
      """
    , para [ "Runs the ", a "https://github.com/augustss/MicroHs" "MicroHs", " compiler built into the tool, with the project's ", c ".miso/", " packages and the tool's as its package path. With a bare ", c "-a", " in ARGS (which clears mhs's package path), miso leaves the package path to ARGS." ]
    , h3 "miso mcabal"
    , pre """
      miso mcabal ARGS...
      """
    , para [ "Runs ", a "https://github.com/augustss/MicroCabal" "MicroCabal", ", which compiles with ", c "miso mhs", "." ]
    ]
  }
-----------------------------------------------------------------------------
