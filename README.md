# servant-realworld-example-app

This codebase was created to demonstrate a fully fledged fullstack application built with [haskell-servant] including CRUD operations, authentication, routing, pagination, and more.

For more information on how this works with other frontends/backends, head over to the [RealWorld] repo.

## How It works

### Project structure
- [`package.yaml`], [`stack.yaml`] — project configuration, see [Build system].

[`package.yaml`]: package.yaml
[`stack.yaml`]: stack.yaml
[`.ghci`]: .ghci
[`app/DevelMain.hs`]: app/DevelMain.hs

### Build system
[Build sytem]: #build-system

[Stack] covers most of our needs and provides a uniform interface so we almost never need to interact with the underlying tools (hpack, Cabal, ghc) directly.
Simple `stack build` is the ultimate command that builds the app. When the build is finished the stack would print the location of the executable.

Relevant files:
- [`package.yaml`] — Package description in the [hpack] format. Defines dependencies, executables, compiler options, etc;
- [`stack.yaml`] — Stack's configuration. While the parameters specified in `package.yaml` are only used by the Cabal, stack is the one who sets it up and runs. In this file, we specify the resolver version, and a few other stack-related things.
- [`.ghci`] — GHCi configuration, see

[stack]: http://haskellstack.org/
[hpack]: https://github.com/sol/hpack

### Development REPL
To launch the REPL you can use `stack ghci`.

Relevant files:
- [`.ghci`] — GHCi configuration, the commands from this file are executed before any modules are loaded

### Live reloading
The `update` function in the `DevelMain` module (you'll need `:load DevelMain`) launches the webserver in background with the logs printed into the repl. When you change the code you can `:reload` or `:load DevelMain` and issue the `update` function again to update webserver with the new code.

Relevant files:
- [`app/DevelMain.hs`] — module that contains functions for launching the
  development server from ghcif. [Rapid] is doing most of the job there.

[rapid]: https://hackage.haskell.org/package/rapid/docs/Rapid.html

### Code formatting

[haskell-servant]: https://github.com/haskell-servant/servant
[realworld]: https://github.com/gothinkster/realworld
