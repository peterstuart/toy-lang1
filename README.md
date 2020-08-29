# toy-lang1

A Haskell-ish language that compiles to JS.

## Development

Run `nix-shell` to set up the development environment.

## Scripts

Run `nix-build release.nix` and then...

Compile a file:

```bash
./scripts/compile.sh input.tl output.js
```

Watch a file:

```bash
./scripts/watch.sh input.tl output.js
```
