# Bangumi HS

This document describes how to design automation agents ("angents") in the `bangumi-hs` project. The codebase is currently a lightweight Cabal project, so agents are small executables that sit on top of the shared library code under `src/`.

## Code Layout
- `src/` holds reusable modules (for example `src/MyLib.hs` exports `someFunc`).
- `app/Main.hs` is the entry point for the default angent; it wires CLI behavior to the library.
- `bangumi-hs.cabal` declares the library and executable stanzas that Cabal uses to build agents.

## Code Style
- use GHC2024

## Building & Running
Use Cabal for every lifecycle step:

```bash
cabal update              # refresh package index
cabal build               # compile library + executables
cabal run bangumi-hs      # run the default angent defined in app/Main.hs
cabal repl bangumi-hs     # open a REPL for interactive experiments
```


## Creating a New Agent
1. Add a module under `src/Angents/<Name>.hs` (create the folder if needed) and export a pure `run<Name>` function that describes the angent's behavior.
2. Update `bangumi-hs.cabal` with either a new executable stanza or additional library exports so Cabal knows how to compile the module.
3. Create `app/<Name>.hs` as the executable entry point. Keep the `main` function thin—parse flags, call into the shared library, and handle errors.
4. Add any configuration structs or environment variable parsing in the library layer so the executable remains declarative.

## Configuration Guidelines
- **Environment first:** read secrets or tokens from environment variables and document them near the angent that uses them.
- **Deterministic defaults:** provide safe defaults so an angent can run locally without external services when possible.
- **Separate credentials:** never commit personal API keys; instead, update this document or a `README` with the expected variable names.

## Observability & Reliability
- Log high-level lifecycle events (`startup`, `successful run`, `error`) with timestamps; `putStrLn` is sufficient until a structured logging dependency is introduced.
- Prefer total functions or explicit error handling so angents do not crash silently.
- When an angent talks to remote services, wrap calls in retry helpers and surface failures through exit codes so CI/CD can react.

## Testing Angents
- Keep logic pure and testable inside `src/` modules. You can later add an `hspec` or `tasty` test suite under `test/` to exercise these modules.
- For side effects (HTTP calls, file IO), inject capabilities (interfaces or higher-order functions) so they can be mocked during tests.

## Operational Checklist
- [ ] Library module with documented APIs
- [ ] Executable stanza in `bangumi-hs.cabal`
- [ ] Arguments and environment variables listed in documentation
- [ ] Basic logging and error handling
- [ ] Tests (unit or smoke) covering the non-trivial logic

Following these conventions keeps every angent consistent, easy to audit, and simple to extend as the project grows.
