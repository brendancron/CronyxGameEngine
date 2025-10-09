## CronyxGameEngine — quick AI coding guide

This file contains short, actionable notes to help AI coding agents be productive in this repository.

High-level architecture
- Core library: `Cronyx/` contains the engine primitives. Look for `Cronyx.fsproj` and these files:
  - `Actions.fs` — central abstraction `ActionBase<'state,'event>`; actions implement `InvokeImpl : 'state -> 'state * 'event list`.
  - `Modifications.fs` — simple domain types and helper transformations (e.g. `Modification = string`).
  - `Events.fs` — present but currently empty; use it for event model types if needed.
- Examples: `Examples/*` show typical usage patterns and small runners:
  - `Examples/Arithmetic/Program.fs` demonstrates how to subclass `ActionBase` (see `AddAction`, `SubAction`) and use a modifier function `invertAndDouble` when calling `Invoke`.
  - `Examples/SimpleScoreGame/` and `Examples/GrammarTester/` reference `Cronyx.Evaluation.*` modules — inspect these for parser/lexer and evaluation patterns.
- Tests: `Tests/Cronyx.Tests/` uses Expecto. Tests reference `Cronyx.Evaluation.Components.Parser` and `Lexer`.

Why code is organized this way
- The engine is small and functional: actions are first-class objects that take/return state + events. Modifiers are passed into `Invoke` to transform actions at runtime (see `ActionBase.Invoke` signature in `Cronyx/Actions.fs`).
- Examples double as living documentation; prefer to update an example when adding or changing public behavior.

Build / test / run workflows
- Build the main library from repository root:

  dotnet build Cronyx/Cronyx.fsproj

- Run the arithmetic example (quick smoke):

  dotnet run --project Examples/Arithmetic/Arithmetic.fsproj

- Run tests (Expecto) from root (test runner is in `Tests/Cronyx.Tests`):

  dotnet test Tests/Cronyx.Tests/Cronyx.Tests.fsproj

- Makefile targets (lightweight):
  - `make build` runs the `dotnet build` above.
  - `make example-a` runs the arithmetic example.

Project-specific conventions and patterns
- Action abstraction: prefer extending `ActionBase<'state,'event>` with small single-responsibility classes (see `AddAction`/`SubAction`). Always implement `InvokeImpl` and use `Invoke(state, modify)` to apply runtime modifications.
- Modifier functions: functions with signature `ActionBase<'s,'e> -> ActionBase<'s,'e>` are used to transform actions at invocation. Examples: `invertAndDouble` (Examples/Arithmetic/Program.fs).
- Small, example-driven changes: change an example in `Examples/*` when you alter high-level behavior so reviewers can see usage.
- Event modelling: `Events.fs` is intentionally empty — add concrete event types here rather than scattering stringly-typed events across the codebase.

Integration points & dependencies
- The project uses .NET (dotnet SDK) and F# projects: `Cronyx/Cronyx.fsproj`, `Examples/*/*.fsproj`, and `Tests/Cronyx.Tests/*.fsproj`.
- Tests use Expecto (see `Tests/Cronyx.Tests/Cronyx.Tests.fsproj` package refs).
- There are internal evaluation modules under `Cronyx.Evaluation.*` referenced by examples and tests — search that namespace when working on parsing/evaluation features.

Suggested quick tasks for AI agents
- Fix small API inconsistencies by updating an example and a matching test.
- Add a simple event type to `Cronyx/Events.fs` and replace string events in `Examples/Arithmetic/Program.fs` with that type.

Files to inspect first (ordered)
1. `Cronyx/Actions.fs` — core abstraction
2. `Cronyx/Modifications.fs` — domain helpers
3. `Examples/Arithmetic/Program.fs` — concrete usage of actions and modifiers
4. `Examples/SimpleScoreGame/Program.fs` and `Examples/GrammarTester/Program.fs` — evaluation usage
5. `Tests/Cronyx.Tests/Program.fs` — test patterns (Expecto)

When in doubt
- Prefer updating examples and tests together. Keep `ActionBase` surface stable; add small helpers instead of changing core method signatures.

If anything in this file is unclear or missing, tell me which area you want expanded (build, test, examples, or architecture) and I will iterate.
