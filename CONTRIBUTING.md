#+Title: Contributing

## Humanities

### Social contract

1. Use and promote welcoming behavior to respect Code of Conduct

2. We are Team as whole: here to help to each others
  - knowing the unknown is a value but ignoring the unknown is a failure
    - do not ask to ask: just ask
    - there is no stupid question(s)
    - there is no unique solution(s)

3. Watch deadlines individually and collectively
  - at 0% of the time of the task, agree on the /estimate of time to fix
    the issue.
  - at 50% of the estimated time of the task, push your last commit with ID of the issue.
    - if the commit has [WIP] label: everything is fine as expected
    - if the commit has [HELP] label: it warns members of the project some
    - if the commit is not pushed, it warns the project manager something is going wrong
  - at 100% of the time of the task, push your commit with ID of the
    issue with the label [PR] for Pull Request and fill /spent time as
    comment on the issue
    - if the commit is not pushed, it warns all the team something is
      going wrong.


### Facing issues

If you get stuck on a bug:
* Set a timer for 1h
* Make it reproducible
  * Commit a version with the issue even if the code is work-in-progress [WIP].
* Try to identify the root cause of the issue:
  * Using the browser inspector
  * Identify the code path
* If it takes too long fill an issue and mark it as blocking.
  Mention it on IRC and go to another task.



## Coordination tools

### IRC Interactions
- OFTC #gargantext
- contribute to the 2 weekly meetings
- your nickname is personal and is used by the same person always

### Code coordination

Git is the main Concurrent Version system used by the project.
[gitlab](https://gitlab.iscpif.fr) is used as development platform
(others platforms are used for communication purpose mainly).

#### Git branches

Main branches are:
* master/prod : stable version used by end users
* testing     : candidate version for stable, tested by end users
* dev         : unstable version for developers only

##### Git Tips
* fix the conflicts on your own branch before ask for [PR]
* `git rebase dev`, make sure you do not diverge
* do not add temporary files in your commits
* `git add -p` : to review what you are about to commit

#### Git Tips

yarn first then psc-package are used to manage dependencies

to build use ./build
to rebuild (after a major upgrade) use ./rebuild
to add a new dependency: psc-package install PACKAGE

use git, commit and pull request :)




# Source files organisation

## Code Design
- 2 spaces or more if needed by the compilator
- line length < ? chars

## Map of the source code

TODO

## Components/Specs

* Small: one file in `Gargantext/Components/X.purs`
* Medium/Big:

```
  Gargantext/Components/
    X.purs
    X/Types.purs
    X/Specs.purs (only if big)
```

# Purescript

## Minor style rules

```
f $ do --> f do

f (do ...) --> f do

if (expr == true) --> if expr
```

## Using the `Show` class

* Always use generic deriving.
* If we want a different output than the generic one it
  means that we should use a different class than `Show`.

## When to newtype?

* When we need type class instances.
* When the type is generic (`Int`, `String`).
* In particular a `newtype` is not needed for records such as `State`.

## Identation

* HTML nodes:

```
  div []
  [ div []
    [ a [] [...]
    , a [] [...]
    ]
  , div []
    [ ...
    , ...
    ]
  ]
```

* Record notation:

```
  { a: exp, b: exp }

  { a: exp
  , b: exp
  }

  { a :: Typ, b :: Typ }

  { a :: Typ
  , b :: Typ
  }
```

# Routing (backend & frontend)

## Backend

Use `toUrl`
Use `Config.REST` (`get`, `put`...)

# Thermite usage

## Minor style rules

```
cotransform --> modifyState

void modifyState --> modifyState_

performAction should be a local function
```
