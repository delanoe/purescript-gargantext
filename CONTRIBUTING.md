# Guidelines to Contribute to the project

## Humanities

### Social contract

1. Be nice, welcome and treat others with respect: [Code of Conduct](https://gitlab.iscpif.fr/humanities/gargantext/blob/master/CODE_OF_CONDUCT.md)

2. We are a team as whole: here to help each other
  - knowing the unknown is a value but ignoring the unknown is a failure
    - do not ask to ask: just ask
    - there are no stupid questions

3. Watch deadlines individually and collectively
  - at 0% of the time of the task, agree on the /estimate of time to fix the issue.
  - at 50% of the estimated time of the task, push your last commit with ID of the issue.
    - if the commit has [WIP] label: everything is fine as expected
    - if the commit has [HELP] label: it warns members of the project some
    - if the commit is not pushed, it warns the project manager something is going wrong
  - at 100% of the time of the task, push your commit with ID of the issue with the label [PR] for Pull Request and fill /spent time as comment on the issue
    - if the commit is not pushed, it warns all the team something is going wrong.


### Facing issues

If you get stuck on a bug:
* Set a timer for 1h
* Make it reproducible
  * Commit a version with the issue even if the code is work-in-progress [WIP] and if help is needed [HELP].
* Try to identify the root cause of the issue:
  * Using the browser inspector
  * Identify the code path
* If it takes too long fill an issue and mark it as blocking. Mention it on IRC and go to another task.


### Coordination tools

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
* `master`  : stable and prod version used by end users
* `testing` : candidate version for stable, tested by end users
* `dev`     : unstable version for developers only

#### Adding a new feature / fixing an issue

1. You should have an open issue in gitlab with number ~#XXX~.
2. ~git checkout -b issue-XXX-issue-short-description~
3. work... ~git commit~ many times
4. Optionally you can clean up your git log history with ~git rebase -i master~
5. Test locally ~./build.sh (TODO && ./build/run-tests.sh)
6. ~git push -u~ will push and create the branch on gitlab
7. Open a PR. In the PR reference the issue by writing (~Close #XXX~ or
   ~Connects to #XXX~).
8. Make changes according to PR feedbacks
9. Either use the =Squash & Merge button= in github or manually rebase. Please
   take the time to write a good commit message. For example be inspired by
   https://chris.beams.io/posts/git-commit/


##### Git Tips
* fix the conflicts on your own branch before asking for [PR]
    * `git rebase dev` daily to make sure you do not diverge
* do not add temporary files in your commits
    * `git add -p` to review what you are about to commit
* Settings to avoid trailing whitespaces and tab characters:
  `git config --global core.whitespace blank-at-eof,blank-at-eol,tab-in-indent`
  `git config --global apply.whitespace fix`


## Technicals

Please configure your editor accordingly (ask for tips if needed or put your tips here)

### Code guidelines

#### Basics

Line length:

* Good lines of code are no more than 80 characters long.
* Acceptable lines of code are no more than 100
* Bad lines of code are no more than 120.

Whitespace:

* 2 spaces per indentation stop, or more if needed by the compiler
* Do not use tab characters.
* Remove trailing whitespace from lines. Includes blank lines.

#### Layout



* HTML nodes:

```
  div {...}
  [ div {...} [ a {} [...], a {...} [...] ]
  , div {...}
  , div
    { ...
    , ...
    }
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



#### Building

* to build use `./build`
* to rebuild (after a major upgrade) use `./rebuild`
* to add a new dependency: `psc-package install PACKAGE`

installation note: yarn first then psc-package are used to manage dependencies


#### Map of the source code

TODO

#### Components/Specs

* Small: one file in `Gargantext/Components/X.purs`
* Medium/Big:

```
  Gargantext/Components/
    X.purs
    X/Types.purs
    X/Specs.purs (only if big)
```

Components modules should contain:

Assuming the component is called comp.

`type Props` unless it is `{}`
`type State` unless it is `{}`
`type Action` unless it is `Void`

```
compSpec :: Spec ...
compClass :: ReactClass ...
compElt :: Props -> ReactElement
```

### Purescript specific

#### Minor style rules

```
f $ do --> f do

f (do ...) --> f do

if (expr == true) --> if expr
```

#### Using the `Show` class

* Always use generic deriving.
* If we want a different output than the generic one it
  means that we should use a different class than `Show`.

#### When to newtype?

* When we need type class instances.
* When the type is generic (`Int`, `String`).
* In particular a `newtype` is not needed for records such as `State`.


### Routing (backend & frontend)

#### Backend

Use `toUrl`
Use `Config.REST` (`get`, `put`...)

### Thermite usage

#### Minor style rules

```
cotransform --> modifyState

void modifyState --> modifyState_

performAction should be a local function
```

