# Gargantext Purescript

## About this project

Gargantext is a collaborative web platform for the exploration of sets
of unstructured documents. It combines tools from natural language
processing, text-mining, complex networks analysis and interactive data
visualization to pave the way toward new kinds of interactions with your
digital corpora.

This repo deals with the frontend or client which needs
a backend server running or being granted access to a
[backend](https://gitlab.iscpif.fr/gargantext/haskell-gargantext).

This software is free software, developed by the CNRS Complex Systems
Institute of Paris Île-de-France (ISC-PIF) and its partners.

## Getting set up

There are two approaches to working with the build:
1. Use our docker setup
2. Install our dependencies yourself

The javascript ecosystem kind of assumes if you're on linux, you're
running on debian or ubuntu. I haven't yet managed to get garg to
build on alpine linux, for example. If you're on an oddball system, I
*strongly* recommend you just use the docker setup.

### Docker setup

You will need docker and docker-compose installed.

First, Source our environment file:

```shell
source ./env.sh
```

WARNING: you must `source ./env.sh` before using the docker
container. If you don't do that, the container will write files as
root and you'll need root powers to get ownership back!

Now build the docker image:

```shell
docker-compose build frontend
```

That's it, skip ahead to "Development".

### Manual setup

The build requires the following system dependencies preinstalled:

* NodeJS (11+)
* Yarn (Recent)

#### NodeJS

On debian testing, debian unstable or ubuntu:

```shell
sudo apt update && sudo apt install nodejs yarn
```

On debian stable:

```shell
curl -sL https://deb.nodesource.com/setup_11.x | sudo bash -
sudo apt update && sudo apt install nodejs
```

<!-- TODO: wtf is all this sudo? -->
<!-- To upgrade to latest version (and not current stable) version, you can -->
<!-- use the `n` module from npm to upgrade node: -->

<!-- ```shell -->
<!-- sudo npm cache clean -f -->
<!-- sudo npm install -g n -->
<!-- sudo n stable -->
<!-- sudo n latest -->
<!-- ``` -->

On Mac OS X with homebrew:

```shell
brew install node
```

For other platforms, please refer to [the nodejs website](https://nodejs.org/en/download/).

#### Yarn (javascript package manager)

On debian or ubuntu:

```shell
curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
sudo apt update && sudo apt install yarn
```

On Mac OS X with homebrew:

```shell
brew install yarn
```

For other platforms, please refer to [the yarn website](https://www.yarnpkg.com/).

## Development

### Docker environment

Are you using the docker setup? Run this:

```shell
source ./env.sh
```

This enables the docker container to run as the current user so any
files it writes will be readable by you. It also creates a `darn`
shell alias (short for `docker yarn`) for running yarn commands inside
the docker container.

### Basic tasks

Now we must install our javascript and purescript dependencies:  
*Note: if you're installing manually you might also need to manually install [psc-package](https://github.com/purescript/psc-package)*

```shell
darn install -D && darn install-ps # for docker setup
yarn install -D && yarn install-ps # for manual setup
```

You will likely want to check your work in a browser. We provide a
local development webserver that serves on port 5000 for this purpose:

```shell
darn server # for docker setup
yarn server # for manual setup
```

To generate a new browser bundle to test:

```shell
darn build # for docker setup
yarn build # for manual setup
```

If you are rapidly iterating and just want to type check your code:

```shell
darn compile # for docker setup
yarn compile # for manual setup
```

You may access a purescript repl if you want to explore:

```shell
darn repl # for docker setup
yarn repl # for manual setup
```

If you need to reinstall dependencies such as after a git pull or branch switch:

```shell
darn install -D && darn install-ps # for docker setup
yarn install -D && yarn install-ps # for manual setup
```

If something goes wrong building after a deps update, you may clean
build artifacts and try again:

```shell
# for docker setup
darn clean-js # clean javascript, very useful
darn clean-ps # clean purescript, should never be required, possible purescript bug
darn clean # clean both purescript and javascript
# for manual setup
yarn clean-js
yarn clean-ps
yarn clean
```

If you edit the SASS, you'll need to rebuild the CSS:

```shell
darn css # for docker setup
yarn css # for manual setup
```

<!-- A `purs ide` connection will be available on port 9002 while the -->
<!-- development server is running. -->

A guide to getting set up with the IDE integration is coming soon.

### Testing

To run unit tests, just run:

``` shell
test-ps
```

### Note to contributors

Please follow CONTRIBUTING.md

### How do I?

#### Add a javascript dependency?

Add it to `package.json`, under `dependencies` if it is needed at
runtime or `devDependencies` if it is not.

#### Add a purescript dependency?

Add it to `spago.dhall` (or run `spago install ...`).

If is not in the package set, you will need to read the next section.

#### Add a custom or override package to the local package set?

You need to add an entry to the relevant map in
`packages.dhall`. There are comments in the file explaining how it
works. It's written in dhall, so you can use comments and such.

## Theory Introduction

Making sense of out text isn't actually that hard, but it does require
a little background knowledge to understand.

### N-grams

N-grams in contexts (of texts) are at the heart of how Gargantext makes
sense out of text.

There are two common meanings in the literature for n-gram:
- a sequence of `n` characters
- a sequence of `n` words

Gargantext is focused on words. Here are some example word n-grams
usually extracted by our Natural Language Process toolkit;

- `coffee` (unigram or 1-gram)
- `black coffee` (bigram or 2-gram)
- `hot black coffee` (trigram or 3-gram)
- `arabica hot black coffee` (4-gram)

N-grams are matched case insensitively and across whole words removing
the linked syntax if exists. Examples:

| Text         | N-gram       | Matches              |
|--------------|--------------|----------------------|
| `Coffee cup` | `coffee`     | YES                  |
| `Coffee cup` | `off`        | NO, not a whole word |
| `Coffee cup` | `coffee cup` | YES                  |

You may read more about n-grams [on wikipedia](https://en.wikipedia.org/wiki/N-gram).

<!-- TODO: Discuss punctuation -->

Gargantext allows you to define and refine n-grams interactively in your
browser and explore the relationships they uncover across a corpus of
text.

Various metrics can be applied to n-grams, the most common of which
is the number of times an n-gram appears in a document (occurrences).
GarganText uses extensively the cooccurrences: times 2 n-grams appear in
same context of text.

## Glossary

document
: One or more texts comprising a single logical document
field
: A portion of a document or metadata, e.g. `title`, `abstract`, `body`
corpus
: A collection of documents as set (with no repetition)
n-gram/ngram
: A word or words to be indexed, consisting of `n` words.
  This technically includes skip-grams, but in the general case
  the words will be contiguous.
unigram/1-gram
: A one-word n-gram, e.g. `cow`, `coffee`
bigram/2-gram
: A two-word n-gram, e.g. `coffee cup`
trigram/3-gram
: A three-word n-gram, e.g. `coffee cup holder`
skip-gram
: An n-gram where the words are not all adjacent. Group 2 different
n-grams to enable such feature.
k-skip-n-gram
: An n-gram where the words are at most distance k from each other. This
feature is used for advanced research in text (not yet supported in
GarganText)

