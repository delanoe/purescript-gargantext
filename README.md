# Gargantext Purescript

## About this project

Gargantext is a collaborative web platform for the exploration of sets
of unstructured documents. It combines tools from natural language
processing, text-mining, complex networks analysis and interactive data
visualization to pave the way toward new kinds of interactions with your
digital corpora.

You will not find this software very useful without also running or being
granted access to a [backend](https://gitlab.iscpif.fr/gargantext/haskell-gargantext).

This software is free software, developed by the CNRS Complex Systems
Institute of Paris Île-de-France (ISC-PIF) and its partners.

## Development

### Installing dependencies

#### Debian

##### Testing Distribution and above
```shell
sudo apt update && sudo apt install nodejs yarn
```

##### Stable Distribution
```shell
curl -sL https://deb.nodesource.com/setup_11.x | sudo bash -
sudo apt update && sudo apt install nodejs
```

```shell
curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
sudo apt update && sudo apt install yarn
```

### OSX
```shell
brew install node yarn
```

### Installing dependencies

#### Debian

##### Testing Distribution and above
```shell
sudo apt update && sudo apt install nodejs yarn
```

##### Stable Distribution
```shell
curl -sL https://deb.nodesource.com/setup_11.x | sudo bash -
sudo apt update && sudo apt install nodejs
```

```shell
curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
sudo apt update && sudo apt install yarn
```

### OSX
```shell
brew install node yarn
```

### Installing dependencies

Before building gargantext, you must install the dependencies. We use
[yarn](https://yarnpkg.com/en/) for this. They have excellent
[installation instructions](https://yarnpkg.com/en/docs/install).

Once you have yarn installed, you may install everything else simply:

```shell
yarn install && yarn install-ps
```

You may now build:

```shell
yarn build
```

And run a repl:

```shell
yarn repl
```

## Note to the contributors

Please follow CONTRIBUTING.md

### How do I?

#### Add a javascript dependency?

Add it to `package.json`, under `dependencies` if it is needed at
runtime or `devDependencies` if it is not.

#### Add a purescript dependency?

Add it to `psc-package.json` without the `purescript-` prefix.

If is not in the package set, you will need to read the next section.

#### Add a custom or override package to the local package set?

You need to add an entry to the relevant map in
`packages.dhall`. There are comments in the file explaining how it
works. It's written in dhall, so you can use comments and such.

You will then need to rebuild the package set:

```shell
yarn rebuild-set
```

#### Upgrade the base package set local is based on to latest?

```shell
yarn rebase-set && yarn rebuild-set
```

## Theory Introduction

Making sense of out text isn't actually that hard, but it does require
a little background knowledge to understand.

### N-grams

N-grams are at the heart of how Gargantext makes sense out of text.

There are two common meanings in the literature for n-gram:
- a sequence of `n` characters
- a sequence of `n` words

Gargantext is focused on words. Here are some example word n-grams;

- `coffee` (unigram or 1-gram)
- `need coffee` (bigram or 2-gram)
- `one coffee please` (trigram or 3-gram)
- `here is your coffee` (4-gram)
- `i need some more coffee` (5-gram)

N-grams are matched case insensitively and across whole words. Examples:

| Text         | N-gram       | Matches              |
|--------------|--------------|----------------------|
| `Coffee cup` | `coffee`     | YES                  |
| `Coffee cup` | `off`        | NO, not a whole word |
| `Coffee cup` | `coffee cup` | YES                  |

You may read more about n-grams [on wikipedia](https://en.wikipedia.org/wiki/N-gram).

<!-- TODO: Discuss punctuation -->

Gargantext allows you to define n-grams interactively in your browser
and explore the relationships they uncover across a corpus of text.

Various metrics can be applied to n-grams, the most common of which is
the number of times an n-gram appears in a document.

## Glossary

document
: One or more texts comprising a single logical document
field
: A portion of a document, e.g. `title`, `abstract`, `body`
corpus
: A collection of documents
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
<!-- skip-grams are not yet supported -->
<!-- skip-gram -->
<!-- : An n-gram where the words are not all adjacent -->
<!-- k-skip-n-gram -->
<!-- : An n-gram where the words are at most distance k from each other -->


