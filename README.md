<div align="center"><img height="180" src="./dist/images/logo.png"></div>

&nbsp;
# Gargantext with Purescript (FrontEnd instance)


![Purescript](https://img.shields.io/badge/Code-Purescript-informational?style=flat&logo=purescript&color=1d222d)&nbsp;&nbsp;![NPM](https://img.shields.io/badge/Tools-npm-informational?style=flat&logo=npm&color=red)&nbsp;&nbsp;![Yarn](https://img.shields.io/badge/Tools-Yarn-informational?style=flat&logo=yarn&color=2188b6)&nbsp;&nbsp;![React](https://img.shields.io/badge/Code-React-informational?style=flat&logo=react&color=61DAFB)&nbsp;&nbsp;![HTML](https://img.shields.io/badge/Code-HTML-informational?style=flat&logo=html5&color=E44D26)&nbsp;&nbsp;![CSS](https://img.shields.io/badge/Style-CSS-informational?style=flat&logo=css3&color=264EE4)&nbsp;&nbsp;![Bootstrap](https://img.shields.io/badge/Style-Bootstrap-informational?style=flat&logo=bootstrap&color=6528e0)&nbsp;&nbsp;![Sass](https://img.shields.io/badge/Tools-Sass-informational?style=flat&logo=sass&color=bf4080)&nbsp;&nbsp;![Python](https://img.shields.io/badge/Tools-Python-informational?style=flat&logo=python&color=ffd343)&nbsp;&nbsp;![NixOs](https://img.shields.io/badge/Package%20manager-NixOs-informational?style=flat&logo=debian&color=6586c8)&nbsp;&nbsp;![Docker](https://img.shields.io/badge/Tools-Docker-informational?style=flat&logo=docker&color=003f8c)



## About the project

GarganText is a collaborative web-decentralized-based macro-service
platform for the exploration of unstructured texts. It combines tools
from natural language processing, text-data-mining tricks, complex
networks analysis algorithms and interactive data visualization tools to
pave the way toward new kinds of interactions with your digital corpora.

This software is free software, developed and offered by the CNRS
Complex Systems Institute of Paris Île-de-France (ISC-PIF) and its
partners.

GarganText Project: this repo builds the
frontend for the backend server built by
[backend](https://gitlab.iscpif.fr/gargantext/haskell-gargantext).


## Getting set up

There are two approaches to working with the build:

1. Use Nix setup
2. Use Docker setup

### 1. Use Nix setup

First install [Nix](https://nixos.org/download.html): 

```shell
sh <(curl -L https://nixos.org/nix/install) --daemon
```

Verify the installation is complete (**Note:** close the current terminal session and open en new session to get nix working)
```shell
$ nix-env --version
nix-env (Nix) 2.11.0
```

To build the frontend just execute the install script at the root at the project:
```
./install
```
Just serve dist/index.html with any server and you are ready to be
connected to any backend. For instance you can serve it :

```
cd dist/ && python3 -mhttp.server
```

**Local instance is ready!** (Example: http://localhost:8000/)


### 2. Use Docker setup

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
docker compose build frontend
```

That's it, skip ahead to "Development".

