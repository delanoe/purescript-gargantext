#!/bin/sh

# README
#
# You can source this file and use the commands defined below:
#
# . ./docker-env.sh
# setup
# dev

dockerrun(){
  P=/app/node_modules/.bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
  sudo docker run -u "$UID" -e PATH="$P" -v $PWD:/app -w /app "$@"
}

NODE=node:buster-garg

dockerrunnode(){
  dockerrun "$NODE" "$@"
}

unalias npm yarn bower pulp repl &>/dev/null || :
unset   npm yarn bower pulp repl &>/dev/null || :

npm(){
  dockerrunnode npm "$@"
}

yarn(){
  dockerrunnode yarn "$@"
}

bower(){
  dockerrunnode bower "$@"
}

dependencies(){
  dockerrunnode psc-dependencies "$@"
}

package(){
  dockerrunnode psc-package "$@"
}

pulp(){
  dockerrunnode pulp --psc-package "$@"
}

repl(){
  dockerrun -ti "$NODE" pulp --psc-package repl "$@"
}

sass(){
  dockerrunnode yarn sass
}

check(){
  pulp test "$@"
}

pscpackagehack(){
  cp $HOME/bin/psc-package node_modules/.bin
}

setup(){
  yarn install     &&
  yarn rebuild-set &&
  pscpackagehack   &&
  yarn install-ps
}

build(){
  echo prefer: compile
  yarn build
 #pulp browserify --to dist/bundle.js
}

compile(){
  yarn compile
}

serve(){
 #yarn dev
 #dockerrunnode http-server -p 2015 --cors dist
  dockerrunnode webpack-dev-server --env dev --mode development
}

dev(){
  build && serve
}
