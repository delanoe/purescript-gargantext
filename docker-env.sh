#!/bin/sh

# README
#
# You can source this file and use the commands defined below:
#
# . ./docker-env.sh
# setup
# dev

NODE=node:buster-garg

dockerrun(){
  P=/app/node_modules/.bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
  sudo docker run -u "$UID" -e PATH="$P" -v $PWD:/app -w /app "$@"
}

unalias npm yarn bower pulp repl &>/dev/null || :
unset   npm yarn bower pulp repl &>/dev/null || :

buildimage(){
  (
    cd devops/docker &&
    sudo docker build -t node:buster-garg .
  )
}

node(){
  dockerrun -ti "$NODE" "$@"
}

pull(){
  sudo docker pull "$NODE"
}

npm(){
  dockerrun "$NODE" npm "$@"
}

yarn(){
  dockerrun "$NODE" yarn "$@"
}

bower(){
  dockerrun "$NODE" bower "$@"
}

dependencies(){
  dockerrun "$NODE" psc-dependencies "$@"
}

package(){
  dockerrun "$NODE" psc-package "$@"
}

pulp(){
  dockerrun "$NODE" pulp --psc-package "$@"
}

repl(){
  node pulp --psc-package repl "$@"
}

check(){
  pulp test "$@"
}

setup(){
  yarn install     &&
  yarn rebuild-set &&
  yarn install-ps
}

build(){
  pulp browserify --to dist/bundle.js
}

serve(){
  dockerrun "$NODE" http-server -p 2015 --cors dist
}

dev(){
  build && serve
}
