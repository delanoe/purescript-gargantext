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

unalias npm yarn bower pulp repl &>/dev/null || :
unset   npm yarn bower pulp repl &>/dev/null || :

npm(){
  dockerrun node npm "$@"
}

yarn(){
  dockerrun node yarn "$@"
}

bower(){
  dockerrun node bower "$@"
}

dependencies(){
  dockerrun node psc-dependencies "$@"
}

package(){
  dockerrun node psc-package "$@"
}

pulp(){
  dockerrun node pulp --psc-package "$@"
}

repl(){
  dockerrun -ti node pulp --psc-package repl "$@"
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
  dockerrun node http-server -p 2015 --cors dist
}

dev(){
  build && serve
}
