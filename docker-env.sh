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

unalias npm bower pulp &>/dev/null || :
unset   npm bower pulp &>/dev/null || :

npm(){
  dockerrun node npm "$@"
}

bower(){
  dockerrun node bower "$@"
}

pulp(){
  dockerrun node pulp "$@"
}

setup(){
  npm install &&
  npm install --no-save purescript pulp bower &&
  bower install
}

build(){
  pulp browserify --to dist/bundle.js
}

dev(){
  build &&
  (cd dist && python -mSimpleHTTPServer)
}
