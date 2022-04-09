#!/usr/bin/env bash

BASEDIR=$(dirname $0)
(cd $BASEDIR/bases/cli/ && clojure -X:uberjar && mv fset.jar ../../)
