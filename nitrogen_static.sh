#!/bin/bash

FILES=apps/web/priv/static/n2o
rm -rf $FILES
ln -s ../../../../deps/n2o/priv/static/n2o $FILES
