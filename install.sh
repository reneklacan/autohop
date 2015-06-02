#!/bin/bash

source='
h() {
  result=`autohop $@`;

  if [ $? -eq 0 ]; then
    cd $result;
  else
    echo $result;
  fi
}
'

echo $source >> ~/.zshrc
