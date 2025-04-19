#!/bin/bash

sudo docker run --rm -v ./input:/input -v ./output:/output nerdfonts/patcher:latest \
  --complete --adjust-line-height --no-progressbars --makegroup 6 --careful #--quiet --careful
