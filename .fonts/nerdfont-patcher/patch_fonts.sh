#!/bin/bash

sudo docker run --rm -v ./input:/input -v ./output:/output ghcr.io/cdalvaro/docker-nerd-fonts-patcher:latest \
  --complete --mono --adjust-line-height --no-progressbars --makegroup 6 --quiet
