#!/bin/bash

sudo docker run --rm -v ./original:/in -v ./patched:/out nerdfonts/patcher \
  --complete --single-width-glyphs --adjust-line-height --progressbars
