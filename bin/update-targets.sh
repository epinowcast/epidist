#!/bin/bash

bash bin/render-targets.sh > render.log 2>&1

nohup bash bin/make-targets.sh > targets.log 2>&1 &
