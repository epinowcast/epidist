#!/bin/bash

bash bin/render-targets.sh > render.log 2>&1

nohup bash bin/make-parallel-targets.sh > targets.log 2>&1 &
