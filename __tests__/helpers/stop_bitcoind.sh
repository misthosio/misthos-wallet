#!/bin/bash

echo "Killing running daemon"
kill "$(ps aux | grep bitcoin | grep daemon | awk '{ print $2 }')" >/dev/null 2>&1
