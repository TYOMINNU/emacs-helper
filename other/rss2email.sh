#!/bin/bash

###############################################
# This is a wrap of r2e command:
# 1. Put this file to your $PATH
# 2. Install firefox ext: AppLauncher
# 3. Configure Applauncher:
#    1. name:      rss2email
#    2. path:      /usr/bin/x-terminal-emulator
#    3. arguments: --command=rss2email.sh &url;
################################################

echo "The Feed url is: $1"
echo "Please enter a name for this feed"
read feed_name
echo "run command: r2e add $feed_name $1"
r2e add $feed_name $1
r2e list
read -t 10
