#!/bin/bash
# use crontab -e add like below:
# */3 * * * * /home/feng/.offlineimap-cron.sh
# * 5 * * * /usr/bin/pkill offlineimap

# 读取环境变量
if [ -f /home/user/.bashrc ]; then
    . /home/user/.bashrc
fi

ps aux | grep "\/usr\/bin\/offlineimap"

if [ $? -eq "0" ]; then
    exit 0;
else
    echo $(date;offlineimap -u Noninteractive.Basic -o) > /home/user/.offlineimap/log
    exit 0;
fi



