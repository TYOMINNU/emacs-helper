#!/bin/bash
# use crontab -e add like below:
# */30 * * * * /usr/bin/pkill offlineimap


ps aux | grep "\/usr\/bin\/offlineimap"
if [ $? -eq  0 ]; then
    echo "offlineimap正在运行！"
    exit 1
else
    echo $(date;offlineimap -u TTYUI -o) > /home/feng/.offlineimapsh.log
    exit 0
fi
