#!/bin/bash
# use crontab -e add like below:
# */30 * * * * /home/user/bin/feed2imap-cron.sh
# * 5 * * * /usr/bin/pkill feed2imap

# 读取环境变量
if [ -f /home/user/.bashrc ]; then
    . /home/user/.bashrc
fi

ps aux | grep "python \/home\/user\/bin\/gappproxy\/proxy.py"

if [ $? -ne "0" ]; then
    /usr/bin/python /home/user/bin/gappproxy/proxy.py > /dev/null 2>&1 &
fi

echo $(date;/usr/bin/feed2imap) > /home/user/.feed2imap.runlog

exit 0;



