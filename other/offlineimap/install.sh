#!/bin/sh

USERNAME="$(id -u -n)"
OFFLINEIMAP_HElPER_DIR=$HOME/.offlineimap_helper
OFFLINEIMAP_BASHPROFILE=$OFFLINEIMAP_HElPER_DIR/bashprofile
OFFLINEIMAP_STARTDAEMON=$OFFLINEIMAP_HElPER_DIR/start_daemon.sh
OFFLINEIMAP_CRON_FILE=$OFFLINEIMAP_HElPER_DIR/offlineimapcron.sh
OFFLINEIMAP_CRON="*/5 * * * * . $OFFLINEIMAP_BASHPROFILE; exec $OFFLINEIMAP_STARTDAEMON -n19 -c2 -p7 python2.7 /usr/bin/offlineimap"

mkdir -p  $OFFLINEIMAP_HElPER_DIR
cp start_daemon $OFFLINEIMAP_STARTDAEMON
chmod +x $OFFLINEIMAP_STARTDAEMON

# dovecot need *USER* environment Variable when it launched from cron
echo "USER=$USERNAME" > $OFFLINEIMAP_BASHPROFILE
echo "export USER" >> $OFFLINEIMAP_BASHPROFILE

# Add this script to the existing crontab.  This relies on your
# sed supporting -i.  if it does not, it is probably easiest to
# write a simple script that does the edit and set VISUAL to that script
if ! crontab -l | grep "offlineimap"  > /dev/null; then
    crontab -l > $OFFLINEIMAP_CRON_FILE
    echo "$OFFLINEIMAP_CRON" >> $OFFLINEIMAP_CRON_FILE
    crontab $OFFLINEIMAP_CRON_FILE
fi
