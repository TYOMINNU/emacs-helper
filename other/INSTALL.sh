#!/bin/sh

USERNAME="$(id -u -n)"
MSMTP_CONFIG=$HOME/.msmtprc
ISYNC_CONFIG=$HOME/.mbsyncrc
OFFLINEIMAP_CONFIG=$HOME/.offlineimaprc
OFFLINEIMAP_PYCONFIG=$HOME/.offlineimap.py
OFFLINEIMAP_HElPER_DIR=$HOME/.offlineimap_helper
OFFLINEIMAP_BASHPROFILE=$OFFLINEIMAP_HElPER_DIR/bashprofile
OFFLINEIMAP_STARTDAEMON=$OFFLINEIMAP_HElPER_DIR/start_daemon.sh
OFFLINEIMAP_CRON_FILE=$OFFLINEIMAP_HElPER_DIR/offlineimapcron.sh
OFFLINEIMAP_CRON="*/5 * * * * . $OFFLINEIMAP_BASHPROFILE; exec $OFFLINEIMAP_STARTDAEMON -n19 -c2 -p7 python2.7 /usr/bin/offlineimap"

# offlineimap configure template
if [ -f "$OFFLINEIMAP_CONFIG" ]
then
    echo "$OFFLINEIMAP_CONFIG exist, do nothing!"
else
    cp offlineimap-template/offlineimaprc   $OFFLINEIMAP_CONFIG
    cp offlineimap-template/offlineimap.py  $OFFLINEIMAP_PYCONFIG
    chmod 600 $OFFLINEIMAP_CONFIG
    chmod 600 $OFFLINEIMAP_PYCONFIG

fi

# msmtp configure template
if [ -f "$MSMTP_CONFIG" ]
then
    echo "$MSMTP_CONFIG exist, do nothing!"
else
    cp msmtp-template/msmtprc               $MSMTP_CONFIG
    chmod 600 $MSMTP_CONFIG
fi

# isync configure template
if [ -f "$ISYNC_CONFIG" ]
then
    echo "$ISYNC_CONFIG exist, do nothing!"
else
    cp isync-template/mbsyncrc               $ISYNC_CONFIG
    chmod 600 $ISYNC_CONFIG
fi

# offlineimap helper
mkdir -p  $OFFLINEIMAP_HElPER_DIR
cp offlineimap-template/start_daemon.sh $OFFLINEIMAP_STARTDAEMON
chmod +x $OFFLINEIMAP_STARTDAEMON

# bashprofile for dovecot
# dovecot need *USER* environment Variable when it launched from cron
echo "USER=$USERNAME" > $OFFLINEIMAP_BASHPROFILE
echo "export USER" >> $OFFLINEIMAP_BASHPROFILE

# Add cronjob for offlineimap
if ! crontab -l | grep "offlineimap"  > /dev/null; then
    crontab -l > $OFFLINEIMAP_CRON_FILE
    echo "$OFFLINEIMAP_CRON" >> $OFFLINEIMAP_CRON_FILE
    crontab $OFFLINEIMAP_CRON_FILE
fi

echo "Install offlineimap configure template success!"
echo "Please edit: ~/.msmtprc , ~/mbsyncrc , ~/.offlineimaprc and  ~/.offlineimap.py"