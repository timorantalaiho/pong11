#!/bin/bash
#cabal install
if [ -z $1 ]
then
  HOST=192.168.1.38
else
  HOST=$1
fi

if [ -z $2 ]
then
  PORT=8091
else
  PORT=$2
fi

TIMESTAMP=`date '+%Y%d%d%H%M'`
MYNAME=pong11-`whoami`-visual-$TIMESTAMP-$RANDOM
LOGFILE=/tmp/huskyrun-$MYNAME.log
cat <<EOM
I am $MYNAME, run to kappeli $PORT without visualisation,
and log stdout/stderr to $LOGFILE . 
To watch my game situation, run 

  watch 'wc -l $LOGFILE ; egrep "(GAME|WINNER)" $LOGFILE '

EOM

huskybot $HOST $PORT $MYNAME nographics > $LOGFILE 2>&1

