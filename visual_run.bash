#!/bin/bash
#cabal install
if [ -z $1 ]
then
  PORT=8090
else
  PORT=$1
fi

TIMESTAMP=`date '+%Y%d%d%H%M'`
MYNAME=pong11-`whoami`-visual-$TIMESTAMP-$RANDOM
LOGFILE=/tmp/huskyrun-$MYNAME.log
cat <<EOM
I am $MYNAME, run to kappeli $PORT with visualisation enabled,
and log stdout/stderr to $LOGFILE . 
To watch my game situation, run 

  watch 'wc -l $LOGFILE ; egrep "(GAME|WINNER)" $LOGFILE '

EOM

huskybot kappeli $PORT $MYNAME true > $LOGFILE 2>&1

