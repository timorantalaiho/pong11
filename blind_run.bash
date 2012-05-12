#!/bin/bash
#cabal install
TIMESTAMP=`date '+%Y%d%d%H%M'`
MYNAME=pong11-`whoami`-blind-$TIMESTAMP-$RANDOM
LOGFILE=/tmp/huskyrun-$MYNAME.log
cat <<EOM
I am $MYNAME, run without visualisation,
and log stdout/stderr to $LOGFILE . 
To watch my game situation, run 

  watch 'wc -l $LOGFILE ; egrep "(GAME|WINNER)" $LOGFILE '

EOM

huskybot kappeli 8090 $MYNAME nographics > $LOGFILE 2>&1

