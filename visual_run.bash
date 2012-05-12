#!/bin/bash
#cabal install
TIMESTAMP=`date '+%Y%d%d%H%M'`
MYNAME=pong11-`whoami`-visual-$TIMESTAMP-$RANDOM
LOGFILE=/tmp/huskyrun-$MYNAME.log
cat <<EOM
I am $MYNAME, run with visualisation enabled,
and log stdout/stderr to $LOGFILE . 
To watch my game situation, run 

  watch 'wc -l $LOGFILE ; egrep "(GAME|WINNER)" $LOGFILE '

EOM

huskybot kappeli 8090 $MYNAME true > $LOGFILE 2>&1

