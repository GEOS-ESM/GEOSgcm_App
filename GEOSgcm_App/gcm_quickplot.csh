#!/bin/csh -f

set JOBID    = $1
set LISTFILE = $2

#######################################################################
#                         Quickplot Commands           
#######################################################################

while( -e $LISTFILE )

     if(    -e LOCKFILE ) /bin/echo "Waiting for LOCKFILE ... "
     if(    -e LOCKFILE ) set LOCK = `stat -c %Z LOCKFILE`
     while( -e LOCKFILE )
               set    EPOCH = `date +'%s'`
               @    LOCKAGE = $EPOCH - $LOCK
               if( $LOCKAGE > 120 ) /bin/rm -f LOCKFILE
     end
                            lockfile -${JOBID} LOCKFILE

     if( -e $LISTFILE ) then
          set PLOT_COMMAND = `head -1 $LISTFILE`
          sed 1,1d -i $LISTFILE
          set  nlist = `wc -c $LISTFILE | cut -d" " -f 1`
          if( $nlist == 0 ) /bin/rm -f $LISTFILE
          /bin/rm -f LOCKFILE
          echo "JOBID: $JOBID   $GEOSUTIL/plots/quickplot $PLOT_COMMAND "
          $GEOSUTIL/plots/quickplot $PLOT_COMMAND
     else
          /bin/rm -f LOCKFILE
     endif

end

