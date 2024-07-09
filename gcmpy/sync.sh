#!/bin/bash

'''
The purpose of this script to sync up the gcmpy directories between the @GEOSgcm_App dir and bin dir without having to make install
'''


# Source and destination directories
source_dir="/discover/nobackup/sshakoor/GEOSgcm/src/Applications/@GEOSgcm_App/gcmpy"
destination_dir="/discover/nobackup/sshakoor/GEOSgcm/install/bin/gcmpy"

# Synchronize scripts
rsync -av --exclude='sync.sh' --exclude='**/CMakeLists.txt' "$source_dir/" "$destination_dir/"
