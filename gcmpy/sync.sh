#!/bin/bash

# Source and destination directories
source_dir="/discover/nobackup/sshakoor/GEOSgcm/src/Applications/@GEOSgcm_App/gcmpy"
destination_dir="/discover/nobackup/sshakoor/GEOSgcm/install/bin/gcmpy"

# Synchronize scripts
rsync -av --exclude='sync.sh' --exclude='**/CMakeLists.txt' "$source_dir/" "$destination_dir/"
