#! /bin/bash

set -euf -o pipefail


###############################################################################
# Config

LOCAL_REPO="$HOME/Backup"
MANIFEST_PATH="$1"
GPG_RECIPIENT="alex.sayers@gmail.com"


###############################################################################
# Helpers

remove_header () {
    sed -n '/------------/,$p' | tail -n +2
}

process_file () {
    xz --compress | gpg --encrypt --recipient "$GPG_RECIPIENT"
}

restore_file () {
    gpg --decrypt | xz --decompress
}


###############################################################################
# Logic

# TODO (asayers): check modification time before transferring each file
# TODO (asayers): Increace niceness/IOniceness
backup_manifest_contents () {
    cat "$MANIFEST_PATH" | remove_header | while read fhash fpath; do
        shorthash=$(echo "$fhash" | head -c 2)
        bpath="$LOCAL_REPO/content/$shorthash/$fhash.xz.gpg"
        if [ -f "$bpath" ]; then
            echo "Skipping: $bpath"
        else
            echo "Writing:  $bpath"
            mkdir -p "$(dirname $bpath)"
            cat "$fpath" | process_file > "$bpath"
        fi
    done
}

# TODO (asayers): Use the hostname and timestamp from the manifest
backup_manifest_file () {
    snapshot_name="$(hostname)-$(date '+%Y-%m-%d')"
    mpath="$LOCAL_REPO/manifests/$snapshot_name.xz.gpg"
    echo "Writing $mpath"
    mkdir -p "$(dirname $mpath)"
    cat "$MANIFEST_PATH" | process_file > "$mpath"
}

# NOTE: Currently unused
recover_file () {
    fhash="$1"
    backup_path="$LOCAL_REPO/content/$(echo $fhash | head -c 2)/$fhash"
    if [ -f "$fhash" ]; then echo "$fhash already exists"; exit; fi
    cat "$backup_path" | restore_file > "./$fhash"
}


###############################################################################
# Entry point

if [ -z "$1" ]; then echo "Must specify manifest file"; exit; fi
backup_manifest_contents
backup_manifest_file
