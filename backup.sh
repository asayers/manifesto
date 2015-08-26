#! /bin/bash


# TODO: nice/ionice
# TODO: check modification time before transferring each file
# TODO: add root manifest to the snapshots file


HOST="alex@bifrost"
HOST_LINKS_DIR="/home/alex/backup/blobs"
LOCAL_LINKS_DIR="/tmp/blobs-$(date +%y-%m-%d-%H-%M-%S)"

echo "Updating manifests..."
mkmanifests make

echo "Linking files in $LINK_DIR..."
mkdir -p $LOCAL_LINKS_DIR
mkmanifests list | while read line; do
    hash=$(echo "$line" | cut -f1)
    path=$(echo "$line" | cut -f2)
    ln -sf "$path" "$LOCAL_LINKS_DIR/$hash"
done

echo "Transferring files to $HOST:$HOST_LINKS_DIR"
rsync --verbose --ignore-existing --copy-links --recursive "$LINK_DIR/" "$HOST:$HOST_LINKS_DIR/"
