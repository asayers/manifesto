# `manifesto`

`manifesto` is a tool for creating and updating manifest files. These manifests
allow you to write your files to a content-addressable store store without
losing information about the original directory structure. `manifesto` is
indended to be a component in a larger backup strategy; specifically, it
provides a simple deduplication layer when making incremental snapshotted
backups.

## Behaviour

I think the behaviour of this program is best conveyed by an example:

```
$ manifesto foobar.manifest
$ cat foobar.manifest
hostname: my-hostname
manifest root: /path/to/working/directory/
excludes:
timestamp: 2015-09-15-15:22:56
------------
d208b2499488547c4a01e41c9db7dc7c549ff86f	file1
9af3e9bfe4ef27448460fbf492f40f285edb8fd0	file2
6ce927e18ca4bc7936be4cd07dbff5120c49a08a	file3
8460fbf492f40f285edb8fd09af3e9bfe4ef2744	dir1/file4
b8b259752fb55de2686e6bce1faf4317a6c2b7ab	dir2/file5
0c86c1dac46113cd8ff85300b402440149b2e7c0	dir2/file6
1711c98bad632d5441f56116ec60cebfd77fa5a1	dir2/file7
```

Invoking `manifesto` with an output path creates a "manifest" file. This
contains some metadata, followed by a recursive list of all the regular files
in the working directory, along with their SHA1 hashes.

If the specified manifest file already exists and is valid, it will be updated.
This means that if a file hasn't been modified since the existing manifest was
created then the hash will be reused. This makes updating a manifest very fast
when done regularly.

There are a couple of command-line switches:

- `-v` for verbose mode
- `-e<filepath>` to specify a file containing a list of directories to ignore

## Motivation

What we're doing is taking a copy of the directory structure of your
filesystem. However, whereas your filesystem refers to the contents of your
files by using a block address, our copy uses content addressing (the SHA1 of
the file contents).

This means that the directory structure is preserved in a way which is
independent of *where* your files are, what they're called, etc. You could move
all your files into one directory and rename them to sequential numbers; so
long as you have the data, you can recover the directory tree from the manifest
files.

The intended use case is for putting your files into a content-addressable
store without losing information about the directory structure - you just
upload the manifests along with your other files. If you want to efficiently
explore your backups, the addressing schemes should be the same. At the moment
`manifesto` only supports SHA1.

The simplest example of this would be to just upload all your files to a
directory on your backup server, using their SHA1 hash as the filename. So long
as you keep a reference to the root manifest, you can still recover your data.

Using a scheme like this for backups has a few advantages.

- Automatic file-level deduplication. If data already exists in the store, it
  won't be duplicated. If you use `rsync --ignore-existing` it won't even be
  re-uploaded.
- Verify file integrity. Using nothing but the hash of the root manifest you
  can verify that none of the files in the snapshot have been tampered with.
  Also, you can detect bit-flips.
- Files are immutable. This makes it easy to guarantee that your backup scheme
  doesn't erase old data. The only file which should be modified is the list of
  snapshot root manifests.
- Flexible regarding underlying blob store. The requirements are very simple:
  once a blob has been written to the store with a certain name, it should be
  retrievable using that name. This allows you to use all manner of fancy
  distributed databases, depending on your needs.

You can use `manifesto` to build a pretty solid backup system. However, it's
not the goal to provide a complete turn-key backup solution. Functionality
which you probably want to add yourself includes:

- Compression
- Encryption
- Transferring data off-site

<a name="stumbling-blocks"></a>
There are also some features which you might want in a backup system which
`manifesto` actually makes difficult to achieve. Consider these when deciding
whether to use it.

- Block-level deduplication. This is nice, but it's tough to get it right.
  `manifesto` is designed to provide the deduplication layer of a backup
  strategy, and it uses file-level dedup, so if you need block-level dedup you
  need to look [elsewhere][zbackup].
- Rock-solid encryption. It's expected that users will encrypt files before
  moving them to untrusted storage, and that this should provide a decent level
  of protection if done right. However, it's worth noting that if you're using
  `manifesto` then an attacker probably has access to the SHA1 of the plaintext
  of every file. Depending on the cypher you use, this may be significant. If
  in doubt, ask a cryptographer!
- Hashes other than SHA1. Perhaps your object-store of choice uses something
  other than SHA1. This is a shame, because it would be convenient to get the
  hashes from the manifests and avoid recomputing them. If this is the case,
  open an issue or submit a pull request.

## Options for object storage

- A directory full of files named by their SHA1 hash. Upload with `rsync
  --ignore-existing`. Keep redundant copies of your backups in sync with unison
  or rsync+cron.
- Amazon S3, Microsoft Azure, Rackspace Managed Cloud, Backblaze B2, etc.
  (However, see the [note on using Glacier](#note-on-using-glacier))
- Any key-value database. (Eventually-consistent ones are fine too!)
- Systems explicitly designed as content-addressable stores (although most of
  these make `manifesto` redundant).

Concerns such as read-availability, write-availability, and data resilience
should be handled at this level (using techniques such as caching, eventual
consistency, and redundant storage respectively).

## Concrete usage instructions

Manifest files are designed to be easy to process in bash. For example, suppose
you wanted to copy every file in "my-manifest" to "/mnt/backup/<hash>"; you
could simply do:

```bash
cat my-manifest                  \            # Read the manifest
    | sed -n '/------------/,$p' \            # Remove the metadata
    | tail -n +2                 \            # Remove the divider
    | while read fhash fpath; do              # Loop over the lines
    cp "$fpath" "/mnt/backup/content/$fhash"  # Copy the files
done
```

As a slightly more sophisticated example, the following script does the same
thing, but also (1) copies the manifest itself, (2) compresses and encrypts all
files, (3) avoids overwriting existing files, (4) avoids putting too many files
in the same directory, and (5) ensures that relevant directories exit.

**Note that this script is only meant to give an idea of how to work with
manifest files. See below.**

```bash
#! /bin/sh

set -euf -o pipefail

remove_header () {
    sed -n '/------------/,$p' | tail -n +2
}

process_file () {
    lzma --compress | gpg --encrypt --recipient "Alex"
}

cat "$1" | remove_header | while read fhash fpath; do
    shorthash=$(echo "$fhash" | head -c 2)
    bpath="/mnt/backup/content/$shorthash/$fhash"
    if [ ! -f "$bpath" ]; then
        mkdir -p "$(dirname $bpath)"
        cat "$fpath" | process_file > "$bpath"
    fi
done

mpath="/mnt/backup/manifests/$(hostname)-$(date '+%Y-%m-%d')"
mkdir -p $(dirname $mpath)
cat "$1" | process_file > "$mpath"
```

Now you can view a recorded manifest (or the files it references) by doing

```
$ cat /mnt/backup/manifests/foobar-2015-09-28 | gpg -d | lzma -d | less
```

Further to the warning above, I'd just like to reiterate my cowardly
disclaimer. When it comes to backups, people's requirements differ a lot, so
you need to figure out a scheme which suits yours. Hopefully `manifesto` can
be a part of that.

In fact, the scheme implemented by the above example script is probably not
what you want. For instance, it assumes that "/mnt/backup" is always mounted,
and writes to the local disk when it isn't. These writes are then masked when
the mount is restored, effectively spliting your data across two repos. This is
no good at all - very dangerous!

Personally I write my backups to a local directory and then rsync the data away
when I have a connection. This allows me to continue taking snapshots even in
the absence of the internet - the local backup directory is like a buffer. When
I transfer the data to a remote location, I leave behind an empty file to
prevent it from being recreated in the buffer.

However, like I said: I don't really want to be giving advice about how to do
backups. Please figure out a scheme which satisfies *your* needs!

## Related work

There are many backup systems with different properties, and the strength of
their advantages/disadvantages is quite user-specific. Here are some
interesting alternative backup strategies:

- [rsnapshot] uses hard links rather than explicit content-addressing to do
  deduplication. This makes it easier to see navigate your snapshots, and
  allows the possibility of deleting old snapshots. If you use a traditional
  unix filesystem to store your backups, this may be a better solution.
- [zbackup] breaks your files into blocks before deduplicating. If you have
  large files which are frequently modified, then this is essential. For
  instance: large log files which aren't rotated often; tradition mail spool
  files (maildir works fine with file-level dedup though).
- [ZFS]/[Btrfs]. If your backup machine has one of these filesystems, it might
  be better to just mirror your files with rsync and use the native
  snapshotting capabilities.

[rsnapshot]: http://rsnapshot.org/
[zbackup]: http://zbackup.org/
[ZFS]: http://open-zfs.org/wiki/Main_Page
[Btrfs]: https://btrfs.wiki.kernel.org/index.php/Main_Page

The above are all good solutions, and may better fit your requirements. I like
content-addressing as a way of doing deduplication. If you do too, you should
also investigate the following dedicated content-addressable stores (CASes):

- [Venti] and Fossil, the Plan9 filesystems. Real forerunners in this space.
- [Tahoe-LAFS], a CAS with an nice authority model.
- [IPFS], a public distributed CAS. This may not be appropriate for making
  backups, but it's interesting.
- Some others I've heard of but don't know much about: [Camlistore], [Keep],
  [blobsnap].

[Venti]: http://doc.cat-v.org/plan_9/4th_edition/papers/venti/
[IPFS]: http://ipfs.io/
[Tahoe-LAFS]: https://www.tahoe-lafs.org/trac/tahoe-lafs
[Camlistore]: https://camlistore.org/
[Keep]: https://arvados.org/projects/arvados/wiki/Keep
[blobsnap]: https://github.com/tsileo/blobsnap

The chief advantages `manifesto` has over the above are *simplicity* and
*flexibility*.

Simplicity: You don't need to install any server software; there's no
protocol for moving data around; there are no special data formats. If you've
read the "Behaviour" section above, then you already understand *everything*
`manifesto` does, including its on-disk format.

Flexibility: You can store your data virtually anywhere, and transfer it by any
means you like. If you have requirements which the authors of turn-key backup
solutions didn't anticipate, you could be in for a bad time. On the other hand,
because its scope is limited, you can probably adjust your strategy and
continue using `manifesto` - hopefully it won't get in your way (although see
[here](#stumbling-blocks) for ways it might).

<!--
## Justifications for technical decisions

### Why create many manifest files rather than just one?

Conceptually, when making deduplicated backups tree-structure of the filesystem
becomes a DAG. By making a manifest file for each directory, the data structure
we end up storing is just the Merkle representation of that DAG. I thought this
was nice.

The other property this gives you is the ability to update the manifests for
just part of the directory tree, or to build manifests for a supertree and
automatically reuse the existing ones.

However, keeping a single manifest file at the root of the tree is not a bad
idea - it prevents your filesystem from becoming littered with manifest files.
Perhaps I'll make this behaviour available under a command-line flag.
-->

## Note on using Glacier

TODO: write me. (Bucket listing is expensive but required for dedup. Keep local
copy of file listing. Still not ideal...)

## To-do

- It should be possible to parallelise the descents into different branches of
  the directory tree.
- In order to properly reproduce the original directory structure we need to
  store ownership, permissions, timestamps, links, etc.
- Support other hashing algorithms?
- Improve support for ignoring certain files/directories.
- Add support for running an arbitrary command which will recieve a path and a
  hash as input over all files in the tree, ensuring that the hash is
  up-to-date at the time the command is invoked?
- Proper command-line flag parsing
