# `manifesto`

`manifesto` is a tool for making a copy of your directory structure in the
form of manifest files. Files can then be written to a content-addressable
store without losing information about the directory structure, giving a simple
way of doing incremental snapshotted backups.

## Behaviour

The behaviour of this program is best conveyed with an example. Suppose the
directory tree under the working directory looks like this:

```
.
├── file1
├── file2
├── file3
├── dir1
│   └── file4
└── dir2
    ├── file5
    ├── file6
    └── file7
```

Running `manifesto my-manifest` will create a manifest file called
"my-manifest" in the working directory. This file will contain some metadata,
followed by a recursive listing of all the regular files in the working
directory, along with the SHA1 hashes of those files.

```
$ cat my-manifest
hostname: the-machines-hostname
manifest root: /path/to/root/
excludes file:
timestamp: 2015-09-15-15:22:56
d208b2499488547c4a01e41c9db7dc7c549ff86f	file1
9af3e9bfe4ef27448460fbf492f40f285edb8fd0	file2
6ce927e18ca4bc7936be4cd07dbff5120c49a08a	file3
8460fbf492f40f285edb8fd09af3e9bfe4ef2744	dir1/file4
b8b259752fb55de2686e6bce1faf4317a6c2b7ab	dir2/file5
0c86c1dac46113cd8ff85300b402440149b2e7c0	dir2/file6
1711c98bad632d5441f56116ec60cebfd77fa5a1	dir2/file7
```

If the specified manifest file already exists and is valid, it will be updated.
If a file hasn't been modified since the existing manifest's timestamp then the
hash will be reused. This is important, since hashing is potentially an
expensive operation.

There are a couple of command-line switches:

- `-v` for verbose mode
- `-e<filepath>` to specify a file containing directories to ignore

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

## Options for object storage

- A directory full of files named by their SHA1 hash. Upload with `rsync
  --ignore-existing`. Keep redundant copies of your backups in sync with unison
  or rsync+cron.
- Amazon S3 and similar. (See the [note on using Glacier](#note-on-using-glacier))
- Any key-value database. (Eventually-consistent ones are fine too!)

Concerns such as read-availability, write-availability, and data resilience
should be handled at this level (using techniques such as caching, eventual
consistency, and redundant storage respectively).

## Concrete usage instructions

See the file "backup.sh" for an example of a simple backup system using
`manifesto`.

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

- [Venti] and Fossil, the Plan9 filesystems.
- [Tahoe-LAFS], a CAS with an nice authority model.
- [IPFS], a distributed CAS. This may not be appropriate for making backups,
  but it's interesting.
- Some others that I don't know much about: [Camlistore], [Keep], [blobsnap].

[Venti]: http://doc.cat-v.org/plan_9/4th_edition/papers/venti/
[IPFS]: http://ipfs.io/
[Tahoe-LAFS]: https://www.tahoe-lafs.org/trac/tahoe-lafs
[Camlistore]: https://camlistore.org/
[Keep]: https://arvados.org/projects/arvados/wiki/Keep
[blobsnap]: https://github.com/tsileo/blobsnap

The chief advantage of using `manifesto` over the above is *simplicity*. You
don't need to install any server software, there's no protocol for moving data
around, there are no proprietary data formats. All you need is an S3 bucket to
put your files in. If you've read the "Behaviour" section above, then you
already understand *everything* `manifesto` does, including its on-disk
format.

You can use `manifesto` to build a pretty solid backup system. However, there
are some nice features which it *doesn't* support:

- Encryption. We assume the storage is private, which is probably a bad
  assumption. Maybe you can write a privacy layer on top?
- Block-level deduplication. This would be nice, but it's tough to get it
  right.

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

## Note on using Glacier

TODO: write me. (Bucket listing is expensive but required for dedup. Keep local
copy of file listing. Still not ideal...)

## To-do

- It should be possible to parallelise the descents into different branches of
  the directory tree.
- Perhaps put all the manifests in the same place, but with different
  filenames; or just have a single manifest.
- In order to properly reproduce the original directory structure we need to
  store ownership, permissions, timestamps, links, etc.
- Support other hashing algorithms?
- Support ignoring certain files/directories.
- Add support for running an arbitrary command which will recieve a path and a
  hash as input over all files in the tree, ensuring that the hash is
  up-to-date at the time the command is invoked.
