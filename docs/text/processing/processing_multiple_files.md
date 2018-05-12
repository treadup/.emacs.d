# Processing Multiple Files
The general idea is to open buffers in Emacs for all the files that
you want to process.

You can then iterate over the buffers and perform text manipulation as
normal.

## Processing buffers
It is easy in Elisp to loop over a list of buffers. The problem then
is how do you get the list of open buffers? The following are some
possible selection criteria.

1. All open buffers
2. All open buffers of a certain major mode

## Processing files
The other approach would be to generate a list of files and then
manipulate the files in place. You would still want to open each file
in a buffer to perform the text manipulation there.

1. All files in a directory
2. All files in a directory of a certain type
3. All files in a project
4. All files in a project of a certain type
5. All files whose file name match a regular expression

## Search
One could think that you would want to be able to perform a search and
the buffers or files that contain matches are the ones that should be
processed.
