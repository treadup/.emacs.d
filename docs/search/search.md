# Search and Replace
This document covers different ways of searching for text in a
document or a project and replacing text in a document.

## Searching
The following key bindings are used for searching.

To search forward in the document use C-s. To search backwards in the
document use C-r.

To use helm-swiper use C-M-s.

## Searching as navigation
You can use C-s and C-r as part of navigating a document. If you want
to navigate down in the document use C-s and search for something
further down. If you want to navigate up in the document use C-r and
search for something further up.

(There is a function called list-matching-lines that can be used as an
alternative to helm-swiper.)

To search forward using regular expressions use M-s.

To search backwards using regular expressions use M-r.

## Newline in search pattern
If you want to have a newline in a search pattern you have to use the
C-q C-j key chord. This will input a newline character in the current
buffer or minibuffer.

## Searching as navigation
You can use C-s and C-r as part of navigating a document. If you want
to navigate down in the document use C-s and search for something
further down. If you want to navigate up in the document use C-r and
search for something further up.

## Smartscan
To move to the next symbol under point use M-n.

To move to the previous symbol under point use M-p.

## Replace
The following functions can be used to search and replace text in a
buffer.

To replace a string in the entire buffer run the buffer-replace-string
function which is bound to the following key chord.

    C-c r

To replace a regexp in the entire buffer run the buffer-replace-regexp
function which is bound to the following key chord.

    C-M-c r

## Searching in Project
Searching in the entire project can be done using one of the following
commands.

To search the entire project using helm-git-grep use the following
key chord.

    C-c s
or

    C-c g

To search the entire project using helm-ag-project-root use the
following key chord.

    C-c a

## Replacing in Project
This is covered more in a separate document about how to work with
projects. Replacing text in a project is a special case of the more
general case work on all files in the project.

## Goto definition
There are two different approaches that you can use for jumping to the
defintion of functions, classes, variables, etc. One is to using
something like a language server that creates an abstract syntax tree
and has full understanding of the code. This requires stuff like
Python virtual environments to be setup correctly. The other approach
is to just do text search. You can use regular expressions to try to
find the definition of a function for example.

Getting Python virtual environments or python running in a docker
image working with python-mode or anaconda-mode can be kind of tricky.
So for now I have decided on going with a pure text based approach for
going to definitions. The package I have picked for this is called
dumbjump.

To jump to the definition of the symbol under point use the following
key chord.

    M-g j
or

    s-j

where s is the Cmd key.

To jump back to where you were use the following key chord

    M-g b

or

    s-b

To jump to the definition in the other window use the following key
chord

    M-g o

or

    s-o
