# Search and Replace

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
