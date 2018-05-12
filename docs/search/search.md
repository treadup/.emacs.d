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

The interactive function rs is an alias for replace-string. You can
run replace-string using the following command.

    M-x rs

The interactive function rr is an alias for replace-regexp. You can
run replace-regexp using the following command.

    M-x rr

The interactive function qrr is an alias for query-replace-regexp. You
can run query-replace-regexp using the following command.

    M-x qrr

## Searching in Project

## Replacing in Project
