# Markdown
Use markdown-mode to work on markdown.

## Markdown mode
In markdown mode you can compile the current buffer containing
markdown to html using the C-c C-c m chord.

To be able to compile markdown to html you have to have the command
line program [multimarkdown](http://fletcherpenney.net/multimarkdown/)
installed.

You can view which command is currently being used by markdown mode by
looking at the markdown-command variable.

There are a bunch of different useful keybindings for markdown-mode.

## HTML to Markdown
The html-to-markdown package can be used to convert html to markdown.
There are two functions.

### html-to-markdown
Is meant for interactive use. It takes the current buffer (or region),
converts to Markdown, and displays the result in a separate window.

### html-to-markdown-string
Is meant for lisp code. It takes a string argument, which is converted
to Markdown, and the result is returned.

## References
https://github.com/jrblevin/markdown-mode
