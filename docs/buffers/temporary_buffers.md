# Temporary Buffers
In emacs you can create a new temporary buffer by using the C-x b
keybinding and then entering a new name.

## Quick temp buffer
You can just mash on the keyboard to create a random name and then
press enter.

    C-x b <mash on keyboard> RET

This will create a new temporary buffer for you.

## Named temp buffer
If you want a named buffer you can press C-x b and enter the name
instead.

    C-x b <name of temp buffer> RET

## Killing the temp buffer
To kill the temp buffer just do C-x k. Since the temp buffer is not
associated with a file is will kill the buffer without prompting.

## Initial major mode
I have configured Emacs so that creating a buffer that is not
associated with a file will still respect the auto-mode-alist.

In other words if you create a new buffer called foo.py this buffer
will open in python-mode. If you create a new buffer called bar.md
this buffer will open in markdown-mode.

## Toggle scratch buffer
You can toggle between the scratch buffer and the other buffer using
the C-x 7 key combination.

    C-x 7
