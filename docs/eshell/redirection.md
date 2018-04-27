# Redirection in Eshell
In Eshell you can use > or >> or >>> for output redirection.

    > means overwrite the file
    >> means append to the file
    >>> means insert at point - This works when redirecting to a buffer
                                or to an open file.

The destination can be either a filename or a buffer.

    echo "hello world" > greeting.txt
    echo "hello world" > #<foo>
    echo "hello world" > #<buffer foo>

Eshell also supports two pseudo-devices.

To redirect to the clipboard use the following pseudo device.

    /dev/clip

To redirect to the kill ring use the following pseudo device.

    /dev/kill

Eshell can write to multiple output targets.

    echo "hello world" > a > b > c

You can redirect to both a pipe and a file at the same time.

    echo "hello world" > a | wc

## References
https://www.emacswiki.org/emacs/EshellRedirection
