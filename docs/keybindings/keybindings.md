# Key bindings
Consider C-f. It moves forward by the character, but M-f moves forward
by word, and C-M-f moves forward by an s-expression. See the
similarities? The C- for character, M- for word and C-M- for s-exp is
a recurring pattern in Emacs.

## Key bind for strings and key bindings
One cool thing you can do is to create a key binding for a string
or for another key binding.

You can create a key binding for any of the following.

1. Interactive function
2. Other key binding
3. Arbitrary strings

### Interactive function
Binding a key to a function is done in the following manner.

    (global-set-key (kbd "<f1>") 'esh)

### Other key bindings
Binding a key to another keybinding is done in the following manner.

    (global-set-key (kbd "C-x 9") (kbd "C-x 2 C-x o C-x 3"))

### Arbitrary strings
To bind a function to an arbitrary string do the following.

    (global-set-key (kbd "C-c C-n") "Hello world!")

## References
https://www.masteringemacs.org/article/my-emacs-keybindings
