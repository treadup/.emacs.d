# Flycheck
Flycheck is a static analysis tool for Emacs. The way you usually work
with it is that you install an external static analysis tool that
Flycheck will then automatically pick up and use.

There is usually no need to explicitly specify which checker should be
used.

## Current checker
I think that if you run M-x flycheck-describe-checker then the default
value will be the checker that is currently being used for the buffer.
