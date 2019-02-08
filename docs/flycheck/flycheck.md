# Flycheck
Flycheck is a static analysis tool for Emacs. The way you usually work
with it is that you install an external static analysis tool that
Flycheck will then automatically pick up and use.

There is usually no need to explicitly specify which checker should be
used.

## Current checker
I think that if you run M-x flycheck-describe-checker then the default
value will be the checker that is currently being used for the buffer.

## Change current checker
You can change the current checker using the flycheck-select-checker
command. This command uses the C-c ! s key chord.

## Verify setup
To show how flycheck is currently configured you can use the
flycheck-verify-setup command. This command uses the C-c ! v key chord.

## Commands
Here are some of the more useful flycheck commands.

Key chord | Description
----------+------------------------
C-c ! c	  | Run syntax checker on current buffer
C-c ! l	  | Show a list of all the current flycheck errors
C-c ! C-w | Copy the current error under point to the clipboard
C-c ! n	  | Next error
C-c ! p	  | Previous error
