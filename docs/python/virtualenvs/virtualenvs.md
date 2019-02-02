# Python Virtualenvs and Emacs
Here are some ideas on using a Python virutal environment with Emacs.

## Python and Emacs
Code completion and launching a python shell will use the system
python interpreter unless you specify that you should use a virtual
environment.

## Activating Virtual Environments
When activating a virutal environment in Emacs the main thing that
happens is that the python-shell-virtualenv-root variable is set to
the folder name of the python virutal environment that should be
activated.

There are a bunch of different packages that does this and other
things like making sure that M-x shell is launched with the virtual
environment activated.

The main packages for activating a python virtual environment that I
have used in Emacs are the following.

1. pyvenv
2. pythonic

## pyvenv
Currently I have been using pyvenv but I think I might want to switch
to using pythonic for better integration into anaconda-mode.

pyvenv has the following commands that are interesting.

pyvenv-activate
pyvenv-workon
pyvenv-deactivate

## pythonic
The pythonic package provides two commands.

1. pythonic-activate
2. pythonic-deactivate

The pythonic-activate command supports tramp paths. This means that
you can probably activate a virtual environment on a remote host or a
virtual environment in a docker container.

## virutalenv
Create my own package called virtualenv that has four functions.

1. virtualenv-activate
2. virtualenv-deactivate
3. virtualenv-workon
4. virtualenv-auto

These functions would in turn delegate to pythonic.

### The virtualenv-activate command
The virtualenv-activate command would activate a virtual environment
given the name of the virutal environment folder.

### The virtualenv-deactivate command
The virtualenv-deactivate command would deactivate the currently
active virtual environment.

### The virtualenv-workon command
The virtualenv-workon command would allow you to enter the name of a
virtual environment that would then be activated.

An improvement would be to show a list of virtual environment names
that you can choose from. The text you type would be interpreted as a
prefix and would filter the list of names. Kind of like how the
current helm-buffers-list works.

### The virtualenv-auto command
The virtualenv-auto command would activate the virtual environment
associated with the current visited file. (If the buffer has no
associated file then this is a no operation.)

It would do this by finding a file called .venv and parsing out the
name of the virtual environment from this file.

## Aliases
It might be a good idea to have shorter names for some of these
operations. That way you could M-x these commands.

## Keybindings
The only command that needs a keybinding is probably virtualenv-auto.

    I need to figure out which keys are available for keybindings in
    python mode.

## Eshell
For eshell I use the vpy thing that I have built. This means that we
can activate a python virtual environment on a per folder basis.

## References
https://github.com/jorgenschaefer/pyvenv
https://github.com/proofit404/pythonic
