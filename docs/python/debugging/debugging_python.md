# Debugging Python
The following options are available for debugging Python code in
Emacs.

1. pdb
2. gud
3. realgud

## Changing directory
Sometimes you want to debug a Python file that is not at the root of
the project. In this case you have to change the current directory of
the buffer. Otherwise the debugger will not be able to import python
modules and packages correctly.

Change to the location of the program that you want to debug with

    M-x cd

## pdb
Python comes with a built in debugger called pdb. To run pdb on a
python script use the following command.

    python -m pdb script.py

To run pdb in emacs run the following command.

    M-x pdb

You will then have to enter someting like python -m pdb script.py in
the prompt in the minibuffer.

## gud
I am not familiar with how gud works.

## realgud
To run realgud on a buffer use the following command.

    M-x realgud:<debugger name>

To run realgud with pdb use the following command.

    M-x realgud:pdb

## Debuggin CLI tools written in Python
Sometimes you want to debug CLI tools that are written in Python. To
do this you will first have to create a small python script that acts
as if the cli program had been run from the command line.

Lets say you want to run a CLI tool called foo. To execute foo from
the command line you usually run the following command.

    foo

In a file called debug.py put

    from foo import main
    main()

The above assumes that foo has a function called main.

Now we can run pdb on that script to debug the CLI tool.

    python -m pdb debug.py

## References
https://github.com/realgud/realgud
