It looks like we can only have one Python virtual environment activated in Emacs
at one time. This means that it is not possible to have two virtual environments
activated at the same time. This in turn means that if we have two buffers showing
files from different Python projects, and therefore use different virtual environments,
then we will not be able to get correct completion in both buffers.


Write new elisp commands that will activate the virtual environment
and set the python path correctly. Then we can just execute these
commands and we are good to go.

perhaps call the command venv?

When you execute projectile commands to switch projects then we could
also activate the appropriate virtual env.

It looks like the best way to work with multiple projects in Emacs is
to have on new Emacs process for each python virtualenv.

If we do create our own commands to activate a virtual environment
then this approach will work with conda too. Just switch the commands
over to what is required to use conda.

Perhaps you could do something with dir local variables to call the
correct activation function.


So we could have a foo activate and a foo deactivate function for each
project. And then we call these as appropriate.

When you open emacs you could check and see if you should activate a
project.

And it looks like I could still be using pyvenv to activate the
virtual environments.

There is a minor mode called pyvenv-tracking-mode in the pyvenv.el
file that I think could be combined with dir local variables to
get the behavior that emacs will automatically switch virtualenvs
when you switch buffers.
