# Docker
Emacs has some nice integrations with Docker.

## Docker and TRAMP
There is a package called docker-tramp that provides TRAMP integration
for docker containers.

It allows you do open a file inside a running docker container using
the following command.

    C-x C-f /docker:user@container:/path/to/file/in/container

Also docker-tramp comes with nice eshell integration which allows you
to cd into a running docker container using the following command.

    cd /docker:user@container:/path/to/folder

## Isolated Environments
This allows one to do things like launching a new docker container and
then installing python packages in the root of the container. Emacs
will pick these packages up automatically when editing a file inside
the docker container.

Things like C-c C-p when you are editing a .py file will launch the
python executable that exists in the docker container.

## References
https://github.com/emacs-pe/docker-tramp.el
