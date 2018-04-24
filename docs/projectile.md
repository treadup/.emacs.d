# Projectile
Projectile is an Emacs package for project management. It introduces
the concept of a project that you can then run commands on.

## Known Projects
There is a variable called projectile-known-projects that contains
information about the currently known projectile projects.

    projectile-known-projects

This is a list.

There is the function (projectile-add-known-project) which will add a
folder to the known projects list.

There is a variable called projectile-known-projects-file which the
filename of the file that holds the list of projectile projects.

The function projectile-cleanup-known-projects will remove known
projects that do no longer exist.

There is a function called projectile-save-known-projects that will
save the list in projectile-known-projects to the file
projectile-known-projects-file.
