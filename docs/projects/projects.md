# Working with Projects in Emacs
This document will cover different ways of working with projects in
Emacs.

## What is a project
I have started viewing a project as a collection of files and folders
rooted in a certain directory called the project root directory. In
other words a project is the tree of all the files and subdirectories
rooted at the project root directory.

## Projectile
Projectile is a package for working with projects in Emacs. My problem
with projectile is that you have to explicitly select which project
you are working on. It is not picked up automatically based on the
current buffer you are working on. This is not a good fit for my work
flow. All I want to do is to perform operations in a buffer and have
the project related functions automatically detect which project I am
currently in.

## Search and replace across project files
The problem of performing a search and replace operation over all the
files in a project is a special case of the more specific problem of
performing operations over all of the files in the project.

## Perform operations on all files in a project.
The core problem to figure out then is how to perform an operation on
all the projects in a file. There are both read only and write operations.

An operation could be read only. In this case we are probably trying
to generate a report based on all files in the project. Or we might be
trying to generate some code.

An operation could be a write operation. In this case the operation
would modify the contents of the file/buffer based on some algorithm.

In the end an operation is an Elisp function that gets called on the
contents of a file/buffer.

There are a couple of different approaches to solving this problem.

### Use external program
An external program can be used to modify all the files in a project.
For example you could use sed to perform global search and replace on
the project.

One thing you have to be careful of when taking this approach is that
if you have buffers open with unsaved changes then the external
program will just modify the underlying files underneath you. This
means that you should probably use the following workflow if you take
this approach.

1. Save or close all unsaved buffers
2. Run the program that performs the modifications
3. Reload all open buffers from disk

### Perform operations on all open buffers
You can use Emacs to perform operations on all open buffers. With this
approach which buffers are currently open becomes important.

You could filter the buffer list down a bit to perform operations on
all open buffers backed by a file. You might also only want to perform
the operation on file backed buffers that are part of the same project
as the buffer you are currently in. Also you it might be good to only
perform operations on buffers of a certain file type. For example one
might want to perform operations on all open Python buffers in a
certain project.

You could split write operations into two steps.

1. Perform an operation that modifies the open buffers without saving
2. Save all unsaved buffers

This allows us to skip implementing save functionality in each
operation. Currently save-all-buffers is bound to s-s.

### Perform operations on all files in the project
