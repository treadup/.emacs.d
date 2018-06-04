# Profiling
Emacs comes with two a built in profilers for Elisp. One is called the
Emacs Lisp Profiler and the other is called the Emacs Native Profiler.

## Emacs Native Profiler
To run the emacs native profiler issue the following command.

    M-x profiler-start

Then do whatever it is that you want to profile. After you think that
you have collected enough data run the following command.

    M-x profiler-report

This will produce a report in a separate buffer. Items marked with a +
can be expanded. Move the point to the item and hit RET to expand or
collapse the item.

## References
https://www.emacswiki.org/emacs/EmacsLispProfiler
https://www.emacswiki.org/emacs/EmacsNativeProfiler
