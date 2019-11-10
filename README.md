Editing GAP files and running GAP in Emacs buffers
==================================================
[![MELPA](http://melpa.org/packages/gap-mode-badge.svg)](http://melpa.org/#/gap-mode)

(Written 20 Feb 1993)
(Revised 1 Nov 2011)

The files "gap-mode.el" and "gap-process.el" provide modes for both editing
GAP  programs in  Emacs and running a  GAP  session within an Emacs buffer.
Brief installation instructions are given at the end of this document.

The latest version can be found at https://gitlab.org/gvol/gap-mode .

Editing GAP files in Emacs
--------------------------

Opening any file ending in ".g" or ".gap" should automatically put you into
gap-mode, the  major mode for  editing of GAP  code. This mode may  also be
invoked in any buffer at any time by typing M-x gap-mode.

Once in gap-mode there are some notable changes in the  behaviour of Emacs.
Whenever you  press return for a  new line Emacs will reindent  the current
line and auto-indent the new line (this behaviour can  be deactivated).  At
any time, the TAB key  will reindent the  current line, `M-q` will reindent
each line in the current region, and `M-C-q` will reindent each line in the
whole buffer.

Gap-mode will add indentation for if..then structures, function definitions
and all looping structures,

    for N in [1..10] do
        Print ( N );
        if N > 5 then
            Print ( N^2 );
        fi;
    od; ,

as well as indenting continued statements  (those that cross  a line break)
in a number  of different ways. For example,  it  will attempt to  match up
each line of a matrix,

    x := [ [ 1, 2, 3 ],
           [ 4, 5, 6 ],
           [ 7, 8, 9 ] ];

and the arguments of a function call,

    Print ( a, b, c
            d, e, f ); .

There are quite a number of variables that control how gap-mode indentation
behaves. Consult the help for gap-mode by  typing `C-h m` for a list of the
variables (and the features of gap-mode in general), and then `C-h v <var>`
for  a  description of  what  the  variable  <var>  controls.  It  is  also
suggested that you  read the GAP menu  in emacs to find  the commands which
might be useful.


Running GAP in an Emacs buffer
------------------------------

Type `M-x gap` to run a GAP process with input  and output through an Emacs
buffer.  Any text typed at the end of the *gap* buffer will  be sent to GAP
when the RETURN key is pressed,  and  GAP's output will  be appended to the
end of the buffer. The mode is based on comint-mode.

Moving back through previous commands is slightly different. Use  `M-p` and
`M-n` for previous and  next input. The  command  `M-l` will find the  last
input  that matches what  has already been  typed.    There  are some other
features that are  inherited (as these are) by  using comint-mode as a base
(see  the documentation for gap-process-mode by typing `C-h m` in the *gap*
buffer, and also the help for comint-mode: `C-h f comint-mode`).

TAB  will complete  as usual, except  that  if there is no unique (partial)
completion then  the list  of completions will   be given immediately  in a
separate *Completions* buffer. Similarly the help  function `?`, which will
ask for  a topic (defaulting to  the current  identifier),  will   give its
results in a *GAP Help* buffer instead of the *gap* buffer.

In fact, if a GAP process is running in the *gap* buffer  and NOT BUSY with
a  calculation, then  completion and help   are also  available in  the gap
editing mode (gap-mode) by typing `M-TAB` and `M-?` respectively.

When starting up  the GAP process, giving a prefix  argument to the command
(eg by typing `C-u M-x gap`) will  cause the contents of the current buffer
to be given to GAP as initial input,  and GAP will behave exactly as if you
had typed all  the current buffer contents into the  new *gap* buffer.  You
can also send the contents of the  current buffer to the *gap* buffer later
via `C-c  C-b`, or if  the buffer is  backed by a  file by `C-c  C-f` which
sends  Read statement  and  therefore is  faster.  You  can  also send  the
current function  defition, current  statement, or  current region.

There is support for adding local variable statements to functions.  Typing
`C-c l` while in a function definition will add (or regenerate) a statement
with  all the  local variables.   Unfortunately, it  will incorrectly  view
global variables  as being local if  they are assigned to.   Typing `C-c a`
will add  the current identifier to  a preexisting local variable  list (or
creates a new one if `gap-local-variable-inserts-statement` is non-nil).

Installation
============

MELPA
-----

You  can now  install gap-mode  through MELPA  (http://melpa.milkbox.net/).
This makes  installation easier and  especially makes it easier  to update.
If  you have  Emacs  24 (or  later) then  package.el  is already  included.
Otherwise you  will have to install  it by downloading an  old version from
https://github.com/technomancy/package.el

After than you have to tell it to use MELPA, so add something like

    (setq package-archives
          '(("melpa"     . "http://melpa.milkbox.net/packages/")
            ("gnu"       . "http://elpa.gnu.org/packages/")))
    (package-initialize)

to your .emacs.  Finally,  run `package-list-packages`, find gap-mode, type
`i` (for install)  and then `x` (to execute).  It  will download the latest
version and  install it automatically.  Running  `package-list-packages` in
the future will allow you to easily updage gap-mode.

Manual Installation
-------------------

Put the files  "gap-mode.el" and "gap-process.el" into a  directory in your
Emacs lisp load path, and add  the following lines to your ".emacs" startup
file.

    ;; gap mode
    (autoload 'gap-mode "gap-mode" "Gap editing mode" t)
    (setq auto-mode-alist (append (list '("\\.g\\'" . gap-mode)
                                        '("\\.gap\\'" . gap-mode))
                                  auto-mode-alist))
    (autoload 'gap "gap-process" "Run GAP in emacs buffer" t)


Configuration
-------------

For gap-mode  to function properly  you will have  to set a  few variables,
either via `M-x customize-group RET gap RET`, or by setting them directly in
your .emacs like

    (setq gap-executable "/usr/algebra/bin/gap")
    (setq gap-start-options (list "-l" "/usr/algebra/gap3.1/lib"
                                  "-m" "2m"))


If you like to  see the help inside emacs, but use  other settings inside a
terminal then you can add the following to your .gaprc file:

    # Versions 4.4 and 4.5 use different formats for environment variables
    if (IsRecord(GAPInfo.SystemEnvironment) and
        IsBound(GAPInfo.SystemEnvironment.INSIDE_EMACS )) or
       (not IsRecord(GAPInfo.SystemEnvironment) and
        Filtered(GAPInfo.SystemEnvironment,
                x->Length(x) > 13 and x{[1..13]} = "INSIDE_EMACS=")) then

        # Emacs colors the prompt anyway
        ColorPrompt( false );
        # See help in emacs
        SetHelpViewer("screen");
        # Print all the help without paging -- faster and more robust
        PAGER := "tail";
        PAGER_OPTIONS := [ "-n" ];

    else
        # Whatever pager etc. you like in a terminal
    fi;


Authors
-------

Originally written by

Michael Smith
Mathematics Research Section
Australian National University.

Contributions by Gary Zablackis and Goetz Pfeiffer

Now maintained by

Ivan Andrus
