          ============================
					 Working Through the Text:
					     Simply Scheme:
					Introducing Computer Science
					           2/e
					  Copyright (C) 1999 MIT
					============================

These directories hold my worked problems and some supporting files
for the textbook:

Harvey, B., & Wright, M. (1999). Simply Scheme: Introducing Computer
Science (2ND ed.). MIT. (https://people.eecs.berkeley.edu/~bh/ss-toc2.html)

...as downloaded mid January 2025.

To work through the text in 2025 you need to do one of the following:

- Load `From_Text/simply.scm' into your Scheme (Chicken 5 tested, R5RS
  seems to be required, does not work with Guile or Chez).

- Use Racket with it's `#lang simply-scheme' directive.


Files from the Text:
-------------------

These can all be found in 'From_Text\'. The authors provide a common
abstraction layer in 'simply.scm' and it must be loaded (or emulated
in Racket). The other files are demonstrations and examples from
the text. 


Tooling Choices:
---------------

I started out using Chicken Scheme and Emacs/Geiser but quite frankly
the Scheme support in Emacs for anything but Guile is lacking. Out of
frustration I turned to Racket and while I don't like the immutable
bindings of a run when I'm debugging, it is a better experience than
Geiser or traditional `inferior scheme' support.


Additional Requirements:
-----------------------

I rely on srfi-78 for a minimal testing framework. I like to have many
small tests and the support in srfi-78 is ideal. There is no required
setup beyond loading it:

- For Chicken: (import srfi-78)

- For Racket: (require srfi/78)

Needed around chapter 13, but useful before then, is a runtime trace
facility. One is available in Racket with simply-scheme enabled, but
in Chicken you must '(import trace)'.


The Worked Problems:
-------------------

My worked problems are in 'Problems\'. The file names for the problem sets
follow this pattern:

ch##--bb-ee.scm

Where:

  ## is the chapter number
  bb is the first (or only) problem in the file
  ee is the last problem in the file

Larger project sized work from the text is named:

ch##--name.scm

While the default extension for Racket is .rkt, I started this work
in Chicken and continued using .scm. 

All the files contain the following as preamble and epilog:

  - Comments appropriate to the problems.
  - #lang simply-scheme
  - (require srfi/78)
  - Optional and minimal setup for srfi-78 reporting.

When I started out using Chicken I had envisioned executing all
worked problems and their tests in batch mode, with the pass/fail
from the tests providing assurance that everything works. This may
or may not be possible/worth doing in Racket, but the problem files
are still written to support doing so.

Several chapters had no problems worth working out in a REPL. So far
the list is chapters 1, 2, 3, 4, 5, 10, and 13. Stub files with no
problem number suffix exist as placeholders.

Chapter 6 ended up being a work through in a stream and has no problem
number designations.

There is some copy/paste reuse of procedures from earlier chapters. A
common load file or module is a better solution but would not be in
the spirit of the text or my work through.


Licensing:
---------

The text is still in copyright and still available to purchase, but
Harvey offers the text online for personal use. The original license
from the text is in `COPYLEFT.txt', my licensing can be found in
`LICENSE' and `MIT-LICENSE' for any derivative work I do.

To boil it down, their license is an early GPL. Anything I write I
consider public domain but for those who require a more explicit
license, you can choose between the UNLICENSE and the MIT License.

I'm quite sure there's nothing I've done here that anyone would want
to borrow and reuse.

Troy Brumley, blametroi@gmail.com, February 2025.

So let it be written,
So let it be done.
