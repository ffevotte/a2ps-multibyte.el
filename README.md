# a2ps-multibyte

This package allows printing Emacs buffers using `a2ps`, even when they use an UTF-8
charset or another multibyte character coding system (such encodings are otherwise not
supported by `a2ps`).

This only works if the buffer contents could have been encoded using another supported
coding system (e.g. a French text stored as UTF-8, but featuring only ISO-8859-15
characters).


## Installation

    (add-to-list 'load-path "/path/to/a2ps-multibyte)
    (require 'a2ps-multibyte)
    
You can optionally bind the `a2ps` commands to keys:

    (global-set-key (kbd "<print>")   'a2ps-buffer)
    (global-set-key (kbd "S-<print>") 'a2ps-region)


## Basic usage

`a2ps-multibyte` reimplements the same commands as the Emacs' standard `a2ps` package. You
can thus use the usual interface:

- `M-x a2ps-buffer`
- `M-x a2ps-region`

Giving these commands a prefix argument allows specifying additional switches to be used
in the `a2ps` command line.


## Customization

The relevant variables are:

- `a2ps-command`: path to the `a2ps` utility.
- `a2ps-switches`: list of additional command-line switches, e.g.

       (setq a2ps-switches '("-l" "100"))


## License

Copyright (C) 2012 François Févotte.

Large parts of this code come from `a2ps-print.el` in the
[a2ps package](http://www.gnu.org/software/a2ps/) and are (C) Bruce Ingalls.

This program is free software: you can redistribute it and/or modify it under the terms of
the GNU General Public License as published by the Free Software Foundation, either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.
If not, see <http://www.gnu.org/licenses/>.
