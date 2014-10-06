# undercover.el [![Coverage Status](https://img.shields.io/coveralls/sviridov/undercover.el.svg)](https://coveralls.io/r/sviridov/undercover.el?branch=master) [![Build Status](https://travis-ci.org/sviridov/undercover.el.svg)](https://travis-ci.org/sviridov/undercover.el) [![license](http://img.shields.io/badge/license-MIT-brightgreen.svg)](https://github.com/sviridov/undercover.el/blob/master/LICENSE)

A test coverage library for [Emacs Lisp](http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html).

## Notes

`undercover.el` is on earlier stage of development and has some important issues:

- it assumes a certain development cycle of your package (using [Cask](https://github.com/cask/cask), [ert-runner](https://github.com/rejeep/ert-runner.el), [Travis CI](https://travis-ci.org/) and [Coveralls](https://coveralls.io/));
- it based on `edebug` and can have some issues with errors and macros coverage.

## Installation

- Add `undercover.el` to your [Cask](https://github.com/cask/cask) file:

  ```lisp
  (source melpa)

  (package-file "awesome-package.el")

  (development
    (depends-on "undercover"))
  ```

- Load your package with `undercover` function in `test/test-helper.el`:

  ```lisp
  (require 'undercover)

  (undercover "awesome.*el$") ; Regexp to match package files
  (require 'awesome-package)
  ```

- Add your repository to [Coveralls](https://coveralls.io/).
