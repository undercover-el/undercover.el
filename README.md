# undercover.el
[![Coverage Status](https://img.shields.io/coveralls/sviridov/undercover.el.svg)](https://coveralls.io/r/sviridov/undercover.el?branch=master) [![Build Status](https://travis-ci.org/sviridov/undercover.el.svg)](https://travis-ci.org/sviridov/undercover.el) [![license](http://img.shields.io/badge/license-MIT-brightgreen.svg)](https://github.com/sviridov/undercover.el/blob/master/LICENSE) [![MELPA](http://melpa.org/packages/undercover-badge.svg)](http://melpa.org/#/undercover) [![MELPA stable](http://stable.melpa.org/packages/undercover-badge.svg)](http://stable.melpa.org/#/undercover)

A test coverage library for [Emacs Lisp](http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html).

## Notes

`undercover.el` is on earlier stage of development and has some important issues:

- it assumes a certain development cycle of your package (using [Cask](https://github.com/cask/cask), [Travis CI](https://travis-ci.org/) and [Coveralls](https://coveralls.io/));
- it based on `edebug` and can have some issues with macros coverage.

## Installation

- Add `undercover.el` to your [Cask](https://github.com/cask/cask) file:

  ```lisp
  (source gnu)
  (source melpa)

  (package-file "awesome-package.el")

  (development
    (depends-on "undercover"))
  ```

- Before `load` or `require` your package in `test/test-helper.el` or `features/support/env.el` (or analogue), call `undercover` with wildcards that will match package files:

  ```lisp
  (require 'undercover)

  (undercover "*.el" "awesome-extensions/*.el" (:exclude "*-test.el"))
  (require 'awesome-package)
  ```

- Add your repository to [Coveralls](https://coveralls.io/).

## Configuration

- If you don't use [Travis CI](https://travis-ci.org/) you need to set `COVERALLS_REPO_TOKEN` environment variable before running tests, for example:

  ```sh
  $ COVERALLS_REPO_TOKEN=<your-coveralls-repo-token> cask exec ert-runner
  ```

- Set `report-file` option if you want to change report location:

  ```lisp
  (undercover "*.el" (:report-file "/tmp/local-report.json"))
  ```

  `undercover.el` will try to merge new report with existing one.

- Set `send-report` option to `nil` if you don't want to send coverage report:

  ```lisp
  (undercover "*.el" (:report-file "/tmp/local-report.json") (:send-report nil))
  ```
