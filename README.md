# undercover.el
[![Coverage Status](https://coveralls.io/repos/sviridov/undercover.el/badge.svg)](https://coveralls.io/r/sviridov/undercover.el?branch=master) [![Build Status](https://travis-ci.org/sviridov/undercover.el.svg)](https://travis-ci.org/sviridov/undercover.el) [![license](http://img.shields.io/badge/license-MIT-brightgreen.svg)](https://github.com/sviridov/undercover.el/blob/master/LICENSE) [![MELPA](http://melpa.org/packages/undercover-badge.svg)](http://melpa.org/#/undercover) [![MELPA stable](http://stable.melpa.org/packages/undercover-badge.svg)](http://stable.melpa.org/#/undercover)

A test coverage library for [Emacs Lisp](http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html).

## Notes

Few important notes about `undercover.el`:

- it assumes a certain development cycle of your package (using [Cask](https://github.com/cask/cask), [Travis CI](https://travis-ci.org/) and [Coveralls](https://coveralls.io/));
- it doesn't support test coverage for byte-compiled files;
- it based on `edebug` and can have some issues with macros coverage. It doesn't support [Circular Objects](http://www.gnu.org/software/emacs/manual/html_node/elisp/Circular-Objects.html).

Check out [combined usage example](https://github.com/sviridov/undercover.el-combined-usage-example) and [buttercup integration example](https://github.com/sviridov/undercover.el-buttercup-integration-example) for more information.

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
  (when (require 'undercover nil t)
    (undercover "*.el" "awesome-extensions/*.el" (:exclude "awesome-examples.el")))

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

- Set `UNDERCOVER_CONFIG` if you want to configure `undercover.el` via environment variables:

  ```lisp
  (when (require 'undercover nil t) (undercover))
  ```

  ```sh
  $ UNDERCOVER_CONFIG='("*.el" (:exclude "awesome-examples.el"))' cask exec ert-runner
  ```
  
- If you get `"UNDERCOVER: No coverage information [...]"`, make sure of the following:
    1. remove byte-compiled files (`*.elc`) of your project
    2. load and configure undercover before your project files (see above)
    3. make sure ert-runner does not load your project files (your project's `.ert-runner` should use `-L` instead of `-l` for files you want to measure coverage of)
    
- If you want to measure code coverage locally, you can set `TRAVIS=true` in the shell environment or `(setq undercover-force-coverage t)` in emacs.
