# undercover.el
[![Coverage Status](https://coveralls.io/repos/undercover-el/undercover.el/badge.svg)](https://coveralls.io/r/undercover-el/undercover.el?branch=master) [![Build Status](https://travis-ci.org/undercover-el/undercover.el.svg)](https://travis-ci.org/undercover-el/undercover.el) [![license](http://img.shields.io/badge/license-MIT-brightgreen.svg)](https://github.com/undercover-el/undercover.el/blob/master/LICENSE) [![MELPA](http://melpa.org/packages/undercover-badge.svg)](http://melpa.org/#/undercover) [![MELPA stable](http://stable.melpa.org/packages/undercover-badge.svg)](http://stable.melpa.org/#/undercover)

A test coverage library for [Emacs Lisp](http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html).

## Notes

A few important notes about `undercover.el`:

- it assumes a certain development cycle of your package (using either [Cask](https://github.com/cask/cask) or [Eldev](https://github.com/doublep/eldev), [Travis CI](https://travis-ci.org/), and [Coveralls](https://coveralls.io/), [Codecov](https://codecov.io/), or compatible);
- it doesn't support test coverage for byte-compiled files;
- it is based on `edebug` and can have some issues with macro coverage
- it doesn't support [Circular Objects](http://www.gnu.org/software/emacs/manual/html_node/elisp/Circular-Objects.html).

See the [combined usage example](https://github.com/undercover-el/undercover.el-combined-usage-example) and [buttercup integration example](https://github.com/undercover-el/undercover.el-buttercup-integration-example) samples for more information.

## Installation

### Cask

- Add `undercover.el` to your [Cask](https://github.com/cask/cask) file:

  ```lisp
  (source gnu)
  (source melpa)

  (package-file "awesome-package.el")

  (development
    (depends-on "undercover"))
  ```

- Before invoking `load` or `require` with your package in your test runner (`test/test-helper.el` / `features/support/env.el` / etc),
  call `undercover` with wildcards that will match your package's source files:

  ```lisp
  (when (require 'undercover nil t)
    (undercover "*.el" "awesome-extensions/*.el" (:exclude "awesome-examples.el")))

  (require 'awesome-package)
  ```

- Add your repository to a coverage reporting service, such as [Coveralls](https://coveralls.io/) or [Codecov](https://codecov.io/).

### Eldev

- Activate `undercover` plugin in your [Eldev](https://github.com/doublep/eldev) file:

  ```lisp
  (eldev-use-plugin 'undercover)
  ```

- When running tests on CI server, make sure not to do it in packaged or byte-compiled mode. Or do it twice: once with your project loaded as source code, once as a package. The reason is that `undercover.el` doesn't work with byte-compiled files.

- Add your repository to a coverage reporting service, such as [Coveralls](https://coveralls.io/) or [Codecov](https://codecov.io/).

See [relevant documentation](https://github.com/doublep/eldev#undercover-plugin) on Eldev's own page for more information.

## Configuration

### Online services

- **[GitHub Actions](https://github.com/features/actions) + [Coveralls](https://coveralls.io/)**

  You will need to set the `COVERALLS_REPO_TOKEN` environment variable. This can be done in the top-level `env` block.

  To enable Coveralls parallel builds, set `COVERALLS_PARALLEL` in the shell environment,
  and add a final job with `if: always()` which pings the webhook.

  Here is a complete example:

  ```yaml
  on: [ push, pull_request ]
  env:
    COVERALLS_PARALLEL: 1
    COVERALLS_REPO_TOKEN: 0123456789abcdefghijklmnopqrstuwx
  jobs:
    test:
      runs-on: ubuntu-latest
      strategy:
        matrix:
          emacs_version:
          - 25.3
          - 26.3
          - 27.1
          - snapshot
      steps:
      - uses: purcell/setup-emacs@master
        with:
          version: ${{ matrix.emacs_version }}
      - uses: conao3/setup-cask@master
      - uses: actions/checkout@v2
      - name: Test
        run: |
          cask install
          cask exec ert-runner
    finalize:
      runs-on: ubuntu-latest
      if: always()
      needs: test
      steps:
      - run: curl "https://coveralls.io/webhook?repo_token=$COVERALLS_REPO_TOKEN" -d "payload[build_num]=$GITHUB_RUN_ID&payload[status]=done"
  ```

- **[Travis CI](https://travis-ci.org/) + [Coveralls](https://coveralls.io/)**

  No configuration necessary.

  To enable Coveralls parallel builds, set `COVERALLS_PARALLEL` in the shell environment,
  and configure the web hook as [described in the Coveralls documentation](https://docs.coveralls.io/parallel-build-webhook).

- **(other CI) + [Coveralls](https://coveralls.io/)**

  You may need to set the `COVERALLS_REPO_TOKEN` environment variable before running tests, for example:

  ```sh
  $ COVERALLS_REPO_TOKEN=<your-coveralls-repo-token> cask exec ert-runner
  ```

  Consult the [Coveralls documentation](https://docs.coveralls.io/supported-ci-services) for details.

- **[Codecov](https://codecov.io/)**

  CodeCov can be used by saving the report and uploading it from your pipeline.

  In your test runner:

  ```lisp
  (undercover "*.el" (:report-file "coverage-final.json")
                     (:send-report nil))
  ```
  
  And in your pipeline (`.travis.yml` or equivalent):

  ``` yaml
  after_success:
    # Upload coverage
    - bash <(curl -s https://codecov.io/bash)
  ```

### Local reports

#### Cask

- Set the `report-file` option to change the report file location:

  ```lisp
  (undercover "*.el" (:report-file "/tmp/local-report.json"))
  ```

  `undercover.el` will try to merge new report with existing one.

- Set the `send-report` option to `nil` to disable uploading the coverage report to an online service:

  ```lisp
  (undercover "*.el" (:report-file "/tmp/local-report.json") (:send-report nil))
  ```

- Set `report-format` to use a different format for the report file:

  ```lisp
  (undercover "*.el" (:report-file "coverage/.resultset.json")
                     (:report-format 'simplecov)
                     (:send-report nil))
  ```

- Set the `UNDERCOVER_FORCE` environment variable to calculate coverage even when not running on a CI:

  ```sh
  $ UNDERCOVER_FORCE=true cask exec ert-runner
  ```

  Alternatively, use `(setq undercover-force-coverage t)` before calling `(undercover)`.

- Set the `UNDERCOVER_CONFIG` variable to configure `undercover.el` via the environment:

  ```lisp
  (when (require 'undercover nil t) (undercover))
  ```

  ```sh
  $ UNDERCOVER_CONFIG='("*.el" (:exclude "awesome-examples.el"))' cask exec ert-runner
  ```

#### Eldev

With Eldev generating local reports is very easy:

  ```sh
  $ eldev test -U local-report.json
  ```

Option `-U` is the short form of `--undercover-report` and is only available if the plugin is activated (see above). Option `-u` (`--undercover`) lets you configure the library from command line. For more information see [Eldev's documentation](https://github.com/doublep/eldev#undercover-plugin).

Selecting which exactly files you want `undercover.el` to instrument is not possible from command line: these always default to all `.el` files in `main` fileset. However, you can adjust variable `eldev-undercover-fileset` in file `Eldev` if you need to change that for some reason.

## Troubleshooting

### Code in macros is not included in coverage reports

You may need to teach `edebug` how to instrument the affected macros.

See ["Instrumenting Macro Calls" in the Emacs manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/Instrumenting-Macro-Calls.html) 
and the documentation of `def-edebug-spec` for more details.

### "UNDERCOVER: Error while loading ... for coverage:"

The problem may be due to edebug failing to parse the file.
Follow the instructions under the error message.

### "UNDERCOVER: No coverage information [...]"

Try the following:

1. remove byte-compiled files (`*.elc`) of your project
2. load and configure undercover before your project files (see above)
3. make sure `ert-runner` does not load your project files (your project's `.ert-runner` should use `-L` instead of `-l` for files you want to measure coverage of)

## Viewing coverage in Emacs

### Simple report

You can generate a simple coverage summary report using the `'text` report format:

```lisp
(require 'undercover)
(setq undercover-force-coverage t)
(undercover "*.el" (:report-file nil) ; or a text file to save the report to
                   (:report-format 'text))
```

### Coverage overlay

1. Install [coverage-mode](https://github.com/Bogdanp/coverage-mode)

2. Create the `coverage` directory in your project root

3. Configure `undercover.el` as follows:

   ```lisp
   (require 'undercover)
   (setq undercover-force-coverage t)
   (undercover "*.el" (:report-file "coverage/.resultset.json")
                      (:report-format 'simplecov)
                      (:send-report nil))
   ```

4. Run your tests

5. Open a source file, and enable `coverage-mode`.
