# undercover.el
[![Coverage Status](https://coveralls.io/repos/undercover-el/undercover.el/badge.svg)](https://coveralls.io/r/undercover-el/undercover.el?branch=master) [![Build Status](https://travis-ci.org/undercover-el/undercover.el.svg)](https://travis-ci.org/undercover-el/undercover.el) [![license](http://img.shields.io/badge/license-MIT-brightgreen.svg)](https://github.com/undercover-el/undercover.el/blob/master/LICENSE) [![MELPA](http://melpa.org/packages/undercover-badge.svg)](http://melpa.org/#/undercover) [![MELPA stable](http://stable.melpa.org/packages/undercover-badge.svg)](http://stable.melpa.org/#/undercover)

A test coverage library for [Emacs Lisp](http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html).

## Notes

A few important notes about `undercover.el`:

- it assumes a certain development cycle of your package (using either [Cask](https://github.com/cask/cask) or [Eldev](https://github.com/doublep/eldev));
- it doesn't support test coverage for byte-compiled files;
- it is based on `edebug` and can have some issues with macro coverage;
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

  You will need to export the GitHub Actions access token into the environment.

  To enable Coveralls parallel builds, set `COVERALLS_PARALLEL` in the shell environment,
  and add a final job with `if: always()` which pings the webhook.

  Here is a complete example:

  ```yaml
  on: [ push, pull_request ]
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
        env:
          COVERALLS_FLAG_NAME: Emacs ${{ matrix.emacs_version }}
          COVERALLS_PARALLEL: 1
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        run: |
          cask install
          cask exec ert-runner
    finalize:
      runs-on: ubuntu-latest
      if: always()
      needs: test
      steps:
      - run: curl "https://coveralls.io/webhook?repo_name=$GITHUB_REPOSITORY&repo_token=${{ secrets.GITHUB_TOKEN }}" -d "payload[build_num]=$GITHUB_RUN_NUMBER&payload[status]=done"
  ```

  Alternatively to exporting `GITHUB_TOKEN`, you may instead specify `COVERALLS_REPO_TOKEN`, as with any other CI service.

- **[Travis CI](https://travis-ci.org/) + [Coveralls](https://coveralls.io/)**

  No configuration necessary.

  To enable Coveralls parallel builds, set `COVERALLS_PARALLEL` in the shell environment,
  and configure the web hook as [described in the Coveralls documentation](https://docs.coveralls.io/parallel-build-webhook).

- **(other CI)**

  `undercover.el` has basic support (for reading and parsing relevant environment variables, such as build ID) for the following CI services:

  - GitHub Actions
  - Travis CI
  - Shippable
  - Drone
  - Jenkins
  - Circle CI
  - CodeShip
  - Wercker
  - GitLab CI
  - AppVeyor
  - Surf
  - BuildKite
  - Semaphore
  - Codefresh

  Detected values may be overridden by setting the following environment variables:

  - `UNDERCOVER_CI_TYPE`
  - `UNDERCOVER_CI_NAME`
  - `UNDERCOVER_COMMIT`
  - `UNDERCOVER_REF`
  - `UNDERCOVER_PULL_REQUEST`
  - `UNDERCOVER_BUILD_ID`
  - `UNDERCOVER_BUILD_NUMBER`
  - `UNDERCOVER_JOB_ID`
  - `UNDERCOVER_JOB_NUMBER`
  - `UNDERCOVER_JOB_NAME`

  See the documentation of `undercover--detect-ci` for a description of the semantics for these variables.

- **(other CI) + [Coveralls](https://coveralls.io/)**

  For CI services which are not "magically" supported by Coveralls,
  you will need to set the `COVERALLS_REPO_TOKEN` environment variable
  before running tests, for example:

  ```sh
  $ COVERALLS_REPO_TOKEN=<your-coveralls-repo-token> cask exec ert-runner
  ```

  Consult the [Coveralls documentation](https://docs.coveralls.io/supported-ci-services) for details.

  The token should not be made public, so it should be placed in the CI service's secrets store.

  Fields in the submitted Coveralls report may be overridden using standard environment variables:

  - `COVERALLS_SERVICE_NAME`
  - `COVERALLS_REPO_TOKEN`
  - `COVERALLS_SERVICE_NUMBER`
  - `COVERALLS_SERVICE_PULL_REQUEST`
  - `COVERALLS_PARALLEL`
  - `COVERALLS_FLAG_NAME`
  - `COVERALLS_RUN_AT`

  See the [Coveralls API reference](https://docs.coveralls.io/api-reference) for a description of these fields.

- **[Codecov](https://codecov.io/)**

  CodeCov is supported in combination with their bash upload script.

  In your test runner:

  ```lisp
  (undercover "*.el" (:report-format 'codecov)
                     (:send-report nil))
  ```

  And in your pipeline (`.travis.yml` or equivalent):

  ``` yaml
  after_success:
    # Upload coverage
    - bash <(curl -s https://codecov.io/bash)
  ```

### Local reports

#### Cask / Emacs Lisp

- Set the `report-file` option to change the report file location:

  ```lisp
  (undercover "*.el" (:report-file "/tmp/local-report.json"))
  ```

  `undercover.el` will try to merge new report with existing one.

- Set the `send-report` option to `nil` to disable uploading the coverage report to an online service:

  ```lisp
  (undercover "*.el" (:report-file "/tmp/local-report.json")
                     (:send-report nil))
  ```

- Set `report-format` to use a different format for the report file:

  ```lisp
  (undercover "*.el" (:report-file "coverage/.resultset.json")
                     (:report-format 'simplecov)
                     (:send-report nil))
  ```

  See the documentation of the `undercover` function for more details.

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

### Viewing coverage in Emacs

#### Simple report

You can generate a simple coverage summary report using the `'text` report format:

```lisp
(require 'undercover)
(setq undercover-force-coverage t)
(undercover "*.el" (:report-file nil) ; or a text file to save the report to
                   (:report-format 'text))
```

#### Coverage overlay

1. Install [coverage-mode](https://github.com/Bogdanp/coverage-mode)

2. Create the `coverage` directory in your project root

3. Configure `undercover.el` as follows:

   ```lisp
   (require 'undercover)
   (setq undercover-force-coverage t)
   (undercover "*.el" (:report-format 'simplecov)
                      (:send-report nil))
   ```

4. Run your tests

5. Open a source file, and enable `coverage-mode`.


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
