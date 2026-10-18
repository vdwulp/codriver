## Test environments

* GitHub Actions ubuntu-latest: devel, release, oldrel-1
* GitHub Actions windows-latest: release
* GitHub Actions macos-latest: release
* win-builder: devel
* mac-builder: release

## R CMD check results

0 errors | 0 warnings | 0 notes on GitHub Actions and win-builder.

The mac-builder check stopped at dependency checking because several standard
CRAN dependencies were unavailable in its environment. The same package passed
the macOS check on GitHub Actions.

## Reverse dependencies

No reverse dependencies were found.

## Additional comments

Thank you for your time.
