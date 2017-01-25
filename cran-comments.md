## Test environments
* local Windows install, R 3.3.2
* ubuntu 12.04.5 (on travis-ci), R 3.3.2
* Win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

## Reverse dependencies

This update is to correct an issue that was revealed when running checks on the ForestTools package, which depends on the TileManager package. On a POSIX file system, some files would be left in /tmp since their file paths were defined using the 'paste' function instead of 'file.path'. This has now been corrected.
