::This script is meant to help resolve path length issues when building on windows
::If you need to use this, make a hardlink from the directory your git repo directory is in to `C:\\r` or some other very short path name
::mklink /J Link Target
::Then uncomment the `cd` below and put in the path the the directory containing the `word-compressor.cabal` file

::cd C:\\r\word-compressor
cabal build