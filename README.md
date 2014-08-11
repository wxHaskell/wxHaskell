wxHaskell
=========

wxWidgets wrapper for Haskell

Status
======

This fork builds & compiles
- against: wxWidgets (http://www.wxwidgets.org/) 2.9.3, 2.9.4, 2.9.5, 3.0.0 and 3.0.1
- with: Haskell platform (http://www.haskell.org/platform/) 2013.2.0.0 and 2014.2.0.0
- on: MacOSX 10.8.4, 64 bits Haskell platform; Windows XP; Windows 8.1

Patches up and until 2014-08-11 have been merged into this fork.

All packages have cabal version 0.91.0.0


Build & installation
====================

As of 2014-08-11:
- Prerequisites:
  - install wxWidgets 2.9.3, 2.9.4, 2.9.5, 3.0.0 or 3.0.1 directly from a source release or use a platform specific installer.
    - if installed from source release configure with
        ./configure --disable-debug_flag	# to avoid debugging popups

- Development build using cabal-dev inside the wxHaskell directory:

    bin/mk-devel	# edit the search path in the file so the generated wxdirect is used

  Then use via the resulting package configuration cabal-dev/packages-7.6.3.conf (or any other compiler version)
  
  Clean:
  
    bin/mk-devel-clean

- Install globally via cabal

    bin/mk-cabal

  Clean:
  
    bin/mk-cabal-clean

Note: cabal and cabal-dev builds share the same build directory. Should not be a problem though...


Platform specifics
==================

MacOSX:
- 20130725: homebrew (http://brew.sh/) does not yet install wxWidgets 2.9.5 (by means of 'brew install wxmac').
