wxHaskell
=========

wxWidgets wrapper for Haskell

This is
=======

This is a fork of jodonoghue/wxHaskell intended to combine/hold the various patches required to keep wxHaskell working.

Status
======

This fork builds & compiles
- against: wxWidgets (http://www.wxwidgets.org/) 2.9.5 released 20130716
- with: Haskell platform (http://www.haskell.org/platform/) 2013.2.0.0
- on: MacOSX 10.8.4, 64 bits Haskell platform

Patches up and until 20130725 have been merged into this fork.

There is no guarantee that this fork builds with other combinations.
This is partly because internally components (in particular wxdirect)
cannot deal with version variation done by means of (C) preprocessing.
OS and Haskell platform differences should be no problem if it was no problem before this fork.

All packages have cabal version 0.90.1.0, intending this to lead to a maintenance release.

Build & installation
====================

As of 20130725:
- Prerequisites:
  - install wxWidgets 2.9.5 directly from a source release or use a platform specific installer.
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
- 20130725: homebrew (http://brew.sh/) does not yet install 2.9.5 (by means of 'brew install wxmac').
