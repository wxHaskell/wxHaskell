wxHaskell
=========

wxWidgets wrapper for Haskell

This is
=======

This is a fork of jodonoghue/wxHaskell intended to combine/hold the various patches required to keep wxHaskell working.

Status
======

This fork builds & compiles
- against: wxWidgets (http://www.wxwidgets.org/) 2.9.5 released Jul 16, 2013
- with: Haskell platform (http://www.haskell.org/platform/) 2013.2.0.0, 64 bits
- on: MacOSX 10.8.4

There is no guarantee that this fork builds with other combinations.
This is partly because internally components (in particular wxdirect)
cannot deal with version variation done by means of (C) preprocessing.
OS and Haskell platform differences should be no problem if it was no problem before this fork.

Build & installation
====================

As of Jul 25, 2013:
- Prerequisites: install wxWidgets 2.9.5 directly from a source release or use a platform specific installer.
  MacOSX specific note: homebrew does not yet install 2.9.5 (by means of 'brew install wxmac').
- Build using cabal-dev from the wxHaskell directory:
    bin/mk
  Then use via the resulting package configuration cabal-dev/packages-7.6.3.conf (or any other compiler version)
