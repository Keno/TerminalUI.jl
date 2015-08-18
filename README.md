# TerminalUI

[![Build Status](https://travis-ci.org/Keno/TerminalUI.jl.svg?branch=master)](https://travis-ci.org/Keno/TerminalUI.jl)

Create beautiful GUIs for your Terminal. WARNING: This package is pre-alpha and
still under active development. Do not expect it to be stable. This README
gives a high level overview of what's available and where to look for the code.
More full documentation will follow as TerminalUI matures.

Traditionally, terminal toolkits generally occupied one of two categories. They
either provide the ability to write full-screen applications that take control
of the terminal, providing the full range of interactivity or the are seen as
prompt toolkits, allowing you to write more fully functional prompts, but are
generally intended to still be used within the REPL. TerminalUI attempts to
provide both and as such can be used both standalone and in the REPL.

The following is a highlevel description of what files are currently available
in the package:

- src/dialog.jl - Contains the functionality to create self contained dialogs or
 applications, either from the prompt or standalone. A widget that is created at
 the REPL will automatically be wrapped in a dialog and displayed inline to
 the user.

- src/foucs.jl - Dealing with focus and mouse input - Still very much WIP

- src/widgets.jl - The different widgets currently available for TerminalUI

- src/inputs.jl - More easily create input widgets for various types

- src/render.jl - The core rendering logic for making sure things get written
  to the underlying terminal efficiently.

- src/TerminalUI.jl - Everything else (including things that haven't been moved
  to their appropriate places yet)

Currently this package has only been tested using iTerm, and other terminal
emulators may not work. I'm happy to accept pull request, but will not make a
special effort to support other terminal emulators out there until this package
is a little more stable. The examples folder has a number of usage examples.
