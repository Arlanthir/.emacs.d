# My Emacs configuration

## Major customizations

- Appearance changes
- Save open files between sessions
- Backup and auto-save inside .emacs.d
- Add MELPA packages for web development
- Add regular shortcuts (less intrusive CUA)
- Add numerous other shortcuts

## TODO

- Include SC-Emacs without overwriting theme colors
- Test ac-js2 (autocomplete for javascript)
- Draw smarter whitespace: tabs and spaces before first word; trailing whitespace
- Move tabs with mouse (seems complicated)
  - Improve the keyboard shortcut when we want to move the last tab to the beginning of the list
- Try out new theme https://melpa.org/#/zenburn-theme or https://melpa.org/#/hc-zenburn-theme

## Instructions

Comment out SISCOG Configuration unless you actually work there.

To open files in Windows:
- Set Environment variable ALTERNATE_EDITOR=C:\path\to\emacs-XY.Z\bin\runemacs
- Also set files to open with "\"C:\\path\\to\\emacs-XY.Z\\bin\\emacsclientw.exe\" -n \"%1\"":
  1. Use the Windows "open with" dialog
  2. Use a .reg/regedit to change relevant registry entries and add the -n
