# YADRLite #

Credit:
---
This dotfile repo is heavily based on [YADR] (https://github.com/skwp/dotfiles) which is a really solid dotfile package created by Yan Pritzker and worked on by several others.  This is not intended to be a fork of YADR.  At the same time, I take no credit here either.  This is just a personal collection of settings from YADR trimmed down considerably for the sake of speed and portability.  

Check out the original repo, [YADR] (https://github.com/skwp/dotfiles) especially if you work in Python or Ruby.


Installation:
---

```bash
bash -c "`curl -fsSL https://raw.githubusercontent.com/bridgesense/dotfiles/master/setup`"
```

###Update:

```bash
bash ~/.yadrlite/setup update
```

###Uninstall and Restore to Prior Configuration:

```bash
bash ~/.yadrlite/setup remove
```


What's Included:
---
Shortcuts have been pulled over from YADR, so those working with YADR should feel right at home.  There are a few minor changes and additions.  A LOT may be missing.  This package will be gearing towards PHP development on local staging environments and setting up shop on web servers for emergencies where there may be limited permission.  The idea is here that the package should be easy to remove while restoring the system to prior user configurations.

As with YADR, most of the key mapping can be found in the settings directory for reference.  More information may come later in offering examples on how to use YADRLite with [Xdebug] (https://github.com/bridgesense/Quick-Scotchbox-Install#vim--vdebug-settings-) and a few other tools.  This package is ready for deployment on MacOS and Linux distros. 


Emacs:
---
Emacs has a huge performance advantage over Vi.  This is a custom brewed configuration of Emacs with influences from YADR, complete with familiar Vim keybindings.  Additionally, there is inline documentation for all keybindings.  Just hit the leader key "," to get started.  You might install an up-to-date GUI version of Emacs to take advantage of all the features.  However, this setup file is also compatible with the terminal version as well.

    
###To Install the Emacs Config Only:
    
```bash
curl https://raw.githubusercontent.com/bridgesense/dotfiles/master/emacs.init > ~/.emacs    
```
