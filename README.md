[xmonad_install_doc]: https://xmonad.org/INSTALL.html
[my_xmonad_build]: https://github.com/dburian/dotfiles/blob/master/.config/xmonad/build

# xmonad_config

This is my configuration of xmonad as a haskell cabal package.

## Install and compilation

To install this config clone it somewhere (e.g. `~/.local/src/`) and run
`./rebuild.sh`. This will install xmonad and xmonad-contrib.

To compile xmonad with this config you need to run:

```bash
cabal install exe:xmonad-x86_64-linux
```

## Integration with default xmonad config

To recompile xmonad with the default key binding `M-q`, one has to place a [build
script (See 'Custom Build Script')][xmonad_install_doc] such as [this
one][my_xmonad_build].
