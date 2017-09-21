# emacs.d

My emacs configuration files. This is a very stripped down version of prelude and was inspired on both [prelude](https://github.com/bbatsov/prelude) and [siren](https://github.com/jimeh/.emacs.d).

Normally when I setup a new system, I setup my [dotfiles](https://github.com/paulohefagundes/dotfiles), [emacs](https://github.com/paulohefagundes/.emacs.d),  and [firefox](https://github.com/paulohefagundes/user.js)

## installation

Personally I like to keep all of my source code in ~/git. So I'd put my emacs.d under `~/git/emacs.d`. If you use a different folder, make sure to clone to the correct folder.

### linux / os x

``` bash
git clone git@github.com:paulohefagundes/.emacs.d.git path/to/local/repo
ln -sf path/to/local/repo ~/.emacs.d
```

### windows

``` powershell
git clone git@github.com:paulohefagundes/.emacs.d.git path/to/local/repo
New-Item -ItemType SymbolicLink -Path "$env:APPDATA\.emacs.d" -Target path\to\local\repo
```

## post install

Normally, after installing, I open up emacs and update the packages

``` emacs-lisp
M-x list-packages
```

Then once in the *Packages* buffer, hit `U` followed by `x`
