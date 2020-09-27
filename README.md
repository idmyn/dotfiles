# my dots

These dotfiles are bootstrapped with
[dotbot](https://github.com/anishathalye/dotbot) and can be installed by cloning
this repo and running `./install` inside it. Much inspiration taken from
[here](https://github.com/nikitavoloboev/dotfiles).

Note: I've initialised [git-crypt](https://github.com/AGWA/git-crypt) in this
repo to encrypt all files with the word `secret` anywhere in their name.

## Backups

Backups are important. You can find my backup script in this repo, and you can
read more of [my thoughts about
backups](https://notes.davidmyno.rs/backups.html) in [my public
notes](https://sr.ht/~idmyn/notes.davidmyno.rs/).

## Nix

[Nix](https://nixos.org/) is a purely functional package manager which I don't
yet totally understand. You're welcome to read [my
notes on it](https://notes.davidmyno.rs/nix.html) for more info/resources/tips,
but for the purposes of this Readme you just need to know that I have [a nix
config for macOS](nix/darwin-configuration.nix) which instructs
[nix-darwin](https://github.com/LnL7/nix-darwin) to setup/install the
nix-related programs like [lorri](https://github.com/target/lorri) which I want
available system-wide.

## Emacs

My current emacs setup consists of a base emacs install of [homebrew's
emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus)
and my [doom](https://github.com/hlissner/doom-emacs)-based
[literate](https://en.wikipedia.org/wiki/Literate_programming)
[config](emacs/.doom.d/config.org). Doom is great because it provides a large
collection of really thoughtfully pieced-together 'modules' for different
tasks/tools, allowing you to get up and running really quickly.
The draw of Emacs for me in general is that all of the
tools I need for editing text are tightly integrated but remain highly
configurable. I'm not immune to the allure of the '[unix
philosophy](https://en.wikipedia.org/wiki/Unix_philosophy)' though, so
I do keep an eye on interesting simpler editors, like
[Kakoune](https://github.com/mawww/kakoune).

## Kitty

[Kitty](https://sw.kovidgoyal.net/kitty/) is my current terminal emulator of
choice, because it supports tabs and has file-based config. Someday I might
[learn tmux](https://www.ocf.berkeley.edu/~ckuehl/tmux/) and switch to
[Alacritty](https://github.com/alacritty/alacritty)...

## Karabiner/Goku

[Karabiner](https://karabiner-elements.pqrs.org/) is amazing, and
[Goku](https://github.com/yqrashawn/GokuRakuJoudo) is makes it much simpler to
configure. [My config](macOS/karabiner/karabiner.edn) isn't the tidiest, but it
works! Basically, I remap caps-lock to control when held and escape when
pressed, and I remap my right command key to f13 when pressed (to launch Alfred)
and ctrl+opt when held (mostly to foreground/launch applications with Phoenix).
I also remap comma to a "[dead key](https://en.wikipedia.org/wiki/Dead_key)"
inspired by the implementation in the [workman
layout](https://workmanlayout.org/).

## Phoenix

[Phoenix](https://github.com/kasper/phoenix) is great! It's a window-manager for
macOS which you can configure with JavaScript. In other words, it allows you to
bind keyboard shortcuts to functions which move windows around your screen(s). I
used [chunkwm](https://github.com/koekeishiya/chunkwm) for a long while, but
ended up moving away from it because I found myself fighting against its
automatic window placement. I also use Phoenix to foreground/launch apps with
keyboard shortcuts.

## Elvish

I'm currently [using](shell/elvish/rc.elv) [Elvish](https://elv.sh/) as my
daily-driver shell. Before I discovered Elvish I used
[fish](http://fishshell.com/), which drew me in as a fast shell with built-in
features that would require plugins in zsh or bash, slowing down their startup
time. 

Elvish replaced fish principally because its scripting language seems much
easier to learn, but I also quite like its directory history feature (like
[fzf](https://github.com/junegunn/fzf), but built-in) and [package
manager](https://elv.sh/ref/epm.html).

## Honourable Mentions

[Duti](https://github.com/moretension/duti) is a ([currently
unmaintained](https://github.com/moretension/duti/pull/39#issuecomment-596996452))
tool for "setting default applications for various document types on macOS,
using Apple's Uniform Type Identifiers (UTI)". I can imagine this being
handy, but I don't currently have a use for it.
