# my dots

These dotfiles are bootstrapped with
[dotbot](https://github.com/anishathalye/dotbot) and can be installed by cloning
this repo and running `./install` inside it. Much inspiration taken from
[here](https://github.com/nikitavoloboev/dotfiles).

Note: I've initialised [git-crypt](https://github.com/AGWA/git-crypt) in this
repo to encrypt all files with the word `secret` anywhere in their name.

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

## Phoenix

[Phoenix](https://github.com/kasper/phoenix) is great! It's a window-manager for
macOS which you can configure with JavaScript. In other words, it allows you to
bind keyboard shortcuts to functions which move windows around your screen(s). I
used [chunkwm](https://github.com/koekeishiya/chunkwm) for a long while, but
ended up moving away from it because I found myself fighting against its
automatic window placement.

## Fish

Nothing too out of the ordinary here. I adopted [fish](https://fishshell.com) as
my daily-driver because I was finding zsh too sluggish. I pretty rarely fire up
a terminal window, so I don't want to have to wait around for it to load when I
need it! Fish also provides a ton of features out of the box, leaving me with
very few tweaks to make in [my config](shell/fish/config.fish). As far
as I can see, its only downside is its
[POSIX-noncompliance](https://en.wikipedia.org/wiki/Friendly_interactive_shell#Syntax),
which has forced me to rewrite a few shell aliases provided by others at work.
Certainly not the end of the world.

## Honourable Mentions

[Duti](https://github.com/moretension/duti) is a ([currently
unmaintained](https://github.com/moretension/duti/pull/39#issuecomment-596996452))
tool for "setting default applications for various document types on macOS,
using Apple's Uniform Type Identifiers (UTI)". I can imagine this being
handy, but I don't currently have a use for it.
