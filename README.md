# my dots

These dotfiles are bootstrapped with
[dotbot](https://github.com/anishathalye/dotbot) and can be installed by cloning
this repo and running `./install` inside it. Much inspiration taken from
[here](https://github.com/nikitavoloboev/dotfiles).

Note: I've initialised [git-crypt](https://github.com/AGWA/git-crypt) in this
repo to encrypt all files with the word `secret` anywhere in their name.

## Emacs

My current emacs setup consists of a base emacs install of [homebrew's
emacs-plus](https://github.com/idmyn/dotfiles/blob/master/macOS/Brewfile#L1-L2)
configured with my
[literate](https://en.wikipedia.org/wiki/Literate_programming)
[init.org](https://github.com/idmyn/dotfiles/blob/master/emacs/vanilla/init.org)
file. Emacs still looks for `~/.emacs.d/init.el` at startup, though, so I use
that file to instruct emacs to configure itself with the elisp code-blocks in
`~/.emacs.d/init.org`. My config currently takes around 4-5 (painfully slow)
seconds to load, which I mitigate by [running emacs as a
daemon](https://www.emacswiki.org/emacs/EmacsAsDaemon) (I followed [handy
macOS-specific instructions for
this](https://web.archive.org/web/20190407092503/https://east.fm/posts/emacs-26-and-macos-mojave/index.html)),
but I'm [working on](https://github.com/idmyn/dotfiles/tree/master/emacs/doom)
porting my config to [doom](https://github.com/hlissner/doom-emacs) to speed
things up. Managing multiple emacs configs is surprisingly simple with
[Chemacs](https://github.com/plexus/chemacs) :)

**EDIT: I've been using doom full-time for a while now and I love it!**

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

## Finicky

My [Finicky](https://github.com/johnste/finicky)
[config](finicky/.finicky.js) changes my 'default browser' based on
which browsers are currently open. Safari is my fallback because it's nice and
smooth, but if I have Brave or Firefox open then I want links to open in those.

## Honourable Mentions

[Duti](https://github.com/moretension/duti) is a ([currently
unmaintained](https://github.com/moretension/duti/pull/39#issuecomment-596996452))
tool for "setting default applications for various document types on macOS,
using Apple's Uniform Type Identifiers (UTI)". I can imagine this being
handy, but I don't currently have a use for it.
