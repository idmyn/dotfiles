# inspo: https://github.com/muesli/dotfiles/blob/master/shell/rc.elv

paths = [/usr/local/bin $@paths]

# prompt
edit:prompt = { put "\n"; tilde-abbr $pwd; put "> " }
edit:rprompt = { put "" }

# keybindings
edit:insert:binding[Alt+Backspace]=$edit:kill-small-word-left~

# aliases
fn q { exit }
fn ls [@a]{ e:ls -GF $@a }
fn la [@a]{ ls -GFa $@a }
fn ll [@a]{ ls -GFalh $@a }
fn otp [@a]{ pass otp -c $@a }
