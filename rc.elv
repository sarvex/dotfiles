use github.com/muesli/elvish-libs/git

use github.com/muesli/elvish-libs/theme/powerline
set edit:prompt-stale-transform = {|x| put $x }
set edit:rprompt-stale-transform = {|x| put $x }

use re
use readline-binding
use path
use str
use math
use epm

epm:install &silent-if-installed       ^
github.com/zzamboni/elvish-modules     ^
github.com/zzamboni/elvish-completions ^
github.com/zzamboni/elvish-themes      ^
github.com/xiaq/edit.elv               ^
github.com/muesli/elvish-libs          ^
github.com/iwoloschin/elvish-packages

set edit:insert:binding[Alt-Backspace] = $edit:kill-small-word-left~
set edit:insert:binding[Alt-d] = $edit:kill-small-word-right~
set edit:insert:binding[Alt-m] = $edit:-instant:start~

use github.com/zzamboni/elvish-modules/alias

use github.com/xiaq/edit.elv/smart-matcher
smart-matcher:apply

if (has-external carapace) {
  eval (carapace _carapace | slurp)
}

use github.com/zzamboni/elvish-modules/bang-bang

use github.com/zzamboni/elvish-modules/dir
alias:new cd &use=[github.com/zzamboni/elvish-modules/dir] dir:cd
alias:new cdb &use=[github.com/zzamboni/elvish-modules/dir] dir:cdb

set edit:insert:binding[Alt-i] = $dir:history-chooser~

set edit:insert:binding[Alt-b] = $dir:left-small-word-or-prev-dir~
set edit:insert:binding[Alt-f] = $dir:right-small-word-or-next-dir~

only-when-external exa {
  var exa-ls~ = { |@_args|
    use github.com/zzamboni/elvish-modules/util
    e:exa --color-scale --git --group-directories-first (each {|o|
        util:cond [
          { eq $o "-lrt" }  "-lsnew"
          { eq $o "-lrta" } "-alsnew"
          :else             $o
        ]
    } $_args)
  }
  edit:add-var ls~ $exa-ls~
}

use github.com/zzamboni/elvish-modules/terminal-title

var private-loaded = ?(use private)

use github.com/zzamboni/elvish-modules/spinners
use github.com/zzamboni/elvish-modules/tty

use github.com/zzamboni/elvish-modules/util-edit
util-edit:electric-delimiters

use github.com/zzamboni/elvish-modules/util-edit
util-edit:electric-delimiters

use github.com/iwoloschin/elvish-packages/update
set update:curl-timeout = 3
update:check-commit &verbose

use github.com/muesli/elvish-libs/git

use github.com/zzamboni/elvish-modules/util
