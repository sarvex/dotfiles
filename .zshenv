export PATH="/usr/local/sbin:/usr/local/sbin:$PATH:$HOME/.rvm/bin:/usr/local/opt/go/libexec/bin:/opt/cask/factor/0.97/factor"
export MANPATH="/usr/local/man:$MANPATH"
fpath=(/usr/local/share/zsh-completions $fpath)

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='mvim'
fi

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/dsa_id"

export ANDROID_HOME=/usr/local/opt/android-sdk
export GROOVY_HOME=/usr/local/opt/groovy/libexec
export SCALA_HOME=/usr/local/opt/scala/idea
export HOMEBREW_BUILD_FROM_SOURCE=1
export HOMEBREW_GITHUB_API_TOKEN=ac53d9a40a814ec6cff19abddb964484a9c9ada5
export HOMEBREW_CASK_OPTS="--appdir=/Applications --caskroom=/opt/cask"
export GRAILS_HOME="/usr/local/opt/grails/libexec"
export GRIFFON_HOME="/usr/local/Cellar/griffon/1.5.0/libexec"
export GOPATH="$HOME/Go"
export STUDIO_JDK=/Library/Java/JavaVirtualMachines/jdk1.8.0_45.jdk
