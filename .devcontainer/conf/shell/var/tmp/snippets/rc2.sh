
# set PATH so it includes cabal's bin if it exists
if [ -d "$HOME/.cabal/bin" ] && [[ "$PATH" != *"$HOME/.cabal/bin"* ]] ; then
    PATH="$HOME/.cabal/bin:$PATH"
fi
