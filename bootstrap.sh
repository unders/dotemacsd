echo ""
echo "Bootstrap of dotemacsd starts."
echo ""

command_exists () {
  type "$1" &> /dev/null;
}

if command_exists emacs
then
  emacs --version &> /dev/null
else
  echo "You must install emacs first"
  echo "On OS X, install emacs with Homebrew:"
  echo "brew install emacs --use-git-head --cocoa --srgb"
  echo "or try:"
  echo "brew install bazaar"
  echo "brew install emacs --HEAD --cocoa --srgb"
  exit 1
fi

if [[ -d "$HOME/.emacs.d" ]]
then
  echo "To install dotemacsd, you must remove:"
  echo "$HOME/.emacs.d"
  exit 1
fi

cwd=$(pwd)
ln -s $cwd ~/.emacs.d

if command_exists cask
then
  cask install
else
  echo "Installing Cask"
  export PATH=$HOME/.cask/bin:$PATH
  curl -fsSkL https://raw.github.com/cask/cask/master/go | python
fi

echo ""
echo "dotemacsd is linked to ~/.emacs.d"
echo "Cask dependency manager is installed"
echo "Boostrap of dotemacsd is done."

if command_exists cask
then
 echo ""
else
  echo ""
  echo "Do't forget to add Cask's bin to your PATH:"
  echo 'export PATH=$HOME/.cask/bin:$PATH'
fi
