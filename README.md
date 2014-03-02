### Installing
1.Clone this directory:

    git clone git://github.com/unders/dotemacsd.git path/to/local/repo

2.cd dotemacsd and run bootstrap:
    ./bootstrap.sh

3.Finally: Add the Cask dependency manager to your $PATH:
    $ export PATH="$HOME/.cask/bin:$PATH"

### Running Emacs


### Commands


### Plugins

#### Magit
To use Magit, run `M-x magit-status`, or type:

    C-x g

this opens a buffer that summarizes its
status. Close the status buffer with `q`.

To get help inside the status buffer, type `C-h m`

Here is an article that describes Magit: [Introduction to magit emacs mode](http://www.masteringemacs.org/articles/2013/12/06/introduction-magit-emacs-mode-git)


#### Evil
Emulates VIM.

### Evil-leader, and ace-jump
Leader key is set to `","` and
    * Leader u undo-tree-visualize
    * Leader e eshell
    * Leader b switch-to-buffer

### Ace Jump Mode
In Normal mode or Visual mode hit `SPC` to activate `ace-jump-word-mode`

### inf-ruby
    * C-c r r projectile-rails-console - Run rails console command in inf-ruby buffer.
    * C-c C-r ruby-send-region
    * C-c C-s inf-ruby
    * C-x C-e ruby-send-last-sexp

### Robe
    * C-c C-d robe-doc
    * C-c C-k robe-rails-refresh
    * M-, pop-tag-mark
    * M-. robe-jump (only works in insert-mode)

### Packages
List available packages:

    M-x package-list-packages

it shows a list of packages.
Mark the packages you want to install with `i` and
the ones you want to remove with `d`. Press `x` to
execute the scheduled actions.


### Links
 * [Homebrew - The missing package manager for OS X](http://brew.sh/)
 * [Cask - A dependency management tool](http://cask.github.io/)
 * [Pallet - A package management tool](https://github.com/rdallasgray/pallet)
 * [emacs 24 use homebrew instead of emacsformacosx](http://struct.tumblr.com/post/46754394733/emacs-24-use-homebrew-instead-of-emacsformacosx)
 * [Magit - An Emacs mode for Git](https://github.com/magit/magit)
 * [Evil](http://www.emacswiki.org/emacs/Evil)
 * [Evil-leader](https://github.com/cofi/evil-leader)
 * [Ace Jump Mode](https://github.com/winterTTr/ace-jump-mode)
 * [Projectile](http://batsov.com/projectile)
 * [Projectile Rails](https://github.com/asok/projectile-rails)
