# Git Setup Dotfiles

## Setting Up

assuming you use Z Shell. If not, change `.zshrc` to your specific shell's config file (e.g `.bashrc` for Bash, or `config/fish/config.fish` for fish shell)

```sh
git init --bare $HOME/.cfg
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
config config --local status.showUntrackedFiles no
echo "alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'" >> $HOME/.zshrc
```

example commands:

```sh
config status
config add .vimrc
config commit -m "feat: update .vimrc"
config push
```


## Importing

on a new system you can migrate to this setup. Prior installation, make sure you have committed the alias to your shell config file:

```sh
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
```

And that your source repository ignores the folder where you'll clone it, so that you don't create weird recursion problems:

```sh
echo ".cfg" >> .gitignore
```

Now clone your dotfiles into a bare repository in a "dot" folder of your `$HOME`:

```sh
git clone --bare <git-repo-url> $HOME/.cfg
```

Define the alias in the current shell scope:

```sh
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
```

Checkout the actual content from the bare repository to your `${HOME}`:

```sh
config checkout
```


The step above might fail with a message like:

```sh
error: The following untracked working tree files would be overwritten by checkout:
    .bashrc
    .gitignore
Please move or remove them before you can switch branches.
Aborting
```

This is because your `$HOME` folder might already have some stock configuration files which would be overwritten by Git. The solution is simple, back up the files if you care about them, remove them if you don't care. I provide you with a possible rough shortcut to move all the offending files automatically to a backup folder:

```sh
mkdir -p .config-backup && \
config checkout 2>&1 | egrep "\s+\." | awk {'print $1'} | \
xargs -I{} mv {} .config-backup/{}
```

Rerun the init:

```sh
config checkout
config config --local status.showUntrackedFiles no
```

Now, you're ready to come `${HOME}`  🏠
