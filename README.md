# .config

## Installation

### Pre-requisites

#### Programs used
- [`fish`](https://github.com/fish-shell/fish-shell)
- [`starship`](https://github.com/starship/starship)
- [`emacs`](https://www.gnu.org/software/emacs)
- [`ripgrep`](https://github.com/BurntSushi/ripgrep)
- [`fd`](https://github.com/sharkdp/fd)

#### Debian-based

```
sudo apt-add-repository ppa:fish-shell/release-3 &&
sudo apt-add-repository ppa:kelleyk/emacs &&
sudo apt update &&
sudo apt install -y fish emacs27 ripgrep fd-find git &&
curl -sSL https://starship.rs/install.sh | sh
```

#### Arch-based

```
sudo pacman -Sy emacs fish ripgrep fd git &&
curl -sSL https://starship.rs/install.sh | sh
```

### Configuration

```
mkdir -p ~/.config &&
cd ~/.config &&
git init &&
git remote add origin https://github.com/dodgez/.config &&
git fetch &&
git checkout main &&
chsh -s /usr/bin/fish
```
