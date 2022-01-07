# .config

## Programs to install
- [`fish`](https://github.com/fish-shell/fish-shell)
- [`starship`](https://github.com/starship/starship)
- [`emacs`](https://www.gnu.org/software/emacs)
- [`ripgrep`](https://github.com/BurntSushi/ripgrep)
- [`fd`](https://github.com/sharkdp/fd)

### Debian-based Installation

```
curl -sSL https://starship/install.sh | sh &&
sudo apt-add-repository ppa:fish-shell/release-3 &&
sudo apt update &&
sudo apt upgrade &&
sudo apt install -y fish emacs ripgrep fd-find git &&
mkdir -p ~/.config &&
cd ~/.config &&
git remote add origin https://github.com/dodgez/.config &&
git fetch &&
git checkout main
```

