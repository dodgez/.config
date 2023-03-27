if [ -f "/etc/arch-release" ]; then
  sudo pacman -Sy fish git-delta starship
else
  sudo apt install fish git-delta && curl -sS https://starship.rs/install.sh | sh
fi

chsh -s /usr/bin/fish
