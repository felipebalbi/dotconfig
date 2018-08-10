#!/usr/bin/env sh

DEBIAN_PACKAGES="		\
		build-essential	\
		emacs		\
		git		\
		gnupg2		\
		i3		\
		i3blocks	\
		i3lock-fancy	\
		libncurses5-dev	\
		libpng-dev	\
		libpoppler-glib-dev \
		libpoppler-private-dev \
		lilyterm	\
		rofi		\
		ruby		\
		scrot		\
		sudo		\
		xpdf		\
		xserver-xorg	\
		zlib1g-dev	\
		zsh		\
		"

REPOSITORIES="https://github.com/felipebalbi/usb-tools.git	\
              https://github.com/felipebalbi/emacs.d.git	\
              https://github.com/felipebalbi/dotconfig.git"

install_debian_packages () {
  su -c "apt-get install -q -y $DEBIAN_PACKAGES && adduser balbi sudo"
}

install_oh_my_zsh () {
  sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
}

install_ssh_dot_file () {
  mkdir -p ~/.ssh
  curl -fsSL https://raw.githubusercontent.com/felipebalbi/dotconfig/master/ssh/config \
       > ~/.ssh/config
}

clone_repositories () {
  mkdir -p ~/workspace
  pushd ~/workspace

  for repo in $REPOSITORIES; do
    git clone $repo
  done

  popd
}

initialize_dotconfig () {
  pushd ~/workspace/dotconfig
  git submodule update --recursive --remote --init
  popd
}

install_fonts () {
  pushd ~/workspace/dotconfig/fonts
  ./install.sh
  popd
}

install_remaining_dot_files () {
  ln -s ~/workspace/dotconfig/emacs.d ~/.emacs.d
  ln -s ~/workspace/dotconfig/git/gitconfig ~/.gitconfig
  ln -s ~/workspace/dotconfig/i3/ ~/.config/i3
  ln -s ~/workspace/dotconfig/xfce4/ ~/.config/xfce4/
  ln -s ~/workspace/dotconfig/i3blocks/i3blocks.conf ~/.i3blocks.conf
  ln -s ~/workspace/dotconfig/lilyterm ~/.config/lilyterm
  ln -s ~/workspace/dotconfig/rofi ~/.config/rofi
  ln -s ~/workspace/dotconfig/zsh/zshrc ~/.zshrc
}

if [[ -a ~/.system_setup_done ]]; then
  echo "System already setup"
  exit 1
fi

echo "Setting system up"

install_sudo
install_debian_packages
install_oh_my_zsh
install_ssh_dot_file
clone_repositories
initialize_dotconfig
install_fonts
install_remaining_dot_files

echo "Almost done, please visit https://www.google.com/chrome"

touch ~/.system_setup_done
