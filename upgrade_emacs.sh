#!/bin/bash

base_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

(cd "${base_dir}"
 git submodule foreach 'git checkout master ; git pull; if [ -f Makefile ]; then make clean && make; fi'
)

os_name=$(uname -s | tr '[:upper:]' '[:lower:]')
os_emacs_exec="${base_dir}/emacs.${os_name}"

function update_darwin() {
    # brew upgrade emacs || \
    #     brew install emacs \
	#          --with-imagemagick \
	#          --with-librsvg \
	#          --with-cocoa

    # brew link --overwrite emacs
    # brew linkapps emacs
    # brew cleanup emacs
    # Cask has better support for cocoa
    brew cask install emacs

    if [ -f "${os_emacs_exec}" ]; then
        rm -f /usr/local/bin/emacs
        cp "${os_emacs_exec}" /usr/local/bin/emacs
        chmod +x /usr/local/bin/emacs
    fi
}

function update_linux() {
    echo "Nothing to do at the moment"
    return 0
}

case "${os_name}" in 
    linux) update_darwin 
           ;;
    darwin) update_linux 
            ;;
    \?)
        echo "the operating system ${os_name} is not supported"; exit
        ;;
esac
