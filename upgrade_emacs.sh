#!/bin/bash

base_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

(cd "${base_dir}"
 git submodule foreach 'git checkout master ; git pull; if [ -f Makefile ]; then make clean && make; fi'
)

function update_darwin() {
    brew upgrade emacs || \
        brew install emacs \
	         --with-imagemagick \
	         --with-librsvg \
	         --with-cocoa

    brew link --overwrite emacs
    brew linkapps emacs
    brew cleanup emacs

    if [ -f "${base_dir}/emacs.sh" ]; then
        rm -f /usr/local/bin/emacs
        cp "${base_dir}/emacs.sh" /usr/local/bin/emacs
        chmod +x /usr/local/bin/emacs
    fi
}

function update_linux() {
    echo "Nothing to do at the moment"
    return 0
}

os_name=$(uname -s | tr '[:upper:]' '[:lower:]')
case "${os_name}" in 
    linux) update_darwin 
           ;;
    darwin) update_linux 
            ;;
    \?)
        echo "the operating system ${os_name} is not supported"; exit
        ;;
esac
