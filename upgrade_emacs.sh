#!/bin/bash

base_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

(cd "${base_dir}"
 git submodule foreach bash -c 'if [ -f Makefile ]; then make clean && make; fi'
)

OS=$(uname -s | tr '[:upper:]' '[:lower:]')
if [ "${OS}" != 'darwin' ]; then
    echo "the operating system ${OS} is not supported"; exit
fi

brew upgrade emacs || \
    brew install emacs \
	     --with-imagemagick \
	     --with-librsvg \
	     --with-cocoa

brew link --overwrite emacs
brew linkapps emacs
brew cleanup emacs

[ -f "${base_dir}/emacs.sh" ] && (
    rm -f /usr/local/bin/emacs
    cp "${base_dir}/emacs.sh" /usr/local/bin/emacs
    chmod +x /usr/local/bin/emacs
)
