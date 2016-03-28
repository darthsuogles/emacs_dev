#!/bin/bash

base_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

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
