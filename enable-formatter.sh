#!/bin/sh
set -eux

cd $HOME/.vscode/extensions/autodesk.autolispext-*
if [ "$?" = 0 ]; then
  sed -i -e "s/\"onLanguage:autolispdcl\"/\"onLanguage:racket\"/" package.json
  sed -i -e "s/\['autolisp', 'lisp'\]/['racket']/" out/format/formatProviders.js
  sed -i -e "s/\['autolisp', 'lisp'\]/['racket']/" out/format/autoIndent.js
fi
