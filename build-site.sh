#!/bin/sh
emacs --batch -l publish.el --eval '(publish-site "docs" t)'
