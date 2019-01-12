#!/bin/bash
chmod +x mondrian.ml
alias mondrian=$PWD/mondrian.ml
echo "alias mondrian=$PWD/mondrian.ml" >> $HOME/.bashrc
exec bash
