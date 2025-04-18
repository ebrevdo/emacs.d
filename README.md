# ebrevdo@'s emacs.d

## Goals

* Avoid MELPA, which is considered less safe than gnu/nongnu ELPA.  Instead, some packages
  are added as submodules, and their versions fixed at a recent release tag.
* A useful dev environment with most of the bells and whistles and reasonable startup time
* Good contrast when operating in non-window mode (e.g., within a console).

## (subset of) Packages

* eglot + flywheel with instructions on setting up python-lsp-server, ruff, and black.
* magit for git
* paredit, markdown, org, pyvenv modes
* aibo (for ChatGPT)  (accessible via `C-M-/`)
* hl-fill-column (fill column indicator that doesn't mess with terminal copy/paste)
* copilot.el (enabled by default in all prog modes)
* vertico for fuzzy completion
* jupyter

## Setup instructions

The following setup is required on MacOS:

```sh
brew install autoconf automake libtool pkg-config zeromq gpg
pip install 'python-lsp-server[all]' python-lsp-black python-lsp-ruff jupyterlab
```


