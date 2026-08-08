

# .emacs.d

Personal Emacs configuration dotfiles featuring custom yasnippets for TypeScript mode.

## Description
This repository houses a minimal Emacs setup along with specialized code snippets. The included TypeScript snippets automate boilerplate generation for the MooTools `Request` class, specifically `Request.HTML` and `Request.JSON`.

## Installation
Clone the repository into your home directory:
```bash
git clone https://github.com/zhenwenc/.emacs.d.git ~/.emacs.d
```
For advanced users, consider using a symlink manager like [GNU Stow](https://www.gnu.org/software/stow/) or integrate the files into your existing Emacs distribution (e.g., Doom Emacs, Spacemacs, or vanilla Emacs with `use-package`).

## Usage
- **Snippet Expansion:** Open a `.ts` file in `typescript-mode`, type `req.html` or `req.json`, and press the snippet trigger key (default is `TAB` or `M-i`) to insert the corresponding `Request` boilerplate.
- **Customization:** Edit your `init.el` or `custom.el` files to adjust Emacs behavior. Snippets are stored in `snippets/` and automatically load via `yasnippet`.

## Repository Structure
```text
snippets/
└── typescript-mode/
    ├── req.html    # MooTools Request.HTML snippet
    └── req.json    # MooTools Request.JSON snippet
```
