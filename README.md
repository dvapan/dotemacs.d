# Emacs config

## Bootstrap on a new machine

1. Install system tools (paths/packages depend on distro):

   ```
   # Void Linux
   xbps-install -S clang-tools-extra gcc git

   # Debian/Ubuntu
   apt install clangd gcc git

   # Arch
   pacman -S clang gcc git
   ```

   - `clangd` - LSP server for C/C++
   - `gcc` - needed at first start to build tree-sitter grammars

2. Clone:

   ```
   git clone <repo> ~/.emacs.d
   ```

3. Start Emacs. On first launch it will:
   - refresh ELPA archives and install all `use-package :ensure t` packages
   - clone and compile tree-sitter grammars for `c` and `cpp` into `~/.emacs.d/tree-sitter/`

   Both steps need internet. The second start is offline-clean.

## C/C++ workflow

Per project, generate `compile_commands.json` so clangd knows compile flags:

- **CMake**: `cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -B build` then
  `ln -s build/compile_commands.json .`
- **Make**: `bear -- make` (requires `bear` package).

Open any `.c`/`.cpp`/`.h` file - mode line should show `c-ts-mode` or
`c++-ts-mode` and `[EGLOT(clangd)]` within a second.

Keys:

| Binding   | Action                  |
|-----------|-------------------------|
| `M-.`     | jump to definition      |
| `M-,`     | jump back               |
| `M-?`     | find references         |
| `C-c l r` | rename symbol           |
| `C-c l a` | code actions            |
| `C-c l f` | format buffer (clangd)  |
| `C-c c`   | `M-x compile`           |
| `C-c p`   | projectile command map  |
