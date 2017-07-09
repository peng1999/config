# config
My configs, and a setup shell, which is aimed to quickly and easily setup my working environment.

## installation
Use git to clone this repository, then run `./setup`:

```bash
git clone https://github.com/peng1999/config.git --recursive
./setup <functions>
```

Currently supported functions:
- vim
    - [x] `vimrc`  
        Add `~/tmp/vimundo` and `~/tmp/vimbackup` folder
    - [x] `vim_plugin`
- zsh
    - [x] `oh_my_zsh`
    - [x] `zshrc`
- fish
    - [x] `config.fish`
    - [x] `fisher`
    - [ ] `z`
    - [ ] `fzf`
- rust
    - [ ] `rustup`
        - [ ] `rustup_completions`
    - [ ] ?mirrior
- git
    - [x] `git_config`  
        cache credential
