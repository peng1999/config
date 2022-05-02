{ config, pkgs, pkgsStable, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "pgw";
  home.homeDirectory = "/home/pgw";

  # Packages that should be installed to the user profile.
  home.packages = with pkgs; [
  ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";

  home.sessionVariables = {
    EDITOR = "nvim";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.fish.enable = true;

  programs.neovim = {
    enable = true;
    extraConfig = ''
      let s:init_lua = stdpath('config') . '/_init.lua'
      if filereadable(s:init_lua)
        exe 'source ' . s:init_lua
      endif
    '';
    extraPackages = with pkgs; [
      gcc
      # sumneko-lua-language-server
      rnix-lsp
    ] ++ [
      pkgsStable.sumneko-lua-language-server
    ];
  };

  programs.tmux = {
    enable = true;
    escapeTime = 50;
    keyMode = "vi";
    newSession = true;
    terminal = "tmux-256color";
    extraConfig = ''
      set -g mouse on

      set-option -ga terminal-overrides ",xterm-256color:RGB"

      bind % split-window -h -c "#{pane_current_path}"
      bind "\"" split-window -v -c "#{pane_current_path}"
    '';
  };

  programs.git = {
    enable = true;
    userEmail = "pg999w@gmail.com";
    userName = "Peng Guanwen";
    extraConfig = {
      pull.ff = "only";
      http."https://github.com".proxy = "http://ipads:ipads123@202.120.40.82:11235";
    };
  };

  programs.gh = {
    enable = true;
    settings = {
      git_protocol = "https";
      prompt = "enabled";
    };
  };
}
