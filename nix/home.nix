{ config, lib, pkgs, pkgsStable, ... }:

with builtins;
let
  inherit (lib) mkIf;
  inherit (lib.trivial) warnIf;
  nixProjectPath = "${config.home.homeDirectory}/.dotfiles/nix";
  # Add addToHomePacakges.<name> to home.packages
  mergeAdditionalHomePackages = config:
    removeAttrs config [ "addToHomePacakges" ]
    // {
      home.packages =
        config.home.packages ++ concatLists (attrValues config.addToHomePacakges);
    };
  # Get value in credentials.nix if exists
  optionalGetCredential = key:
    let
      credentialsPath = "${nixProjectPath}/credentials.nix";
      credentials = import credentialsPath;
      value = getAttr key credentials;

      testPath = pathExists credentialsPath;
      testKey = hasAttr key credentials;
      enable =
        warnIf (!testPath) "can not find ${credentialsPath}, try add --impure flag" testPath
        && warnIf (!testKey) "`credential.${key}` not exists" testKey;
    in
    mkIf enable value;
in
mergeAdditionalHomePackages {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "pgw";
  home.homeDirectory = "/home/pgw";

  # Packages that should be installed to the user profile.
  home.packages = with pkgs; [
    lsof
    ripgrep
    python3
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

  addToHomePacakges.fish = [ pkgs.any-nix-shell ];
  programs.fish = {
    enable = true;
    # interactiveShellInit = ''
    #   any-nix-shell fish --info-right | source
    # '';
  };

  xdg.configFile."nvim/_init.lua".source = config.lib.file.mkOutOfStoreSymlink "${nixProjectPath}/../vimrc/init.lua";
  xdg.configFile."nvim/lua".source = config.lib.file.mkOutOfStoreSymlink "${nixProjectPath}/../vimrc/lua/";
  programs.neovim = {
    enable = true;
    extraConfig = ''
      let s:init_lua = stdpath('config') . '/_init.lua'
      if filereadable(s:init_lua)
        exe 'source ' . s:init_lua
      endif
    '';
    extraPackages = with pkgs; [
      # for tree-sitter to compile
      gcc
      # for lua lsp
      sumneko-lua-language-server
      # for nix lsp
      rnix-lsp
      # for python lsp
      nodePackages.pyright
      # for copilot
      nodejs
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
      http."https://github.com".proxy = optionalGetCredential "httpProxy";
      credential.helper = "cache --timeout=86400";
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
