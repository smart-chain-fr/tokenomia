{ pkgs }:
pkgs.vscode-with-extensions.override {
  vscode = pkgs.vscodium;
  vscodeExtensions = [
    pkgs.vscode-extensions.bbenoist.nix
    pkgs.vscode-extensions.shardulm94.trailing-spaces
  ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace  [
    {
      name = "language-haskell";
      publisher = "justusadam";
      version = "3.6.0";
      sha256 = "sha256-rZXRzPmu7IYmyRWANtpJp3wp0r/RwB7eGHEJa7hBvoQ=";
    }
    {
      name = "haskell";
      publisher = "haskell";
      version = "2.2.2";
      sha256 = "sha256-zWdIVdz+kZg7KZQ7LeBCB4aB9wg8dUbkWfzGlM0Fq7Q=";
    }
  ];
}
