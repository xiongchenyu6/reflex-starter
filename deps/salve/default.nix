{ nixpkgs ? import <nixpkgs> {}}:
  import ../github.nix { 
    inherit nixpkgs; 
    owner = "tfausak"; 
    repo = "salve"; 
    jsonFile = ./github.json;
  }

