let
  spec = builtins.fromJSON (builtins.readFile ./src.json);
  fetchTarball =
    # fetchTarball version that is compatible between all the versions of Nix
    { url, sha256 }@attrs:
    let
      inherit (builtins) lessThan nixVersion fetchTarball;
    in
    if lessThan nixVersion "1.12" then
      fetchTarball { inherit url; }
    else
      fetchTarball attrs;
  pkgs = fetchTarball {
    url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
    sha256 = spec.sha256;
  };
in
  import pkgs
