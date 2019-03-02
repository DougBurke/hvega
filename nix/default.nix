let
  fetch = package:
    let
      versions = builtins.fromJSON (builtins.readFile ./versions.json);
      spec = versions.${package};
      fetchTarball =
        # fetchTarball version that is compatible between all the versions of
        # Nix
        { url, sha256 }@attrs:
        let
          inherit (builtins) lessThan nixVersion fetchTarball;
        in
          if lessThan nixVersion "1.12" then
            fetchTarball { inherit url; }
          else
            fetchTarball attrs;
    in
      fetchTarball {
        url =
          with spec;
          "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
        sha256 = spec.sha256;
      };
in { nixpkgs ? fetch "nixpkgs" }: import nixpkgs {
  config = {};
  overlays =
    [
      (self: super:
        { haskellPackages =
            super.haskellPackages.extend
              (super.haskell.lib.packageSourceOverrides
                { hvega = self.lib.cleanSource ../hvega;
                  ihaskell-hvega = self.lib.cleanSource ../ihaskell-hvega;
                }
              );
        }
      )

      # can I over-ride zeromq4 because of its test failures
      # https://gitlab.com/twittner/zeromq-haskell/issues/63
      #
      (self: super:
        { haskellPackages =
	    super.haskellPackages.extend
	      (super.haskell.lib.packageSourceOverrides
	        {
		  zeromq4-haskell = self.fetchFromGitLab {
		    owner = "twittner";
		    repo = "zeromq-haskell";
		    domain = "gitlab.com";
		    rev = "3ba947586baf44dc8c2acc03bd81c3a8dd9bf18c";
		    sha256 = "05x74h8pniclgqak8pvlv5hjhm124xglri1xwc49lv43z3z1c4qh";
		  };
		}
	      );
	 }
      )

      (self: super:
        { ihaskellWithPackages = ps:
            self.callPackage
              "${self.path}/pkgs/development/tools/haskell/ihaskell/wrapper.nix"
              {
                ghcWithPackages = self.haskellPackages.ghcWithPackages;
                jupyter = self.python3.withPackages (ps: [ ps.jupyter ps.notebook ]);
                packages = ps;
	      };
        }
      )
    ];
  }
