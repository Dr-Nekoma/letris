{
  description = "Letris configuration";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        {
          devShells.default = pkgs.mkShell {
            buildInputs = [
              pkgs.sbcl
              pkgs.xorg.libX11
              pkgs.xorg.libX11.dev
              pkgs.libGL
              pkgs.glfw
              pkgs.libevdev
            ];
            shellHook = ''
                export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath([pkgs.xorg.libX11])}:${pkgs.lib.makeLibraryPath([pkgs.xorg.libX11.dev])}:${pkgs.lib.makeLibraryPath([pkgs.libGL])}:${pkgs.lib.makeLibraryPath([pkgs.glfw])}:${pkgs.lib.makeLibraryPath([pkgs.libevdev])}
            '';      
          };
        }
      );
}
