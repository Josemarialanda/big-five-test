# Bellroy take home test

## Requirements
* [Nix](https://nixos.org/download.html)
* [Nix Flakes](https://nixos.wiki/wiki/Flakes)

## Usage

### Build:
```bash
$ nix build
```
### Help
```bash
$ nix run .#exe -- --help
```

### Run and print results:
```bash
$ nix run .#exe -- "res/big5test.txt" "Jose Maria Landa Chavez" "josemaria.landa@gmail.com"
```

### Run and upload results:
```bash
$ nix run .#exe -- --upload "res/big5test.txt" "Jose Maria Landa Chavez" "josemaria.landa@gmail.com"
```