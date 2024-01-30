# Terminal and Shell Config

My terminal emulator and shell config files.

# Usage

## Nerd Font

- Install CaskaydiaCove nerdfont [https://www.nerdfonts.com/font-downloads](https://www.nerdfonts.com/font-downloads)

## Alacritty

- Install alacritty [https://alacritty.org/](https://alacritty.org/)

- Place `alacritty.toml` in one of the config locations.

    Windows: `%appdata%\alacritty\alacritty.toml`

    Linux: `$XDG_CONFIG_HOME/alacritty/alacritty.toml`, `$XDG_CONFIG_HOME/alacritty.toml`, `$HOME/.config/alacritty/alacritty.toml`, or `$HOME/.alacritty.toml`

- If you are not on windows or do not want to use clink, change the `shell:    program: cmd.exe /k clink inject` section of the config. 

    For example, replace `cmd.exe /k clink inject` with `powershell`, `bash`, or `zsh`. Using `cmd` is not recommended as you cannot use `Oh My Posh`.

## Oh My Posh

- Install Oh My Posh

    Windows: `winget install JanDeDobbeleer.OhMyPosh -s winget` or `Set-ExecutionPolicy Bypass -Scope Process -Force; Invoke-Expression ((New-Object System.Net.WebClient).DownloadString('https://ohmyposh.dev/install.ps1'))`

    *nix: `brew install jandedobbeleer/oh-my-posh/oh-my-posh`

- Place `agnoster.minimal_tokyonight_storm.omp.json` into the Oh My Posh themes directory.

    Windows: `%localappdata%\Programs\oh-my-posh\themes`

## Clink (windows only)

- Download clink from here [https://github.com/chrisant996/clink](https://github.com/chrisant996/clink). You may find an older, incompatible version of clink if you search for it yourself.

- Add the clink folder to PATH. By default the directory to add is `C:\Program Files (x86)\clink`.

- Add `oh-my-posh.lua` and `clink_settings` to `%localappdata%\clink`

- Optionally, add `clinkcmd.bat` to `C:\Program Files (x86)\clink`. This allows a clink injected cmd instance to start with the command `clinkcmd` while not conflicting with the existing `clink` command.

## Powershell (windows only)

- Add `Microsoft.Powershell_profile.ps1` to the profile directory. 

    Enter `$PROFILE` in powershell to get the profile directory. If the directory does not exist, create it.

- You may need to set the execution policy for powershell. To set it to unrestricted, use: `Set-ExecutionPolicy -ExecutionPolicy Unrestricted -Scope CurrentUser`.

## Other shells

- Follow the instuctions here to enable Oh My Posh and set a theme: [https://ohmyposh.dev/docs/installation/prompt](https://ohmyposh.dev/docs/installation/prompt)