if status is-interactive
    # Commands to run in interactive sessions can go here
    # starts the desktop environment
    if test -z "$DISPLAY" -a "$XDG_VTNR" = 1
        exec startx -- -keeptty
        # exec startplasma-wayland
    end
end
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin /home/aquabeam/.ghcup/bin $PATH # ghcup-env
