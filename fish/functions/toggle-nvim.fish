function change-nvim-dir-suffix
    set -l from $argv[1]
    set -l to $argv[2]

    set -l dirs ~/.config ~/.local/share ~/.local/state ~/.cache

    for dir in $dirs
        echo >&2 mv $dir/nvim{$from,$to}
        mv $dir/nvim{$from,$to}
    end
end

function toggle-nvim
    set -l lazy_suffix .lazyvim
    set -l custom_suffix .custom

    if test -d ~/.config/nvim$custom_suffix
        echo >&2 "Disabling LazyVim Neovim config."
        change-nvim-dir-suffix "" $lazy_suffix
        echo >&2
        echo >&2 "Switching to custom Neovim config."
        change-nvim-dir-suffix $custom_suffix ""

    else
        echo >&2 "Disabling custom Neovim config."
        change-nvim-dir-suffix "" $custom_suffix
        echo >&2
        echo >&2 "Switching to LazyVim Neovim config."
        change-nvim-dir-suffix $lazy_suffix ""

    end
end
