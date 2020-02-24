function pbc --description 'Copy from pasteboard'
  if type -q pbcopy
    pbcopy
  else if set -q WAYLAND_DISPLAY; and type -q wl-copy
    wl-copy
  else if type -q xsel
    xsel --clipboard --input
  else if type -q xclip
    xclip -selection clipboard -in
  else
    echo >&2 'No pasteboard copy command found'
  end
end
