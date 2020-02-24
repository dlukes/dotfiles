function pbp --description 'Paste from pasteboard'
  if type -q pbpaste
    pbpaste
  else if set -q WAYLAND_DISPLAY; and type -q wl-paste
    wl-paste
  else if type -q xsel
    xsel --clipboard --output
  else if type -q xclip
    xclip -selection clipboard -out
  else
    echo >&2 'No pasteboard paste command found'
  end
end
