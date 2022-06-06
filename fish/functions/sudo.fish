# Wrapper to make sudo preserve my environment (some tweaks to /etc/sudoers are needed
# alongside the -E option, see etc/install.sh), and also to make it work with my custom
# fish functions (see https://github.com/fish-shell/fish-shell/issues/4710, where
# I lifted the implementation from).
function sudo --wraps sudo
  # -i requires special handling -- it performs a login, so it can't be used alongside
  # -E. But you should probably get into the habit of using -s instead anyway, unless
  # you specifically need the login part.
  if string match -qr '^-i' -- $argv[1]
    command sudo $argv
  else
    if functions -q -- $argv[1]
      set argv fish -c "$argv"
    end
    command sudo -E $argv
  end
end
