function docker-nuke
  docker system df
  read -lP "\
------------------------------------------------------------------------
This will reclaim all disk space used by Docker images and containers.
You will have to re-download/re-build them all. If you're unsure, you
might want to run `type docker-nuke` instead to see the steps and run
them manually, inspecting the effect with `docker system df`. In
particular, you might want to leave images alone.

Proceed? [y/N]" proceed
  switch $proceed
    case "" N n
      return
  end
  docker stop (docker ps -aq)
  docker rm (docker ps -aq)
  docker image prune -af
  docker system prune --volumes -f
  docker system df
end
