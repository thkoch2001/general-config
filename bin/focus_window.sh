#!/usr/bin/env bash

# by @picnoir@social.alternativebit.fr / https://alternativebit.fr/
# via https://social.alternativebit.fr/notice/AwHd6ZoJsdhE6dNEBc
# alternative:
# https://github.com/openlab-aux/vuizvui/blob/36f3d8041c95942ba7436d9760c94366e0ec1e90/modules/user/sternenseemann/profiles/desktop.nix#L43-L55

selectedWinId=$(\
  niri msg -j windows | \
  jq -r '.[] | (.id | tostring)   + ") " + .app_id + " | " + .title' |\
  fuzzel --dmenu --width 120 |\
  cut -f 1 -d ')' \
  )
niri msg action focus-window --id "$selectedWinId"
