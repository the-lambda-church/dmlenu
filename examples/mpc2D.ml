open Dmlenu

let (!!) x = Lazy.from_val (Engine.singleton x)

let engine = Engine.csum [
  "play", !! Dmlenu_extra.Sources.Mpc.current_playlist ;
  "load", !! Dmlenu_extra.Sources.Mpc.playlists ;
  "random", !! (Source.from_list_ ["on" ; "off"]) ;
]

let _ = App.run ~prompt:"mpc" ~layout:(State.Grid (10, None)) engine
