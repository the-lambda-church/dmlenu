let (!!) x = Lazy.from_val (Engine.singleton x)

let engine = Engine.csum [
  "play", !! Extra_sources.Mpc.current_playlist ;
  "load", !! Extra_sources.Mpc.playlists ;
  "random", !! (Source.from_list_ ["on" ; "off"]) ;
]

let _ = Dmlenu.run ~prompt:"mpc" ~layout:(State.Grid (10, None)) engine
