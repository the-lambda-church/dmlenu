let (!!) x = Lazy.from_val (Program.singleton x)

let engine = Program.csum [
  "play", !! Extra_sources.Mpc.current_playlist ;
  "load", !! Extra_sources.Mpc.playlists ;
  "random", !! (Source.from_list_ ["on" ; "off"]) ;
]

let _ = Dmlenu.run ~prompt:"mpc" ~layout:(State.Grid (10, None)) engine
