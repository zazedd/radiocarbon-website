val run : Dream.handler -> (unit, [ `Msg of string ]) result Lwt.t
val handler : Dream.handler
