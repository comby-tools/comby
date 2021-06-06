include Types.External

let default_external =
  fun ~name:_ ~filepath:_ ~line:_ ~column:_ -> None

let default =
  let module External = struct
    let handler = default_external
  end
  in
  (module External : Types.External.S)

module Default = (val default)
