let hash ~password =
  let prf = `SHA256
  and salt = open_in "env/salt" |> input_line |> String.trim |> Cstruct.of_hex
  and password = Cstruct.of_string password
  and count = 100
  and dk_len = 64l in
  Pbkdf.pbkdf2 ~prf ~password ~salt ~count ~dk_len |> Cstruct.to_string
