open MParser

let between p from until =
  string from >> p << string until
