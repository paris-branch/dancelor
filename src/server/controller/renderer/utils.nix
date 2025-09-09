{ ... }:

let
  withArgumentType =
    name: type: cont: args:
    if !(type.check args) then
      throw "The value passed to `${name}` does not have the expected type."
    else
      let
        merged =
          type.merge
            [ ]
            [
              {
                value = args;
                file = "argument passed to ${name}";
              }
            ];
      in
      cont merged;

in
{
  inherit
    withArgumentType
    ;
}
