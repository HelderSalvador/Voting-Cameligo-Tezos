#include "./MethodsVote.mligo"

let main (action, s : entrypoint * storage_farm) : return =
    match action with
    | SetAdmin(admin) -> setAdmin(admin, s)
    | Vote(value) -> vote(value, s)
    | CreateVote(value) -> createVote(value, s)