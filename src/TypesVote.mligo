type vote = {
    question : string;
    didVoteUsers : (address, (bool * timestamp)) big_map;
    yes : nat;
    no : nat;
    creator : address;
    lifetime : timestamp;
}

type storage = {
    admin : address;
    votes : (nat, vote) big_map;
    counter : nat;
}

let noOperations : operation list = []
type return = operation list * storage

type entrypoints = 
SetAdmin of address
| Vote of string
| CreateVote of (nat * bool)
