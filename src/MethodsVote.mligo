#include "./TypesVote.mligo"

let setAdmin(admin, storage : address * storage) : return =
    let _check_if_no_tez : unit = assert_with_error (Tezos.amount = 0tez) "Ne pas envoyer de tez svp" in
    let is_admin = assert_with_error (Tezos.sender = storage.admin ) "Pas admin, vous êtes" in
    let new_admin = admin in
    (noOperations, {storage with admin = new_admin})

let createVote (question_, storage : string * storage) : return = 
    let _check_if_no_tez : unit = assert_with_error (Tezos.amount = 0tez) "Ne pas envoyer de tez svp" in
    let empty_voters_map : (address, (bool * timestamp)) big_map = Big_map.empty in
    let vote = {
        question = question_;
        didVoteUsers = empty_voters_map;
        yes = 0n;
        no = 0n;
        creator = Tezos.sender;
        lifetime = Tezos.now + 86_400;
    } in

    let votes_map = storage.votes in
    let new_votes_map = Big_map.add(storage.counter)(vote) votes_map in
    let new_counter = storage.counter + 1n in
    (noOperations, {storage with votes = new_votes_map; counter = new_counter})

// nat * bool = id du vote + la réponse
let vote(answer, storage : (nat * bool) * storage) : return =
    let _check_if_no_tez : unit = assert_with_error (Tezos.amount = 0tez) "Ne pas envoyer de tez svp" in
    let is_admin = assert_with_error (Tezos.sender <> storage.admin ) "Les admins ne votent pas" in

    // Destructuration du tuple
    let (idVote, voteAnswer) : (nat * bool) = answer in

    // On check si un vote à l'id donné existe
    let vote_opt : vote option = Big_map.find_opt(idVote) storage.votes in

    // On unpack l'option
    let new_vote : vote = match vote_opt with 
        | None -> (failwith("Vous essayez de voter sur un sondage qui n'existe pas"))
        // On incrémente les compteurs en fonction de si c'est oui ou non
        | Some(v) -> v in
    
    let check_if_already_voted : unit = match Tezos.sender with
        // On ajoute le vote de l'adresse dans le big didVoteUsers (address, (bool * timestamp))
        | None -> Big_map.add Tezos.sender ((voteAnswer, Tezos.now)) new_vote.didVoteUsers in
        | Some(v) -> (failwith("Vous avez déjà voté") : unit)

    let new_vote_added = 
        if voteAnswer = true
        then new_vote.yes = new_vote.yes + 1n
        else new_vote.no = new_vote.no + 1n in

    let new_votes : (nat, vote) big_map = Big_map.update idVote (new_vote_added) new_votes in
    (noOperations, { storage with votes = new_votes })



