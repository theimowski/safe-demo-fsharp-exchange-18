open System.IO
open System.Net

open Suave
open Suave.Operators

open Fable.Remoting.Suave

open Shared

let clientPath = Path.Combine("..","Client") |> Path.GetFullPath 
let port = 8085us

let config =
  { defaultConfig with 
      homeFolder = Some clientPath
      bindings = [ HttpBinding.create HTTP (IPAddress.Parse "0.0.0.0") port ] }

let votes = System.Collections.Concurrent.ConcurrentBag<Vote> ()

let countVotes () : VotingResults =
  let vs =
    votes
    |> Seq.filter (fun v -> v.Name <> "" && v.Comment <> "")
    |> Seq.toArray

  let comments =
    vs
    |> Array.map (fun v -> v.Name, v.Comment)

  let scores =
    vs
    |> Array.countBy (fun v->v.Score)
    |> Map.ofArray

  { Comments = comments
    Scores   = scores }

let addVote (vote : Vote) : Async<VotingResults> =
  async {
    do votes.Add vote
    do! Async.Sleep 500
    return countVotes()
  }


let getResults () : Async<VotingResults> =
  async {
    do! Async.Sleep 500
    return countVotes()
  }

let getInitCounter () : Async<Counter> = async { return 42 }

let init : WebPart = 
  let votingProcotol = 
    { getResults = getResults
      addVote    = addVote }
  // creates a WebPart for the given implementation
  FableSuaveAdapter.webPartWithBuilderFor votingProcotol Route.builder

let webPart =
  choose [
    init
    Filters.path "/" >=> Files.browseFileHome "index.html"
    Files.browseHome
    RequestErrors.NOT_FOUND "Not found!"
  ]

startWebServer config webPart