module Client

open Elmish
open Elmish.React

open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Shared

open Fulma
open Fulma.Layouts
open Fulma.Elements
open Fulma.Elements.Form
open Fulma.Components
open Fulma.BulmaClasses

open Fulma.BulmaClasses.Bulma
open Fulma.BulmaClasses.Bulma.Properties
open Fulma.Extra.FontAwesome

type Model =
  { Comment : string 
    Name    : string 
    Score   : Score option
    Loading : bool
    Results : VotingResults option }

type Msg =
| SetComment of string
| SetName    of string
| SetScore   of Score
| Submit
| SeeScores
| GotResults of Result<VotingResults, exn>


module Server = 

  open Shared
  open Fable.Remoting.Client
  
  /// A proxy you can use to talk to server directly
  let api : IVotingProtocol = 
    Proxy.createWithBuilder<IVotingProtocol> Route.builder
    

let init () : Model * Cmd<Msg> =
  let model =
    { Comment = ""
      Name    = ""
      Score   = None
      Loading = false
      Results = None }
  let cmd =
    Cmd.none
  model, cmd

let mkVote (model : Model) : Vote =
  { Comment = model.Comment
    Name    = model.Name
    Score   = defaultArg model.Score Good }

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
  let model' =
    match msg with
    | SetComment comment -> { model with Comment = comment }
    | SetName    name    -> { model with Name    = name }
    | SetScore   score   -> { model with Score   = Some score }
    | SeeScores          -> { model with Loading = true }
    | Submit             -> { model with Loading = true }
    | GotResults (Ok r)  -> { model with Loading = false
                                         Results = Some r }
    | GotResults _       -> { model with Loading = false }

  let cmd =
    match msg with
    | Submit ->
      Cmd.ofAsync
        Server.api.addVote
        (mkVote model')
        (Ok >> GotResults)
        (Error >> GotResults)
    | SeeScores ->
      Cmd.ofAsync
        Server.api.getResults
        ()
        (Ok >> GotResults)
        (Error >> GotResults)
    | _ ->
      Cmd.none    

  model', cmd

let show = function
| Some x -> string x
| None -> "Loading..."

let navBrand =
  Navbar.Brand.div [ ] 
    [ Navbar.Item.a 
        [ Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
          Navbar.Item.IsActive true ] 
        [ img [ Src "https://safe-stack.github.io/images/safe_top.png"
                Alt "Logo" ] ] 
      Navbar.burger [ ] 
        [ span [ ] [ ]
          span [ ] [ ]
          span [ ] [ ] ] ]

let navMenu =
  Navbar.menu [ ]
    [ Navbar.End.div [ ] 
        [ Navbar.Item.a [ ] 
            [ str "Home" ] 
          Navbar.Item.a [ ]
            [ str "Examples" ]
          Navbar.Item.a [ ]
            [ str "Documentation" ]
          Navbar.Item.div [ ]
            [ Button.a 
                [ Button.Color IsWhite
                  Button.IsOutlined
                  Button.Size IsSmall
                  Button.Props [ Href "https://github.com/SAFE-Stack/SAFE-template" ] ] 
                [ Icon.faIcon [ ] 
                    [ Fa.icon Fa.I.Github; Fa.fw ]
                  span [ ] [ str "View Source" ] ] ] ] ]

let field input =
  Field.div [ ]
    [ Field.body [ ]
        [ input ] ]

let onChange action =
  OnChange (fun e -> action !!e.target?value)

let scoreIcon = function
| Good -> Fa.I.SmileO
| SoSo -> Fa.I.MehO
| Poor -> Fa.I.FrownO


let scoreColor (model : Model) (score : Score) =
  match model.Score with
  | None -> IsWhite
  | Some s when s <> score -> IsWhite
  | _ -> 
    match score with
    | Good -> IsSuccess
    | SoSo -> IsWarning
    | Poor -> IsDanger


let scores (model : Model) (dispatch : Msg -> unit) =
  let item score =
    Level.item [ ]
      [ Button.a
          [ Button.Color (scoreColor model score)
            Button.Disabled model.Loading
            Button.OnClick (fun _ -> dispatch (SetScore score)) ] 
          [ Icon.faIcon [ ]
              [ Fa.icon (scoreIcon score)
                Fa.fa2x ] ]]

  Level.level [ Level.Level.IsMobile ]
    [ item Good
      item SoSo
      item Poor ]

let comment (model : Model) (dispatch : Msg -> unit) =
  Textarea.textarea
    [ Textarea.Placeholder "Comment"
      Textarea.DefaultValue model.Comment
      Textarea.Disabled model.Loading
      
      Textarea.Props [ onChange (SetComment >> dispatch) ] ]
    [ ]


let name (model : Model) (dispatch : Msg -> unit) =
  Input.text
    [ Input.Placeholder "Name"
      Input.DefaultValue model.Name
      Input.Disabled model.Loading
      Input.Props [ onChange (SetName >> dispatch) ] ]

let submit (model : Model) (dispatch : Msg -> unit) =
  Button.a
    [ Button.Color IsPrimary
      Button.IsFullwidth
      Button.OnClick (fun _ -> dispatch Submit)
      Button.IsLoading model.Loading ]
    [ str "Submit" ]


let see (model : Model) (dispatch : Msg -> unit) =
  Button.a
    [ Button.Color IsLight
      Button.IsFullwidth
      Button.OnClick (fun _ -> dispatch SeeScores)
      Button.IsLoading model.Loading ]
    [ str "See scores" ]



let formBox (model : Model) (dispatch : Msg -> unit) =
  Box.box' [ ]
    [ field (scores  model dispatch)
      field (comment model dispatch)
      field (name    model dispatch)
      field (submit  model dispatch)
      field (see     model dispatch) ]


let resultsBox (model : VotingResults) (dispatch : Msg -> unit) =
  let item score =
    let count = defaultArg (Map.tryFind score model.Scores) 0
    Level.item [ ]
      [ div
          [ ] 
          [ Icon.faIcon [ ]
              [ Fa.icon (scoreIcon score)
                Fa.fa2x ]
            h2 [ ] [ str (string count)] ]]

  Box.box' [ ]
    [ Level.level [ Level.Level.IsMobile ]
        [ item Good
          item SoSo
          item Poor ]
      Content.content [ Content.Size IsSmall ]
        [ h3 [ ] [ str "Comments" ]
          ol [ Style [ TextAlign "left" ] ]
            [ for (name, comment) in model.Comments -> 
                li [ ]
                  [ i [ ] [ str (sprintf "'%s'" comment)]
                    str (sprintf " - %s" name) ] ] ] ]


let containerBox (model : Model) (dispatch : Msg -> unit) =
  match model.Results with
  | None -> formBox model dispatch
  | Some r -> resultsBox r dispatch

let imgSrc = "https://res.cloudinary.com/skillsmatter/image/upload/c_fill,w_300,h_300,g_north_west/v1519237242/jtvg3oimrx6qdxwofs4a.png"

let view (model : Model) (dispatch : Msg -> unit) =
  Hero.hero [ Hero.Color IsPrimary; Hero.IsFullHeight ] 
    [ Hero.head [ ] 
        [ Navbar.navbar [ ]
            [ Container.container [ ]
                [ navBrand
                  navMenu ] ] ]
      
      Hero.body [ ] 
        [ Container.container [ Container.CustomClass Alignment.HasTextCentered ]
            [ Column.column 
                [ Column.Width (Column.All, Column.Is6)
                  Column.Offset (Column.All, Column.Is3) ]
                [ Level.level [ ]
                    [ Level.item [ ]
                        [ Image.image [ Image.Is128x128 ]
                            [ img 
                                [ Src imgSrc
                                  Style [ Border "2px solid" ] ] ] ] ]
                  h1 [ ClassName "title" ] 
                    [ str "SAFE Demo" ]
                  div [ ClassName "subtitle" ]
                    [ str "Score my talk @F# eXchange '18" ]
                  containerBox model dispatch ] ] ] ]

  
#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
