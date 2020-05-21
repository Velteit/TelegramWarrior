// vim: noai:ts=2:sw=2

module TelegramWarrrior.EntryPoint

open System
open Funogram.Api
open Funogram.Types
open Funogram.Telegram.Bot
open Funogram.Telegram.Api
open Funogram.Telegram.Types
open Thoth.Json.Net
open FSharpPlus
open TelegramWarrrior.Utils
open TelegramWarrrior.Model
open System.Text.RegularExpressions


let getTasks () =
  let mutable tasks : Task list = []
  let mutable hasResult: bool = true
  Process.run
    "task"
    ["export"]
    [(fun _ args ->
        // printfn "%s" args.Data
        if (not << String.IsNullOrEmpty) args.Data then Decode.fromString Task.Decoder args.Data |> (function | Ok(task) -> tasks <- task::tasks | Error(e) -> printf "%s" e))]
    (fun ex ->
        // printfn "%s" ex.Message
        hasResult <- false)
  if hasResult then Some(tasks) else None

let onHello context =
  monad' {
    let! message = context.Update.Message
    let! name = message.Chat.FirstName

    sprintf "Hello, %s!" name
    |> sendMessage message.Chat.Id
    |> api context.Config
    |> Async.Ignore
    |> Async.Start
  }
  |> ignore

let markup (task: Task) =
  sprintf """Id: %d
Description: %s
Tags: [%s]
""" (task.Id) (task.Description |> Option.defaultValue "") (String.Join(", ", task.Tags))

let onStart context =
  ()

let onHelp context =
  ()

let onTask context =
  monad' {
    let! message = context.Update.Message;
    let! text = message.Text;
    ()
  }
  |> ignore

let onTaskList (arguments: (string * string list) list)  context =
  monad' {
    let! tasks = getTasks()
    let! message = context.Update.Message;
    // let! text = message.Text;

    for arg in arguments do
      printf "%s: [" (fst arg)
      for value in (snd arg) do
        printf " %s" value
      printfn "]"

    // TODO lenses
    tasks
    |> List.where (fun t -> t.Status |> Option.map (fun s -> s = Status.Pending) |> Option.defaultValue false)
    |> List.sortBy (fun t -> t.Id)
    |> List.iter (
      fun task ->
        task
        |> markup
        |> sendMessage message.Chat.Id
        |> api context.Config
        |> Async.Ignore
        |> Async.Start
      )
  }
  |> ignore

let groups (m: Match) =
  [|for group in m.Groups -> group|]

let success (group: Group) =
  group.Success

let captureGetValue (c: Capture) =
  c.Value

let tee f a =
  f a
  a

let group (group: Group) : string * string list =
  (group.Name, [for c in group.Captures -> c.Value])

let regexCmd (pattern: Regex) (handler: (string * string list) list -> UpdateContext -> unit) (context: UpdateContext) =
  context.Update.Message
  |> Option.bind (fun message -> getTextForCommand context.Me message.Text)
  // |> tee (Option.iter (printfn "Message: %s"))
  |> Option.map (fun cmd -> pattern.Matches cmd)
  // |> tee (Option.iter (fun matches -> for m in matches do for g in m.Groups do for c in g.Captures do printfn "notice me! %s: %s" g.Name c.Value))
  // |> Option.map (Array.ofSeq)
  // |> Option.map (Array.collect groups)
  // |> Option.map (Array.filter success)
  // |> Option.map (Array.map group)
  // |> Option.map (Array.groupBy fst)
  // |> Option.map (Array.map (fun t -> (fst t, snd t |> Array.map snd |> Array.collect Array.ofList |> List.ofArray )))
  // |> Option.map (List.ofArray)
  |> Option.map (fun matches -> [for g in (matches |> Seq.collect (fun m -> m.Groups)) -> (g.Name, [for c in g.Captures -> c.Value])])
  |> Option.map (fun m -> handler m context)
  |> Option.isSome
  |> not

let onUpdate (context: UpdateContext) =
  processCommands context [
    cmd "/start" onStart
    cmd "/help" onHelp
    // regexCmd (Regex("/list (?<tags>\+[a-z]*) (?<>)", RegexOptions.Compiled)) (onTaskList)
    regexCmd (Regex("/list( ?(?<tags>\\+[a-z]*) ?)+( ?due:(?<due>[0-9][0-9]))", RegexOptions.Compiled)) (onTaskList)
    cmd "/list" (onTaskList [])
  ]
  |> ignore


[<EntryPoint>]
let main argv =
  printfn "Starting bot"
  startBot { defaultConfig with Token = "763014420:AAG2BE6MOQR5T6l3n5gwutDOfEZmyGCHUIw" } (onUpdate) None
  |> Async.RunSynchronously
  |> ignore

  0 // return an integer exit code
