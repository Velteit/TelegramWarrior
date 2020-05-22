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

let tee f a =
  f a
  a

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

let onTaskList (arguments: Map<string, string list>)  context =
  monad' {
    let! tasks = getTasks()
    let! message = context.Update.Message;
    // let! text = message.Text;

    let tagsOpt =
      arguments
      |> Map.tryFind "tags"
      |> Option.map (fun tags -> tags |> List.map (fun tag -> tag.Substring(1)))

    // TODO lenses
    tasks
    |> List.where (fun t -> t.Status |> Option.map (fun s -> s = Status.Pending) |> Option.defaultValue false)
    |> (fun list ->
      tagsOpt
      |> tee (Option.iter (fun tags -> for tag in tags do printf "%s" tag))
      |> Option.map (fun tags ->
        list
        |> List.where (fun task -> task.Tags |> List.exists (fun t -> List.contains t tags))
      )
      |> Option.defaultValue list
    )
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


let group (group: Group) : string * string list =
  (group.Name, [for c in group.Captures -> c.Value])

let regexCmd (pattern: Regex) (handler: Map<string, string list> -> UpdateContext -> unit) (context: UpdateContext) =
  context.Update.Message
  |> Option.bind (fun message -> getTextForCommand context.Me message.Text)
  |> Option.map (fun cmd -> pattern.Matches cmd)
  |> Option.map (fun matches -> [for g in (matches |> Seq.collect (fun m -> m.Groups)) -> (g.Name, [for c in g.Captures -> c.Value])])
  |> Option.map (fun groups -> groups |> Map.ofList)
  |> Option.map (fun m -> handler m context)
  |> Option.isSome
  |> not

let onUpdate (context: UpdateContext) =
  processCommands context [
    cmd "/start" onStart
    cmd "/help" onHelp
    // regexCmd (Regex("/list (?<tags>\+[a-z]*) (?<>)", RegexOptions.Compiled)) (onTaskList)
    regexCmd (Regex("/list( ?(?<tags>\\+[a-z]*) ?)+( ?due:(?<due>[0-9][0-9]))?", RegexOptions.Compiled)) (onTaskList)
    cmd "/list" (onTaskList Map.empty)
  ]
  |> ignore


[<EntryPoint>]
let main argv =
  printfn "Starting bot"
  startBot { defaultConfig with Token = "763014420:AAG2BE6MOQR5T6l3n5gwutDOfEZmyGCHUIw" } (onUpdate) None
  |> Async.RunSynchronously
  |> ignore

  0 // return an integer exit code
