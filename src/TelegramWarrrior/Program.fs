// vim: noai:ts=2:sw=2

module TelegramWarrrior.EntryPoint

open System
open Funogram.Bot
open Funogram.Api
open Thoth.Json.Net
open FSharpPlus
open TelegramWarrrior.Utils
open TelegramWarrrior.Model


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

let onTaskList (tasks: Task list) context =
  monad' {
    let! message = context.Update.Message;
    // let! text = message.Text;

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


let onUpdate (tasks: Task list) (context: UpdateContext) =
  processCommands context [
    cmd "/start" onStart
    cmd "/help" onHelp
    cmd "/list" (onTaskList tasks)
  ]
  |> ignore


[<EntryPoint>]
let main argv =
  let mutable tasks : Task list = [];
  Process.run
    "task"
    ["export"]
    [(fun _ args ->
        printfn "%s" args.Data
        if (not << String.IsNullOrEmpty) args.Data then Decode.fromString Task.Decoder args.Data |> (function | Ok(task) -> tasks <- task::tasks | Error(e) -> printf "%s" e))]
    (fun ex -> printfn "%s" ex.Message)

  startBot { defaultConfig with Token = "763014420:AAG2BE6MOQR5T6l3n5gwutDOfEZmyGCHUIw" } (onUpdate tasks) None
  |> Async.RunSynchronously
  |> ignore

  0 // return an integer exit code
