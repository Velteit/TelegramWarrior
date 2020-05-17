// vim: noai:ts=2:sw=2

open System
open System.Globalization
open System.Collections.Generic
open Funogram.Bot
open Funogram.Api
open FSharpPlus
open Thoth.Json.Net
open TelegramWarrrior.Model
open Utils


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

let onTaskList context =
  monad' {
    let! message = context.Update.Message;
    let! text = message.Text;

    printfn "%s" text

    let printMessage _ (e: DataReceivedEventArgs) =
      printfn "%s" e.Data
      //e.Data
      //|> sendMessage message.Chat.Id
      //|> api context.Config
      //|> Async.Ignore
      //|> Async.Start
    ()
//ex.Message
//    |> sendMessage message.Chat.Id
//    |> api context.Config
//    |> Async.Ignore
//    |> Async.Start
  }
  |> ignore


let onUpdate (context: UpdateContext) =
  processCommands context [
    cmd "/hello" onHello
    cmd "/start" onStart
    cmd "/help" onHelp
    cmd "/task" onTask
    cmd "/task list" onTaskList
  ]
  |> ignore


[<EntryPoint>]
let main argv =
  let tasks = List<Task>();
  Process.run ["+work"; "export"] [(fun _ args -> if (not << String.IsNullOrEmpty) args.Data then Decode.fromString Task.Decoder args.Data |> (function | Ok(task) -> tasks.Add(task) | Error(e) -> printf "%s" e))] (fun ex -> printfn "%s" ex.Message)

  for task in tasks do
    printfn "%d %s " task.Id (task.Description |> Option.defaultValue "")

  let tags = tasks |> Seq.collect (fun task -> task.Tags) |> Seq.distinct

  for tag in tags do
    printfn "%s" tag
  for task in tasks do
    for (key,value) in task.UDAs do
      printfn "%s: %s" key value
  // startBot { defaultConfig with Token = "763014420:AAG2BE6MOQR5T6l3n5gwutDOfEZmyGCHUIw" } onUpdate None
  // |> Async.RunSynchronously
  // |> ignore

  0 // return an integer exit code
