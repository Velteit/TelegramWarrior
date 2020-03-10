// vim: noai:ts=2:sw=2

open System
open System.Globalization
open System.Collections.Generic
open System.Diagnostics
open Funogram.Bot
open Funogram.Api
open FSharpPlus
open System.Text.RegularExpressions
open Thoth.Json.Net

module Decode =
  open Newtonsoft.Json
  open Newtonsoft.Json.Linq

  let inline isString (token: JsonValue) = not(isNull token) && token.Type = JTokenType.String
  let inline asString (token: JsonValue): string = token.Value<string>()

  let customDateTime (pattern: string) : Decoder<DateTime> =
    fun path value ->
      if (isString value) then
        match DateTime.TryParseExact(asString value, pattern, CultureInfo.InvariantCulture, DateTimeStyles.None) with
        | true, x -> x.ToUniversalTime() |> Ok
        | _ -> (path, BadPrimitive("a datetime", value)) |> Error
      else
        (path, BadPrimitive("a datetime", value)) |> Error



let (|Regex|_|) pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
  else None

type Prioriy =
  | H
  | M
  | L
  // static member Decode : Decoder<Prioriy> =
  //   Decode.fromString

type Status =
  | Active
  | Deleted
  | Completed



type Task =
  { Id: int32;
    Due: DateTime option;
    End: DateTime option;
    Entry: DateTime;
    Priority: Prioriy option;
    Project: string option;
    Tags: string list;
    Description: string option;
    Status: Status option
    Urgency: float;
    UDAs: (string * string) list }
  static member Decoder : Decoder<Task> =
    Decode.object
      (fun get ->
        { Id = get.Required.Field "id" Decode.int
          Due = get.Optional.Field "due" (Decode.customDateTime "yyyyMMddTHHmmssK")
          End = get.Optional.Field "end" (Decode.customDateTime "yyyyMMddTHHmmssK")
          Entry = get.Required.Field "entry" (Decode.customDateTime "yyyyMMddTHHmmssK")
          Project = get.Optional.Field "project" Decode.string
          Description = get.Optional.Field "description" Decode.string
          Tags = get.Optional.Field "tags" (Decode.list Decode.string)
                |> Option.defaultValue []
          Urgency = get.Required.Field "urgency" Decode.float
          Priority = None
          Status = None
          UDAs = []  })

let taskExport (arguments: string list) (handlers: (obj -> DataReceivedEventArgs -> unit) list) (exceptionHandler: Exception -> unit) =
  let startInfo =
    ProcessStartInfo(
      RedirectStandardOutput = true,
      FileName = "task"
    );

  for argument in arguments do
    startInfo.ArgumentList.Add argument
  startInfo.ArgumentList.Add "export"

  let task = new Process(StartInfo = startInfo);

  for handler in handlers do
    task.OutputDataReceived.AddHandler(DataReceivedEventHandler (handler))

  try
    task.Start() |> ignore
  with | ex ->
    exceptionHandler ex

  task.BeginOutputReadLine();
  task.WaitForExit();

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
  taskExport ["+work"] [(fun _ args -> Decode.fromString Task.Decoder args.Data |> (function | Ok(task) -> tasks.Add(task) | Error(e) -> printf "%s" e))] (fun ex -> printfn "%s" ex.Message)
  for task in tasks do
    printfn "%d %s " task.Id (task.Description |> Option.defaultValue "")
  // startBot { defaultConfig with Token = "763014420:AAG2BE6MOQR5T6l3n5gwutDOfEZmyGCHUIw" } onUpdate None
  // |> Async.RunSynchronously
  // |> ignore

  0 // return an integer exit code
