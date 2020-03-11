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

  let tuples (except: string list) (decoder: Decoder<'t>): Decoder<(string * 't) list> =
    fun path value ->
      let json = (value :?> JObject)
      let properties = [
        for property in json.Properties() do
          if (not (List.contains property.Name except)) then
            yield (property.Name, property.Value)
      ]

      let rec gen (result: (string * 't) list) = function
        | (key, value)::tail ->
          match decoder (sprintf "%s.%s" path key) value with
          | Ok(e) ->  gen ((key, e)::result) tail
          | _ -> gen result tail
        | [] -> result

      properties
      |> gen []
      |> Result.Ok




let (|Regex|_|) pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
  else None

type Prioriy =
  | H
  | M
  | L
  static member Decoder : Decoder<Prioriy> =
    fun path value ->
      if (Decode.isString value) then
        match (Decode.asString >> String.toUpper) value with
        | "H" -> Ok Prioriy.H
        | "M" -> Ok Prioriy.M
        | "L" -> Ok Prioriy.L
        | _ -> (path, BadPrimitive("a priority", value)) |> Error
      else
        (path, BadPrimitive("a priority", value)) |> Error


type Status =
  | Pending
  | Deleted
  | Completed
  | Waiting
  | Recurring
  static member Decoder : Decoder<Status> =
    fun path value ->
      if (Decode.isString value) then
        match (Decode.asString >> String.toLower) value with
        | "pending" -> Ok Status.Pending
        | "deleted" -> Ok Status.Deleted
        | "completed" -> Ok Status.Completed
        | "waiting" -> Ok Status.Waiting
        | "recurring" -> Ok Status.Recurring
        | _ -> (path, BadPrimitive("a priority", value)) |> Error
      else
        (path, BadPrimitive("a priority", value)) |> Error

type Annotation =
  { Entry: DateTime;
    Description: string; }
  static member Decoder: Decoder<Annotation> =
    Decode.object
      (fun get ->
        { Entry = get.Required.Field "entry" (Decode.customDateTime "yyyyMMddTHHmmssK")
          Description = get.Required.Field "description" Decode.string  })

let exceptNames = ["id"; "urgency"; "uuid"; "entry"; "due"; "end"; "start"; "until"; "wait"; "modified"; "scheduled"; "project"; "description"; "mask"; "tags"; "priority"; "status"; "annotations"]

type Task =
  { Id: int32;
    Uid: Guid;
    Due: DateTime option;
    End: DateTime option;
    Start: DateTime option;
    Until: DateTime option;
    Scheduled: DateTime option;
    Wait: DateTime option;
    Modified: DateTime option;
    Entry: DateTime;
    Priority: Prioriy option;
    Project: string option;
    Tags: string list;
    Description: string option;
    Status: Status option
    Urgency: float;
    Annotations: Annotation list;
    Mask: string option;
    UDAs: (string * string) list; }
  static member Decoder : Decoder<Task> =
    Decode.object
      (fun get ->
        { Id = get.Required.Field "id" Decode.int;
          Urgency = get.Required.Field "urgency" Decode.float;
          Uid = get.Required.Field "uuid" Decode.guid;
          Entry = get.Required.Field "entry" (Decode.customDateTime "yyyyMMddTHHmmssK");
          Due = get.Optional.Field "due" (Decode.customDateTime "yyyyMMddTHHmmssK");
          End = get.Optional.Field "end" (Decode.customDateTime "yyyyMMddTHHmmssK");
          Start = get.Optional.Field "start" (Decode.customDateTime "yyyyMMddTHHmmssK");
          Until = get.Optional.Field "until" (Decode.customDateTime "yyyyMMddTHHmmssK");
          Wait = get.Optional.Field "wait" (Decode.customDateTime "yyyyMMddTHHmmssK");
          Modified = get.Optional.Field "modified" (Decode.customDateTime "yyyyMMddTHHmmssK");
          Scheduled = get.Optional.Field "scheduled" (Decode.customDateTime "yyyyMMddTHHmmssK");
          Project = get.Optional.Field "project" Decode.string;
          Description = get.Optional.Field "description" Decode.string;
          Mask = get.Optional.Field "mask" Decode.string;
          Tags = get.Optional.Field "tags" (Decode.list Decode.string)
                 |> Option.defaultValue [];
          Priority = get.Optional.Field "priority" (Prioriy.Decoder);
          Status = get.Optional.Field "status" Status.Decoder;
          Annotations = get.Optional.Field "annotations" (Decode.list Annotation.Decoder)
                        |> Option.defaultValue [];
          UDAs = get.Required.Raw (Decode.tuples exceptNames Decode.string) ;  })

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
  taskExport ["+work"] [(fun _ args -> if (not << String.IsNullOrEmpty) args.Data then Decode.fromString Task.Decoder args.Data |> (function | Ok(task) -> tasks.Add(task) | Error(e) -> printf "%s" e))] (fun ex -> printfn "%s" ex.Message)

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
