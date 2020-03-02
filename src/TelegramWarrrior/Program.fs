// vim: noai:ts=2:sw=2

open System
open System.Diagnostics
open Funogram.Bot
open Funogram.Api
open FSharpPlus
open System.Text.RegularExpressions
open System.Text.RegularExpressions
open Newtonsoft.Json

let (|Regex|_|) pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
  else None

type Prioriy =
  | H
  | M
  | L

type Task =
  { Id: int32;
    Age: string;
    Priority: Prioriy option;
    Project: string option;
    Tags: string list;
    Description: string option;
    Urgency: float;
    UDAs: string list }
  member _.FromJson str =
    use reader = new JsonTextReader(str)

    reader.Read();



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
  taskExport ["+work"] [(fun _ args -> printfn "%s" args.Data)] (fun ex -> printfn "%s" ex.Message)
  // startBot { defaultConfig with Token = "763014420:AAG2BE6MOQR5T6l3n5gwutDOfEZmyGCHUIw" } onUpdate None
  // |> Async.RunSynchronously
  // |> ignore

  0 // return an integer exit code
